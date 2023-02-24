// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression interface RE2.
//
// Originally the PCRE C++ wrapper, but adapted to use
// the new automata-based regular expression engines.

#include "re2/re2.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#ifdef _MSC_VER
#include <intrin.h>
#endif
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <atomic>
#include <iterator>
#include <mutex>
#include <string>
#include <utility>
#include <vector>

#include "util/util.h"
#include "util/logging.h"
#include "util/strutil.h"
#include "util/utf.h"
#include "re2/prog.h"
#include "re2/regexp.h"
#include "re2/sparse_array.h"

namespace re2 {

// Controls the maximum count permitted by GlobalReplace(); -1 is unlimited.
static int maximum_global_replace_count = -1;

void RE2::FUZZING_ONLY_set_maximum_global_replace_count(int i) {
  maximum_global_replace_count = i;
}

// Maximum number of args we can set
static const int kMaxArgs = 16;
static const int kVecSize = 1+kMaxArgs;

const int RE2::Options::kDefaultMaxMem;  // initialized in re2.h

RE2::Options::Options(RE2::CannedOptions opt)
  : max_mem_(kDefaultMaxMem),
    encoding_(opt == RE2::Latin1 ? EncodingLatin1 : EncodingUTF8),
    posix_syntax_(opt == RE2::POSIX),
    longest_match_(opt == RE2::POSIX),
    log_errors_(opt != RE2::Quiet),
    literal_(false),
    never_nl_(false),
    dot_nl_(false),
    never_capture_(false),
    case_sensitive_(true),
    perl_classes_(false),
    word_boundary_(false),
    one_line_(false) {
}

// Empty objects for use as const references.
// Statically allocating the storage and then
// lazily constructing the objects (in a once
// in RE2::Init()) avoids global constructors
// and the false positives (thanks, Valgrind)
// about memory leaks at program termination.
struct EmptyStorage {
  std::string empty_string;
  std::map<std::string, int> empty_named_groups;
  std::map<int, std::string> empty_group_names;
};
alignas(EmptyStorage) static char empty_storage[sizeof(EmptyStorage)];

static inline std::string* empty_string() {
  return &reinterpret_cast<EmptyStorage*>(empty_storage)->empty_string;
}

static inline std::map<std::string, int>* empty_named_groups() {
  return &reinterpret_cast<EmptyStorage*>(empty_storage)->empty_named_groups;
}

static inline std::map<int, std::string>* empty_group_names() {
  return &reinterpret_cast<EmptyStorage*>(empty_storage)->empty_group_names;
}

// Converts from Regexp error code to RE2 error code.
// Maybe some day they will diverge.  In any event, this
// hides the existence of Regexp from RE2 users.
static RE2::ErrorCode RegexpErrorToRE2(re2::RegexpStatusCode code) {
  switch (code) {
    case re2::kRegexpSuccess:
      return RE2::NoError;
    case re2::kRegexpInternalError:
      return RE2::ErrorInternal;
    case re2::kRegexpBadEscape:
      return RE2::ErrorBadEscape;
    case re2::kRegexpBadCharClass:
      return RE2::ErrorBadCharClass;
    case re2::kRegexpBadCharRange:
      return RE2::ErrorBadCharRange;
    case re2::kRegexpMissingBracket:
      return RE2::ErrorMissingBracket;
    case re2::kRegexpMissingParen:
      return RE2::ErrorMissingParen;
    case re2::kRegexpUnexpectedParen:
      return RE2::ErrorUnexpectedParen;
    case re2::kRegexpTrailingBackslash:
      return RE2::ErrorTrailingBackslash;
    case re2::kRegexpRepeatArgument:
      return RE2::ErrorRepeatArgument;
    case re2::kRegexpRepeatSize:
      return RE2::ErrorRepeatSize;
    case re2::kRegexpRepeatOp:
      return RE2::ErrorRepeatOp;
    case re2::kRegexpBadPerlOp:
      return RE2::ErrorBadPerlOp;
    case re2::kRegexpBadUTF8:
      return RE2::ErrorBadUTF8;
    case re2::kRegexpBadNamedCapture:
      return RE2::ErrorBadNamedCapture;
  }
  return RE2::ErrorInternal;
}

static std::string trunc(const StringPiece& pattern) {
  if (pattern.size() < 100)
    return std::string(pattern);
  return std::string(pattern.substr(0, 100)) + "...";
}


RE2::RE2(const char* pattern) {
  Init(pattern, DefaultOptions);
}

RE2::RE2(const std::string& pattern) {
  Init(pattern, DefaultOptions);
}

RE2::RE2(const StringPiece& pattern) {
  Init(pattern, DefaultOptions);
}

RE2::RE2(const StringPiece& pattern, const Options& options) {
  Init(pattern, options);
}

int RE2::Options::ParseFlags() const {
  int flags = Regexp::ClassNL;
  switch (encoding()) {
    default:
      if (log_errors())
        LOG(ERROR) << "Unknown encoding " << encoding();
      break;
    case RE2::Options::EncodingUTF8:
      break;
    case RE2::Options::EncodingLatin1:
      flags |= Regexp::Latin1;
      break;
  }

  if (!posix_syntax())
    flags |= Regexp::LikePerl;

  if (literal())
    flags |= Regexp::Literal;

  if (never_nl())
    flags |= Regexp::NeverNL;

  if (dot_nl())
    flags |= Regexp::DotNL;

  if (never_capture())
    flags |= Regexp::NeverCapture;

  if (!case_sensitive())
    flags |= Regexp::FoldCase;

  if (perl_classes())
    flags |= Regexp::PerlClasses;

  if (word_boundary())
    flags |= Regexp::PerlB;

  if (one_line())
    flags |= Regexp::OneLine;

  return flags;
}

void RE2::Init(const StringPiece& pattern, const Options& options) {
  static std::once_flag empty_once;
  std::call_once(empty_once, []() {
    (void) new (empty_storage) EmptyStorage;
  });

  pattern_ = new std::string(pattern);
  options_.Copy(options);
  entire_regexp_ = NULL;
  suffix_regexp_ = NULL;
  error_ = empty_string();
  error_arg_ = empty_string();

  num_captures_ = -1;
  error_code_ = NoError;
  longest_match_ = options_.longest_match();
  is_one_pass_ = false;
  prefix_foldcase_ = false;
  prefix_.clear();
  prog_ = NULL;

  rprog_ = NULL;
  named_groups_ = NULL;
  group_names_ = NULL;

  RegexpStatus status;
  entire_regexp_ = Regexp::Parse(
    *pattern_,
    static_cast<Regexp::ParseFlags>(options_.ParseFlags()),
    &status);
  if (entire_regexp_ == NULL) {
    if (options_.log_errors()) {
      LOG(ERROR) << "Error parsing '" << trunc(*pattern_) << "': "
                 << status.Text();
    }
    error_ = new std::string(status.Text());
    error_code_ = RegexpErrorToRE2(status.code());
    error_arg_ = new std::string(status.error_arg());
    return;
  }

  bool foldcase;
  re2::Regexp* suffix;
  if (entire_regexp_->RequiredPrefix(&prefix_, &foldcase, &suffix)) {
    prefix_foldcase_ = foldcase;
    suffix_regexp_ = suffix;
  }
  else {
    suffix_regexp_ = entire_regexp_->Incref();
  }

  // Two thirds of the memory goes to the forward Prog,
  // one third to the reverse prog, because the forward
  // Prog has two DFAs but the reverse prog has one.
  prog_ = suffix_regexp_->CompileToProg(options_.max_mem()*2/3);
  if (prog_ == NULL) {
    if (options_.log_errors())
      LOG(ERROR) << "Error compiling '" << trunc(*pattern_) << "'";
    error_ = new std::string("pattern too large - compile failed");
    error_code_ = RE2::ErrorPatternTooLarge;
    return;
  }

  // We used to compute this lazily, but it's used during the
  // typical control flow for a match call, so we now compute
  // it eagerly, which avoids the overhead of std::once_flag.
  num_captures_ = suffix_regexp_->NumCaptures();

  // Could delay this until the first match call that
  // cares about submatch information, but the one-pass
  // machine's memory gets cut from the DFA memory budget,
  // and that is harder to do if the DFA has already
  // been built.
  is_one_pass_ = prog_->IsOnePass();
}

// Returns rprog_, computing it if needed.
re2::Prog* RE2::ReverseProg() const {
  std::call_once(rprog_once_, [](const RE2* re) {
    re->rprog_ =
        re->suffix_regexp_->CompileToReverseProg(re->options_.max_mem() / 3);
    if (re->rprog_ == NULL) {
      if (re->options_.log_errors())
        LOG(ERROR) << "Error reverse compiling '" << trunc(*re->pattern_)
                   << "'";
      // We no longer touch error_ and error_code_ because failing to compile
      // the reverse Prog is not a showstopper: falling back to NFA execution
      // is fine. More importantly, an RE2 object is supposed to be logically
      // immutable: whatever ok() would have returned after Init() completed,
      // it should continue to return that no matter what ReverseProg() does.
    }
  }, this);
  return rprog_;
}

RE2::~RE2() {
  if (group_names_ != empty_group_names())
    delete group_names_;
  if (named_groups_ != empty_named_groups())
    delete named_groups_;
  delete rprog_;
  delete prog_;
  if (error_arg_ != empty_string())
    delete error_arg_;
  if (error_ != empty_string())
    delete error_;
  if (suffix_regexp_)
    suffix_regexp_->Decref();
  if (entire_regexp_)
    entire_regexp_->Decref();
  delete pattern_;
}

int RE2::ProgramSize() const {
  if (prog_ == NULL)
    return -1;
  return prog_->size();
}

int RE2::ReverseProgramSize() const {
  if (prog_ == NULL)
    return -1;
  Prog* prog = ReverseProg();
  if (prog == NULL)
    return -1;
  return prog->size();
}

// Finds the most significant non-zero bit in n.
static int FindMSBSet(uint32_t n) {
  DCHECK_NE(n, 0);
#if defined(__GNUC__)
  return 31 ^ __builtin_clz(n);
#elif defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
  unsigned long c;
  _BitScanReverse(&c, n);
  return static_cast<int>(c);
#else
  int c = 0;
  for (int shift = 1 << 4; shift != 0; shift >>= 1) {
    uint32_t word = n >> shift;
    if (word != 0) {
      n = word;
      c += shift;
    }
  }
  return c;
#endif
}

static int Fanout(Prog* prog, std::vector<int>* histogram) {
  SparseArray<int> fanout(prog->size());
  prog->Fanout(&fanout);
  int data[32] = {};
  int size = 0;
  for (SparseArray<int>::iterator i = fanout.begin(); i != fanout.end(); ++i) {
    if (i->value() == 0)
      continue;
    uint32_t value = i->value();
    int bucket = FindMSBSet(value);
    bucket += value & (value-1) ? 1 : 0;
    ++data[bucket];
    size = std::max(size, bucket+1);
  }
  if (histogram != NULL)
    histogram->assign(data, data+size);
  return size-1;
}

int RE2::ProgramFanout(std::vector<int>* histogram) const {
  if (prog_ == NULL)
    return -1;
  return Fanout(prog_, histogram);
}

int RE2::ReverseProgramFanout(std::vector<int>* histogram) const {
  if (prog_ == NULL)
    return -1;
  Prog* prog = ReverseProg();
  if (prog == NULL)
    return -1;
  return Fanout(prog, histogram);
}

// Returns named_groups_, computing it if needed.
const std::map<std::string, int>& RE2::NamedCapturingGroups() const {
  std::call_once(named_groups_once_, [](const RE2* re) {
    if (re->suffix_regexp_ != NULL)
      re->named_groups_ = re->suffix_regexp_->NamedCaptures();
    if (re->named_groups_ == NULL)
      re->named_groups_ = empty_named_groups();
  }, this);
  return *named_groups_;
}

// Returns group_names_, computing it if needed.
const std::map<int, std::string>& RE2::CapturingGroupNames() const {
  std::call_once(group_names_once_, [](const RE2* re) {
    if (re->suffix_regexp_ != NULL)
      re->group_names_ = re->suffix_regexp_->CaptureNames();
    if (re->group_names_ == NULL)
      re->group_names_ = empty_group_names();
  }, this);
  return *group_names_;
}

bool RE2::PossibleMatchRange(std::string* min, std::string* max,
                             int maxlen) const {
  if (prog_ == NULL)
    return false;

  int n = static_cast<int>(prefix_.size());
  if (n > maxlen)
    n = maxlen;

  // Determine initial min max from prefix_ literal.
  *min = prefix_.substr(0, n);
  *max = prefix_.substr(0, n);
  if (prefix_foldcase_) {
    // prefix is ASCII lowercase; change *min to uppercase.
    for (int i = 0; i < n; i++) {
      char& c = (*min)[i];
      if ('a' <= c && c <= 'z')
        c += 'A' - 'a';
    }
  }

  // Add to prefix min max using PossibleMatchRange on regexp.
  std::string dmin, dmax;
  maxlen -= n;
  if (maxlen > 0 && prog_->PossibleMatchRange(&dmin, &dmax, maxlen)) {
    min->append(dmin);
    max->append(dmax);
  } else if (!max->empty()) {
    // prog_->PossibleMatchRange has failed us,
    // but we still have useful information from prefix_.
    // Round up *max to allow any possible suffix.
    PrefixSuccessor(max);
  } else {
    // Nothing useful.
    *min = "";
    *max = "";
    return false;
  }

  return true;
}

// Avoid possible locale nonsense in standard strcasecmp.
// The string a is known to be all lowercase.
static int ascii_strcasecmp(const char* a, const char* b, size_t len) {
  const char* ae = a + len;

  for (; a < ae; a++, b++) {
    uint8_t x = *a;
    uint8_t y = *b;
    if ('A' <= y && y <= 'Z')
      y += 'a' - 'A';
    if (x != y)
      return x - y;
  }
  return 0;
}


// Checks that the rewrite string is well-formed with respect to this
// regular expression.
bool RE2::CheckRewriteString(const StringPiece& rewrite,
                             std::string* error) const {
  int max_token = -1;
  for (const char *s = rewrite.data(), *end = s + rewrite.size();
       s < end; s++) {
    int c = *s;
    if (c != '\\') {
      continue;
    }
    if (++s == end) {
      *error = "Rewrite schema error: '\\' not allowed at end.";
      return false;
    }
    c = *s;
    if (c == '\\') {
      continue;
    }
    if (!isdigit(c)) {
      *error = "Rewrite schema error: "
               "'\\' must be followed by a digit or '\\'.";
      return false;
    }
    int n = (c - '0');
    if (max_token < n) {
      max_token = n;
    }
  }

  if (max_token > NumberOfCapturingGroups()) {
    *error = StringPrintf(
        "Rewrite schema requests %d matches, but the regexp only has %d "
        "parenthesized subexpressions.",
        max_token, NumberOfCapturingGroups());
    return false;
  }
  return true;
}

// Returns the maximum submatch needed for the rewrite to be done by Replace().
// E.g. if rewrite == "foo \\2,\\1", returns 2.
int RE2::MaxSubmatch(const StringPiece& rewrite) {
  int max = 0;
  for (const char *s = rewrite.data(), *end = s + rewrite.size();
       s < end; s++) {
    if (*s == '\\') {
      s++;
      int c = (s < end) ? *s : -1;
      if (isdigit(c)) {
        int n = (c - '0');
        if (n > max)
          max = n;
      }
    }
  }
  return max;
}

// Append the "rewrite" string, with backslash subsitutions from "vec",
// to string "out".
bool RE2::Rewrite(std::string* out,
                  const StringPiece& rewrite,
                  const StringPiece* vec,
                  int veclen) const {
  for (const char *s = rewrite.data(), *end = s + rewrite.size();
       s < end; s++) {
    if (*s != '\\') {
      out->push_back(*s);
      continue;
    }
    s++;
    int c = (s < end) ? *s : -1;
    if (isdigit(c)) {
      int n = (c - '0');
      if (n >= veclen) {
        if (options_.log_errors()) {
          LOG(ERROR) << "invalid substitution \\" << n
                     << " from " << veclen << " groups";
        }
        return false;
      }
      StringPiece snip = vec[n];
      if (!snip.empty())
        out->append(snip.data(), snip.size());
    } else if (c == '\\') {
      out->push_back('\\');
    } else {
      if (options_.log_errors())
        LOG(ERROR) << "invalid rewrite pattern: " << rewrite.data();
      return false;
    }
  }
  return true;
}

/***** Parsers for various types *****/

namespace re2_internal {

template <>
bool Parse(const char* str, size_t n, void* dest) {
  // We fail if somebody asked us to store into a non-NULL void* pointer
  return (dest == NULL);
}

template <>
bool Parse(const char* str, size_t n, std::string* dest) {
  if (dest == NULL) return true;
  dest->assign(str, n);
  return true;
}

template <>
bool Parse(const char* str, size_t n, StringPiece* dest) {
  if (dest == NULL) return true;
  *dest = StringPiece(str, n);
  return true;
}

template <>
bool Parse(const char* str, size_t n, char* dest) {
  if (n != 1) return false;
  if (dest == NULL) return true;
  *dest = str[0];
  return true;
}

template <>
bool Parse(const char* str, size_t n, signed char* dest) {
  if (n != 1) return false;
  if (dest == NULL) return true;
  *dest = str[0];
  return true;
}

template <>
bool Parse(const char* str, size_t n, unsigned char* dest) {
  if (n != 1) return false;
  if (dest == NULL) return true;
  *dest = str[0];
  return true;
}

// Largest number spec that we are willing to parse
static const int kMaxNumberLength = 32;

// REQUIRES "buf" must have length at least nbuf.
// Copies "str" into "buf" and null-terminates.
// Overwrites *np with the new length.
static const char* TerminateNumber(char* buf, size_t nbuf, const char* str,
                                   size_t* np, bool accept_spaces) {
  size_t n = *np;
  if (n == 0) return "";
  if (n > 0 && isspace(*str)) {
    // We are less forgiving than the strtoxxx() routines and do not
    // allow leading spaces. We do allow leading spaces for floats.
    if (!accept_spaces) {
      return "";
    }
    while (n > 0 && isspace(*str)) {
      n--;
      str++;
    }
  }

  // Although buf has a fixed maximum size, we can still handle
  // arbitrarily large integers correctly by omitting leading zeros.
  // (Numbers that are still too long will be out of range.)
  // Before deciding whether str is too long,
  // remove leading zeros with s/000+/00/.
  // Leaving the leading two zeros in place means that
  // we don't change 0000x123 (invalid) into 0x123 (valid).
  // Skip over leading - before replacing.
  bool neg = false;
  if (n >= 1 && str[0] == '-') {
    neg = true;
    n--;
    str++;
  }

  if (n >= 3 && str[0] == '0' && str[1] == '0') {
    while (n >= 3 && str[2] == '0') {
      n--;
      str++;
    }
  }

  if (neg) {  // make room in buf for -
    n++;
    str--;
  }

  if (n > nbuf-1) return "";

  memmove(buf, str, n);
  if (neg) {
    buf[0] = '-';
  }
  buf[n] = '\0';
  *np = n;
  return buf;
}

template <>
bool Parse(const char* str, size_t n, float* dest) {
  if (n == 0) return false;
  static const int kMaxLength = 200;
  char buf[kMaxLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, true);
  char* end;
  errno = 0;
  float r = strtof(str, &end);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, double* dest) {
  if (n == 0) return false;
  static const int kMaxLength = 200;
  char buf[kMaxLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, true);
  char* end;
  errno = 0;
  double r = strtod(str, &end);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, long* dest, int radix) {
  if (n == 0) return false;
  char buf[kMaxNumberLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, false);
  char* end;
  errno = 0;
  long r = strtol(str, &end, radix);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, unsigned long* dest, int radix) {
  if (n == 0) return false;
  char buf[kMaxNumberLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, false);
  if (str[0] == '-') {
    // strtoul() will silently accept negative numbers and parse
    // them.  This module is more strict and treats them as errors.
    return false;
  }

  char* end;
  errno = 0;
  unsigned long r = strtoul(str, &end, radix);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, short* dest, int radix) {
  long r;
  if (!Parse(str, n, &r, radix)) return false;  // Could not parse
  if ((short)r != r) return false;              // Out of range
  if (dest == NULL) return true;
  *dest = (short)r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, unsigned short* dest, int radix) {
  unsigned long r;
  if (!Parse(str, n, &r, radix)) return false;  // Could not parse
  if ((unsigned short)r != r) return false;     // Out of range
  if (dest == NULL) return true;
  *dest = (unsigned short)r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, int* dest, int radix) {
  long r;
  if (!Parse(str, n, &r, radix)) return false;  // Could not parse
  if ((int)r != r) return false;                // Out of range
  if (dest == NULL) return true;
  *dest = (int)r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, unsigned int* dest, int radix) {
  unsigned long r;
  if (!Parse(str, n, &r, radix)) return false;  // Could not parse
  if ((unsigned int)r != r) return false;       // Out of range
  if (dest == NULL) return true;
  *dest = (unsigned int)r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, long long* dest, int radix) {
  if (n == 0) return false;
  char buf[kMaxNumberLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, false);
  char* end;
  errno = 0;
  long long r = strtoll(str, &end, radix);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

template <>
bool Parse(const char* str, size_t n, unsigned long long* dest, int radix) {
  if (n == 0) return false;
  char buf[kMaxNumberLength+1];
  str = TerminateNumber(buf, sizeof buf, str, &n, false);
  if (str[0] == '-') {
    // strtoull() will silently accept negative numbers and parse
    // them.  This module is more strict and treats them as errors.
    return false;
  }
  char* end;
  errno = 0;
  unsigned long long r = strtoull(str, &end, radix);
  if (end != str + n) return false;   // Leftover junk
  if (errno) return false;
  if (dest == NULL) return true;
  *dest = r;
  return true;
}

}  // namespace re2_internal

namespace hooks {

#ifdef RE2_HAVE_THREAD_LOCAL
thread_local const RE2* context = NULL;
#endif

template <typename T>
union Hook {
  void Store(T* cb) { cb_.store(cb, std::memory_order_release); }
  T* Load() const { return cb_.load(std::memory_order_acquire); }

#if !defined(__clang__) && defined(_MSC_VER)
  // Citing https://github.com/protocolbuffers/protobuf/pull/4777 as precedent,
  // this is a gross hack to make std::atomic<T*> constant-initialized on MSVC.
  static_assert(ATOMIC_POINTER_LOCK_FREE == 2,
                "std::atomic<T*> must be always lock-free");
  T* cb_for_constinit_;
#endif

  std::atomic<T*> cb_;
};

template <typename T>
static void DoNothing(const T&) {}

#define DEFINE_HOOK(type, name)                                       \
  static Hook<type##Callback> name##_hook = {{&DoNothing<type>}};     \
  void Set##type##Hook(type##Callback* cb) { name##_hook.Store(cb); } \
  type##Callback* Get##type##Hook() { return name##_hook.Load(); }

DEFINE_HOOK(DFAStateCacheReset, dfa_state_cache_reset)
DEFINE_HOOK(DFASearchFailure, dfa_search_failure)

#undef DEFINE_HOOK

}  // namespace hooks

}  // namespace re2
