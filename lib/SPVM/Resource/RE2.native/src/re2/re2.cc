#include <atomic>
#include <stack>
#include <stddef.h>
#include <stdint.h>
#include <algorithm>
#include <map>
#include <mutex>
#include <string>
#include <type_traits>
#include <vector>
#include <stddef.h>
#include <stdint.h>
#include <map>
#include <set>
#include <string>
#include <stddef.h>
#include <string.h>
#include <algorithm>
#include <iosfwd>
#include <iterator>
#include <string>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ostream>
#include <sstream>
#include <stdint.h>
#include <iosfwd>
#include <iterator>
#ifdef __cpp_lib_string_view
#include <string_view>
#endif
#ifdef RE2_NO_THREADS
#include <assert.h>
#define MUTEX_IS_LOCK_COUNTER
#else
#ifdef _WIN32
// Requires Windows Vista or Windows Server 2008 at minimum.
#include <windows.h>
#if defined(WINVER) && WINVER >= 0x0600
#define MUTEX_IS_WIN32_SRWLOCK
#endif
#else
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#include <unistd.h>
#if defined(_POSIX_READER_WRITER_LOCKS) && _POSIX_READER_WRITER_LOCKS > 0
#define MUTEX_IS_PTHREAD_RWLOCK
#endif
#endif
#endif

#if defined(MUTEX_IS_LOCK_COUNTER)
typedef int MutexType;
#elif defined(MUTEX_IS_WIN32_SRWLOCK)
typedef SRWLOCK MutexType;
#elif defined(MUTEX_IS_PTHREAD_RWLOCK)
#include <pthread.h>
#include <stdlib.h>
typedef pthread_rwlock_t MutexType;
#else
#include <mutex>
typedef std::mutex MutexType;
#endif

#if defined(__APPLE__)
#include <TargetConditionals.h>
#endif

// Copyright 2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef UTIL_LOGGING_H_
#define UTIL_LOGGING_H_

// Simplified version of Google's logging.

// Debug-only checking.
#define DCHECK(condition) assert(condition)
#define DCHECK_EQ(val1, val2) assert((val1) == (val2))
#define DCHECK_NE(val1, val2) assert((val1) != (val2))
#define DCHECK_LE(val1, val2) assert((val1) <= (val2))
#define DCHECK_LT(val1, val2) assert((val1) < (val2))
#define DCHECK_GE(val1, val2) assert((val1) >= (val2))
#define DCHECK_GT(val1, val2) assert((val1) > (val2))

// Always-on checking
#define CHECK(x)	if(x){}else LogMessageFatal(__FILE__, __LINE__).stream() << "Check failed: " #x
#define CHECK_LT(x, y)	CHECK((x) < (y))
#define CHECK_GT(x, y)	CHECK((x) > (y))
#define CHECK_LE(x, y)	CHECK((x) <= (y))
#define CHECK_GE(x, y)	CHECK((x) >= (y))
#define CHECK_EQ(x, y)	CHECK((x) == (y))
#define CHECK_NE(x, y)	CHECK((x) != (y))

#define LOG_INFO LogMessage(__FILE__, __LINE__)
#define LOG_WARNING LogMessage(__FILE__, __LINE__)
#define LOG_ERROR LogMessage(__FILE__, __LINE__)
#define LOG_FATAL LogMessageFatal(__FILE__, __LINE__)
#define LOG_QFATAL LOG_FATAL

// It seems that one of the Windows header files defines ERROR as 0.
#ifdef _WIN32
#define LOG_0 LOG_INFO
#endif

#ifdef NDEBUG
#define LOG_DFATAL LOG_ERROR
#else
#define LOG_DFATAL LOG_FATAL
#endif

#define LOG(severity) LOG_ ## severity.stream()

#define VLOG(x) if((x)>0){}else LOG_INFO.stream()

class LogMessage {
 public:
  LogMessage(const char* file, int line)
      : flushed_(false) {
    stream() << file << ":" << line << ": ";
  }
  void Flush() {
    stream() << "\n";
    std::string s = str_.str();
    size_t n = s.size();
    if (fwrite(s.data(), 1, n, stderr) < n) {}  // shut up gcc
    flushed_ = true;
  }
  ~LogMessage() {
    if (!flushed_) {
      Flush();
    }
  }
  std::ostream& stream() { return str_; }

 private:
  bool flushed_;
  std::ostringstream str_;

  LogMessage(const LogMessage&) = delete;
  LogMessage& operator=(const LogMessage&) = delete;
};

// Silence "destructor never returns" warning for ~LogMessageFatal().
// Since this is a header file, push and then pop to limit the scope.
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4722)
#endif

class LogMessageFatal : public LogMessage {
 public:
  LogMessageFatal(const char* file, int line)
      : LogMessage(file, line) {}
    ~LogMessageFatal() {
    Flush();
    abort();
  }
 private:
  LogMessageFatal(const LogMessageFatal&) = delete;
  LogMessageFatal& operator=(const LogMessageFatal&) = delete;
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif  // UTIL_LOGGING_H_

#ifndef UTIL_UTF_H_
#define UTIL_UTF_H_

namespace re2 {

typedef signed int Rune;	/* Code-point values in Unicode 4.0 are 21 bits wide.*/

enum
{
  UTFmax	= 4,		/* maximum bytes per rune */
  Runesync	= 0x80,		/* cannot represent part of a UTF sequence (<) */
  Runeself	= 0x80,		/* rune and UTF sequences are the same (<) */
  Runeerror	= 0xFFFD,	/* decoding error in UTF */
  Runemax	= 0x10FFFF,	/* maximum rune value */
};

int runetochar(char* s, const Rune* r);
int chartorune(Rune* r, const char* s);
int fullrune(const char* s, int n);
int utflen(const char* s);
char* utfrune(const char*, Rune);

}  // namespace re2

#endif  // UTIL_UTF_H_

// Copyright 2001-2010 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RE2_STRINGPIECE_H_
#define RE2_STRINGPIECE_H_

namespace re2 {

class StringPiece {
 public:
  typedef std::char_traits<char> traits_type;
  typedef char value_type;
  typedef char* pointer;
  typedef const char* const_pointer;
  typedef char& reference;
  typedef const char& const_reference;
  typedef const char* const_iterator;
  typedef const_iterator iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
  typedef const_reverse_iterator reverse_iterator;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  static const size_type npos = static_cast<size_type>(-1);

  // We provide non-explicit singleton constructors so users can pass
  // in a "const char*" or a "string" wherever a "StringPiece" is
  // expected.
  StringPiece()
      : data_(NULL), size_(0) {}
#ifdef __cpp_lib_string_view
  StringPiece(const std::string_view& str)
      : data_(str.data()), size_(str.size()) {}
#endif
  StringPiece(const std::string& str)
      : data_(str.data()), size_(str.size()) {}
  StringPiece(const char* str)
      : data_(str), size_(str == NULL ? 0 : strlen(str)) {}
  StringPiece(const char* str, size_type len)
      : data_(str), size_(len) {}

  const_iterator begin() const { return data_; }
  const_iterator end() const { return data_ + size_; }
  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(data_ + size_);
  }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(data_);
  }

  size_type size() const { return size_; }
  size_type length() const { return size_; }
  bool empty() const { return size_ == 0; }

  const_reference operator[](size_type i) const { return data_[i]; }
  const_pointer data() const { return data_; }

  void remove_prefix(size_type n) {
    data_ += n;
    size_ -= n;
  }

  void remove_suffix(size_type n) {
    size_ -= n;
  }

  void set(const char* str) {
    data_ = str;
    size_ = str == NULL ? 0 : strlen(str);
  }

  void set(const char* str, size_type len) {
    data_ = str;
    size_ = len;
  }

#ifdef __cpp_lib_string_view
  // Converts to `std::basic_string_view`.
  operator std::basic_string_view<char, traits_type>() const {
    if (!data_) return {};
    return std::basic_string_view<char, traits_type>(data_, size_);
  }
#endif

  // Converts to `std::basic_string`.
  template <typename A>
  explicit operator std::basic_string<char, traits_type, A>() const {
    if (!data_) return {};
    return std::basic_string<char, traits_type, A>(data_, size_);
  }

  std::string as_string() const {
    return std::string(data_, size_);
  }

  // We also define ToString() here, since many other string-like
  // interfaces name the routine that converts to a C++ string
  // "ToString", and it's confusing to have the method that does that
  // for a StringPiece be called "as_string()".  We also leave the
  // "as_string()" method defined here for existing code.
  std::string ToString() const {
    return std::string(data_, size_);
  }

  void CopyToString(std::string* target) const {
    target->assign(data_, size_);
  }

  void AppendToString(std::string* target) const {
    target->append(data_, size_);
  }

  size_type copy(char* buf, size_type n, size_type pos = 0) const;
  StringPiece substr(size_type pos = 0, size_type n = npos) const;

  int compare(const StringPiece& x) const {
    size_type min_size = std::min(size(), x.size());
    if (min_size > 0) {
      int r = memcmp(data(), x.data(), min_size);
      if (r < 0) return -1;
      if (r > 0) return 1;
    }
    if (size() < x.size()) return -1;
    if (size() > x.size()) return 1;
    return 0;
  }

  // Does "this" start with "x"?
  bool starts_with(const StringPiece& x) const {
    return x.empty() ||
           (size() >= x.size() && memcmp(data(), x.data(), x.size()) == 0);
  }

  // Does "this" end with "x"?
  bool ends_with(const StringPiece& x) const {
    return x.empty() ||
           (size() >= x.size() &&
            memcmp(data() + (size() - x.size()), x.data(), x.size()) == 0);
  }

  bool contains(const StringPiece& s) const {
    return find(s) != npos;
  }

  size_type find(const StringPiece& s, size_type pos = 0) const;
  size_type find(char c, size_type pos = 0) const;
  size_type rfind(const StringPiece& s, size_type pos = npos) const;
  size_type rfind(char c, size_type pos = npos) const;

 private:
  const_pointer data_;
  size_type size_;
};

inline bool operator==(const StringPiece& x, const StringPiece& y) {
  StringPiece::size_type len = x.size();
  if (len != y.size()) return false;
  return x.data() == y.data() || len == 0 ||
         memcmp(x.data(), y.data(), len) == 0;
}

inline bool operator!=(const StringPiece& x, const StringPiece& y) {
  return !(x == y);
}

inline bool operator<(const StringPiece& x, const StringPiece& y) {
  StringPiece::size_type min_size = std::min(x.size(), y.size());
  int r = min_size == 0 ? 0 : memcmp(x.data(), y.data(), min_size);
  return (r < 0) || (r == 0 && x.size() < y.size());
}

inline bool operator>(const StringPiece& x, const StringPiece& y) {
  return y < x;
}

inline bool operator<=(const StringPiece& x, const StringPiece& y) {
  return !(x > y);
}

inline bool operator>=(const StringPiece& x, const StringPiece& y) {
  return !(x < y);
}

// Allow StringPiece to be logged.
std::ostream& operator<<(std::ostream& o, const StringPiece& p);

}  // namespace re2

#endif  // RE2_STRINGPIECE_H_


// Copyright 2006 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RE2_REGEXP_H_
#define RE2_REGEXP_H_


namespace re2 {

class Regexp {
 public:

  // Flags for parsing.  Can be ORed together.
  enum ParseFlags {
    NoParseFlags  = 0,
    FoldCase      = 1<<0,   // Fold case during matching (case-insensitive).
    Literal       = 1<<1,   // Treat s as literal string instead of a regexp.
    ClassNL       = 1<<2,   // Allow char classes like [^a-z] and \D and \s
                            // and [[:space:]] to match newline.
    DotNL         = 1<<3,   // Allow . to match newline.
    MatchNL       = ClassNL | DotNL,
    OneLine       = 1<<4,   // Treat ^ and $ as only matching at beginning and
                            // end of text, not around embedded newlines.
                            // (Perl's default)
    Latin1        = 1<<5,   // Regexp and text are in Latin1, not UTF-8.
    NonGreedy     = 1<<6,   // Repetition operators are non-greedy by default.
    PerlClasses   = 1<<7,   // Allow Perl character classes like \d.
    PerlB         = 1<<8,   // Allow Perl's \b and \B.
    PerlX         = 1<<9,   // Perl extensions:
                            //   non-capturing parens - (?: )
                            //   non-greedy operators - *? +? ?? {}?
                            //   flag edits - (?i) (?-i) (?i: )
                            //     i - FoldCase
                            //     m - !OneLine
                            //     s - DotNL
                            //     U - NonGreedy
                            //   line ends: \A \z
                            //   \Q and \E to disable/enable metacharacters
                            //   (?P<name>expr) for named captures
                            //   \C to match any single byte
    UnicodeGroups = 1<<10,  // Allow \p{Han} for Unicode Han group
                            //   and \P{Han} for its negation.
    NeverNL       = 1<<11,  // Never match NL, even if the regexp mentions
                            //   it explicitly.
    NeverCapture  = 1<<12,  // Parse all parens as non-capturing.

    // As close to Perl as we can get.
    LikePerl      = ClassNL | OneLine | PerlClasses | PerlB | PerlX |
                    UnicodeGroups,

    AllParseFlags = (1<<14)-1,
  };

  int nsub() { return nsub_; }
  bool simple() { return simple_ != 0; }
  ParseFlags parse_flags() { return static_cast<ParseFlags>(parse_flags_); }
  int Ref();  // For testing.

  Regexp** sub() {
    if(nsub_ <= 1)
      return &subone_;
    else
      return submany_;
  }

  // Increments reference count, returns object as convenience.
  Regexp* Incref();

  // Decrements reference count and deletes this object if count reaches 0.
  void Decref();

  // Returns a _new_ simplified version of the current regexp.
  // Does not edit the current regexp.
  // Caller must release return value with re->Decref().
  // Simplified means that counted repetition has been rewritten
  // into simpler terms and all Perl/POSIX features have been
  // removed.  The result will capture exactly the same
  // subexpressions the original did, unless formatted with ToString.
  Regexp* Simplify();
  friend class CoalesceWalker;
  friend class SimplifyWalker;

  // Returns the number of capturing groups in the regexp.
  int NumCaptures();
  friend class NumCapturesWalker;

  // Returns a map from names to capturing group indices,
  // or NULL if the regexp contains no named capture groups.
  // The caller is responsible for deleting the map.
  std::map<std::string, int>* NamedCaptures();

  // Returns a map from capturing group indices to capturing group
  // names or NULL if the regexp contains no named capture groups. The
  // caller is responsible for deleting the map.
  std::map<int, std::string>* CaptureNames();

  // Returns a string representation of the current regexp,
  // using as few parentheses as possible.
  std::string ToString();

  // Convenience functions.  They consume the passed reference,
  // so in many cases you should use, e.g., Plus(re->Incref(), flags).
  // They do not consume allocated arrays like subs or runes.
  static Regexp* Plus(Regexp* sub, ParseFlags flags);
  static Regexp* Star(Regexp* sub, ParseFlags flags);
  static Regexp* Quest(Regexp* sub, ParseFlags flags);
  static Regexp* Concat(Regexp** subs, int nsubs, ParseFlags flags);
  static Regexp* Alternate(Regexp** subs, int nsubs, ParseFlags flags);
  static Regexp* Capture(Regexp* sub, ParseFlags flags, int cap);
  static Regexp* Repeat(Regexp* sub, ParseFlags flags, int min, int max);
  static Regexp* HaveMatch(int match_id, ParseFlags flags);

  // Like Alternate but does not factor out common prefixes.
  static Regexp* AlternateNoFactor(Regexp** subs, int nsubs, ParseFlags flags);

  // Debugging function.  Returns string format for regexp
  // that makes structure clear.  Does NOT use regexp syntax.
  std::string Dump();

  // Helper traversal class, defined fully in walker-inl.h.
  template<typename T> class Walker;

  bool MimicsPCRE();

  // Benchmarking function.
  void NullWalk();

  // Whether every match of this regexp must be anchored and
  // begin with a non-empty fixed string (perhaps after ASCII
  // case-folding).  If so, returns the prefix and the sub-regexp that
  // follows it.
  // Callers should expect *prefix, *foldcase and *suffix to be "zeroed"
  // regardless of the return value.
  bool RequiredPrefix(std::string* prefix, bool* foldcase,
                      Regexp** suffix);

  // Whether every match of this regexp must be unanchored and
  // begin with a non-empty fixed string (perhaps after ASCII
  // case-folding).  If so, returns the prefix.
  // Callers should expect *prefix and *foldcase to be "zeroed"
  // regardless of the return value.
  bool RequiredPrefixForAccel(std::string* prefix, bool* foldcase);

  // Controls the maximum repeat count permitted by the parser.
  // FOR FUZZING ONLY.
  static void FUZZING_ONLY_set_maximum_repeat_count(int i);

 private:
  // Constructor allocates vectors as appropriate for operator.
  explicit Regexp(int op, ParseFlags parse_flags);

  // Use Decref() instead of delete to release Regexps.
  // This is private to catch deletes at compile time.
  ~Regexp();
  void Destroy();
  bool QuickDestroy();

  // Helpers for Parse.  Listed here so they can edit Regexps.
  class ParseState;

  friend class ParseState;

  // Helper for testing [sic].
  friend bool RegexpEqualTestingOnly(Regexp*, Regexp*);

  // Computes whether Regexp is already simple.
  bool ComputeSimple();

  // Constructor that generates a Star, Plus or Quest,
  // squashing the pair if sub is also a Star, Plus or Quest.
  static Regexp* StarPlusOrQuest(int op, Regexp* sub, ParseFlags flags);

  // Constructor that generates a concatenation or alternation,
  // enforcing the limit on the number of subexpressions for
  // a particular Regexp.
  static Regexp* ConcatOrAlternate(int op, Regexp** subs, int nsubs,
                                   ParseFlags flags, bool can_factor);

  // Removes the first n leading runes from the beginning of re.
  // Edits re in place.
  static void RemoveLeadingString(Regexp* re, int n);

  // Returns the leading regexp in re's top-level concatenation.
  // The returned Regexp* points at re or a sub-expression of re,
  // so it must not be used after the caller calls re->Decref().
  static Regexp* LeadingRegexp(Regexp* re);

  // Removes LeadingRegexp(re) from re and returns the remainder.
  // Might edit re in place.
  static Regexp* RemoveLeadingRegexp(Regexp* re);

  // Simplifies an alternation of literal strings by factoring out
  // common prefixes.
  static int FactorAlternation(Regexp** sub, int nsub, ParseFlags flags);
  friend class FactorAlternationImpl;

  static bool Equal(Regexp* a, Regexp* b);

  // Allocate space for n sub-regexps.
  void AllocSub(int n) {
    DCHECK(n >= 0 && static_cast<uint16_t>(n) == n);
    if (n > 1)
      submany_ = new Regexp*[n];
    nsub_ = static_cast<uint16_t>(n);
  }

  // Swaps this with that, in place.
  void Swap(Regexp *that);

  // Operator.  See description of operators above.
  uint8_t op_;

  // Is this regexp structure already simple
  // (has it been returned by Simplify)?
  // uint8_t instead of bool to control space usage.
  uint8_t simple_;

  // Flags saved from parsing and used during execution.
  // (Only FoldCase is used.)
  // uint16_t instead of ParseFlags to control space usage.
  uint16_t parse_flags_;

  // Reference count.  Exists so that SimplifyRegexp can build
  // regexp structures that are dags rather than trees to avoid
  // exponential blowup in space requirements.
  // uint16_t to control space usage.
  // The standard regexp routines will never generate a
  // ref greater than the maximum repeat count (kMaxRepeat),
  // but even so, Incref and Decref consult an overflow map
  // when ref_ reaches kMaxRef.
  uint16_t ref_;
  static const uint16_t kMaxRef = 0xffff;

  // Subexpressions.
  // uint16_t to control space usage.
  // Concat and Alternate handle larger numbers of subexpressions
  // by building concatenation or alternation trees.
  // Other routines should call Concat or Alternate instead of
  // filling in sub() by hand.
  uint16_t nsub_;
  static const uint16_t kMaxNsub = 0xffff;
  union {
    Regexp** submany_;  // if nsub_ > 1
    Regexp* subone_;  // if nsub_ == 1
  };

  // Extra space for parse and teardown stacks.
  Regexp* down_;

  // Arguments to operator.  See description of operators above.
  union {
    struct {  // Repeat
      int max_;
      int min_;
    };
    struct {  // Capture
      int cap_;
      std::string* name_;
    };
    struct {  // LiteralString
      int nrunes_;
    };
    int match_id_;  // HaveMatch
    void *the_union_[2];  // as big as any other element, for memset
  };

  Regexp(const Regexp&) = delete;
  Regexp& operator=(const Regexp&) = delete;
};

}  // namespace re2

#endif  // RE2_REGEXP_H_

#ifndef UTIL_MUTEX_H_
#define UTIL_MUTEX_H_

/*
 * A simple mutex wrapper, supporting locks and read-write locks.
 * You should assume the locks are *not* re-entrant.
 */

namespace re2 {

class Mutex {
 public:
  inline Mutex();
  inline ~Mutex();
  inline void Lock();    // Block if needed until free then acquire exclusively
  inline void Unlock();  // Release a lock acquired via Lock()
  // Note that on systems that don't support read-write locks, these may
  // be implemented as synonyms to Lock() and Unlock().  So you can use
  // these for efficiency, but don't use them anyplace where being able
  // to do shared reads is necessary to avoid deadlock.
  inline void ReaderLock();   // Block until free or shared then acquire a share
  inline void ReaderUnlock(); // Release a read share of this Mutex
  inline void WriterLock() { Lock(); }     // Acquire an exclusive lock
  inline void WriterUnlock() { Unlock(); } // Release a lock from WriterLock()

 private:
  MutexType mutex_;

  // Catch the error of writing Mutex when intending MutexLock.
  Mutex(Mutex *ignored);

  Mutex(const Mutex&) = delete;
  Mutex& operator=(const Mutex&) = delete;
};

#if defined(MUTEX_IS_LOCK_COUNTER)

Mutex::Mutex()             : mutex_(0) { }
Mutex::~Mutex()            { assert(mutex_ == 0); }
void Mutex::Lock()         { assert(--mutex_ == -1); }
void Mutex::Unlock()       { assert(mutex_++ == -1); }
void Mutex::ReaderLock()   { assert(++mutex_ > 0); }
void Mutex::ReaderUnlock() { assert(mutex_-- > 0); }

#elif defined(MUTEX_IS_WIN32_SRWLOCK)

Mutex::Mutex()             : mutex_(SRWLOCK_INIT) { }
Mutex::~Mutex()            { }
void Mutex::Lock()         { AcquireSRWLockExclusive(&mutex_); }
void Mutex::Unlock()       { ReleaseSRWLockExclusive(&mutex_); }
void Mutex::ReaderLock()   { AcquireSRWLockShared(&mutex_); }
void Mutex::ReaderUnlock() { ReleaseSRWLockShared(&mutex_); }

#elif defined(MUTEX_IS_PTHREAD_RWLOCK)

#define SAFE_PTHREAD(fncall)    \
  do {                          \
    if ((fncall) != 0) abort(); \
  } while (0)

Mutex::Mutex()             { SAFE_PTHREAD(pthread_rwlock_init(&mutex_, NULL)); }
Mutex::~Mutex()            { SAFE_PTHREAD(pthread_rwlock_destroy(&mutex_)); }
void Mutex::Lock()         { SAFE_PTHREAD(pthread_rwlock_wrlock(&mutex_)); }
void Mutex::Unlock()       { SAFE_PTHREAD(pthread_rwlock_unlock(&mutex_)); }
void Mutex::ReaderLock()   { SAFE_PTHREAD(pthread_rwlock_rdlock(&mutex_)); }
void Mutex::ReaderUnlock() { SAFE_PTHREAD(pthread_rwlock_unlock(&mutex_)); }

#undef SAFE_PTHREAD

#else

Mutex::Mutex()             { }
Mutex::~Mutex()            { }
void Mutex::Lock()         { mutex_.lock(); }
void Mutex::Unlock()       { mutex_.unlock(); }
void Mutex::ReaderLock()   { Lock(); }  // C++11 doesn't have std::shared_mutex.
void Mutex::ReaderUnlock() { Unlock(); }
#endif

// --------------------------------------------------------------------------
// Some helper classes

// MutexLock(mu) acquires mu when constructed and releases it when destroyed.
class MutexLock {
 public:
  explicit MutexLock(Mutex *mu) : mu_(mu) { mu_->Lock(); }
  ~MutexLock() { mu_->Unlock(); }
 private:
  Mutex * const mu_;

  MutexLock(const MutexLock&) = delete;
  MutexLock& operator=(const MutexLock&) = delete;
};

// ReaderMutexLock and WriterMutexLock do the same, for rwlocks
class ReaderMutexLock {
 public:
  explicit ReaderMutexLock(Mutex *mu) : mu_(mu) { mu_->ReaderLock(); }
  ~ReaderMutexLock() { mu_->ReaderUnlock(); }
 private:
  Mutex * const mu_;

  ReaderMutexLock(const ReaderMutexLock&) = delete;
  ReaderMutexLock& operator=(const ReaderMutexLock&) = delete;
};

class WriterMutexLock {
 public:
  explicit WriterMutexLock(Mutex *mu) : mu_(mu) { mu_->WriterLock(); }
  ~WriterMutexLock() { mu_->WriterUnlock(); }
 private:
  Mutex * const mu_;

  WriterMutexLock(const WriterMutexLock&) = delete;
  WriterMutexLock& operator=(const WriterMutexLock&) = delete;
};

// Catch bug where variable name is omitted, e.g. MutexLock (&mu);
#define MutexLock(x) static_assert(false, "MutexLock declaration missing variable name")
#define ReaderMutexLock(x) static_assert(false, "ReaderMutexLock declaration missing variable name")
#define WriterMutexLock(x) static_assert(false, "WriterMutexLock declaration missing variable name")

}  // namespace re2

#endif  // UTIL_MUTEX_H_

namespace re2 {

Regexp::~Regexp() {
}

// Similar to EmptyStorage in re2.cc.
struct RefStorage {
  Mutex ref_mutex;
  std::map<Regexp*, int> ref_map;
};
alignas(RefStorage) static char ref_storage[sizeof(RefStorage)];

static inline Mutex* ref_mutex() {
  return &reinterpret_cast<RefStorage*>(ref_storage)->ref_mutex;
}

static inline std::map<Regexp*, int>* ref_map() {
  return &reinterpret_cast<RefStorage*>(ref_storage)->ref_map;
}

// Deletes this object; ref count has count reached 0.
void Regexp::Destroy() {

  // Handle recursive Destroy with explicit stack
  // to avoid arbitrarily deep recursion on process stack [sigh].
  down_ = NULL;
  Regexp* stack = this;
  while (stack != NULL) {
    Regexp* re = stack;
    stack = re->down_;
    if (re->ref_ != 0)
      LOG(DFATAL) << "Bad reference count " << re->ref_;
    if (re->nsub_ > 0) {
      Regexp** subs = re->sub();
      for (int i = 0; i < re->nsub_; i++) {
        Regexp* sub = subs[i];
        if (sub == NULL)
          continue;
        else
          --sub->ref_;
      }
      if (re->nsub_ > 1)
        delete[] subs;
      re->nsub_ = 0;
    }
    delete re;
  }
}

}  // namespace re2
