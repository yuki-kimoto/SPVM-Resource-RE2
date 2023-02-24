// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression interface RE2.
//
// Originally the PCRE C++ wrapper, but adapted to use
// the new automata-based regular expression engines.

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

#include "util/logging.h"
#include "re2/regexp.h"
#include "re2/stringpiece.h"

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



// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RE2_RE2_H_
#define RE2_RE2_H_

// C++ interface to the re2 regular-expression library.
// RE2 supports Perl-style regular expressions (with extensions like
// \d, \w, \s, ...).
//
// -----------------------------------------------------------------------
// REGEXP SYNTAX:
//
// This module uses the re2 library and hence supports
// its syntax for regular expressions, which is similar to Perl's with
// some of the more complicated things thrown away.  In particular,
// backreferences and generalized assertions are not available, nor is \Z.
//
// See https://github.com/google/re2/wiki/Syntax for the syntax
// supported by RE2, and a comparison with PCRE and PERL regexps.
//
// For those not familiar with Perl's regular expressions,
// here are some examples of the most commonly used extensions:
//
//   "hello (\\w+) world"  -- \w matches a "word" character
//   "version (\\d+)"      -- \d matches a digit
//   "hello\\s+world"      -- \s matches any whitespace character
//   "\\b(\\w+)\\b"        -- \b matches non-empty string at word boundary
//   "(?i)hello"           -- (?i) turns on case-insensitive matching
//   "/\\*(.*?)\\*/"       -- .*? matches . minimum no. of times possible
//
// The double backslashes are needed when writing C++ string literals.
// However, they should NOT be used when writing C++11 raw string literals:
//
//   R"(hello (\w+) world)"  -- \w matches a "word" character
//   R"(version (\d+))"      -- \d matches a digit
//   R"(hello\s+world)"      -- \s matches any whitespace character
//   R"(\b(\w+)\b)"          -- \b matches non-empty string at word boundary
//   R"((?i)hello)"          -- (?i) turns on case-insensitive matching
//   R"(/\*(.*?)\*/)"        -- .*? matches . minimum no. of times possible
//
// When using UTF-8 encoding, case-insensitive matching will perform
// simple case folding, not full case folding.
//
// -----------------------------------------------------------------------
// MATCHING INTERFACE:
//
// The "FullMatch" operation checks that supplied text matches a
// supplied pattern exactly.
//
// Example: successful match
//    CHECK(RE2::FullMatch("hello", "h.*o"));
//
// Example: unsuccessful match (requires full match):
//    CHECK(!RE2::FullMatch("hello", "e"));
//
// -----------------------------------------------------------------------
// UTF-8 AND THE MATCHING INTERFACE:
//
// By default, the pattern and input text are interpreted as UTF-8.
// The RE2::Latin1 option causes them to be interpreted as Latin-1.
//
// Example:
//    CHECK(RE2::FullMatch(utf8_string, RE2(utf8_pattern)));
//    CHECK(RE2::FullMatch(latin1_string, RE2(latin1_pattern, RE2::Latin1)));
//
// -----------------------------------------------------------------------
// SUBMATCH EXTRACTION:
//
// You can supply extra pointer arguments to extract submatches.
// On match failure, none of the pointees will have been modified.
// On match success, the submatches will be converted (as necessary) and
// their values will be assigned to their pointees until all conversions
// have succeeded or one conversion has failed.
// On conversion failure, the pointees will be in an indeterminate state
// because the caller has no way of knowing which conversion failed.
// However, conversion cannot fail for types like string and StringPiece
// that do not inspect the submatch contents. Hence, in the common case
// where all of the pointees are of such types, failure is always due to
// match failure and thus none of the pointees will have been modified.
//
// Example: extracts "ruby" into "s" and 1234 into "i"
//    int i;
//    std::string s;
//    CHECK(RE2::FullMatch("ruby:1234", "(\\w+):(\\d+)", &s, &i));
//
// Example: fails because string cannot be stored in integer
//    CHECK(!RE2::FullMatch("ruby", "(.*)", &i));
//
// Example: fails because there aren't enough sub-patterns
//    CHECK(!RE2::FullMatch("ruby:1234", "\\w+:\\d+", &s));
//
// Example: does not try to extract any extra sub-patterns
//    CHECK(RE2::FullMatch("ruby:1234", "(\\w+):(\\d+)", &s));
//
// Example: does not try to extract into NULL
//    CHECK(RE2::FullMatch("ruby:1234", "(\\w+):(\\d+)", NULL, &i));
//
// Example: integer overflow causes failure
//    CHECK(!RE2::FullMatch("ruby:1234567891234", "\\w+:(\\d+)", &i));
//
// NOTE(rsc): Asking for submatches slows successful matches quite a bit.
// This may get a little faster in the future, but right now is slower
// than PCRE.  On the other hand, failed matches run *very* fast (faster
// than PCRE), as do matches without submatch extraction.
//
// -----------------------------------------------------------------------
//
// -----------------------------------------------------------------------
// PRE-COMPILED REGULAR EXPRESSIONS
//
// RE2 makes it easy to use any string as a regular expression, without
// requiring a separate compilation step.
//
// If speed is of the essence, you can create a pre-compiled "RE2"
// object from the pattern and use it multiple times.  If you do so,
// you can typically parse text faster than with sscanf.
//
// Example: precompile pattern for faster matching:
//    RE2 pattern("h.*o");
//    while (ReadLine(&str)) {
//      if (RE2::FullMatch(str, pattern)) ...;
//    }
//
// -----------------------------------------------------------------------
// SCANNING TEXT INCREMENTALLY
//
// The "Consume" operation may be useful if you want to repeatedly
// match regular expressions at the front of a string and skip over
// them as they match.  This requires use of the "StringPiece" type,
// which represents a sub-range of a real string.
//
// Example: read lines of the form "var = value" from a string.
//      std::string contents = ...;     // Fill string somehow
//      StringPiece input(contents);    // Wrap a StringPiece around it
//
//      std::string var;
//      int value;
//      while (RE2::Consume(&input, "(\\w+) = (\\d+)\n", &var, &value)) {
//        ...;
//      }
//
// Each successful call to "Consume" will set "var/value", and also
// advance "input" so it points past the matched text.  Note that if the
// regular expression matches an empty string, input will advance
// by 0 bytes.  If the regular expression being used might match
// an empty string, the loop body must check for this case and either
// advance the string or break out of the loop.
//
// The "FindAndConsume" operation is similar to "Consume" but does not
// anchor your match at the beginning of the string.  For example, you
// could extract all words from a string by repeatedly calling
//     RE2::FindAndConsume(&input, "(\\w+)", &word)
//
// -----------------------------------------------------------------------
// USING VARIABLE NUMBER OF ARGUMENTS
//
// The above operations require you to know the number of arguments
// when you write the code.  This is not always possible or easy (for
// example, the regular expression may be calculated at run time).
// You can use the "N" version of the operations when the number of
// match arguments are determined at run time.
//
// Example:
//   const RE2::Arg* args[10];
//   int n;
//   // ... populate args with pointers to RE2::Arg values ...
//   // ... set n to the number of RE2::Arg objects ...
//   bool match = RE2::FullMatchN(input, pattern, args, n);
//
// The last statement is equivalent to
//
//   bool match = RE2::FullMatch(input, pattern,
//                               *args[0], *args[1], ..., *args[n - 1]);
//
// -----------------------------------------------------------------------
// PARSING HEX/OCTAL/C-RADIX NUMBERS
//
// By default, if you pass a pointer to a numeric value, the
// corresponding text is interpreted as a base-10 number.  You can
// instead wrap the pointer with a call to one of the operators Hex(),
// Octal(), or CRadix() to interpret the text in another base.  The
// CRadix operator interprets C-style "0" (base-8) and "0x" (base-16)
// prefixes, but defaults to base-10.
//
// Example:
//   int a, b, c, d;
//   CHECK(RE2::FullMatch("100 40 0100 0x40", "(.*) (.*) (.*) (.*)",
//         RE2::Octal(&a), RE2::Hex(&b), RE2::CRadix(&c), RE2::CRadix(&d));
// will leave 64 in a, b, c, and d.

namespace re2 {
class Prog;
class Regexp;
}  // namespace re2

namespace re2 {

// Interface for regular expression matching.  Also corresponds to a
// pre-compiled regular expression.  An "RE2" object is safe for
// concurrent use by multiple threads.
class RE2 {
 public:
  // We convert user-passed pointers into special Arg objects
  class Arg;
  class Options;

  // Defined in set.h.
  class Set;

  enum ErrorCode {
    NoError = 0,

    // Unexpected error
    ErrorInternal,

    // Parse errors
    ErrorBadEscape,          // bad escape sequence
    ErrorBadCharClass,       // bad character class
    ErrorBadCharRange,       // bad character class range
    ErrorMissingBracket,     // missing closing ]
    ErrorMissingParen,       // missing closing )
    ErrorUnexpectedParen,    // unexpected closing )
    ErrorTrailingBackslash,  // trailing \ at end of regexp
    ErrorRepeatArgument,     // repeat argument missing, e.g. "*"
    ErrorRepeatSize,         // bad repetition argument
    ErrorRepeatOp,           // bad repetition operator
    ErrorBadPerlOp,          // bad perl operator
    ErrorBadUTF8,            // invalid UTF-8 in regexp
    ErrorBadNamedCapture,    // bad named capture group
    ErrorPatternTooLarge     // pattern too large (compile failed)
  };

  // Predefined common options.
  // If you need more complicated things, instantiate
  // an Option class, possibly passing one of these to
  // the Option constructor, change the settings, and pass that
  // Option class to the RE2 constructor.
  enum CannedOptions {
    DefaultOptions = 0,
    Latin1, // treat input as Latin-1 (default UTF-8)
    POSIX, // POSIX syntax, leftmost-longest match
    Quiet // do not log about regexp parse errors
  };

  RE2(const char* pattern);
  RE2(const std::string& pattern);
  RE2(const StringPiece& pattern);
  RE2(const StringPiece& pattern, const Options& options);
  ~RE2();

  // Not copyable.
  // RE2 objects are expensive. You should probably use std::shared_ptr<RE2>
  // instead. If you really must copy, RE2(first.pattern(), first.options())
  // effectively does so: it produces a second object that mimics the first.
  RE2(const RE2&) = delete;
  RE2& operator=(const RE2&) = delete;
  // Not movable.
  // RE2 objects are thread-safe and logically immutable. You should probably
  // use std::unique_ptr<RE2> instead. Otherwise, consider std::deque<RE2> if
  // direct emplacement into a container is desired. If you really must move,
  // be prepared to submit a design document along with your feature request.
  RE2(RE2&&) = delete;
  RE2& operator=(RE2&&) = delete;

  // Returns whether RE2 was created properly.
  bool ok() const { return error_code() == NoError; }

  // The string specification for this RE2.  E.g.
  //   RE2 re("ab*c?d+");
  //   re.pattern();    // "ab*c?d+"
  const std::string& pattern() const { return *pattern_; }

  // If RE2 could not be created properly, returns an error string.
  // Else returns the empty string.
  const std::string& error() const { return *error_; }

  // If RE2 could not be created properly, returns an error code.
  // Else returns RE2::NoError (== 0).
  ErrorCode error_code() const { return error_code_; }

  // If RE2 could not be created properly, returns the offending
  // portion of the regexp.
  const std::string& error_arg() const { return *error_arg_; }

  // If histogram is not null, outputs the program fanout
  // as a histogram bucketed by powers of 2.
  // Returns the number of the largest non-empty bucket.
  int ProgramFanout(std::vector<int>* histogram) const;
  int ReverseProgramFanout(std::vector<int>* histogram) const;

  // Returns the underlying Regexp; not for general use.
  // Returns entire_regexp_ so that callers don't need
  // to know about prefix_ and prefix_foldcase_.
  re2::Regexp* Regexp() const { return entire_regexp_; }

 private:
  template <typename F, typename SP>
  static inline bool Apply(F f, SP sp, const RE2& re) {
    return f(sp, re, NULL, 0);
  }

  template <typename F, typename SP, typename... A>
  static inline bool Apply(F f, SP sp, const RE2& re, const A&... a) {
    const Arg* const args[] = {&a...};
    const int n = sizeof...(a);
    return f(sp, re, args, n);
  }

 public:
  // In order to allow FullMatch() et al. to be called with a varying number
  // of arguments of varying types, we use two layers of variadic templates.
  // The first layer constructs the temporary Arg objects. The second layer
  // (above) constructs the array of pointers to the temporary Arg objects.

  // Replace the first match of "re" in "str" with "rewrite".
  // Within "rewrite", backslash-escaped digits (\1 to \9) can be
  // used to insert text matching corresponding parenthesized group
  // from the pattern.  \0 in "rewrite" refers to the entire matching
  // text.  E.g.,
  //
  //   std::string s = "yabba dabba doo";
  //   CHECK(RE2::Replace(&s, "b+", "d"));
  //
  // will leave "s" containing "yada dabba doo"
  //
  // Returns true if the pattern matches and a replacement occurs,
  // false otherwise.
  static bool Replace(std::string* str,
                      const RE2& re,
                      const StringPiece& rewrite);

  // Like Replace(), except replaces successive non-overlapping occurrences
  // of the pattern in the string with the rewrite. E.g.
  //
  //   std::string s = "yabba dabba doo";
  //   CHECK(RE2::GlobalReplace(&s, "b+", "d"));
  //
  // will leave "s" containing "yada dada doo"
  // Replacements are not subject to re-matching.
  //
  // Because GlobalReplace only replaces non-overlapping matches,
  // replacing "ana" within "banana" makes only one replacement, not two.
  //
  // Returns the number of replacements made.
  static int GlobalReplace(std::string* str,
                           const RE2& re,
                           const StringPiece& rewrite);

  // Computes range for any strings matching regexp. The min and max can in
  // some cases be arbitrarily precise, so the caller gets to specify the
  // maximum desired length of string returned.
  //
  // Assuming PossibleMatchRange(&min, &max, N) returns successfully, any
  // string s that is an anchored match for this regexp satisfies
  //   min <= s && s <= max.
  //
  // Note that PossibleMatchRange() will only consider the first copy of an
  // infinitely repeated element (i.e., any regexp element followed by a '*' or
  // '+' operator). Regexps with "{N}" constructions are not affected, as those
  // do not compile down to infinite repetitions.
  //
  // Returns true on success, false on error.
  bool PossibleMatchRange(std::string* min, std::string* max,
                          int maxlen) const;

  // Generic matching interface

  // Type of match.
  enum Anchor {
    UNANCHORED,         // No anchoring
    ANCHOR_START,       // Anchor at start only
    ANCHOR_BOTH         // Anchor at start and end
  };

  // Return the number of capturing subpatterns, or -1 if the
  // regexp wasn't valid on construction.  The overall match ($0)
  // does not count: if the regexp is "(a)(b)", returns 2.
  int NumberOfCapturingGroups() const { return num_captures_; }

  // Return a map from names to capturing indices.
  // The map records the index of the leftmost group
  // with the given name.
  // Only valid until the re is deleted.
  const std::map<std::string, int>& NamedCapturingGroups() const;

  // Return a map from capturing indices to names.
  // The map has no entries for unnamed groups.
  // Only valid until the re is deleted.
  const std::map<int, std::string>& CapturingGroupNames() const;

  bool CheckRewriteString(const StringPiece& rewrite,
                          std::string* error) const;

  // Returns the maximum submatch needed for the rewrite to be done by
  // Replace(). E.g. if rewrite == "foo \\2,\\1", returns 2.
  static int MaxSubmatch(const StringPiece& rewrite);

  // Append the "rewrite" string, with backslash subsitutions from "vec",
  // to string "out".
  // Returns true on success.  This method can fail because of a malformed
  // rewrite string.  CheckRewriteString guarantees that the rewrite will
  // be sucessful.
  bool Rewrite(std::string* out,
               const StringPiece& rewrite,
               const StringPiece* vec,
               int veclen) const;

  // Constructor options
  class Options {
   public:
    // The options are (defaults in parentheses):
    //
    //   utf8             (true)  text and pattern are UTF-8; otherwise Latin-1
    //   posix_syntax     (false) restrict regexps to POSIX egrep syntax
    //   longest_match    (false) search for longest match, not first match
    //   log_errors       (true)  log syntax and execution errors to ERROR
    //   max_mem          (see below)  approx. max memory footprint of RE2
    //   literal          (false) interpret string as literal, not regexp
    //   never_nl         (false) never match \n, even if it is in regexp
    //   dot_nl           (false) dot matches everything including new line
    //   never_capture    (false) parse all parens as non-capturing
    //   case_sensitive   (true)  match is case-sensitive (regexp can override
    //                              with (?i) unless in posix_syntax mode)
    //
    // The following options are only consulted when posix_syntax == true.
    // When posix_syntax == false, these features are always enabled and
    // cannot be turned off; to perform multi-line matching in that case,
    // begin the regexp with (?m).
    //   perl_classes     (false) allow Perl's \d \s \w \D \S \W
    //   word_boundary    (false) allow Perl's \b \B (word boundary and not)
    //   one_line         (false) ^ and $ only match beginning and end of text
    //
    // The max_mem option controls how much memory can be used
    // to hold the compiled form of the regexp (the Prog) and
    // its cached DFA graphs.  Code Search placed limits on the number
    // of Prog instructions and DFA states: 10,000 for both.
    // In RE2, those limits would translate to about 240 KB per Prog
    // and perhaps 2.5 MB per DFA (DFA state sizes vary by regexp; RE2 does a
    // better job of keeping them small than Code Search did).
    // Each RE2 has two Progs (one forward, one reverse), and each Prog
    // can have two DFAs (one first match, one longest match).
    // That makes 4 DFAs:
    //
    //   forward, first-match    - used for UNANCHORED or ANCHOR_START searches
    //                               if opt.longest_match() == false
    //   forward, longest-match  - used for all ANCHOR_BOTH searches,
    //                               and the other two kinds if
    //                               opt.longest_match() == true
    //   reverse, first-match    - never used
    //   reverse, longest-match  - used as second phase for unanchored searches
    //
    // The RE2 memory budget is statically divided between the two
    // Progs and then the DFAs: two thirds to the forward Prog
    // and one third to the reverse Prog.  The forward Prog gives half
    // of what it has left over to each of its DFAs.  The reverse Prog
    // gives it all to its longest-match DFA.
    //
    // Once a DFA fills its budget, it flushes its cache and starts over.
    // If this happens too often, RE2 falls back on the NFA implementation.

    // For now, make the default budget something close to Code Search.
    static const int kDefaultMaxMem = 8<<20;

    enum Encoding {
      EncodingUTF8 = 1,
      EncodingLatin1
    };

    Options() :
      max_mem_(kDefaultMaxMem),
      encoding_(EncodingUTF8),
      posix_syntax_(false),
      longest_match_(false),
      log_errors_(true),
      literal_(false),
      never_nl_(false),
      dot_nl_(false),
      never_capture_(false),
      case_sensitive_(true),
      perl_classes_(false),
      word_boundary_(false),
      one_line_(false) {
    }

    /*implicit*/ Options(CannedOptions);

    int64_t max_mem() const { return max_mem_; }
    void set_max_mem(int64_t m) { max_mem_ = m; }

    Encoding encoding() const { return encoding_; }
    void set_encoding(Encoding encoding) { encoding_ = encoding; }

    bool posix_syntax() const { return posix_syntax_; }
    void set_posix_syntax(bool b) { posix_syntax_ = b; }

    bool longest_match() const { return longest_match_; }
    void set_longest_match(bool b) { longest_match_ = b; }

    bool log_errors() const { return log_errors_; }
    void set_log_errors(bool b) { log_errors_ = b; }

    bool literal() const { return literal_; }
    void set_literal(bool b) { literal_ = b; }

    bool never_nl() const { return never_nl_; }
    void set_never_nl(bool b) { never_nl_ = b; }

    bool dot_nl() const { return dot_nl_; }
    void set_dot_nl(bool b) { dot_nl_ = b; }

    bool never_capture() const { return never_capture_; }
    void set_never_capture(bool b) { never_capture_ = b; }

    bool case_sensitive() const { return case_sensitive_; }
    void set_case_sensitive(bool b) { case_sensitive_ = b; }

    bool perl_classes() const { return perl_classes_; }
    void set_perl_classes(bool b) { perl_classes_ = b; }

    bool word_boundary() const { return word_boundary_; }
    void set_word_boundary(bool b) { word_boundary_ = b; }

    bool one_line() const { return one_line_; }
    void set_one_line(bool b) { one_line_ = b; }

    void Copy(const Options& src) {
      *this = src;
    }

    int ParseFlags() const;

   private:
    int64_t max_mem_;
    Encoding encoding_;
    bool posix_syntax_;
    bool longest_match_;
    bool log_errors_;
    bool literal_;
    bool never_nl_;
    bool dot_nl_;
    bool never_capture_;
    bool case_sensitive_;
    bool perl_classes_;
    bool word_boundary_;
    bool one_line_;
  };

  // Returns the options set in the constructor.
  const Options& options() const { return options_; }

  // Argument converters; see below.
  template <typename T>
  static Arg CRadix(T* ptr);
  template <typename T>
  static Arg Hex(T* ptr);
  template <typename T>
  static Arg Octal(T* ptr);

  // Controls the maximum count permitted by GlobalReplace(); -1 is unlimited.
  // FOR FUZZING ONLY.
  static void FUZZING_ONLY_set_maximum_global_replace_count(int i);

 private:
  void Init(const StringPiece& pattern, const Options& options);

  re2::Prog* ReverseProg() const;

  // First cache line is relatively cold fields.
  const std::string* pattern_;    // string regular expression
  Options options_;               // option flags
  re2::Regexp* entire_regexp_;    // parsed regular expression
  re2::Regexp* suffix_regexp_;    // parsed regular expression, prefix_ removed
  const std::string* error_;      // error indicator (or points to empty string)
  const std::string* error_arg_;  // fragment of regexp showing error (or ditto)

  // Second cache line is relatively hot fields.
  // These are ordered oddly to pack everything.
  int num_captures_;              // number of capturing groups
  ErrorCode error_code_ : 29;     // error code (29 bits is more than enough)
  bool longest_match_ : 1;        // cached copy of options_.longest_match()
  bool is_one_pass_ : 1;          // can use prog_->SearchOnePass?
  bool prefix_foldcase_ : 1;      // prefix_ is ASCII case-insensitive
  std::string prefix_;            // required prefix (before suffix_regexp_)
  re2::Prog* prog_;               // compiled program for regexp

  // Reverse Prog for DFA execution only
  mutable re2::Prog* rprog_;
  // Map from capture names to indices
  mutable const std::map<std::string, int>* named_groups_;
  // Map from capture indices to names
  mutable const std::map<int, std::string>* group_names_;

  mutable std::once_flag rprog_once_;
  mutable std::once_flag named_groups_once_;
  mutable std::once_flag group_names_once_;
};

/***** Implementation details *****/

namespace re2_internal {

// Types for which the 3-ary Parse() function template has specializations.
template <typename T> struct Parse3ary : public std::false_type {};
template <> struct Parse3ary<void> : public std::true_type {};
template <> struct Parse3ary<std::string> : public std::true_type {};
template <> struct Parse3ary<StringPiece> : public std::true_type {};
template <> struct Parse3ary<char> : public std::true_type {};
template <> struct Parse3ary<signed char> : public std::true_type {};
template <> struct Parse3ary<unsigned char> : public std::true_type {};
template <> struct Parse3ary<float> : public std::true_type {};
template <> struct Parse3ary<double> : public std::true_type {};

template <typename T>
bool Parse(const char* str, size_t n, T* dest);

// Types for which the 4-ary Parse() function template has specializations.
template <typename T> struct Parse4ary : public std::false_type {};
template <> struct Parse4ary<long> : public std::true_type {};
template <> struct Parse4ary<unsigned long> : public std::true_type {};
template <> struct Parse4ary<short> : public std::true_type {};
template <> struct Parse4ary<unsigned short> : public std::true_type {};
template <> struct Parse4ary<int> : public std::true_type {};
template <> struct Parse4ary<unsigned int> : public std::true_type {};
template <> struct Parse4ary<long long> : public std::true_type {};
template <> struct Parse4ary<unsigned long long> : public std::true_type {};

template <typename T>
bool Parse(const char* str, size_t n, T* dest, int radix);

}  // namespace re2_internal

class RE2::Arg {
 private:
  template <typename T>
  using CanParse3ary = typename std::enable_if<
      re2_internal::Parse3ary<T>::value,
      int>::type;

  template <typename T>
  using CanParse4ary = typename std::enable_if<
      re2_internal::Parse4ary<T>::value,
      int>::type;

#if !defined(_MSC_VER)
  template <typename T>
  using CanParseFrom = typename std::enable_if<
      std::is_member_function_pointer<
          decltype(static_cast<bool (T::*)(const char*, size_t)>(
              &T::ParseFrom))>::value,
      int>::type;
#endif

 public:
  Arg() : Arg(nullptr) {}
  Arg(std::nullptr_t ptr) : arg_(ptr), parser_(DoNothing) {}

  template <typename T, CanParse3ary<T> = 0>
  Arg(T* ptr) : arg_(ptr), parser_(DoParse3ary<T>) {}

  template <typename T, CanParse4ary<T> = 0>
  Arg(T* ptr) : arg_(ptr), parser_(DoParse4ary<T>) {}

#if !defined(_MSC_VER)
  template <typename T, CanParseFrom<T> = 0>
  Arg(T* ptr) : arg_(ptr), parser_(DoParseFrom<T>) {}
#endif

  typedef bool (*Parser)(const char* str, size_t n, void* dest);

  template <typename T>
  Arg(T* ptr, Parser parser) : arg_(ptr), parser_(parser) {}

  bool Parse(const char* str, size_t n) const {
    return (*parser_)(str, n, arg_);
  }

 private:
  static bool DoNothing(const char* /*str*/, size_t /*n*/, void* /*dest*/) {
    return true;
  }

  template <typename T>
  static bool DoParse3ary(const char* str, size_t n, void* dest) {
    return re2_internal::Parse(str, n, reinterpret_cast<T*>(dest));
  }

  template <typename T>
  static bool DoParse4ary(const char* str, size_t n, void* dest) {
    return re2_internal::Parse(str, n, reinterpret_cast<T*>(dest), 10);
  }

#if !defined(_MSC_VER)
  template <typename T>
  static bool DoParseFrom(const char* str, size_t n, void* dest) {
    if (dest == NULL) return true;
    return reinterpret_cast<T*>(dest)->ParseFrom(str, n);
  }
#endif

  void*         arg_;
  Parser        parser_;
};

template <typename T>
inline RE2::Arg RE2::CRadix(T* ptr) {
  return RE2::Arg(ptr, [](const char* str, size_t n, void* dest) -> bool {
    return re2_internal::Parse(str, n, reinterpret_cast<T*>(dest), 0);
  });
}

template <typename T>
inline RE2::Arg RE2::Hex(T* ptr) {
  return RE2::Arg(ptr, [](const char* str, size_t n, void* dest) -> bool {
    return re2_internal::Parse(str, n, reinterpret_cast<T*>(dest), 16);
  });
}

template <typename T>
inline RE2::Arg RE2::Octal(T* ptr) {
  return RE2::Arg(ptr, [](const char* str, size_t n, void* dest) -> bool {
    return re2_internal::Parse(str, n, reinterpret_cast<T*>(dest), 8);
  });
}

// Silence warnings about missing initializers for members of LazyRE2.
#if !defined(__clang__) && defined(__GNUC__) && __GNUC__ >= 6
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

// Helper for writing global or static RE2s safely.
// Write
//     static LazyRE2 re = {".*"};
// and then use *re instead of writing
//     static RE2 re(".*");
// The former is more careful about multithreaded
// situations than the latter.
//
// N.B. This class never deletes the RE2 object that
// it constructs: that's a feature, so that it can be used
// for global and function static variables.
class LazyRE2 {
 private:
  struct NoArg {};

 public:
  typedef RE2 element_type;  // support std::pointer_traits

  // Constructor omitted to preserve braced initialization in C++98.

  // Pretend to be a pointer to Type (never NULL due to on-demand creation):
  RE2& operator*() const { return *get(); }
  RE2* operator->() const { return get(); }

  // Named accessor/initializer:
  RE2* get() const {
    std::call_once(once_, &LazyRE2::Init, this);
    return ptr_;
  }

  // All data fields must be public to support {"foo"} initialization.
  const char* pattern_;
  RE2::CannedOptions options_;
  NoArg barrier_against_excess_initializers_;

  mutable RE2* ptr_;
  mutable std::once_flag once_;

 private:
  static void Init(const LazyRE2* lazy_re2) {
    lazy_re2->ptr_ = new RE2(lazy_re2->pattern_, lazy_re2->options_);
  }

  void operator=(const LazyRE2&);  // disallowed
};

namespace hooks {

// Most platforms support thread_local. Older versions of iOS don't support
// thread_local, but for the sake of brevity, we lump together all versions
// of Apple platforms that aren't macOS. If an iOS application really needs
// the context pointee someday, we can get more specific then...
//
// As per https://github.com/google/re2/issues/325, thread_local support in
// MinGW seems to be buggy. (FWIW, Abseil folks also avoid it.)
#define RE2_HAVE_THREAD_LOCAL
#if (defined(__APPLE__) && !(defined(TARGET_OS_OSX) && TARGET_OS_OSX)) || defined(__MINGW32__)
#undef RE2_HAVE_THREAD_LOCAL
#endif

// A hook must not make any assumptions regarding the lifetime of the context
// pointee beyond the current invocation of the hook. Pointers and references
// obtained via the context pointee should be considered invalidated when the
// hook returns. Hence, any data about the context pointee (e.g. its pattern)
// would have to be copied in order for it to be kept for an indefinite time.
//
// A hook must not use RE2 for matching. Control flow reentering RE2::Match()
// could result in infinite mutual recursion. To discourage that possibility,
// RE2 will not maintain the context pointer correctly when used in that way.
#ifdef RE2_HAVE_THREAD_LOCAL
extern thread_local const RE2* context;
#endif

struct DFAStateCacheReset {
  int64_t state_budget;
  size_t state_cache_size;
};

struct DFASearchFailure {
  // Nothing yet...
};

#define DECLARE_HOOK(type)                  \
  using type##Callback = void(const type&); \
  void Set##type##Hook(type##Callback* cb); \
  type##Callback* Get##type##Hook();

DECLARE_HOOK(DFAStateCacheReset)
DECLARE_HOOK(DFASearchFailure)

#undef DECLARE_HOOK

}  // namespace hooks

}  // namespace re2

using re2::RE2;
using re2::LazyRE2;

#endif  // RE2_RE2_H_

// Copyright 2007 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

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

// Copyright 2006 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RE2_WALKER_INL_H_
#define RE2_WALKER_INL_H_

// Helper class for traversing Regexps without recursion.
// Clients should declare their own subclasses that override
// the PreVisit and PostVisit methods, which are called before
// and after visiting the subexpressions.

// Not quite the Visitor pattern, because (among other things)
// the Visitor pattern is recursive.

namespace re2 {

template<typename T> struct WalkState;

template<typename T> class Regexp::Walker {
 public:
  Walker();
  virtual ~Walker();

  // Virtual method called before visiting re's children.
  // PreVisit passes ownership of its return value to its caller.
  // The Arg* that PreVisit returns will be passed to PostVisit as pre_arg
  // and passed to the child PreVisits and PostVisits as parent_arg.
  // At the top-most Regexp, parent_arg is arg passed to walk.
  // If PreVisit sets *stop to true, the walk does not recurse
  // into the children.  Instead it behaves as though the return
  // value from PreVisit is the return value from PostVisit.
  // The default PreVisit returns parent_arg.
  virtual T PreVisit(Regexp* re, T parent_arg, bool* stop);

  // Virtual method called after visiting re's children.
  // The pre_arg is the T that PreVisit returned.
  // The child_args is a vector of the T that the child PostVisits returned.
  // PostVisit takes ownership of pre_arg.
  // PostVisit takes ownership of the Ts
  // in *child_args, but not the vector itself.
  // PostVisit passes ownership of its return value
  // to its caller.
  // The default PostVisit simply returns pre_arg.
  virtual T PostVisit(Regexp* re, T parent_arg, T pre_arg,
                      T* child_args, int nchild_args);

  // Virtual method called to copy a T,
  // when Walk notices that more than one child is the same re.
  virtual T Copy(T arg);

  // Virtual method called to do a "quick visit" of the re,
  // but not its children.  Only called once the visit budget
  // has been used up and we're trying to abort the walk
  // as quickly as possible.  Should return a value that
  // makes sense for the parent PostVisits still to be run.
  // This function is (hopefully) only called by
  // WalkExponential, but must be implemented by all clients,
  // just in case.
  virtual T ShortVisit(Regexp* re, T parent_arg) = 0;

  // Walks over a regular expression.
  // Top_arg is passed as parent_arg to PreVisit and PostVisit of re.
  // Returns the T returned by PostVisit on re.
  T Walk(Regexp* re, T top_arg);

  // Like Walk, but doesn't use Copy.  This can lead to
  // exponential runtimes on cross-linked Regexps like the
  // ones generated by Simplify.  To help limit this,
  // at most max_visits nodes will be visited and then
  // the walk will be cut off early.
  // If the walk *is* cut off early, ShortVisit(re)
  // will be called on regexps that cannot be fully
  // visited rather than calling PreVisit/PostVisit.
  T WalkExponential(Regexp* re, T top_arg, int max_visits);

  // Clears the stack.  Should never be necessary, since
  // Walk always enters and exits with an empty stack.
  // Logs DFATAL if stack is not already clear.
  void Reset();

  // Returns whether walk was cut off.
  bool stopped_early() { return stopped_early_; }

 private:
  // Walk state for the entire traversal.
  std::stack<WalkState<T>> stack_;
  bool stopped_early_;
  int max_visits_;

  T WalkInternal(Regexp* re, T top_arg, bool use_copy);

  Walker(const Walker&) = delete;
  Walker& operator=(const Walker&) = delete;
};

template<typename T> T Regexp::Walker<T>::PreVisit(Regexp* re,
                                                   T parent_arg,
                                                   bool* stop) {
  return parent_arg;
}

template<typename T> T Regexp::Walker<T>::PostVisit(Regexp* re,
                                                    T parent_arg,
                                                    T pre_arg,
                                                    T* child_args,
                                                    int nchild_args) {
  return pre_arg;
}

template<typename T> T Regexp::Walker<T>::Copy(T arg) {
  return arg;
}

// State about a single level in the traversal.
template<typename T> struct WalkState {
  WalkState(Regexp* re, T parent)
    : re(re),
      n(-1),
      parent_arg(parent),
      child_args(NULL) { }

  Regexp* re;  // The regexp
  int n;  // The index of the next child to process; -1 means need to PreVisit
  T parent_arg;  // Accumulated arguments.
  T pre_arg;
  T child_arg;  // One-element buffer for child_args.
  T* child_args;
};

template<typename T> Regexp::Walker<T>::Walker() {
  stopped_early_ = false;
}

template<typename T> Regexp::Walker<T>::~Walker() {
  Reset();
}

// Clears the stack.  Should never be necessary, since
// Walk always enters and exits with an empty stack.
// Logs DFATAL if stack is not already clear.
template<typename T> void Regexp::Walker<T>::Reset() {
  if (!stack_.empty()) {
    LOG(DFATAL) << "Stack not empty.";
    while (!stack_.empty()) {
      if (stack_.top().re->nsub_ > 1)
        delete[] stack_.top().child_args;
      stack_.pop();
    }
  }
}

template<typename T> T Regexp::Walker<T>::WalkInternal(Regexp* re, T top_arg,
                                                       bool use_copy) {
  Reset();

  if (re == NULL) {
    LOG(DFATAL) << "Walk NULL";
    return top_arg;
  }

  stack_.push(WalkState<T>(re, top_arg));

  WalkState<T>* s;
  for (;;) {
    T t;
    s = &stack_.top();
    re = s->re;
    switch (s->n) {
      case -1: {
        if (--max_visits_ < 0) {
          stopped_early_ = true;
          t = ShortVisit(re, s->parent_arg);
          break;
        }
        bool stop = false;
        s->pre_arg = PreVisit(re, s->parent_arg, &stop);
        if (stop) {
          t = s->pre_arg;
          break;
        }
        s->n = 0;
        s->child_args = NULL;
        if (re->nsub_ == 1)
          s->child_args = &s->child_arg;
        else if (re->nsub_ > 1)
          s->child_args = new T[re->nsub_];
        FALLTHROUGH_INTENDED;
      }
      default: {
        if (re->nsub_ > 0) {
          Regexp** sub = re->sub();
          if (s->n < re->nsub_) {
            if (use_copy && s->n > 0 && sub[s->n - 1] == sub[s->n]) {
              s->child_args[s->n] = Copy(s->child_args[s->n - 1]);
              s->n++;
            } else {
              stack_.push(WalkState<T>(sub[s->n], s->pre_arg));
            }
            continue;
          }
        }

        t = PostVisit(re, s->parent_arg, s->pre_arg, s->child_args, s->n);
        if (re->nsub_ > 1)
          delete[] s->child_args;
        break;
      }
    }

    // We've finished stack_.top().
    // Update next guy down.
    stack_.pop();
    if (stack_.empty())
      return t;
    s = &stack_.top();
    if (s->child_args != NULL)
      s->child_args[s->n] = t;
    else
      s->child_arg = t;
    s->n++;
  }
}

template<typename T> T Regexp::Walker<T>::Walk(Regexp* re, T top_arg) {
  // Without the exponential walking behavior,
  // this budget should be more than enough for any
  // regexp, and yet not enough to get us in trouble
  // as far as CPU time.
  max_visits_ = 1000000;
  return WalkInternal(re, top_arg, true);
}

template<typename T> T Regexp::Walker<T>::WalkExponential(Regexp* re, T top_arg,
                                                          int max_visits) {
  max_visits_ = max_visits;
  return WalkInternal(re, top_arg, false);
}

}  // namespace re2

#endif  // RE2_WALKER_INL_H_

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
