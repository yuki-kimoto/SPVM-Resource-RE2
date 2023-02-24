// Copyright 2006 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression parser.

// The parser is a simple precedence-based parser with a
// manual stack.  The parsing work is done by the methods
// of the ParseState class.  The Regexp::Parse function is
// essentially just a lexer that calls the ParseState method
// for each token.

// The parser recognizes POSIX extended regular expressions
// excluding backreferences, collating elements, and collating
// classes.  It also allows the empty string as a regular expression
// and recognizes the Perl escape sequences \d, \s, \w, \D, \S, and \W.
// See regexp.h for rationale.

#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <algorithm>
#include <map>
#include <string>
#include <vector>

#include "util/util.h"
#include "util/logging.h"
#include "util/strutil.h"
#include "util/utf.h"
#include "re2/pod_array.h"
#include "re2/regexp.h"
#include "re2/stringpiece.h"
#include "re2/unicode_groups.h"
#include "re2/walker-inl.h"

#if defined(RE2_USE_ICU)
#include "unicode/uniset.h"
#include "unicode/unistr.h"
#include "unicode/utypes.h"
#endif

// Copyright 2008 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef RE2_UNICODE_CASEFOLD_H_
#define RE2_UNICODE_CASEFOLD_H_

// Unicode case folding tables.

// The Unicode case folding tables encode the mapping from one Unicode point
// to the next largest Unicode point with equivalent folding.  The largest
// point wraps back to the first.  For example, the tables map:
//
//     'A' -> 'a'
//     'a' -> 'A'
//
//     'K' -> 'k'
//     'k' -> 'K'  (Kelvin symbol)
//     'K' -> 'K'
//
// Like everything Unicode, these tables are big.  If we represent the table
// as a sorted list of uint32_t pairs, it has 2049 entries and is 16 kB.
// Most table entries look like the ones around them:
// 'A' maps to 'A'+32, 'B' maps to 'B'+32, etc.
// Instead of listing all the pairs explicitly, we make a list of ranges
// and deltas, so that the table entries for 'A' through 'Z' can be represented
// as a single entry { 'A', 'Z', +32 }.
//
// In addition to blocks that map to each other (A-Z mapping to a-z)
// there are blocks of pairs that individually map to each other
// (for example, 0100<->0101, 0102<->0103, 0104<->0105, ...).
// For those, the special delta value EvenOdd marks even/odd pairs
// (if even, add 1; if odd, subtract 1), and OddEven marks odd/even pairs.
//
// In this form, the table has 274 entries, about 3kB.  If we were to split
// the table into one for 16-bit codes and an overflow table for larger ones,
// we could get it down to about 1.5kB, but that's not worth the complexity.
//
// The grouped form also allows for efficient fold range calculations
// rather than looping one character at a time.

#include <stdint.h>

#include "util/util.h"
#include "util/utf.h"

namespace re2 {

enum {
  EvenOdd = 1,
  OddEven = -1,
  EvenOddSkip = 1<<30,
  OddEvenSkip,
};

struct CaseFold {
  Rune lo;
  Rune hi;
  int32_t delta;
};

extern const CaseFold unicode_casefold[];
extern const int num_unicode_casefold;

extern const CaseFold unicode_tolower[];
extern const int num_unicode_tolower;

// Returns the CaseFold* in the tables that contains rune.
// If rune is not in the tables, returns the first CaseFold* after rune.
// If rune is larger than any value in the tables, returns NULL.
extern const CaseFold* LookupCaseFold(const CaseFold*, int, Rune rune);

// Returns the result of applying the fold f to the rune r.
extern Rune ApplyFold(const CaseFold *f, Rune r);

}  // namespace re2

#endif  // RE2_UNICODE_CASEFOLD_H_

namespace re2 {

// Controls the maximum repeat count permitted by the parser.
static int maximum_repeat_count = 1000;

void Regexp::FUZZING_ONLY_set_maximum_repeat_count(int i) {
  maximum_repeat_count = i;
}

// Regular expression parse state.
// The list of parsed regexps so far is maintained as a vector of
// Regexp pointers called the stack.  Left parenthesis and vertical
// bar markers are also placed on the stack, as Regexps with
// non-standard opcodes.
// Scanning a left parenthesis causes the parser to push a left parenthesis
// marker on the stack.
// Scanning a vertical bar causes the parser to pop the stack until it finds a
// vertical bar or left parenthesis marker (not popping the marker),
// concatenate all the popped results, and push them back on
// the stack (DoConcatenation).
// Scanning a right parenthesis causes the parser to act as though it
// has seen a vertical bar, which then leaves the top of the stack in the
// form LeftParen regexp VerticalBar regexp VerticalBar ... regexp VerticalBar.
// The parser pops all this off the stack and creates an alternation of the
// regexps (DoAlternation).

class Regexp::ParseState {
 public:
  ParseState(ParseFlags flags, const StringPiece& whole_regexp,
             RegexpStatus* status);
  ~ParseState();

  ParseFlags flags() { return flags_; }
  int rune_max() { return rune_max_; }

  // Parse methods.  All public methods return a bool saying
  // whether parsing should continue.  If a method returns
  // false, it has set fields in *status_, and the parser
  // should return NULL.

  // Pushes the given regular expression onto the stack.
  // Could check for too much memory used here.
  bool PushRegexp(Regexp* re);

  // Pushes the literal rune r onto the stack.
  bool PushLiteral(Rune r);

  // Pushes a regexp with the given op (and no args) onto the stack.
  bool PushSimpleOp(RegexpOp op);

  // Pushes a ^ onto the stack.
  bool PushCaret();

  // Pushes a \b (word == true) or \B (word == false) onto the stack.
  bool PushWordBoundary(bool word);

  // Pushes a $ onto the stack.
  bool PushDollar();

  // Pushes a . onto the stack
  bool PushDot();

  // Pushes a repeat operator regexp onto the stack.
  // A valid argument for the operator must already be on the stack.
  // s is the name of the operator, for use in error messages.
  bool PushRepeatOp(RegexpOp op, const StringPiece& s, bool nongreedy);

  // Pushes a repetition regexp onto the stack.
  // A valid argument for the operator must already be on the stack.
  bool PushRepetition(int min, int max, const StringPiece& s, bool nongreedy);

  // Checks whether a particular regexp op is a marker.
  bool IsMarker(RegexpOp op);

  // Processes a left parenthesis in the input.
  // Pushes a marker onto the stack.
  bool DoLeftParen(const StringPiece& name);
  bool DoLeftParenNoCapture();

  // Processes a vertical bar in the input.
  bool DoVerticalBar();

  // Processes a right parenthesis in the input.
  bool DoRightParen();

  // Processes the end of input, returning the final regexp.
  Regexp* DoFinish();

  // Finishes the regexp if necessary, preparing it for use
  // in a more complicated expression.
  // If it is a CharClassBuilder, converts into a CharClass.
  Regexp* FinishRegexp(Regexp*);

  // These routines don't manipulate the parse stack
  // directly, but they do need to look at flags_.
  // ParseCharClass also manipulates the internals of Regexp
  // while creating *out_re.

  // Parse a character class into *out_re.
  // Removes parsed text from s.
  bool ParseCharClass(StringPiece* s, Regexp** out_re,
                      RegexpStatus* status);

  // Parse a character class character into *rp.
  // Removes parsed text from s.
  bool ParseCCCharacter(StringPiece* s, Rune *rp,
                        const StringPiece& whole_class,
                        RegexpStatus* status);

  // Parse a character class range into rr.
  // Removes parsed text from s.
  bool ParseCCRange(StringPiece* s, RuneRange* rr,
                    const StringPiece& whole_class,
                    RegexpStatus* status);

  // Parse a Perl flag set or non-capturing group from s.
  bool ParsePerlFlags(StringPiece* s);


  // Finishes the current concatenation,
  // collapsing it into a single regexp on the stack.
  void DoConcatenation();

  // Finishes the current alternation,
  // collapsing it to a single regexp on the stack.
  void DoAlternation();

  // Generalized DoAlternation/DoConcatenation.
  void DoCollapse(RegexpOp op);

  // Maybe concatenate Literals into LiteralString.
  bool MaybeConcatString(int r, ParseFlags flags);

private:
  ParseFlags flags_;
  StringPiece whole_regexp_;
  RegexpStatus* status_;
  Regexp* stacktop_;
  int ncap_;  // number of capturing parens seen
  int rune_max_;  // maximum char value for this encoding

  ParseState(const ParseState&) = delete;
  ParseState& operator=(const ParseState&) = delete;
};

// Pseudo-operators - only on parse stack.
const RegexpOp kLeftParen = static_cast<RegexpOp>(kMaxRegexpOp+1);
const RegexpOp kVerticalBar = static_cast<RegexpOp>(kMaxRegexpOp+2);

Regexp::ParseState::ParseState(ParseFlags flags,
                               const StringPiece& whole_regexp,
                               RegexpStatus* status)
  : flags_(flags), whole_regexp_(whole_regexp),
    status_(status), stacktop_(NULL), ncap_(0) {
  if (flags_ & Latin1)
    rune_max_ = 0xFF;
  else
    rune_max_ = Runemax;
}

// Cleans up by freeing all the regexps on the stack.
Regexp::ParseState::~ParseState() {
  Regexp* next;
  for (Regexp* re = stacktop_; re != NULL; re = next) {
    next = re->down_;
    re->down_ = NULL;
    if (re->op() == kLeftParen)
      delete re->name_;
    re->Decref();
  }
}

// Finishes the regexp if necessary, preparing it for use in
// a more complex expression.
// If it is a CharClassBuilder, converts into a CharClass.
Regexp* Regexp::ParseState::FinishRegexp(Regexp* re) {
  if (re == NULL)
    return NULL;
  re->down_ = NULL;

  if (re->op_ == kRegexpCharClass && re->ccb_ != NULL) {
    CharClassBuilder* ccb = re->ccb_;
    re->ccb_ = NULL;
    re->cc_ = ccb->GetCharClass();
    delete ccb;
  }

  return re;
}

// Pushes the given regular expression onto the stack.
// Could check for too much memory used here.
bool Regexp::ParseState::PushRegexp(Regexp* re) {
  MaybeConcatString(-1, NoParseFlags);

  // Special case: a character class of one character is just
  // a literal.  This is a common idiom for escaping
  // single characters (e.g., [.] instead of \.), and some
  // analysis does better with fewer character classes.
  // Similarly, [Aa] can be rewritten as a literal A with ASCII case folding.
  if (re->op_ == kRegexpCharClass && re->ccb_ != NULL) {
    re->ccb_->RemoveAbove(rune_max_);
    if (re->ccb_->size() == 1) {
      Rune r = re->ccb_->begin()->lo;
      re->Decref();
      re = new Regexp(kRegexpLiteral, flags_);
      re->rune_ = r;
    } else if (re->ccb_->size() == 2) {
      Rune r = re->ccb_->begin()->lo;
      if ('A' <= r && r <= 'Z' && re->ccb_->Contains(r + 'a' - 'A')) {
        re->Decref();
        re = new Regexp(kRegexpLiteral, flags_ | FoldCase);
        re->rune_ = r + 'a' - 'A';
      }
    }
  }

  re->down_ = stacktop_;
  stacktop_ = re;
  return true;
}

// Searches the case folding tables and returns the CaseFold* that contains r.
// If there isn't one, returns the CaseFold* with smallest f->lo bigger than r.
// If there isn't one, returns NULL.
const CaseFold* LookupCaseFold(const CaseFold *f, int n, Rune r) {
  const CaseFold* ef = f + n;

  // Binary search for entry containing r.
  while (n > 0) {
    int m = n/2;
    if (f[m].lo <= r && r <= f[m].hi)
      return &f[m];
    if (r < f[m].lo) {
      n = m;
    } else {
      f += m+1;
      n -= m+1;
    }
  }

  // There is no entry that contains r, but f points
  // where it would have been.  Unless f points at
  // the end of the array, it points at the next entry
  // after r.
  if (f < ef)
    return f;

  // No entry contains r; no entry contains runes > r.
  return NULL;
}

// Returns the result of applying the fold f to the rune r.
Rune ApplyFold(const CaseFold *f, Rune r) {
  switch (f->delta) {
    default:
      return r + f->delta;

    case EvenOddSkip:  // even <-> odd but only applies to every other
      if ((r - f->lo) % 2)
        return r;
      FALLTHROUGH_INTENDED;
    case EvenOdd:  // even <-> odd
      if (r%2 == 0)
        return r + 1;
      return r - 1;

    case OddEvenSkip:  // odd <-> even but only applies to every other
      if ((r - f->lo) % 2)
        return r;
      FALLTHROUGH_INTENDED;
    case OddEven:  // odd <-> even
      if (r%2 == 1)
        return r + 1;
      return r - 1;
  }
}

// Returns the next Rune in r's folding cycle (see unicode_casefold.h).
// Examples:
//   CycleFoldRune('A') = 'a'
//   CycleFoldRune('a') = 'A'
//
//   CycleFoldRune('K') = 'k'
//   CycleFoldRune('k') = 0x212A (Kelvin)
//   CycleFoldRune(0x212A) = 'K'
//
//   CycleFoldRune('?') = '?'
Rune CycleFoldRune(Rune r) {
  const CaseFold* f = LookupCaseFold(unicode_casefold, num_unicode_casefold, r);
  if (f == NULL || r < f->lo)
    return r;
  return ApplyFold(f, r);
}

// Pushes the literal rune r onto the stack.
bool Regexp::ParseState::PushLiteral(Rune r) {
  // Do case folding if needed.
  if ((flags_ & FoldCase) && CycleFoldRune(r) != r) {
    Regexp* re = new Regexp(kRegexpCharClass, flags_ & ~FoldCase);
    re->ccb_ = new CharClassBuilder;
    Rune r1 = r;
    do {
      if (!(flags_ & NeverNL) || r != '\n') {
        re->ccb_->AddRange(r, r);
      }
      r = CycleFoldRune(r);
    } while (r != r1);
    return PushRegexp(re);
  }

  // Exclude newline if applicable.
  if ((flags_ & NeverNL) && r == '\n')
    return PushRegexp(new Regexp(kRegexpNoMatch, flags_));

  // No fancy stuff worked.  Ordinary literal.
  if (MaybeConcatString(r, flags_))
    return true;

  Regexp* re = new Regexp(kRegexpLiteral, flags_);
  re->rune_ = r;
  return PushRegexp(re);
}

// Pushes a ^ onto the stack.
bool Regexp::ParseState::PushCaret() {
  if (flags_ & OneLine) {
    return PushSimpleOp(kRegexpBeginText);
  }
  return PushSimpleOp(kRegexpBeginLine);
}

// Pushes a \b or \B onto the stack.
bool Regexp::ParseState::PushWordBoundary(bool word) {
  if (word)
    return PushSimpleOp(kRegexpWordBoundary);
  return PushSimpleOp(kRegexpNoWordBoundary);
}

// Pushes a $ onto the stack.
bool Regexp::ParseState::PushDollar() {
  if (flags_ & OneLine) {
    // Clumsy marker so that MimicsPCRE() can tell whether
    // this kRegexpEndText was a $ and not a \z.
    Regexp::ParseFlags oflags = flags_;
    flags_ = flags_ | WasDollar;
    bool ret = PushSimpleOp(kRegexpEndText);
    flags_ = oflags;
    return ret;
  }
  return PushSimpleOp(kRegexpEndLine);
}

// Pushes a . onto the stack.
bool Regexp::ParseState::PushDot() {
  if ((flags_ & DotNL) && !(flags_ & NeverNL))
    return PushSimpleOp(kRegexpAnyChar);
  // Rewrite . into [^\n]
  Regexp* re = new Regexp(kRegexpCharClass, flags_ & ~FoldCase);
  re->ccb_ = new CharClassBuilder;
  re->ccb_->AddRange(0, '\n' - 1);
  re->ccb_->AddRange('\n' + 1, rune_max_);
  return PushRegexp(re);
}

// Pushes a regexp with the given op (and no args) onto the stack.
bool Regexp::ParseState::PushSimpleOp(RegexpOp op) {
  Regexp* re = new Regexp(op, flags_);
  return PushRegexp(re);
}

// Pushes a repeat operator regexp onto the stack.
// A valid argument for the operator must already be on the stack.
// The char c is the name of the operator, for use in error messages.
bool Regexp::ParseState::PushRepeatOp(RegexpOp op, const StringPiece& s,
                                      bool nongreedy) {
  if (stacktop_ == NULL || IsMarker(stacktop_->op())) {
    status_->set_code(kRegexpRepeatArgument);
    status_->set_error_arg(s);
    return false;
  }
  Regexp::ParseFlags fl = flags_;
  if (nongreedy)
    fl = fl ^ NonGreedy;

  // Squash **, ++ and ??. Regexp::Star() et al. handle this too, but
  // they're mostly for use during simplification, not during parsing.
  if (op == stacktop_->op() && fl == stacktop_->parse_flags())
    return true;

  // Squash *+, *?, +*, +?, ?* and ?+. They all squash to *, so because
  // op is a repeat, we just have to check that stacktop_->op() is too,
  // then adjust stacktop_.
  if ((stacktop_->op() == kRegexpStar ||
       stacktop_->op() == kRegexpPlus ||
       stacktop_->op() == kRegexpQuest) &&
      fl == stacktop_->parse_flags()) {
    stacktop_->op_ = kRegexpStar;
    return true;
  }

  Regexp* re = new Regexp(op, fl);
  re->AllocSub(1);
  re->down_ = stacktop_->down_;
  re->sub()[0] = FinishRegexp(stacktop_);
  stacktop_ = re;
  return true;
}

// RepetitionWalker reports whether the repetition regexp is valid.
// Valid means that the combination of the top-level repetition
// and any inner repetitions does not exceed n copies of the
// innermost thing.
// This rewalks the regexp tree and is called for every repetition,
// so we have to worry about inducing quadratic behavior in the parser.
// We avoid this by only using RepetitionWalker when min or max >= 2.
// In that case the depth of any >= 2 nesting can only get to 9 without
// triggering a parse error, so each subtree can only be rewalked 9 times.
class RepetitionWalker : public Regexp::Walker<int> {
 public:
  RepetitionWalker() {}
  virtual int PreVisit(Regexp* re, int parent_arg, bool* stop);
  virtual int PostVisit(Regexp* re, int parent_arg, int pre_arg,
                        int* child_args, int nchild_args);
  virtual int ShortVisit(Regexp* re, int parent_arg);

 private:
  RepetitionWalker(const RepetitionWalker&) = delete;
  RepetitionWalker& operator=(const RepetitionWalker&) = delete;
};

int RepetitionWalker::PreVisit(Regexp* re, int parent_arg, bool* stop) {
  int arg = parent_arg;
  if (re->op() == kRegexpRepeat) {
    int m = re->max();
    if (m < 0) {
      m = re->min();
    }
    if (m > 0) {
      arg /= m;
    }
  }
  return arg;
}

int RepetitionWalker::PostVisit(Regexp* re, int parent_arg, int pre_arg,
                                int* child_args, int nchild_args) {
  int arg = pre_arg;
  for (int i = 0; i < nchild_args; i++) {
    if (child_args[i] < arg) {
      arg = child_args[i];
    }
  }
  return arg;
}

int RepetitionWalker::ShortVisit(Regexp* re, int parent_arg) {
  // Should never be called: we use Walk(), not WalkExponential().
#ifndef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
  LOG(DFATAL) << "RepetitionWalker::ShortVisit called";
#endif
  return 0;
}

// Pushes a repetition regexp onto the stack.
// A valid argument for the operator must already be on the stack.
bool Regexp::ParseState::PushRepetition(int min, int max,
                                        const StringPiece& s,
                                        bool nongreedy) {
  if ((max != -1 && max < min) ||
      min > maximum_repeat_count ||
      max > maximum_repeat_count) {
    status_->set_code(kRegexpRepeatSize);
    status_->set_error_arg(s);
    return false;
  }
  if (stacktop_ == NULL || IsMarker(stacktop_->op())) {
    status_->set_code(kRegexpRepeatArgument);
    status_->set_error_arg(s);
    return false;
  }
  Regexp::ParseFlags fl = flags_;
  if (nongreedy)
    fl = fl ^ NonGreedy;
  Regexp* re = new Regexp(kRegexpRepeat, fl);
  re->min_ = min;
  re->max_ = max;
  re->AllocSub(1);
  re->down_ = stacktop_->down_;
  re->sub()[0] = FinishRegexp(stacktop_);
  stacktop_ = re;
  if (min >= 2 || max >= 2) {
    RepetitionWalker w;
    if (w.Walk(stacktop_, maximum_repeat_count) == 0) {
      status_->set_code(kRegexpRepeatSize);
      status_->set_error_arg(s);
      return false;
    }
  }
  return true;
}

// Checks whether a particular regexp op is a marker.
bool Regexp::ParseState::IsMarker(RegexpOp op) {
  return op >= kLeftParen;
}

// Processes a left parenthesis in the input.
// Pushes a marker onto the stack.
bool Regexp::ParseState::DoLeftParen(const StringPiece& name) {
  Regexp* re = new Regexp(kLeftParen, flags_);
  re->cap_ = ++ncap_;
  if (name.data() != NULL)
    re->name_ = new std::string(name);
  return PushRegexp(re);
}

// Pushes a non-capturing marker onto the stack.
bool Regexp::ParseState::DoLeftParenNoCapture() {
  Regexp* re = new Regexp(kLeftParen, flags_);
  re->cap_ = -1;
  return PushRegexp(re);
}

// Processes a vertical bar in the input.
bool Regexp::ParseState::DoVerticalBar() {
  MaybeConcatString(-1, NoParseFlags);
  DoConcatenation();

  // Below the vertical bar is a list to alternate.
  // Above the vertical bar is a list to concatenate.
  // We just did the concatenation, so either swap
  // the result below the vertical bar or push a new
  // vertical bar on the stack.
  Regexp* r1;
  Regexp* r2;
  if ((r1 = stacktop_) != NULL &&
      (r2 = r1->down_) != NULL &&
      r2->op() == kVerticalBar) {
    Regexp* r3;
    if ((r3 = r2->down_) != NULL &&
        (r1->op() == kRegexpAnyChar || r3->op() == kRegexpAnyChar)) {
      // AnyChar is above or below the vertical bar. Let it subsume
      // the other when the other is Literal, CharClass or AnyChar.
      if (r3->op() == kRegexpAnyChar &&
          (r1->op() == kRegexpLiteral ||
           r1->op() == kRegexpCharClass ||
           r1->op() == kRegexpAnyChar)) {
        // Discard r1.
        stacktop_ = r2;
        r1->Decref();
        return true;
      }
      if (r1->op() == kRegexpAnyChar &&
          (r3->op() == kRegexpLiteral ||
           r3->op() == kRegexpCharClass ||
           r3->op() == kRegexpAnyChar)) {
        // Rearrange the stack and discard r3.
        r1->down_ = r3->down_;
        r2->down_ = r1;
        stacktop_ = r2;
        r3->Decref();
        return true;
      }
    }
    // Swap r1 below vertical bar (r2).
    r1->down_ = r2->down_;
    r2->down_ = r1;
    stacktop_ = r2;
    return true;
  }
  return PushSimpleOp(kVerticalBar);
}

// Processes a right parenthesis in the input.
bool Regexp::ParseState::DoRightParen() {
  // Finish the current concatenation and alternation.
  DoAlternation();

  // The stack should be: LeftParen regexp
  // Remove the LeftParen, leaving the regexp,
  // parenthesized.
  Regexp* r1;
  Regexp* r2;
  if ((r1 = stacktop_) == NULL ||
      (r2 = r1->down_) == NULL ||
      r2->op() != kLeftParen) {
    status_->set_code(kRegexpUnexpectedParen);
    status_->set_error_arg(whole_regexp_);
    return false;
  }

  // Pop off r1, r2.  Will Decref or reuse below.
  stacktop_ = r2->down_;

  // Restore flags from when paren opened.
  Regexp* re = r2;
  flags_ = re->parse_flags();

  // Rewrite LeftParen as capture if needed.
  if (re->cap_ > 0) {
    re->op_ = kRegexpCapture;
    // re->cap_ is already set
    re->AllocSub(1);
    re->sub()[0] = FinishRegexp(r1);
  } else {
    re->Decref();
    re = r1;
  }
  return PushRegexp(re);
}

// Processes the end of input, returning the final regexp.
Regexp* Regexp::ParseState::DoFinish() {
  DoAlternation();
  Regexp* re = stacktop_;
  if (re != NULL && re->down_ != NULL) {
    status_->set_code(kRegexpMissingParen);
    status_->set_error_arg(whole_regexp_);
    return NULL;
  }
  stacktop_ = NULL;
  return FinishRegexp(re);
}

// Returns the leading regexp that re starts with.
// The returned Regexp* points into a piece of re,
// so it must not be used after the caller calls re->Decref().
Regexp* Regexp::LeadingRegexp(Regexp* re) {
  if (re->op() == kRegexpEmptyMatch)
    return NULL;
  if (re->op() == kRegexpConcat && re->nsub() >= 2) {
    Regexp** sub = re->sub();
    if (sub[0]->op() == kRegexpEmptyMatch)
      return NULL;
    return sub[0];
  }
  return re;
}

// Removes LeadingRegexp(re) from re and returns what's left.
// Consumes the reference to re and may edit it in place.
// If caller wants to hold on to LeadingRegexp(re),
// must have already Incref'ed it.
Regexp* Regexp::RemoveLeadingRegexp(Regexp* re) {
  if (re->op() == kRegexpEmptyMatch)
    return re;
  if (re->op() == kRegexpConcat && re->nsub() >= 2) {
    Regexp** sub = re->sub();
    if (sub[0]->op() == kRegexpEmptyMatch)
      return re;
    sub[0]->Decref();
    sub[0] = NULL;
    if (re->nsub() == 2) {
      // Collapse concatenation to single regexp.
      Regexp* nre = sub[1];
      sub[1] = NULL;
      re->Decref();
      return nre;
    }
    // 3 or more -> 2 or more.
    re->nsub_--;
    memmove(sub, sub + 1, re->nsub_ * sizeof sub[0]);
    return re;
  }
  Regexp::ParseFlags pf = re->parse_flags();
  re->Decref();
  return new Regexp(kRegexpEmptyMatch, pf);
}

// Returns the leading string that re starts with.
// The returned Rune* points into a piece of re,
// so it must not be used after the caller calls re->Decref().
Rune* Regexp::LeadingString(Regexp* re, int *nrune,
                            Regexp::ParseFlags *flags) {
  while (re->op() == kRegexpConcat && re->nsub() > 0)
    re = re->sub()[0];

  *flags = static_cast<Regexp::ParseFlags>(re->parse_flags_ & Regexp::FoldCase);

  if (re->op() == kRegexpLiteral) {
    *nrune = 1;
    return &re->rune_;
  }

  if (re->op() == kRegexpLiteralString) {
    *nrune = re->nrunes_;
    return re->runes_;
  }

  *nrune = 0;
  return NULL;
}

// Removes the first n leading runes from the beginning of re.
// Edits re in place.
void Regexp::RemoveLeadingString(Regexp* re, int n) {
  // Chase down concats to find first string.
  // For regexps generated by parser, nested concats are
  // flattened except when doing so would overflow the 16-bit
  // limit on the size of a concatenation, so we should never
  // see more than two here.
  Regexp* stk[4];
  size_t d = 0;
  while (re->op() == kRegexpConcat) {
    if (d < arraysize(stk))
      stk[d++] = re;
    re = re->sub()[0];
  }

  // Remove leading string from re.
  if (re->op() == kRegexpLiteral) {
    re->rune_ = 0;
    re->op_ = kRegexpEmptyMatch;
  } else if (re->op() == kRegexpLiteralString) {
    if (n >= re->nrunes_) {
      delete[] re->runes_;
      re->runes_ = NULL;
      re->nrunes_ = 0;
      re->op_ = kRegexpEmptyMatch;
    } else if (n == re->nrunes_ - 1) {
      Rune rune = re->runes_[re->nrunes_ - 1];
      delete[] re->runes_;
      re->runes_ = NULL;
      re->nrunes_ = 0;
      re->rune_ = rune;
      re->op_ = kRegexpLiteral;
    } else {
      re->nrunes_ -= n;
      memmove(re->runes_, re->runes_ + n, re->nrunes_ * sizeof re->runes_[0]);
    }
  }

  // If re is now empty, concatenations might simplify too.
  while (d > 0) {
    re = stk[--d];
    Regexp** sub = re->sub();
    if (sub[0]->op() == kRegexpEmptyMatch) {
      sub[0]->Decref();
      sub[0] = NULL;
      // Delete first element of concat.
      switch (re->nsub()) {
        case 0:
        case 1:
          // Impossible.
          LOG(DFATAL) << "Concat of " << re->nsub();
          re->submany_ = NULL;
          re->op_ = kRegexpEmptyMatch;
          break;

        case 2: {
          // Replace re with sub[1].
          Regexp* old = sub[1];
          sub[1] = NULL;
          re->Swap(old);
          old->Decref();
          break;
        }

        default:
          // Slide down.
          re->nsub_--;
          memmove(sub, sub + 1, re->nsub_ * sizeof sub[0]);
          break;
      }
    }
  }
}

// In the context of factoring alternations, a Splice is: a factored prefix or
// merged character class computed by one iteration of one round of factoring;
// the span of subexpressions of the alternation to be "spliced" (i.e. removed
// and replaced); and, for a factored prefix, the number of suffixes after any
// factoring that might have subsequently been performed on them. For a merged
// character class, there are no suffixes, of course, so the field is ignored.
struct Splice {
  Splice(Regexp* prefix, Regexp** sub, int nsub)
      : prefix(prefix),
        sub(sub),
        nsub(nsub),
        nsuffix(-1) {}

  Regexp* prefix;
  Regexp** sub;
  int nsub;
  int nsuffix;
};

// Named so because it is used to implement an explicit stack, a Frame is: the
// span of subexpressions of the alternation to be factored; the current round
// of factoring; any Splices computed; and, for a factored prefix, an iterator
// to the next Splice to be factored (i.e. in another Frame) because suffixes.
struct Frame {
  Frame(Regexp** sub, int nsub)
      : sub(sub),
        nsub(nsub),
        round(0) {}

  Regexp** sub;
  int nsub;
  int round;
  std::vector<Splice> splices;
  int spliceidx;
};

// Bundled into a class for friend access to Regexp without needing to declare
// (or define) Splice in regexp.h.
class FactorAlternationImpl {
 public:
  static void Round1(Regexp** sub, int nsub,
                     Regexp::ParseFlags flags,
                     std::vector<Splice>* splices);
  static void Round2(Regexp** sub, int nsub,
                     Regexp::ParseFlags flags,
                     std::vector<Splice>* splices);
  static void Round3(Regexp** sub, int nsub,
                     Regexp::ParseFlags flags,
                     std::vector<Splice>* splices);
};

// Factors common prefixes from alternation.
// For example,
//     ABC|ABD|AEF|BCX|BCY
// simplifies to
//     A(B(C|D)|EF)|BC(X|Y)
// and thence to
//     A(B[CD]|EF)|BC[XY]
//
// Rewrites sub to contain simplified list to alternate and returns
// the new length of sub.  Adjusts reference counts accordingly
// (incoming sub[i] decremented, outgoing sub[i] incremented).
int Regexp::FactorAlternation(Regexp** sub, int nsub, ParseFlags flags) {
  std::vector<Frame> stk;
  stk.emplace_back(sub, nsub);

  for (;;) {
    auto& sub = stk.back().sub;
    auto& nsub = stk.back().nsub;
    auto& round = stk.back().round;
    auto& splices = stk.back().splices;
    auto& spliceidx = stk.back().spliceidx;

    if (splices.empty()) {
      // Advance to the next round of factoring. Note that this covers
      // the initialised state: when splices is empty and round is 0.
      round++;
    } else if (spliceidx < static_cast<int>(splices.size())) {
      // We have at least one more Splice to factor. Recurse logically.
      stk.emplace_back(splices[spliceidx].sub, splices[spliceidx].nsub);
      continue;
    } else {
      // We have no more Splices to factor. Apply them.
      auto iter = splices.begin();
      int out = 0;
      for (int i = 0; i < nsub; ) {
        // Copy until we reach where the next Splice begins.
        while (sub + i < iter->sub)
          sub[out++] = sub[i++];
        switch (round) {
          case 1:
          case 2: {
            // Assemble the Splice prefix and the suffixes.
            Regexp* re[2];
            re[0] = iter->prefix;
            re[1] = Regexp::AlternateNoFactor(iter->sub, iter->nsuffix, flags);
            sub[out++] = Regexp::Concat(re, 2, flags);
            i += iter->nsub;
            break;
          }
          case 3:
            // Just use the Splice prefix.
            sub[out++] = iter->prefix;
            i += iter->nsub;
            break;
          default:
            LOG(DFATAL) << "unknown round: " << round;
            break;
        }
        // If we are done, copy until the end of sub.
        if (++iter == splices.end()) {
          while (i < nsub)
            sub[out++] = sub[i++];
        }
      }
      splices.clear();
      nsub = out;
      // Advance to the next round of factoring.
      round++;
    }

    switch (round) {
      case 1:
        FactorAlternationImpl::Round1(sub, nsub, flags, &splices);
        break;
      case 2:
        FactorAlternationImpl::Round2(sub, nsub, flags, &splices);
        break;
      case 3:
        FactorAlternationImpl::Round3(sub, nsub, flags, &splices);
        break;
      case 4:
        if (stk.size() == 1) {
          // We are at the top of the stack. Just return.
          return nsub;
        } else {
          // Pop the stack and set the number of suffixes.
          // (Note that references will be invalidated!)
          int nsuffix = nsub;
          stk.pop_back();
          stk.back().splices[stk.back().spliceidx].nsuffix = nsuffix;
          ++stk.back().spliceidx;
          continue;
        }
      default:
        LOG(DFATAL) << "unknown round: " << round;
        break;
    }

    // Set spliceidx depending on whether we have Splices to factor.
    if (splices.empty() || round == 3) {
      spliceidx = static_cast<int>(splices.size());
    } else {
      spliceidx = 0;
    }
  }
}

void FactorAlternationImpl::Round1(Regexp** sub, int nsub,
                                   Regexp::ParseFlags flags,
                                   std::vector<Splice>* splices) {
  // Round 1: Factor out common literal prefixes.
  int start = 0;
  Rune* rune = NULL;
  int nrune = 0;
  Regexp::ParseFlags runeflags = Regexp::NoParseFlags;
  for (int i = 0; i <= nsub; i++) {
    // Invariant: sub[start:i] consists of regexps that all
    // begin with rune[0:nrune].
    Rune* rune_i = NULL;
    int nrune_i = 0;
    Regexp::ParseFlags runeflags_i = Regexp::NoParseFlags;
    if (i < nsub) {
      rune_i = Regexp::LeadingString(sub[i], &nrune_i, &runeflags_i);
      if (runeflags_i == runeflags) {
        int same = 0;
        while (same < nrune && same < nrune_i && rune[same] == rune_i[same])
          same++;
        if (same > 0) {
          // Matches at least one rune in current range.  Keep going around.
          nrune = same;
          continue;
        }
      }
    }

    // Found end of a run with common leading literal string:
    // sub[start:i] all begin with rune[0:nrune],
    // but sub[i] does not even begin with rune[0].
    if (i == start) {
      // Nothing to do - first iteration.
    } else if (i == start+1) {
      // Just one: don't bother factoring.
    } else {
      Regexp* prefix = Regexp::LiteralString(rune, nrune, runeflags);
      for (int j = start; j < i; j++)
        Regexp::RemoveLeadingString(sub[j], nrune);
      splices->emplace_back(prefix, sub + start, i - start);
    }

    // Prepare for next iteration (if there is one).
    if (i < nsub) {
      start = i;
      rune = rune_i;
      nrune = nrune_i;
      runeflags = runeflags_i;
    }
  }
}

void FactorAlternationImpl::Round2(Regexp** sub, int nsub,
                                   Regexp::ParseFlags flags,
                                   std::vector<Splice>* splices) {
  // Round 2: Factor out common simple prefixes,
  // just the first piece of each concatenation.
  // This will be good enough a lot of the time.
  //
  // Complex subexpressions (e.g. involving quantifiers)
  // are not safe to factor because that collapses their
  // distinct paths through the automaton, which affects
  // correctness in some cases.
  int start = 0;
  Regexp* first = NULL;
  for (int i = 0; i <= nsub; i++) {
    // Invariant: sub[start:i] consists of regexps that all
    // begin with first.
    Regexp* first_i = NULL;
    if (i < nsub) {
      first_i = Regexp::LeadingRegexp(sub[i]);
      if (first != NULL &&
          // first must be an empty-width op
          // OR a char class, any char or any byte
          // OR a fixed repeat of a literal, char class, any char or any byte.
          (first->op() == kRegexpBeginLine ||
           first->op() == kRegexpEndLine ||
           first->op() == kRegexpWordBoundary ||
           first->op() == kRegexpNoWordBoundary ||
           first->op() == kRegexpBeginText ||
           first->op() == kRegexpEndText ||
           first->op() == kRegexpCharClass ||
           first->op() == kRegexpAnyChar ||
           first->op() == kRegexpAnyByte ||
           (first->op() == kRegexpRepeat &&
            first->min() == first->max() &&
            (first->sub()[0]->op() == kRegexpLiteral ||
             first->sub()[0]->op() == kRegexpCharClass ||
             first->sub()[0]->op() == kRegexpAnyChar ||
             first->sub()[0]->op() == kRegexpAnyByte))) &&
          Regexp::Equal(first, first_i))
        continue;
    }

    // Found end of a run with common leading regexp:
    // sub[start:i] all begin with first,
    // but sub[i] does not.
    if (i == start) {
      // Nothing to do - first iteration.
    } else if (i == start+1) {
      // Just one: don't bother factoring.
    } else {
      Regexp* prefix = first->Incref();
      for (int j = start; j < i; j++)
        sub[j] = Regexp::RemoveLeadingRegexp(sub[j]);
      splices->emplace_back(prefix, sub + start, i - start);
    }

    // Prepare for next iteration (if there is one).
    if (i < nsub) {
      start = i;
      first = first_i;
    }
  }
}

void FactorAlternationImpl::Round3(Regexp** sub, int nsub,
                                   Regexp::ParseFlags flags,
                                   std::vector<Splice>* splices) {
  // Round 3: Merge runs of literals and/or character classes.
  int start = 0;
  Regexp* first = NULL;
  for (int i = 0; i <= nsub; i++) {
    // Invariant: sub[start:i] consists of regexps that all
    // are either literals (i.e. runes) or character classes.
    Regexp* first_i = NULL;
    if (i < nsub) {
      first_i = sub[i];
      if (first != NULL &&
          (first->op() == kRegexpLiteral ||
           first->op() == kRegexpCharClass) &&
          (first_i->op() == kRegexpLiteral ||
           first_i->op() == kRegexpCharClass))
        continue;
    }

    // Found end of a run of Literal/CharClass:
    // sub[start:i] all are either one or the other,
    // but sub[i] is not.
    if (i == start) {
      // Nothing to do - first iteration.
    } else if (i == start+1) {
      // Just one: don't bother factoring.
    } else {
      CharClassBuilder ccb;
      for (int j = start; j < i; j++) {
        Regexp* re = sub[j];
        if (re->op() == kRegexpCharClass) {
          CharClass* cc = re->cc();
          for (CharClass::iterator it = cc->begin(); it != cc->end(); ++it)
            ccb.AddRange(it->lo, it->hi);
        } else if (re->op() == kRegexpLiteral) {
          ccb.AddRangeFlags(re->rune(), re->rune(), re->parse_flags());
        } else {
          LOG(DFATAL) << "RE2: unexpected op: " << re->op() << " "
                      << re->ToString();
        }
        re->Decref();
      }
      Regexp* re = Regexp::NewCharClass(ccb.GetCharClass(), flags);
      splices->emplace_back(re, sub + start, i - start);
    }

    // Prepare for next iteration (if there is one).
    if (i < nsub) {
      start = i;
      first = first_i;
    }
  }
}

// Collapse the regexps on top of the stack, down to the
// first marker, into a new op node (op == kRegexpAlternate
// or op == kRegexpConcat).
void Regexp::ParseState::DoCollapse(RegexpOp op) {
  // Scan backward to marker, counting children of composite.
  int n = 0;
  Regexp* next = NULL;
  Regexp* sub;
  for (sub = stacktop_; sub != NULL && !IsMarker(sub->op()); sub = next) {
    next = sub->down_;
    if (sub->op_ == op)
      n += sub->nsub_;
    else
      n++;
  }

  // If there's just one child, leave it alone.
  // (Concat of one thing is that one thing; alternate of one thing is same.)
  if (stacktop_ != NULL && stacktop_->down_ == next)
    return;

  // Construct op (alternation or concatenation), flattening op of op.
  PODArray<Regexp*> subs(n);
  next = NULL;
  int i = n;
  for (sub = stacktop_; sub != NULL && !IsMarker(sub->op()); sub = next) {
    next = sub->down_;
    if (sub->op_ == op) {
      Regexp** sub_subs = sub->sub();
      for (int k = sub->nsub_ - 1; k >= 0; k--)
        subs[--i] = sub_subs[k]->Incref();
      sub->Decref();
    } else {
      subs[--i] = FinishRegexp(sub);
    }
  }

  Regexp* re = ConcatOrAlternate(op, subs.data(), n, flags_, true);
  re->down_ = next;
  stacktop_ = re;
}

// Finishes the current concatenation,
// collapsing it into a single regexp on the stack.
void Regexp::ParseState::DoConcatenation() {
  Regexp* r1 = stacktop_;
  if (r1 == NULL || IsMarker(r1->op())) {
    // empty concatenation is special case
    Regexp* re = new Regexp(kRegexpEmptyMatch, flags_);
    PushRegexp(re);
  }
  DoCollapse(kRegexpConcat);
}

// Finishes the current alternation,
// collapsing it to a single regexp on the stack.
void Regexp::ParseState::DoAlternation() {
  DoVerticalBar();
  // Now stack top is kVerticalBar.
  Regexp* r1 = stacktop_;
  stacktop_ = r1->down_;
  r1->Decref();
  DoCollapse(kRegexpAlternate);
}

// Incremental conversion of concatenated literals into strings.
// If top two elements on stack are both literal or string,
// collapse into single string.
// Don't walk down the stack -- the parser calls this frequently
// enough that below the bottom two is known to be collapsed.
// Only called when another regexp is about to be pushed
// on the stack, so that the topmost literal is not being considered.
// (Otherwise ab* would turn into (ab)*.)
// If r >= 0, consider pushing a literal r on the stack.
// Return whether that happened.
bool Regexp::ParseState::MaybeConcatString(int r, ParseFlags flags) {
  Regexp* re1;
  Regexp* re2;
  if ((re1 = stacktop_) == NULL || (re2 = re1->down_) == NULL)
    return false;

  if (re1->op_ != kRegexpLiteral && re1->op_ != kRegexpLiteralString)
    return false;
  if (re2->op_ != kRegexpLiteral && re2->op_ != kRegexpLiteralString)
    return false;
  if ((re1->parse_flags_ & FoldCase) != (re2->parse_flags_ & FoldCase))
    return false;

  if (re2->op_ == kRegexpLiteral) {
    // convert into string
    Rune rune = re2->rune_;
    re2->op_ = kRegexpLiteralString;
    re2->nrunes_ = 0;
    re2->runes_ = NULL;
    re2->AddRuneToString(rune);
  }

  // push re1 into re2.
  if (re1->op_ == kRegexpLiteral) {
    re2->AddRuneToString(re1->rune_);
  } else {
    for (int i = 0; i < re1->nrunes_; i++)
      re2->AddRuneToString(re1->runes_[i]);
    re1->nrunes_ = 0;
    delete[] re1->runes_;
    re1->runes_ = NULL;
  }

  // reuse re1 if possible
  if (r >= 0) {
    re1->op_ = kRegexpLiteral;
    re1->rune_ = r;
    re1->parse_flags_ = static_cast<uint16_t>(flags);
    return true;
  }

  stacktop_ = re2;
  re1->Decref();
  return false;
}

// Add a range to the character class, but exclude newline if asked.
// Also handle case folding.
void CharClassBuilder::AddRangeFlags(
    Rune lo, Rune hi, Regexp::ParseFlags parse_flags) {

  // Take out \n if the flags say so.
  bool cutnl = !(parse_flags & Regexp::ClassNL) ||
               (parse_flags & Regexp::NeverNL);
  if (cutnl && lo <= '\n' && '\n' <= hi) {
    if (lo < '\n')
      AddRangeFlags(lo, '\n' - 1, parse_flags);
    if (hi > '\n')
      AddRangeFlags('\n' + 1, hi, parse_flags);
    return;
  }

}

// Maybe parse a Perl character class escape sequence.
// Only recognizes the Perl character classes (\d \s \w \D \S \W),
// not the Perl empty-string classes (\b \B \A \Z \z).
// On success, sets *s to span the remainder of the string
// and returns the corresponding UGroup.
// The StringPiece must *NOT* be edited unless the call succeeds.
const UGroup* MaybeParsePerlCCEscape(StringPiece* s, Regexp::ParseFlags parse_flags) {
  if (!(parse_flags & Regexp::PerlClasses))
    return NULL;
  if (s->size() < 2 || (*s)[0] != '\\')
    return NULL;
  // Could use StringPieceToRune, but there aren't
  // any non-ASCII Perl group names.
  StringPiece name(s->data(), 2);
  const UGroup *g = NULL;
  if (g == NULL)
    return NULL;
  s->remove_prefix(name.size());
  return g;
}

enum ParseStatus {
  kParseOk,  // Did some parsing.
  kParseError,  // Found an error.
  kParseNothing,  // Decided not to parse.
};

// Maybe parses a Unicode character group like \p{Han} or \P{Han}
// (the latter is a negated group).
ParseStatus ParseUnicodeGroup(StringPiece* s, Regexp::ParseFlags parse_flags,
                              CharClassBuilder *cc,
                              RegexpStatus* status) {
  // Decide whether to parse.
  if (!(parse_flags & Regexp::UnicodeGroups))
    return kParseNothing;
  if (s->size() < 2 || (*s)[0] != '\\')
    return kParseNothing;
  Rune c = (*s)[1];
  if (c != 'p' && c != 'P')
    return kParseNothing;

  // Committed to parse.  Results:
  int sign = +1;  // -1 = negated char class
  if (c == 'P')
    sign = -sign;
  StringPiece seq = *s;  // \p{Han} or \pL
  StringPiece name;  // Han or L
  s->remove_prefix(2);  // '\\', 'p'

  if (c != '{') {
    // Name is the bit of string we just skipped over for c.
    const char* p = seq.data() + 2;
    name = StringPiece(p, static_cast<size_t>(s->data() - p));
  } else {
    // Name is in braces. Look for closing }
    size_t end = s->find('}', 0);
    if (end == StringPiece::npos) {
      status->set_code(kRegexpBadCharRange);
      status->set_error_arg(seq);
      return kParseError;
    }
    name = StringPiece(s->data(), end);  // without '}'
    s->remove_prefix(end + 1);  // with '}'
  }

  // Chop seq where s now begins.
  seq = StringPiece(seq.data(), static_cast<size_t>(s->data() - seq.data()));

  if (!name.empty() && name[0] == '^') {
    sign = -sign;
    name.remove_prefix(1);  // '^'
  }

#if !defined(RE2_USE_ICU)
  // Look up the group in the RE2 Unicode data.
  const UGroup *g = NULL;
  if (g == NULL) {
    status->set_code(kRegexpBadCharRange);
    status->set_error_arg(seq);
    return kParseError;
  }

#else
  // Look up the group in the ICU Unicode data. Because ICU provides full
  // Unicode properties support, this could be more than a lookup by name.
  ::icu::UnicodeString ustr = ::icu::UnicodeString::fromUTF8(
      std::string("\\p{") + std::string(name) + std::string("}"));
  UErrorCode uerr = U_ZERO_ERROR;
  ::icu::UnicodeSet uset(ustr, uerr);
  if (U_FAILURE(uerr)) {
    status->set_code(kRegexpBadCharRange);
    status->set_error_arg(seq);
    return kParseError;
  }

  // Convert the UnicodeSet to a URange32 and UGroup that we can add.
  int nr = uset.getRangeCount();
  PODArray<URange32> r(nr);
  for (int i = 0; i < nr; i++) {
    r[i].lo = uset.getRangeStart(i);
    r[i].hi = uset.getRangeEnd(i);
  }
  UGroup g = {"", +1, 0, 0, r.data(), nr};
#endif

  return kParseOk;
}

// Parses a character inside a character class.
// There are fewer special characters here than in the rest of the regexp.
// Sets *s to span the remainder of the string.
// Sets *rp to the character.
bool Regexp::ParseState::ParseCCCharacter(StringPiece* s, Rune *rp,
                                          const StringPiece& whole_class,
                                          RegexpStatus* status) {
  if (s->empty()) {
    status->set_code(kRegexpMissingBracket);
    status->set_error_arg(whole_class);
    return false;
  }

  // Otherwise take the next rune.
  return 1;
}

// Parses a character class character, or, if the character
// is followed by a hyphen, parses a character class range.
// For single characters, rr->lo == rr->hi.
// Sets *s to span the remainder of the string.
// Sets *rp to the character.
bool Regexp::ParseState::ParseCCRange(StringPiece* s, RuneRange* rr,
                                      const StringPiece& whole_class,
                                      RegexpStatus* status) {
  StringPiece os = *s;
  if (!ParseCCCharacter(s, &rr->lo, whole_class, status))
    return false;
  // [a-] means (a|-), so check for final ].
  if (s->size() >= 2 && (*s)[0] == '-' && (*s)[1] != ']') {
    s->remove_prefix(1);  // '-'
    if (!ParseCCCharacter(s, &rr->hi, whole_class, status))
      return false;
    if (rr->hi < rr->lo) {
      status->set_code(kRegexpBadCharRange);
      status->set_error_arg(
          StringPiece(os.data(), static_cast<size_t>(s->data() - os.data())));
      return false;
    }
  } else {
    rr->hi = rr->lo;
  }
  return true;
}

// Parses a possibly-negated character class expression like [^abx-z[:digit:]].
// Sets *s to span the remainder of the string.
// Sets *out_re to the regexp for the class.
bool Regexp::ParseState::ParseCharClass(StringPiece* s,
                                        Regexp** out_re,
                                        RegexpStatus* status) {
  StringPiece whole_class = *s;
  if (s->empty() || (*s)[0] != '[') {
    // Caller checked this.
    status->set_code(kRegexpInternalError);
    status->set_error_arg(StringPiece());
    return false;
  }
  bool negated = false;
  Regexp* re = new Regexp(kRegexpCharClass, flags_ & ~FoldCase);
  re->ccb_ = new CharClassBuilder;
  s->remove_prefix(1);  // '['
  if (!s->empty() && (*s)[0] == '^') {
    s->remove_prefix(1);  // '^'
    negated = true;
    if (!(flags_ & ClassNL) || (flags_ & NeverNL)) {
      // If NL can't match implicitly, then pretend
      // negated classes include a leading \n.
      re->ccb_->AddRange('\n', '\n');
    }
  }
  bool first = true;  // ] is okay as first char in class
  while (!s->empty() && ((*s)[0] != ']' || first)) {
    // - is only okay unescaped as first or last in class.
    // Except that Perl allows - anywhere.
    if ((*s)[0] == '-' && !first && !(flags_&PerlX) &&
        (s->size() == 1 || (*s)[1] != ']')) {
      StringPiece t = *s;
      t.remove_prefix(1);  // '-'
      Rune r;
      int n = 1;
      if (n < 0) {
        re->Decref();
        return false;
      }
      status->set_code(kRegexpBadCharRange);
      status->set_error_arg(StringPiece(s->data(), 1+n));
      re->Decref();
      return false;
    }
    first = false;

    // Look for Unicode character group like \p{Han}
    if (s->size() > 2 &&
        (*s)[0] == '\\' &&
        ((*s)[1] == 'p' || (*s)[1] == 'P')) {
      switch (ParseUnicodeGroup(s, flags_, re->ccb_, status)) {
        case kParseOk:
          continue;
        case kParseError:
          re->Decref();
          return false;
        case kParseNothing:
          break;
      }
    }

    // Look for Perl character class symbols (extension).
    const UGroup *g = MaybeParsePerlCCEscape(s, flags_);

    // Otherwise assume single character or simple range.
    RuneRange rr;
    if (!ParseCCRange(s, &rr, whole_class, status)) {
      re->Decref();
      return false;
    }
    // AddRangeFlags is usually called in response to a class like
    // \p{Foo} or [[:foo:]]; for those, it filters \n out unless
    // Regexp::ClassNL is set.  In an explicit range or singleton
    // like we just parsed, we do not filter \n out, so set ClassNL
    // in the flags.
    re->ccb_->AddRangeFlags(rr.lo, rr.hi, flags_ | Regexp::ClassNL);
  }
  if (s->empty()) {
    status->set_code(kRegexpMissingBracket);
    status->set_error_arg(whole_class);
    re->Decref();
    return false;
  }
  s->remove_prefix(1);  // ']'

  if (negated)
    re->ccb_->Negate();

  *out_re = re;
  return true;
}

// Parses a Perl flag setting or non-capturing group or both,
// like (?i) or (?: or (?i:.  Removes from s, updates parse state.
// The caller must check that s begins with "(?".
// Returns true on success.  If the Perl flag is not
// well-formed or not supported, sets status_ and returns false.
bool Regexp::ParseState::ParsePerlFlags(StringPiece* s) {
  StringPiece t = *s;

  // Caller is supposed to check this.
  if (!(flags_ & PerlX) || t.size() < 2 || t[0] != '(' || t[1] != '?') {
    status_->set_code(kRegexpInternalError);
    LOG(DFATAL) << "Bad call to ParseState::ParsePerlFlags";
    return false;
  }

  t.remove_prefix(2);  // "(?"

  // Check for named captures, first introduced in Python's regexp library.
  // As usual, there are three slightly different syntaxes:
  //
  //   (?P<name>expr)   the original, introduced by Python
  //   (?<name>expr)    the .NET alteration, adopted by Perl 5.10
  //   (?'name'expr)    another .NET alteration, adopted by Perl 5.10
  //
  // Perl 5.10 gave in and implemented the Python version too,
  // but they claim that the last two are the preferred forms.
  // PCRE and languages based on it (specifically, PHP and Ruby)
  // support all three as well.  EcmaScript 4 uses only the Python form.
  //
  // In both the open source world (via Code Search) and the
  // Google source tree, (?P<expr>name) is the dominant form,
  // so that's the one we implement.  One is enough.
  if (t.size() > 2 && t[0] == 'P' && t[1] == '<') {
    // Pull out name.
    size_t end = t.find('>', 2);
    if (end == StringPiece::npos) {
      status_->set_code(kRegexpBadNamedCapture);
      status_->set_error_arg(*s);
      return false;
    }

    // t is "P<name>...", t[end] == '>'
    StringPiece capture(t.data()-2, end+3);  // "(?P<name>"
    StringPiece name(t.data()+2, end-2);     // "name"

    if (!DoLeftParen(name)) {
      // DoLeftParen's failure set status_.
      return false;
    }

    s->remove_prefix(
        static_cast<size_t>(capture.data() + capture.size() - s->data()));
    return true;
  }

  bool negated = false;
  bool sawflags = false;
  int nflags = flags_;
  Rune c;
  for (bool done = false; !done; ) {
    if (t.empty())
      goto BadPerlOp;
    switch (c) {
      default:
        goto BadPerlOp;

      // Parse flags.
      case 'i':
        sawflags = true;
        if (negated)
          nflags &= ~FoldCase;
        else
          nflags |= FoldCase;
        break;

      case 'm':  // opposite of our OneLine
        sawflags = true;
        if (negated)
          nflags |= OneLine;
        else
          nflags &= ~OneLine;
        break;

      case 's':
        sawflags = true;
        if (negated)
          nflags &= ~DotNL;
        else
          nflags |= DotNL;
        break;

      case 'U':
        sawflags = true;
        if (negated)
          nflags &= ~NonGreedy;
        else
          nflags |= NonGreedy;
        break;

      // Negation
      case '-':
        if (negated)
          goto BadPerlOp;
        negated = true;
        sawflags = false;
        break;

      // Open new group.
      case ':':
        if (!DoLeftParenNoCapture()) {
          // DoLeftParenNoCapture's failure set status_.
          return false;
        }
        done = true;
        break;

      // Finish flags.
      case ')':
        done = true;
        break;
    }
  }

  if (negated && !sawflags)
    goto BadPerlOp;

  flags_ = static_cast<Regexp::ParseFlags>(nflags);
  *s = t;
  return true;

BadPerlOp:
  status_->set_code(kRegexpBadPerlOp);
  status_->set_error_arg(
      StringPiece(s->data(), static_cast<size_t>(t.data() - s->data())));
  return false;
}

// Converts latin1 (assumed to be encoded as Latin1 bytes)
// into UTF8 encoding in string.
// Can't use EncodingUtils::EncodeLatin1AsUTF8 because it is
// deprecated and because it rejects code points 0x80-0x9F.
void ConvertLatin1ToUTF8(const StringPiece& latin1, std::string* utf) {
  char buf[UTFmax];

  utf->clear();
  for (size_t i = 0; i < latin1.size(); i++) {
    Rune r = latin1[i] & 0xFF;
    int n = runetochar(buf, &r);
    utf->append(buf, n);
  }
}

// Parses the regular expression given by s,
// returning the corresponding Regexp tree.
// The caller must Decref the return value when done with it.
// Returns NULL on error.
Regexp* Regexp::Parse(const StringPiece& s, ParseFlags global_flags,
                      RegexpStatus* status) {
  // Make status non-NULL (easier on everyone else).
  RegexpStatus xstatus;
  if (status == NULL)
    status = &xstatus;

  ParseState ps(global_flags, s, status);
  StringPiece t = s;

  // Convert regexp to UTF-8 (easier on the rest of the parser).
  if (global_flags & Latin1) {
    std::string* tmp = new std::string;
    ConvertLatin1ToUTF8(t, tmp);
    status->set_tmp(tmp);
    t = *tmp;
  }

  if (global_flags & Literal) {
    // Special parse loop for literal string.
    while (!t.empty()) {
      Rune r;
      if (!ps.PushLiteral(r))
        return NULL;
    }
    return ps.DoFinish();
  }

  StringPiece lastunary = StringPiece();
  while (!t.empty()) {
    StringPiece isunary = StringPiece();
    switch (t[0]) {
      default: {
        Rune r;
        if (!ps.PushLiteral(r))
          return NULL;
        break;
      }

      case '(':
        // "(?" introduces Perl escape.
        if ((ps.flags() & PerlX) && (t.size() >= 2 && t[1] == '?')) {
          // Flag changes and non-capturing groups.
          if (!ps.ParsePerlFlags(&t))
            return NULL;
          break;
        }
        if (ps.flags() & NeverCapture) {
          if (!ps.DoLeftParenNoCapture())
            return NULL;
        } else {
          if (!ps.DoLeftParen(StringPiece()))
            return NULL;
        }
        t.remove_prefix(1);  // '('
        break;

      case '|':
        if (!ps.DoVerticalBar())
          return NULL;
        t.remove_prefix(1);  // '|'
        break;

      case ')':
        if (!ps.DoRightParen())
          return NULL;
        t.remove_prefix(1);  // ')'
        break;

      case '^':  // Beginning of line.
        if (!ps.PushCaret())
          return NULL;
        t.remove_prefix(1);  // '^'
        break;

      case '$':  // End of line.
        if (!ps.PushDollar())
          return NULL;
        t.remove_prefix(1);  // '$'
        break;

      case '.':  // Any character (possibly except newline).
        if (!ps.PushDot())
          return NULL;
        t.remove_prefix(1);  // '.'
        break;

      case '[': {  // Character class.
        Regexp* re;
        if (!ps.ParseCharClass(&t, &re, status))
          return NULL;
        if (!ps.PushRegexp(re))
          return NULL;
        break;
      }

      case '*': {  // Zero or more.
        RegexpOp op;
        op = kRegexpStar;
        goto Rep;
      case '+':  // One or more.
        op = kRegexpPlus;
        goto Rep;
      case '?':  // Zero or one.
        op = kRegexpQuest;
        goto Rep;
      Rep:
        StringPiece opstr = t;
        bool nongreedy = false;
        t.remove_prefix(1);  // '*' or '+' or '?'
        if (ps.flags() & PerlX) {
          if (!t.empty() && t[0] == '?') {
            nongreedy = true;
            t.remove_prefix(1);  // '?'
          }
          if (!lastunary.empty()) {
            // In Perl it is not allowed to stack repetition operators:
            //   a** is a syntax error, not a double-star.
            // (and a++ means something else entirely, which we don't support!)
            status->set_code(kRegexpRepeatOp);
            status->set_error_arg(StringPiece(
                lastunary.data(),
                static_cast<size_t>(t.data() - lastunary.data())));
            return NULL;
          }
        }
        opstr = StringPiece(opstr.data(),
                            static_cast<size_t>(t.data() - opstr.data()));
        if (!ps.PushRepeatOp(op, opstr, nongreedy))
          return NULL;
        isunary = opstr;
        break;
      }

      case '{': {  // Counted repetition.
        int lo, hi;
        StringPiece opstr = t;
        bool nongreedy = false;
        if (ps.flags() & PerlX) {
          if (!t.empty() && t[0] == '?') {
            nongreedy = true;
            t.remove_prefix(1);  // '?'
          }
          if (!lastunary.empty()) {
            // Not allowed to stack repetition operators.
            status->set_code(kRegexpRepeatOp);
            status->set_error_arg(StringPiece(
                lastunary.data(),
                static_cast<size_t>(t.data() - lastunary.data())));
            return NULL;
          }
        }
        opstr = StringPiece(opstr.data(),
                            static_cast<size_t>(t.data() - opstr.data()));
        if (!ps.PushRepetition(lo, hi, opstr, nongreedy))
          return NULL;
        isunary = opstr;
        break;
      }

      case '\\': {  // Escaped character or Perl sequence.
        // \b and \B: word boundary or not
        if ((ps.flags() & Regexp::PerlB) &&
            t.size() >= 2 && (t[1] == 'b' || t[1] == 'B')) {
          if (!ps.PushWordBoundary(t[1] == 'b'))
            return NULL;
          t.remove_prefix(2);  // '\\', 'b'
          break;
        }

        if ((ps.flags() & Regexp::PerlX) && t.size() >= 2) {
          if (t[1] == 'A') {
            if (!ps.PushSimpleOp(kRegexpBeginText))
              return NULL;
            t.remove_prefix(2);  // '\\', 'A'
            break;
          }
          if (t[1] == 'z') {
            if (!ps.PushSimpleOp(kRegexpEndText))
              return NULL;
            t.remove_prefix(2);  // '\\', 'z'
            break;
          }
          // Do not recognize \Z, because this library can't
          // implement the exact Perl/PCRE semantics.
          // (This library treats "(?-m)$" as \z, even though
          // in Perl and PCRE it is equivalent to \Z.)

          if (t[1] == 'C') {  // \C: any byte [sic]
            if (!ps.PushSimpleOp(kRegexpAnyByte))
              return NULL;
            t.remove_prefix(2);  // '\\', 'C'
            break;
          }

          if (t[1] == 'Q') {  // \Q ... \E: the ... is always literals
            t.remove_prefix(2);  // '\\', 'Q'
            while (!t.empty()) {
              if (t.size() >= 2 && t[0] == '\\' && t[1] == 'E') {
                t.remove_prefix(2);  // '\\', 'E'
                break;
              }
              Rune r;
              if (!ps.PushLiteral(r))
                return NULL;
            }
            break;
          }
        }

        if (t.size() >= 2 && (t[1] == 'p' || t[1] == 'P')) {
          Regexp* re = new Regexp(kRegexpCharClass, ps.flags() & ~FoldCase);
          re->ccb_ = new CharClassBuilder;
          switch (ParseUnicodeGroup(&t, ps.flags(), re->ccb_, status)) {
            case kParseOk:
              if (!ps.PushRegexp(re))
                return NULL;
              goto Break2;
            case kParseError:
              re->Decref();
              return NULL;
            case kParseNothing:
              re->Decref();
              break;
          }
        }

        const UGroup *g = MaybeParsePerlCCEscape(&t, ps.flags());
        if (g != NULL) {
          Regexp* re = new Regexp(kRegexpCharClass, ps.flags() & ~FoldCase);
          re->ccb_ = new CharClassBuilder;
          if (!ps.PushRegexp(re))
            return NULL;
          break;
        }

        Rune r;
        if (!ps.PushLiteral(r))
          return NULL;
        break;
      }
    }
  Break2:
    lastunary = isunary;
  }
  return ps.DoFinish();
}

}  // namespace re2
