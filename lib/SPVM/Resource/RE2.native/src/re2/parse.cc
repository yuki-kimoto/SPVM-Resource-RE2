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

}  // namespace re2

#endif  // RE2_UNICODE_CASEFOLD_H_

