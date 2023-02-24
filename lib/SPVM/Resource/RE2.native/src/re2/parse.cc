#include "util/utf.h"

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

