// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression interface RE2.
//
// Originally the PCRE C++ wrapper, but adapted to use
// the new automata-based regular expression engines.

#include <atomic>
#include <ostream>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <algorithm>
#include <map>
#include <mutex>
#include <string>
#include <vector>

#include "re2/re2.h"
#include "re2/regexp.h"
#include "re2/unicode_casefold.h"
#include "re2/stringpiece.h"
#include "util/util.h"
#include "util/util.h"
#include "util/logging.h"
#include "util/mutex.h"
#include "util/utf.h"
#include "re2/pod_array.h"
#include "re2/stringpiece.h"
#include "re2/walker-inl.h"

namespace re2 {

const StringPiece::size_type StringPiece::npos;  // initialized in stringpiece.h

StringPiece::size_type StringPiece::copy(char* buf, size_type n,
                                         size_type pos) const {
  size_type ret = std::min(size_ - pos, n);
  memcpy(buf, data_ + pos, ret);
  return ret;
}

StringPiece StringPiece::substr(size_type pos, size_type n) const {
  if (pos > size_) pos = size_;
  if (n > size_ - pos) n = size_ - pos;
  return StringPiece(data_ + pos, n);
}

StringPiece::size_type StringPiece::find(const StringPiece& s,
                                         size_type pos) const {
  if (pos > size_) return npos;
  const_pointer result = std::search(data_ + pos, data_ + size_,
                                     s.data_, s.data_ + s.size_);
  size_type xpos = result - data_;
  return xpos + s.size_ <= size_ ? xpos : npos;
}

StringPiece::size_type StringPiece::find(char c, size_type pos) const {
  if (size_ <= 0 || pos >= size_) return npos;
  const_pointer result = std::find(data_ + pos, data_ + size_, c);
  return result != data_ + size_ ? result - data_ : npos;
}

StringPiece::size_type StringPiece::rfind(const StringPiece& s,
                                          size_type pos) const {
  if (size_ < s.size_) return npos;
  if (s.size_ == 0) return std::min(size_, pos);
  const_pointer last = data_ + std::min(size_ - s.size_, pos) + s.size_;
  const_pointer result = std::find_end(data_, last, s.data_, s.data_ + s.size_);
  return result != last ? result - data_ : npos;
}

StringPiece::size_type StringPiece::rfind(char c, size_type pos) const {
  if (size_ <= 0) return npos;
  for (size_t i = std::min(pos + 1, size_); i != 0;) {
    if (data_[--i] == c) return i;
  }
  return npos;
}

std::ostream& operator<<(std::ostream& o, const StringPiece& p) {
  o.write(p.data(), p.size());
  return o;
}

}  // namespace re2

namespace re2 {


// 1424 groups, 2878 pairs, 367 ranges
const CaseFold unicode_casefold[] = {
	{ 65, 90, 32 },
	{ 97, 106, -32 },
	{ 107, 107, 8383 },
	{ 108, 114, -32 },
	{ 115, 115, 268 },
	{ 116, 122, -32 },
	{ 181, 181, 743 },
	{ 192, 214, 32 },
	{ 216, 222, 32 },
	{ 223, 223, 7615 },
	{ 224, 228, -32 },
	{ 229, 229, 8262 },
	{ 230, 246, -32 },
	{ 248, 254, -32 },
	{ 255, 255, 121 },
	{ 256, 303, EvenOdd },
	{ 306, 311, EvenOdd },
	{ 313, 328, OddEven },
	{ 330, 375, EvenOdd },
	{ 376, 376, -121 },
	{ 377, 382, OddEven },
	{ 383, 383, -300 },
	{ 384, 384, 195 },
	{ 385, 385, 210 },
	{ 386, 389, EvenOdd },
	{ 390, 390, 206 },
	{ 391, 392, OddEven },
	{ 393, 394, 205 },
	{ 395, 396, OddEven },
	{ 398, 398, 79 },
	{ 399, 399, 202 },
	{ 400, 400, 203 },
	{ 401, 402, OddEven },
	{ 403, 403, 205 },
	{ 404, 404, 207 },
	{ 405, 405, 97 },
	{ 406, 406, 211 },
	{ 407, 407, 209 },
	{ 408, 409, EvenOdd },
	{ 410, 410, 163 },
	{ 412, 412, 211 },
	{ 413, 413, 213 },
	{ 414, 414, 130 },
	{ 415, 415, 214 },
	{ 416, 421, EvenOdd },
	{ 422, 422, 218 },
	{ 423, 424, OddEven },
	{ 425, 425, 218 },
	{ 428, 429, EvenOdd },
	{ 430, 430, 218 },
	{ 431, 432, OddEven },
	{ 433, 434, 217 },
	{ 435, 438, OddEven },
	{ 439, 439, 219 },
	{ 440, 441, EvenOdd },
	{ 444, 445, EvenOdd },
	{ 447, 447, 56 },
	{ 452, 452, EvenOdd },
	{ 453, 453, OddEven },
	{ 454, 454, -2 },
	{ 455, 455, OddEven },
	{ 456, 456, EvenOdd },
	{ 457, 457, -2 },
	{ 458, 458, EvenOdd },
	{ 459, 459, OddEven },
	{ 460, 460, -2 },
	{ 461, 476, OddEven },
	{ 477, 477, -79 },
	{ 478, 495, EvenOdd },
	{ 497, 497, OddEven },
	{ 498, 498, EvenOdd },
	{ 499, 499, -2 },
	{ 500, 501, EvenOdd },
	{ 502, 502, -97 },
	{ 503, 503, -56 },
	{ 504, 543, EvenOdd },
	{ 544, 544, -130 },
	{ 546, 563, EvenOdd },
	{ 570, 570, 10795 },
	{ 571, 572, OddEven },
	{ 573, 573, -163 },
	{ 574, 574, 10792 },
	{ 575, 576, 10815 },
	{ 577, 578, OddEven },
	{ 579, 579, -195 },
	{ 580, 580, 69 },
	{ 581, 581, 71 },
	{ 582, 591, EvenOdd },
	{ 592, 592, 10783 },
	{ 593, 593, 10780 },
	{ 594, 594, 10782 },
	{ 595, 595, -210 },
	{ 596, 596, -206 },
	{ 598, 599, -205 },
	{ 601, 601, -202 },
	{ 603, 603, -203 },
	{ 604, 604, 42319 },
	{ 608, 608, -205 },
	{ 609, 609, 42315 },
	{ 611, 611, -207 },
	{ 613, 613, 42280 },
	{ 614, 614, 42308 },
	{ 616, 616, -209 },
	{ 617, 617, -211 },
	{ 618, 618, 42308 },
	{ 619, 619, 10743 },
	{ 620, 620, 42305 },
	{ 623, 623, -211 },
	{ 625, 625, 10749 },
	{ 626, 626, -213 },
	{ 629, 629, -214 },
	{ 637, 637, 10727 },
	{ 640, 640, -218 },
	{ 642, 642, 42307 },
	{ 643, 643, -218 },
	{ 647, 647, 42282 },
	{ 648, 648, -218 },
	{ 649, 649, -69 },
	{ 650, 651, -217 },
	{ 652, 652, -71 },
	{ 658, 658, -219 },
	{ 669, 669, 42261 },
	{ 670, 670, 42258 },
	{ 837, 837, 84 },
	{ 880, 883, EvenOdd },
	{ 886, 887, EvenOdd },
	{ 891, 893, 130 },
	{ 895, 895, 116 },
	{ 902, 902, 38 },
	{ 904, 906, 37 },
	{ 908, 908, 64 },
	{ 910, 911, 63 },
	{ 913, 929, 32 },
	{ 931, 931, 31 },
	{ 932, 939, 32 },
	{ 940, 940, -38 },
	{ 941, 943, -37 },
	{ 945, 945, -32 },
	{ 946, 946, 30 },
	{ 947, 948, -32 },
	{ 949, 949, 64 },
	{ 950, 951, -32 },
	{ 952, 952, 25 },
	{ 953, 953, 7173 },
	{ 954, 954, 54 },
	{ 955, 955, -32 },
	{ 956, 956, -775 },
	{ 957, 959, -32 },
	{ 960, 960, 22 },
	{ 961, 961, 48 },
	{ 962, 962, EvenOdd },
	{ 963, 965, -32 },
	{ 966, 966, 15 },
	{ 967, 968, -32 },
	{ 969, 969, 7517 },
	{ 970, 971, -32 },
	{ 972, 972, -64 },
	{ 973, 974, -63 },
	{ 975, 975, 8 },
	{ 976, 976, -62 },
	{ 977, 977, 35 },
	{ 981, 981, -47 },
	{ 982, 982, -54 },
	{ 983, 983, -8 },
	{ 984, 1007, EvenOdd },
	{ 1008, 1008, -86 },
	{ 1009, 1009, -80 },
	{ 1010, 1010, 7 },
	{ 1011, 1011, -116 },
	{ 1012, 1012, -92 },
	{ 1013, 1013, -96 },
	{ 1015, 1016, OddEven },
	{ 1017, 1017, -7 },
	{ 1018, 1019, EvenOdd },
	{ 1021, 1023, -130 },
	{ 1024, 1039, 80 },
	{ 1040, 1071, 32 },
	{ 1072, 1073, -32 },
	{ 1074, 1074, 6222 },
	{ 1075, 1075, -32 },
	{ 1076, 1076, 6221 },
	{ 1077, 1085, -32 },
	{ 1086, 1086, 6212 },
	{ 1087, 1088, -32 },
	{ 1089, 1090, 6210 },
	{ 1091, 1097, -32 },
	{ 1098, 1098, 6204 },
	{ 1099, 1103, -32 },
	{ 1104, 1119, -80 },
	{ 1120, 1122, EvenOdd },
	{ 1123, 1123, 6180 },
	{ 1124, 1153, EvenOdd },
	{ 1162, 1215, EvenOdd },
	{ 1216, 1216, 15 },
	{ 1217, 1230, OddEven },
	{ 1231, 1231, -15 },
	{ 1232, 1327, EvenOdd },
	{ 1329, 1366, 48 },
	{ 1377, 1414, -48 },
	{ 4256, 4293, 7264 },
	{ 4295, 4295, 7264 },
	{ 4301, 4301, 7264 },
	{ 4304, 4346, 3008 },
	{ 4349, 4351, 3008 },
	{ 5024, 5103, 38864 },
	{ 5104, 5109, 8 },
	{ 5112, 5117, -8 },
	{ 7296, 7296, -6254 },
	{ 7297, 7297, -6253 },
	{ 7298, 7298, -6244 },
	{ 7299, 7299, -6242 },
	{ 7300, 7300, EvenOdd },
	{ 7301, 7301, -6243 },
	{ 7302, 7302, -6236 },
	{ 7303, 7303, -6181 },
	{ 7304, 7304, 35266 },
	{ 7312, 7354, -3008 },
	{ 7357, 7359, -3008 },
	{ 7545, 7545, 35332 },
	{ 7549, 7549, 3814 },
	{ 7566, 7566, 35384 },
	{ 7680, 7776, EvenOdd },
	{ 7777, 7777, 58 },
	{ 7778, 7829, EvenOdd },
	{ 7835, 7835, -59 },
	{ 7838, 7838, -7615 },
	{ 7840, 7935, EvenOdd },
	{ 7936, 7943, 8 },
	{ 7944, 7951, -8 },
	{ 7952, 7957, 8 },
	{ 7960, 7965, -8 },
	{ 7968, 7975, 8 },
	{ 7976, 7983, -8 },
	{ 7984, 7991, 8 },
	{ 7992, 7999, -8 },
	{ 8000, 8005, 8 },
	{ 8008, 8013, -8 },
	{ 8017, 8017, 8 },
	{ 8019, 8019, 8 },
	{ 8021, 8021, 8 },
	{ 8023, 8023, 8 },
	{ 8025, 8025, -8 },
	{ 8027, 8027, -8 },
	{ 8029, 8029, -8 },
	{ 8031, 8031, -8 },
	{ 8032, 8039, 8 },
	{ 8040, 8047, -8 },
	{ 8048, 8049, 74 },
	{ 8050, 8053, 86 },
	{ 8054, 8055, 100 },
	{ 8056, 8057, 128 },
	{ 8058, 8059, 112 },
	{ 8060, 8061, 126 },
	{ 8064, 8071, 8 },
	{ 8072, 8079, -8 },
	{ 8080, 8087, 8 },
	{ 8088, 8095, -8 },
	{ 8096, 8103, 8 },
	{ 8104, 8111, -8 },
	{ 8112, 8113, 8 },
	{ 8115, 8115, 9 },
	{ 8120, 8121, -8 },
	{ 8122, 8123, -74 },
	{ 8124, 8124, -9 },
	{ 8126, 8126, -7289 },
	{ 8131, 8131, 9 },
	{ 8136, 8139, -86 },
	{ 8140, 8140, -9 },
	{ 8144, 8145, 8 },
	{ 8152, 8153, -8 },
	{ 8154, 8155, -100 },
	{ 8160, 8161, 8 },
	{ 8165, 8165, 7 },
	{ 8168, 8169, -8 },
	{ 8170, 8171, -112 },
	{ 8172, 8172, -7 },
	{ 8179, 8179, 9 },
	{ 8184, 8185, -128 },
	{ 8186, 8187, -126 },
	{ 8188, 8188, -9 },
	{ 8486, 8486, -7549 },
	{ 8490, 8490, -8415 },
	{ 8491, 8491, -8294 },
	{ 8498, 8498, 28 },
	{ 8526, 8526, -28 },
	{ 8544, 8559, 16 },
	{ 8560, 8575, -16 },
	{ 8579, 8580, OddEven },
	{ 9398, 9423, 26 },
	{ 9424, 9449, -26 },
	{ 11264, 11311, 48 },
	{ 11312, 11359, -48 },
	{ 11360, 11361, EvenOdd },
	{ 11362, 11362, -10743 },
	{ 11363, 11363, -3814 },
	{ 11364, 11364, -10727 },
	{ 11365, 11365, -10795 },
	{ 11366, 11366, -10792 },
	{ 11367, 11372, OddEven },
	{ 11373, 11373, -10780 },
	{ 11374, 11374, -10749 },
	{ 11375, 11375, -10783 },
	{ 11376, 11376, -10782 },
	{ 11378, 11379, EvenOdd },
	{ 11381, 11382, OddEven },
	{ 11390, 11391, -10815 },
	{ 11392, 11491, EvenOdd },
	{ 11499, 11502, OddEven },
	{ 11506, 11507, EvenOdd },
	{ 11520, 11557, -7264 },
	{ 11559, 11559, -7264 },
	{ 11565, 11565, -7264 },
	{ 42560, 42570, EvenOdd },
	{ 42571, 42571, -35267 },
	{ 42572, 42605, EvenOdd },
	{ 42624, 42651, EvenOdd },
	{ 42786, 42799, EvenOdd },
	{ 42802, 42863, EvenOdd },
	{ 42873, 42876, OddEven },
	{ 42877, 42877, -35332 },
	{ 42878, 42887, EvenOdd },
	{ 42891, 42892, OddEven },
	{ 42893, 42893, -42280 },
	{ 42896, 42899, EvenOdd },
	{ 42900, 42900, 48 },
	{ 42902, 42921, EvenOdd },
	{ 42922, 42922, -42308 },
	{ 42923, 42923, -42319 },
	{ 42924, 42924, -42315 },
	{ 42925, 42925, -42305 },
	{ 42926, 42926, -42308 },
	{ 42928, 42928, -42258 },
	{ 42929, 42929, -42282 },
	{ 42930, 42930, -42261 },
	{ 42931, 42931, 928 },
	{ 42932, 42947, EvenOdd },
	{ 42948, 42948, -48 },
	{ 42949, 42949, -42307 },
	{ 42950, 42950, -35384 },
	{ 42951, 42954, OddEven },
	{ 42960, 42961, EvenOdd },
	{ 42966, 42969, EvenOdd },
	{ 42997, 42998, OddEven },
	{ 43859, 43859, -928 },
	{ 43888, 43967, -38864 },
	{ 65313, 65338, 32 },
	{ 65345, 65370, -32 },
	{ 66560, 66599, 40 },
	{ 66600, 66639, -40 },
	{ 66736, 66771, 40 },
	{ 66776, 66811, -40 },
	{ 66928, 66938, 39 },
	{ 66940, 66954, 39 },
	{ 66956, 66962, 39 },
	{ 66964, 66965, 39 },
	{ 66967, 66977, -39 },
	{ 66979, 66993, -39 },
	{ 66995, 67001, -39 },
	{ 67003, 67004, -39 },
	{ 68736, 68786, 64 },
	{ 68800, 68850, -64 },
	{ 71840, 71871, 32 },
	{ 71872, 71903, -32 },
	{ 93760, 93791, 32 },
	{ 93792, 93823, -32 },
	{ 125184, 125217, 34 },
	{ 125218, 125251, -34 },
};
const int num_unicode_casefold = 367;

// 1424 groups, 1454 pairs, 205 ranges
const CaseFold unicode_tolower[] = {
	{ 65, 90, 32 },
	{ 181, 181, 775 },
	{ 192, 214, 32 },
	{ 216, 222, 32 },
	{ 256, 302, EvenOddSkip },
	{ 306, 310, EvenOddSkip },
	{ 313, 327, OddEvenSkip },
	{ 330, 374, EvenOddSkip },
	{ 376, 376, -121 },
	{ 377, 381, OddEvenSkip },
	{ 383, 383, -268 },
	{ 385, 385, 210 },
	{ 386, 388, EvenOddSkip },
	{ 390, 390, 206 },
	{ 391, 391, OddEven },
	{ 393, 394, 205 },
	{ 395, 395, OddEven },
	{ 398, 398, 79 },
	{ 399, 399, 202 },
	{ 400, 400, 203 },
	{ 401, 401, OddEven },
	{ 403, 403, 205 },
	{ 404, 404, 207 },
	{ 406, 406, 211 },
	{ 407, 407, 209 },
	{ 408, 408, EvenOdd },
	{ 412, 412, 211 },
	{ 413, 413, 213 },
	{ 415, 415, 214 },
	{ 416, 420, EvenOddSkip },
	{ 422, 422, 218 },
	{ 423, 423, OddEven },
	{ 425, 425, 218 },
	{ 428, 428, EvenOdd },
	{ 430, 430, 218 },
	{ 431, 431, OddEven },
	{ 433, 434, 217 },
	{ 435, 437, OddEvenSkip },
	{ 439, 439, 219 },
	{ 440, 440, EvenOdd },
	{ 444, 444, EvenOdd },
	{ 452, 452, 2 },
	{ 453, 453, OddEven },
	{ 455, 455, 2 },
	{ 456, 456, EvenOdd },
	{ 458, 458, 2 },
	{ 459, 475, OddEvenSkip },
	{ 478, 494, EvenOddSkip },
	{ 497, 497, 2 },
	{ 498, 500, EvenOddSkip },
	{ 502, 502, -97 },
	{ 503, 503, -56 },
	{ 504, 542, EvenOddSkip },
	{ 544, 544, -130 },
	{ 546, 562, EvenOddSkip },
	{ 570, 570, 10795 },
	{ 571, 571, OddEven },
	{ 573, 573, -163 },
	{ 574, 574, 10792 },
	{ 577, 577, OddEven },
	{ 579, 579, -195 },
	{ 580, 580, 69 },
	{ 581, 581, 71 },
	{ 582, 590, EvenOddSkip },
	{ 837, 837, 116 },
	{ 880, 882, EvenOddSkip },
	{ 886, 886, EvenOdd },
	{ 895, 895, 116 },
	{ 902, 902, 38 },
	{ 904, 906, 37 },
	{ 908, 908, 64 },
	{ 910, 911, 63 },
	{ 913, 929, 32 },
	{ 931, 939, 32 },
	{ 962, 962, EvenOdd },
	{ 975, 975, 8 },
	{ 976, 976, -30 },
	{ 977, 977, -25 },
	{ 981, 981, -15 },
	{ 982, 982, -22 },
	{ 984, 1006, EvenOddSkip },
	{ 1008, 1008, -54 },
	{ 1009, 1009, -48 },
	{ 1012, 1012, -60 },
	{ 1013, 1013, -64 },
	{ 1015, 1015, OddEven },
	{ 1017, 1017, -7 },
	{ 1018, 1018, EvenOdd },
	{ 1021, 1023, -130 },
	{ 1024, 1039, 80 },
	{ 1040, 1071, 32 },
	{ 1120, 1152, EvenOddSkip },
	{ 1162, 1214, EvenOddSkip },
	{ 1216, 1216, 15 },
	{ 1217, 1229, OddEvenSkip },
	{ 1232, 1326, EvenOddSkip },
	{ 1329, 1366, 48 },
	{ 4256, 4293, 7264 },
	{ 4295, 4295, 7264 },
	{ 4301, 4301, 7264 },
	{ 5112, 5117, -8 },
	{ 7296, 7296, -6222 },
	{ 7297, 7297, -6221 },
	{ 7298, 7298, -6212 },
	{ 7299, 7300, -6210 },
	{ 7301, 7301, -6211 },
	{ 7302, 7302, -6204 },
	{ 7303, 7303, -6180 },
	{ 7304, 7304, 35267 },
	{ 7312, 7354, -3008 },
	{ 7357, 7359, -3008 },
	{ 7680, 7828, EvenOddSkip },
	{ 7835, 7835, -58 },
	{ 7838, 7838, -7615 },
	{ 7840, 7934, EvenOddSkip },
	{ 7944, 7951, -8 },
	{ 7960, 7965, -8 },
	{ 7976, 7983, -8 },
	{ 7992, 7999, -8 },
	{ 8008, 8013, -8 },
	{ 8025, 8025, -8 },
	{ 8027, 8027, -8 },
	{ 8029, 8029, -8 },
	{ 8031, 8031, -8 },
	{ 8040, 8047, -8 },
	{ 8072, 8079, -8 },
	{ 8088, 8095, -8 },
	{ 8104, 8111, -8 },
	{ 8120, 8121, -8 },
	{ 8122, 8123, -74 },
	{ 8124, 8124, -9 },
	{ 8126, 8126, -7173 },
	{ 8136, 8139, -86 },
	{ 8140, 8140, -9 },
	{ 8152, 8153, -8 },
	{ 8154, 8155, -100 },
	{ 8168, 8169, -8 },
	{ 8170, 8171, -112 },
	{ 8172, 8172, -7 },
	{ 8184, 8185, -128 },
	{ 8186, 8187, -126 },
	{ 8188, 8188, -9 },
	{ 8486, 8486, -7517 },
	{ 8490, 8490, -8383 },
	{ 8491, 8491, -8262 },
	{ 8498, 8498, 28 },
	{ 8544, 8559, 16 },
	{ 8579, 8579, OddEven },
	{ 9398, 9423, 26 },
	{ 11264, 11311, 48 },
	{ 11360, 11360, EvenOdd },
	{ 11362, 11362, -10743 },
	{ 11363, 11363, -3814 },
	{ 11364, 11364, -10727 },
	{ 11367, 11371, OddEvenSkip },
	{ 11373, 11373, -10780 },
	{ 11374, 11374, -10749 },
	{ 11375, 11375, -10783 },
	{ 11376, 11376, -10782 },
	{ 11378, 11378, EvenOdd },
	{ 11381, 11381, OddEven },
	{ 11390, 11391, -10815 },
	{ 11392, 11490, EvenOddSkip },
	{ 11499, 11501, OddEvenSkip },
	{ 11506, 11506, EvenOdd },
	{ 42560, 42604, EvenOddSkip },
	{ 42624, 42650, EvenOddSkip },
	{ 42786, 42798, EvenOddSkip },
	{ 42802, 42862, EvenOddSkip },
	{ 42873, 42875, OddEvenSkip },
	{ 42877, 42877, -35332 },
	{ 42878, 42886, EvenOddSkip },
	{ 42891, 42891, OddEven },
	{ 42893, 42893, -42280 },
	{ 42896, 42898, EvenOddSkip },
	{ 42902, 42920, EvenOddSkip },
	{ 42922, 42922, -42308 },
	{ 42923, 42923, -42319 },
	{ 42924, 42924, -42315 },
	{ 42925, 42925, -42305 },
	{ 42926, 42926, -42308 },
	{ 42928, 42928, -42258 },
	{ 42929, 42929, -42282 },
	{ 42930, 42930, -42261 },
	{ 42931, 42931, 928 },
	{ 42932, 42946, EvenOddSkip },
	{ 42948, 42948, -48 },
	{ 42949, 42949, -42307 },
	{ 42950, 42950, -35384 },
	{ 42951, 42953, OddEvenSkip },
	{ 42960, 42960, EvenOdd },
	{ 42966, 42968, EvenOddSkip },
	{ 42997, 42997, OddEven },
	{ 43888, 43967, -38864 },
	{ 65313, 65338, 32 },
	{ 66560, 66599, 40 },
	{ 66736, 66771, 40 },
	{ 66928, 66938, 39 },
	{ 66940, 66954, 39 },
	{ 66956, 66962, 39 },
	{ 66964, 66965, 39 },
	{ 68736, 68786, 64 },
	{ 71840, 71871, 32 },
	{ 93760, 93791, 32 },
	{ 125184, 125217, 34 },
};
const int num_unicode_tolower = 205;



} // namespace re2

namespace re2 {

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

namespace re2 {

// Constructor.  Allocates vectors as appropriate for operator.
Regexp::Regexp(RegexpOp op, ParseFlags parse_flags)
  : op_(static_cast<uint8_t>(op)),
    simple_(false),
    parse_flags_(static_cast<uint16_t>(parse_flags)),
    ref_(1),
    nsub_(0),
    down_(NULL) {
  subone_ = NULL;
  memset(the_union_, 0, sizeof the_union_);
}

// Destructor.  Assumes already cleaned up children.
// Private: use Decref() instead of delete to destroy Regexps.
// Can't call Decref on the sub-Regexps here because
// that could cause arbitrarily deep recursion, so
// required Decref() to have handled them for us.
Regexp::~Regexp() {
  if (nsub_ > 0)
    LOG(DFATAL) << "Regexp not destroyed.";

  switch (op_) {
    default:
      break;
    case kRegexpCapture:
      delete name_;
      break;
    case kRegexpLiteralString:
      delete[] runes_;
      break;
    case kRegexpCharClass:
      if (cc_)
        cc_->Delete();
      delete ccb_;
      break;
  }
}

// If it's possible to destroy this regexp without recurring,
// do so and return true.  Else return false.
bool Regexp::QuickDestroy() {
  if (nsub_ == 0) {
    delete this;
    return true;
  }
  return false;
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

int Regexp::Ref() {
  if (ref_ < kMaxRef)
    return ref_;

  MutexLock l(ref_mutex());
  return (*ref_map())[this];
}

// Increments reference count, returns object as convenience.
Regexp* Regexp::Incref() {
  if (ref_ >= kMaxRef-1) {
    static std::once_flag ref_once;
    std::call_once(ref_once, []() {
      (void) new (ref_storage) RefStorage;
    });

    // Store ref count in overflow map.
    MutexLock l(ref_mutex());
    if (ref_ == kMaxRef) {
      // already overflowed
      (*ref_map())[this]++;
    } else {
      // overflowing now
      (*ref_map())[this] = kMaxRef;
      ref_ = kMaxRef;
    }
    return this;
  }

  ref_++;
  return this;
}

// Decrements reference count and deletes this object if count reaches 0.
void Regexp::Decref() {
  if (ref_ == kMaxRef) {
    // Ref count is stored in overflow map.
    MutexLock l(ref_mutex());
    int r = (*ref_map())[this] - 1;
    if (r < kMaxRef) {
      ref_ = static_cast<uint16_t>(r);
      ref_map()->erase(this);
    } else {
      (*ref_map())[this] = r;
    }
    return;
  }
  ref_--;
  if (ref_ == 0)
    Destroy();
}

// Deletes this object; ref count has count reached 0.
void Regexp::Destroy() {
  if (QuickDestroy())
    return;

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
        if (sub->ref_ == kMaxRef)
          sub->Decref();
        else
          --sub->ref_;
        if (sub->ref_ == 0 && !sub->QuickDestroy()) {
          sub->down_ = stack;
          stack = sub;
        }
      }
      if (re->nsub_ > 1)
        delete[] subs;
      re->nsub_ = 0;
    }
    delete re;
  }
}

void Regexp::AddRuneToString(Rune r) {
  DCHECK(op_ == kRegexpLiteralString);
  if (nrunes_ == 0) {
    // start with 8
    runes_ = new Rune[8];
  } else if (nrunes_ >= 8 && (nrunes_ & (nrunes_ - 1)) == 0) {
    // double on powers of two
    Rune *old = runes_;
    runes_ = new Rune[nrunes_ * 2];
    for (int i = 0; i < nrunes_; i++)
      runes_[i] = old[i];
    delete[] old;
  }

  runes_[nrunes_++] = r;
}

Regexp* Regexp::HaveMatch(int match_id, ParseFlags flags) {
  Regexp* re = new Regexp(kRegexpHaveMatch, flags);
  re->match_id_ = match_id;
  return re;
}

Regexp* Regexp::StarPlusOrQuest(RegexpOp op, Regexp* sub, ParseFlags flags) {
  // Squash **, ++ and ??.
  if (op == sub->op() && flags == sub->parse_flags())
    return sub;

  // Squash *+, *?, +*, +?, ?* and ?+. They all squash to *, so because
  // op is Star/Plus/Quest, we just have to check that sub->op() is too.
  if ((sub->op() == kRegexpStar ||
       sub->op() == kRegexpPlus ||
       sub->op() == kRegexpQuest) &&
      flags == sub->parse_flags()) {
    // If sub is Star, no need to rewrite it.
    if (sub->op() == kRegexpStar)
      return sub;

    // Rewrite sub to Star.
    Regexp* re = new Regexp(kRegexpStar, flags);
    re->AllocSub(1);
    re->sub()[0] = sub->sub()[0]->Incref();
    sub->Decref();  // We didn't consume the reference after all.
    return re;
  }

  Regexp* re = new Regexp(op, flags);
  re->AllocSub(1);
  re->sub()[0] = sub;
  return re;
}

Regexp* Regexp::Plus(Regexp* sub, ParseFlags flags) {
  return StarPlusOrQuest(kRegexpPlus, sub, flags);
}

Regexp* Regexp::Star(Regexp* sub, ParseFlags flags) {
  return StarPlusOrQuest(kRegexpStar, sub, flags);
}

Regexp* Regexp::Quest(Regexp* sub, ParseFlags flags) {
  return StarPlusOrQuest(kRegexpQuest, sub, flags);
}

Regexp* Regexp::ConcatOrAlternate(RegexpOp op, Regexp** sub, int nsub,
                                  ParseFlags flags, bool can_factor) {
  if (nsub == 1)
    return sub[0];

  if (nsub == 0) {
    if (op == kRegexpAlternate)
      return new Regexp(kRegexpNoMatch, flags);
    else
      return new Regexp(kRegexpEmptyMatch, flags);
  }

  PODArray<Regexp*> subcopy;
  if (op == kRegexpAlternate && can_factor) {
    // Going to edit sub; make a copy so we don't step on caller.
    subcopy = PODArray<Regexp*>(nsub);
    memmove(subcopy.data(), sub, nsub * sizeof sub[0]);
    sub = subcopy.data();
    if (nsub == 1) {
      Regexp* re = sub[0];
      return re;
    }
  }

  if (nsub > kMaxNsub) {
    // Too many subexpressions to fit in a single Regexp.
    // Make a two-level tree.  Two levels gets us to 65535^2.
    int nbigsub = (nsub+kMaxNsub-1)/kMaxNsub;
    Regexp* re = new Regexp(op, flags);
    re->AllocSub(nbigsub);
    Regexp** subs = re->sub();
    for (int i = 0; i < nbigsub - 1; i++)
      subs[i] = ConcatOrAlternate(op, sub+i*kMaxNsub, kMaxNsub, flags, false);
    subs[nbigsub - 1] = ConcatOrAlternate(op, sub+(nbigsub-1)*kMaxNsub,
                                          nsub - (nbigsub-1)*kMaxNsub, flags,
                                          false);
    return re;
  }

  Regexp* re = new Regexp(op, flags);
  re->AllocSub(nsub);
  Regexp** subs = re->sub();
  for (int i = 0; i < nsub; i++)
    subs[i] = sub[i];
  return re;
}

Regexp* Regexp::Concat(Regexp** sub, int nsub, ParseFlags flags) {
  return ConcatOrAlternate(kRegexpConcat, sub, nsub, flags, false);
}

Regexp* Regexp::Alternate(Regexp** sub, int nsub, ParseFlags flags) {
  return ConcatOrAlternate(kRegexpAlternate, sub, nsub, flags, true);
}

Regexp* Regexp::AlternateNoFactor(Regexp** sub, int nsub, ParseFlags flags) {
  return ConcatOrAlternate(kRegexpAlternate, sub, nsub, flags, false);
}

Regexp* Regexp::Capture(Regexp* sub, ParseFlags flags, int cap) {
  Regexp* re = new Regexp(kRegexpCapture, flags);
  re->AllocSub(1);
  re->sub()[0] = sub;
  re->cap_ = cap;
  return re;
}

Regexp* Regexp::Repeat(Regexp* sub, ParseFlags flags, int min, int max) {
  Regexp* re = new Regexp(kRegexpRepeat, flags);
  re->AllocSub(1);
  re->sub()[0] = sub;
  re->min_ = min;
  re->max_ = max;
  return re;
}

Regexp* Regexp::NewLiteral(Rune rune, ParseFlags flags) {
  Regexp* re = new Regexp(kRegexpLiteral, flags);
  re->rune_ = rune;
  return re;
}

Regexp* Regexp::LiteralString(Rune* runes, int nrunes, ParseFlags flags) {
  if (nrunes <= 0)
    return new Regexp(kRegexpEmptyMatch, flags);
  if (nrunes == 1)
    return NewLiteral(runes[0], flags);
  Regexp* re = new Regexp(kRegexpLiteralString, flags);
  for (int i = 0; i < nrunes; i++)
    re->AddRuneToString(runes[i]);
  return re;
}

Regexp* Regexp::NewCharClass(CharClass* cc, ParseFlags flags) {
  Regexp* re = new Regexp(kRegexpCharClass, flags);
  re->cc_ = cc;
  return re;
}

void Regexp::Swap(Regexp* that) {
  // Regexp is not trivially copyable, so we cannot freely copy it with
  // memmove(3), but swapping objects like so is safe for our purposes.
  char tmp[sizeof *this];
  void* vthis = reinterpret_cast<void*>(this);
  void* vthat = reinterpret_cast<void*>(that);
  memmove(tmp, vthis, sizeof *this);
  memmove(vthis, vthat, sizeof *this);
  memmove(vthat, tmp, sizeof *this);
}

// Tests equality of all top-level structure but not subregexps.
static bool TopEqual(Regexp* a, Regexp* b) {
  if (a->op() != b->op())
    return false;

  switch (a->op()) {
    case kRegexpNoMatch:
    case kRegexpEmptyMatch:
    case kRegexpAnyChar:
    case kRegexpAnyByte:
    case kRegexpBeginLine:
    case kRegexpEndLine:
    case kRegexpWordBoundary:
    case kRegexpNoWordBoundary:
    case kRegexpBeginText:
      return true;

    case kRegexpEndText:
      // The parse flags remember whether it's \z or (?-m:$),
      // which matters when testing against PCRE.
      return ((a->parse_flags() ^ b->parse_flags()) & Regexp::WasDollar) == 0;

    case kRegexpLiteral:
      return a->rune() == b->rune() &&
             ((a->parse_flags() ^ b->parse_flags()) & Regexp::FoldCase) == 0;

    case kRegexpLiteralString:
      return a->nrunes() == b->nrunes() &&
             ((a->parse_flags() ^ b->parse_flags()) & Regexp::FoldCase) == 0 &&
             memcmp(a->runes(), b->runes(),
                    a->nrunes() * sizeof a->runes()[0]) == 0;

    case kRegexpAlternate:
    case kRegexpConcat:
      return a->nsub() == b->nsub();

    case kRegexpStar:
    case kRegexpPlus:
    case kRegexpQuest:
      return ((a->parse_flags() ^ b->parse_flags()) & Regexp::NonGreedy) == 0;

    case kRegexpRepeat:
      return ((a->parse_flags() ^ b->parse_flags()) & Regexp::NonGreedy) == 0 &&
             a->min() == b->min() &&
             a->max() == b->max();

    case kRegexpCapture:
      return a->cap() == b->cap() && a->name() == b->name();

    case kRegexpHaveMatch:
      return a->match_id() == b->match_id();

    case kRegexpCharClass: {
      CharClass* acc = a->cc();
      CharClass* bcc = b->cc();
      return acc->size() == bcc->size() &&
             acc->end() - acc->begin() == bcc->end() - bcc->begin() &&
             memcmp(acc->begin(), bcc->begin(),
                    (acc->end() - acc->begin()) * sizeof acc->begin()[0]) == 0;
    }
  }

  LOG(DFATAL) << "Unexpected op in Regexp::Equal: " << a->op();
  return 0;
}

bool Regexp::Equal(Regexp* a, Regexp* b) {
  if (a == NULL || b == NULL)
    return a == b;

  if (!TopEqual(a, b))
    return false;

  // Fast path:
  // return without allocating vector if there are no subregexps.
  switch (a->op()) {
    case kRegexpAlternate:
    case kRegexpConcat:
    case kRegexpStar:
    case kRegexpPlus:
    case kRegexpQuest:
    case kRegexpRepeat:
    case kRegexpCapture:
      break;

    default:
      return true;
  }

  // Committed to doing real work.
  // The stack (vector) has pairs of regexps waiting to
  // be compared.  The regexps are only equal if
  // all the pairs end up being equal.
  std::vector<Regexp*> stk;

  for (;;) {
    // Invariant: TopEqual(a, b) == true.
    Regexp* a2;
    Regexp* b2;
    switch (a->op()) {
      default:
        break;
      case kRegexpAlternate:
      case kRegexpConcat:
        for (int i = 0; i < a->nsub(); i++) {
          a2 = a->sub()[i];
          b2 = b->sub()[i];
          if (!TopEqual(a2, b2))
            return false;
          stk.push_back(a2);
          stk.push_back(b2);
        }
        break;

      case kRegexpStar:
      case kRegexpPlus:
      case kRegexpQuest:
      case kRegexpRepeat:
      case kRegexpCapture:
        a2 = a->sub()[0];
        b2 = b->sub()[0];
        if (!TopEqual(a2, b2))
          return false;
        // Really:
        //   stk.push_back(a2);
        //   stk.push_back(b2);
        //   break;
        // but faster to assign directly and loop.
        a = a2;
        b = b2;
        continue;
    }

    size_t n = stk.size();
    if (n == 0)
      break;

    DCHECK_GE(n, 2);
    a = stk[n-2];
    b = stk[n-1];
    stk.resize(n-2);
  }

  return true;
}

// Keep in sync with enum RegexpStatusCode in regexp.h
static const char *kErrorStrings[] = {
  "no error",
  "unexpected error",
  "invalid escape sequence",
  "invalid character class",
  "invalid character class range",
  "missing ]",
  "missing )",
  "unexpected )",
  "trailing \\",
  "no argument for repetition operator",
  "invalid repetition size",
  "bad repetition operator",
  "invalid perl operator",
  "invalid UTF-8",
  "invalid named capture group",
};

std::string RegexpStatus::CodeText(enum RegexpStatusCode code) {
  if (code < 0 || code >= arraysize(kErrorStrings))
    code = kRegexpInternalError;
  return kErrorStrings[code];
}

std::string RegexpStatus::Text() const {
  if (error_arg_.empty())
    return CodeText(code_);
  std::string s;
  s.append(CodeText(code_));
  s.append(": ");
  s.append(error_arg_.data(), error_arg_.size());
  return s;
}

void RegexpStatus::Copy(const RegexpStatus& status) {
  code_ = status.code_;
  error_arg_ = status.error_arg_;
}

typedef int Ignored;  // Walker<void> doesn't exist

// Walker subclass to count capturing parens in regexp.
class NumCapturesWalker : public Regexp::Walker<Ignored> {
 public:
  NumCapturesWalker() : ncapture_(0) {}
  int ncapture() { return ncapture_; }

  virtual Ignored PreVisit(Regexp* re, Ignored ignored, bool* stop) {
    if (re->op() == kRegexpCapture)
      ncapture_++;
    return ignored;
  }

  virtual Ignored ShortVisit(Regexp* re, Ignored ignored) {
    // Should never be called: we use Walk(), not WalkExponential().
#ifndef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
    LOG(DFATAL) << "NumCapturesWalker::ShortVisit called";
#endif
    return ignored;
  }

 private:
  int ncapture_;

  NumCapturesWalker(const NumCapturesWalker&) = delete;
  NumCapturesWalker& operator=(const NumCapturesWalker&) = delete;
};

int Regexp::NumCaptures() {
  NumCapturesWalker w;
  w.Walk(this, 0);
  return w.ncapture();
}

// Walker class to build map of named capture groups and their indices.
class NamedCapturesWalker : public Regexp::Walker<Ignored> {
 public:
  NamedCapturesWalker() : map_(NULL) {}
  ~NamedCapturesWalker() { delete map_; }

  std::map<std::string, int>* TakeMap() {
    std::map<std::string, int>* m = map_;
    map_ = NULL;
    return m;
  }

  virtual Ignored PreVisit(Regexp* re, Ignored ignored, bool* stop) {
    if (re->op() == kRegexpCapture && re->name() != NULL) {
      // Allocate map once we find a name.
      if (map_ == NULL)
        map_ = new std::map<std::string, int>;

      // Record first occurrence of each name.
      // (The rule is that if you have the same name
      // multiple times, only the leftmost one counts.)
      map_->insert({*re->name(), re->cap()});
    }
    return ignored;
  }

  virtual Ignored ShortVisit(Regexp* re, Ignored ignored) {
    // Should never be called: we use Walk(), not WalkExponential().
#ifndef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
    LOG(DFATAL) << "NamedCapturesWalker::ShortVisit called";
#endif
    return ignored;
  }

 private:
  std::map<std::string, int>* map_;

  NamedCapturesWalker(const NamedCapturesWalker&) = delete;
  NamedCapturesWalker& operator=(const NamedCapturesWalker&) = delete;
};

std::map<std::string, int>* Regexp::NamedCaptures() {
  NamedCapturesWalker w;
  w.Walk(this, 0);
  return w.TakeMap();
}

// Walker class to build map from capture group indices to their names.
class CaptureNamesWalker : public Regexp::Walker<Ignored> {
 public:
  CaptureNamesWalker() : map_(NULL) {}
  ~CaptureNamesWalker() { delete map_; }

  std::map<int, std::string>* TakeMap() {
    std::map<int, std::string>* m = map_;
    map_ = NULL;
    return m;
  }

  virtual Ignored PreVisit(Regexp* re, Ignored ignored, bool* stop) {
    if (re->op() == kRegexpCapture && re->name() != NULL) {
      // Allocate map once we find a name.
      if (map_ == NULL)
        map_ = new std::map<int, std::string>;

      (*map_)[re->cap()] = *re->name();
    }
    return ignored;
  }

  virtual Ignored ShortVisit(Regexp* re, Ignored ignored) {
    // Should never be called: we use Walk(), not WalkExponential().
#ifndef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
    LOG(DFATAL) << "CaptureNamesWalker::ShortVisit called";
#endif
    return ignored;
  }

 private:
  std::map<int, std::string>* map_;

  CaptureNamesWalker(const CaptureNamesWalker&) = delete;
  CaptureNamesWalker& operator=(const CaptureNamesWalker&) = delete;
};

std::map<int, std::string>* Regexp::CaptureNames() {
  CaptureNamesWalker w;
  w.Walk(this, 0);
  return w.TakeMap();
}

void ConvertRunesToBytes(bool latin1, Rune* runes, int nrunes,
                         std::string* bytes) {
  if (latin1) {
    bytes->resize(nrunes);
    for (int i = 0; i < nrunes; i++)
      (*bytes)[i] = static_cast<char>(runes[i]);
  } else {
    bytes->resize(nrunes * UTFmax);  // worst case
    char* p = &(*bytes)[0];
    for (int i = 0; i < nrunes; i++)
      p += runetochar(p, &runes[i]);
    bytes->resize(p - &(*bytes)[0]);
    bytes->shrink_to_fit();
  }
}

// Determines whether regexp matches must be anchored
// with a fixed string prefix.  If so, returns the prefix and
// the regexp that remains after the prefix.  The prefix might
// be ASCII case-insensitive.
bool Regexp::RequiredPrefix(std::string* prefix, bool* foldcase,
                            Regexp** suffix) {
  prefix->clear();
  *foldcase = false;
  *suffix = NULL;

  // No need for a walker: the regexp must be of the form
  // 1. some number of ^ anchors
  // 2. a literal char or string
  // 3. the rest
  if (op_ != kRegexpConcat)
    return false;
  int i = 0;
  while (i < nsub_ && sub()[i]->op_ == kRegexpBeginText)
    i++;
  if (i == 0 || i >= nsub_)
    return false;
  Regexp* re = sub()[i];
  if (re->op_ != kRegexpLiteral &&
      re->op_ != kRegexpLiteralString)
    return false;
  i++;
  if (i < nsub_) {
    for (int j = i; j < nsub_; j++)
      sub()[j]->Incref();
    *suffix = Concat(sub() + i, nsub_ - i, parse_flags());
  } else {
    *suffix = new Regexp(kRegexpEmptyMatch, parse_flags());
  }

  bool latin1 = (re->parse_flags() & Latin1) != 0;
  Rune* runes = re->op_ == kRegexpLiteral ? &re->rune_ : re->runes_;
  int nrunes = re->op_ == kRegexpLiteral ? 1 : re->nrunes_;
  ConvertRunesToBytes(latin1, runes, nrunes, prefix);
  *foldcase = (re->parse_flags() & FoldCase) != 0;
  return true;
}

// Determines whether regexp matches must be unanchored
// with a fixed string prefix.  If so, returns the prefix.
// The prefix might be ASCII case-insensitive.
bool Regexp::RequiredPrefixForAccel(std::string* prefix, bool* foldcase) {
  prefix->clear();
  *foldcase = false;

  // No need for a walker: the regexp must either begin with or be
  // a literal char or string. We "see through" capturing groups,
  // but make no effort to glue multiple prefix fragments together.
  Regexp* re = op_ == kRegexpConcat && nsub_ > 0 ? sub()[0] : this;
  while (re->op_ == kRegexpCapture) {
    re = re->sub()[0];
    if (re->op_ == kRegexpConcat && re->nsub_ > 0)
      re = re->sub()[0];
  }
  if (re->op_ != kRegexpLiteral &&
      re->op_ != kRegexpLiteralString)
    return false;

  bool latin1 = (re->parse_flags() & Latin1) != 0;
  Rune* runes = re->op_ == kRegexpLiteral ? &re->rune_ : re->runes_;
  int nrunes = re->op_ == kRegexpLiteral ? 1 : re->nrunes_;
  ConvertRunesToBytes(latin1, runes, nrunes, prefix);
  *foldcase = (re->parse_flags() & FoldCase) != 0;
  return true;
}

// Character class builder is a balanced binary tree (STL set)
// containing non-overlapping, non-abutting RuneRanges.
// The less-than operator used in the tree treats two
// ranges as equal if they overlap at all, so that
// lookups for a particular Rune are possible.

CharClassBuilder::CharClassBuilder() {
  nrunes_ = 0;
  upper_ = 0;
  lower_ = 0;
}

// Add lo-hi to the class; return whether class got bigger.
bool CharClassBuilder::AddRange(Rune lo, Rune hi) {
  if (hi < lo)
    return false;

  if (lo <= 'z' && hi >= 'A') {
    // Overlaps some alpha, maybe not all.
    // Update bitmaps telling which ASCII letters are in the set.
    Rune lo1 = std::max<Rune>(lo, 'A');
    Rune hi1 = std::min<Rune>(hi, 'Z');
    if (lo1 <= hi1)
      upper_ |= ((1 << (hi1 - lo1 + 1)) - 1) << (lo1 - 'A');

    lo1 = std::max<Rune>(lo, 'a');
    hi1 = std::min<Rune>(hi, 'z');
    if (lo1 <= hi1)
      lower_ |= ((1 << (hi1 - lo1 + 1)) - 1) << (lo1 - 'a');
  }

  {  // Check whether lo, hi is already in the class.
    iterator it = ranges_.find(RuneRange(lo, lo));
    if (it != end() && it->lo <= lo && hi <= it->hi)
      return false;
  }

  // Look for a range abutting lo on the left.
  // If it exists, take it out and increase our range.
  if (lo > 0) {
    iterator it = ranges_.find(RuneRange(lo-1, lo-1));
    if (it != end()) {
      lo = it->lo;
      if (it->hi > hi)
        hi = it->hi;
      nrunes_ -= it->hi - it->lo + 1;
      ranges_.erase(it);
    }
  }

  // Look for a range abutting hi on the right.
  // If it exists, take it out and increase our range.
  if (hi < Runemax) {
    iterator it = ranges_.find(RuneRange(hi+1, hi+1));
    if (it != end()) {
      hi = it->hi;
      nrunes_ -= it->hi - it->lo + 1;
      ranges_.erase(it);
    }
  }

  // Look for ranges between lo and hi.  Take them out.
  // This is only safe because the set has no overlapping ranges.
  // We've already removed any ranges abutting lo and hi, so
  // any that overlap [lo, hi] must be contained within it.
  for (;;) {
    iterator it = ranges_.find(RuneRange(lo, hi));
    if (it == end())
      break;
    nrunes_ -= it->hi - it->lo + 1;
    ranges_.erase(it);
  }

  // Finally, add [lo, hi].
  nrunes_ += hi - lo + 1;
  ranges_.insert(RuneRange(lo, hi));
  return true;
}

void CharClassBuilder::AddCharClass(CharClassBuilder *cc) {
  for (iterator it = cc->begin(); it != cc->end(); ++it)
    AddRange(it->lo, it->hi);
}

bool CharClassBuilder::Contains(Rune r) {
  return ranges_.find(RuneRange(r, r)) != end();
}

// Does the character class behave the same on A-Z as on a-z?
bool CharClassBuilder::FoldsASCII() {
  return ((upper_ ^ lower_) & AlphaMask) == 0;
}

CharClassBuilder* CharClassBuilder::Copy() {
  CharClassBuilder* cc = new CharClassBuilder;
  for (iterator it = begin(); it != end(); ++it)
    cc->ranges_.insert(RuneRange(it->lo, it->hi));
  cc->upper_ = upper_;
  cc->lower_ = lower_;
  cc->nrunes_ = nrunes_;
  return cc;
}



void CharClassBuilder::RemoveAbove(Rune r) {
  if (r >= Runemax)
    return;

  if (r < 'z') {
    if (r < 'a')
      lower_ = 0;
    else
      lower_ &= AlphaMask >> ('z' - r);
  }

  if (r < 'Z') {
    if (r < 'A')
      upper_ = 0;
    else
      upper_ &= AlphaMask >> ('Z' - r);
  }

  for (;;) {

    iterator it = ranges_.find(RuneRange(r + 1, Runemax));
    if (it == end())
      break;
    RuneRange rr = *it;
    ranges_.erase(it);
    nrunes_ -= rr.hi - rr.lo + 1;
    if (rr.lo <= r) {
      rr.hi = r;
      ranges_.insert(rr);
      nrunes_ += rr.hi - rr.lo + 1;
    }
  }
}

void CharClassBuilder::Negate() {
  // Build up negation and then copy in.
  // Could edit ranges in place, but C++ won't let me.
  std::vector<RuneRange> v;
  v.reserve(ranges_.size() + 1);

  // In negation, first range begins at 0, unless
  // the current class begins at 0.
  iterator it = begin();
  if (it == end()) {
    v.push_back(RuneRange(0, Runemax));
  } else {
    int nextlo = 0;
    if (it->lo == 0) {
      nextlo = it->hi + 1;
      ++it;
    }
    for (; it != end(); ++it) {
      v.push_back(RuneRange(nextlo, it->lo - 1));
      nextlo = it->hi + 1;
    }
    if (nextlo <= Runemax)
      v.push_back(RuneRange(nextlo, Runemax));
  }

  ranges_.clear();
  for (size_t i = 0; i < v.size(); i++)
    ranges_.insert(v[i]);

  upper_ = AlphaMask & ~upper_;
  lower_ = AlphaMask & ~lower_;
  nrunes_ = Runemax+1 - nrunes_;
}

// Character class is a sorted list of ranges.
// The ranges are allocated in the same block as the header,
// necessitating a special allocator and Delete method.

CharClass* CharClass::New(size_t maxranges) {
  CharClass* cc;
  uint8_t* data = new uint8_t[sizeof *cc + maxranges*sizeof cc->ranges_[0]];
  cc = reinterpret_cast<CharClass*>(data);
  cc->ranges_ = reinterpret_cast<RuneRange*>(data + sizeof *cc);
  cc->nranges_ = 0;
  cc->folds_ascii_ = false;
  cc->nrunes_ = 0;
  return cc;
}

void CharClass::Delete() {
  uint8_t* data = reinterpret_cast<uint8_t*>(this);
  delete[] data;
}

CharClass* CharClass::Negate() {
  CharClass* cc = CharClass::New(static_cast<size_t>(nranges_+1));
  cc->folds_ascii_ = folds_ascii_;
  cc->nrunes_ = Runemax + 1 - nrunes_;
  int n = 0;
  int nextlo = 0;
  for (CharClass::iterator it = begin(); it != end(); ++it) {
    if (it->lo == nextlo) {
      nextlo = it->hi + 1;
    } else {
      cc->ranges_[n++] = RuneRange(nextlo, it->lo - 1);
      nextlo = it->hi + 1;
    }
  }
  if (nextlo <= Runemax)
    cc->ranges_[n++] = RuneRange(nextlo, Runemax);
  cc->nranges_ = n;
  return cc;
}

bool CharClass::Contains(Rune r) const {
  RuneRange* rr = ranges_;
  int n = nranges_;
  while (n > 0) {
    int m = n/2;
    if (rr[m].hi < r) {
      rr += m+1;
      n -= m+1;
    } else if (r < rr[m].lo) {
      n = m;
    } else {  // rr[m].lo <= r && r <= rr[m].hi
      return true;
    }
  }
  return false;
}

CharClass* CharClassBuilder::GetCharClass() {
  CharClass* cc = CharClass::New(ranges_.size());
  int n = 0;
  for (iterator it = begin(); it != end(); ++it)
    cc->ranges_[n++] = *it;
  cc->nranges_ = n;
  DCHECK_LE(n, static_cast<int>(ranges_.size()));
  cc->nrunes_ = nrunes_;
  cc->folds_ascii_ = FoldsASCII();
  return cc;
}

}  // namespace re2
