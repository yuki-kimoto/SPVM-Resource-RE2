// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression interface RE2.
//
// Originally the PCRE C++ wrapper, but adapted to use
// the new automata-based regular expression engines.

#include <atomic>

#include "re2/re2.h"
#include "util/mutex.h"
#include "re2/walker-inl.h"

namespace re2 {

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

}  // namespace re2
