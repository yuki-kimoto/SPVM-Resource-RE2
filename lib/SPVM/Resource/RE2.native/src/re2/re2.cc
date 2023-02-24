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

}  // namespace re2
