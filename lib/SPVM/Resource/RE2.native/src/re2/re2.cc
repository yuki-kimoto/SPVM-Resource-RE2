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

}  // namespace re2
