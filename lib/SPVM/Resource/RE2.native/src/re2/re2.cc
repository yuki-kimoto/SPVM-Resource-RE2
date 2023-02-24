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

#include "re2/prog.h"
#include "re2/regexp.h"

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
