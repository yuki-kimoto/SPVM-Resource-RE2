// Copyright 2003-2009 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Regular expression interface RE2.
//
// Originally the PCRE C++ wrapper, but adapted to use
// the new automata-based regular expression engines.

#include <atomic>

#include "re2/re2.h"

// Copyright 2007 The RE2 Authors.  All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef UTIL_MUTEX_H_
#define UTIL_MUTEX_H_

/*
 * A simple mutex wrapper, supporting locks and read-write locks.
 * You should assume the locks are *not* re-entrant.
 */

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

#include <stack>

#include "util/logging.h"
#include "re2/regexp.h"

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
