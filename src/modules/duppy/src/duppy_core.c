/*
 * Copyright 2003-2026 Savonet team
 *
 * This file is part of Liquidsoap.
 *
 * Liquidsoap is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Liquidsoap is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details, fully stated in the COPYING
 * file at the root of the liquidsoap distribution.
 *
 * You should have received a copy of the GNU General Public License
 * along with Liquidsoap; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "duppy_core.h"

#include <errno.h>
#include <math.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#define poll WSAPoll
#else
#include <poll.h>
#include <signal.h>
#include <unistd.h>
#endif

#if defined(__linux__)
#include <sys/epoll.h>
#include <sys/timerfd.h>
#define DUPPY_NATIVE "epoll"
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) ||    \
    defined(__NetBSD__) || defined(__DragonFly__)
#include <sys/event.h>
#include <sys/types.h>
#define DUPPY_NATIVE "kqueue"
#endif

#define MAX_EVENTS 512

/* An error or a hangup, which satisfies whatever its descriptor is watched
   for. */
#define FD_FAILED 4
#define NOT_IN_HEAP SIZE_MAX

/* A wake-up is a byte the writer drops when the buffer is full, so the event
   thread never waits on one alone. */
#define LONGEST_WAIT 1.0

typedef struct task task;
typedef struct fd_entry fd_entry;

typedef struct watch {
  duppy_fd fd;
  int interest;
  int fired;
  task *owner;
  fd_entry *entry;
  struct watch *previous;
  struct watch *next;
} watch;

struct task {
  intptr_t handle;
  uint64_t sequence;
  int rank;
  duppy_class task_class;
  int pin;
  uint64_t accepted_by;
  double deadline;
  int delay_fired;
  int collected;
  size_t heap_index;
  /* Links a ready queue, or the tasks collected by one round of events. */
  task *next;
  task *waiting_previous;
  task *waiting_next;
  size_t watch_count;
  watch watches[];
};

struct fd_entry {
  duppy_fd fd;
  watch *watchers;
  int armed;
  fd_entry *next;
};

typedef struct {
  task *head;
  task *tail;
} queue;

typedef struct {
  pthread_cond_t wake_condition;
  int wake;
  int idle;
  int took_batch;
  int running;
  int blocking;
  queue pinned_immediate;
  queue pinned_direct[DUPPY_RANKS];
  queue pinned_threaded[DUPPY_RANKS];
  uint64_t pinned_direct_ranks;
  uint64_t pinned_threaded_ranks;
} worker;

typedef struct {
  duppy_fd fd;
  int flags;
} fd_event;

struct duppy_core {
  pthread_mutex_t mutex;
  duppy_fd wake_read;
  duppy_fd wake_write;
  int fallback;
  int native;
  int timer;
  int started;
  int stopped;
  int error;
  int error_reported;
  pthread_t event_thread;
  uint64_t next_sequence;
  /* The deadline the event thread sleeps until, so that only an earlier one
     has to wake it. */
  int sleeping;
  double sleeping_until;
  task **heap;
  size_t heap_count;
  size_t heap_capacity;
  fd_entry **buckets;
  size_t bucket_count;
  size_t entry_count;
  task *waiting;
  queue immediate;
  queue direct[DUPPY_RANKS];
  queue threaded[DUPPY_RANKS];
  uint64_t direct_ranks;
  uint64_t threaded_ranks;
  worker *workers;
  int worker_count;
  int max_blocking;
  int reserved;
  int slots;
  int blocking;
  struct pollfd *poll_fds;
  size_t poll_capacity;
};

double duppy_core_now(void) {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)now.tv_sec + (double)now.tv_nsec * 1e-9;
}

static void wake_event_thread(duppy_core *core) {
#ifdef _WIN32
  send(core->wake_write, "x", 1, 0);
#else
  if (write(core->wake_write, "x", 1) < 0) {
  }
#endif
}

static void drain_wake(duppy_core *core) {
  char sink[1024];
#ifdef _WIN32
  recv(core->wake_read, sink, sizeof(sink), 0);
#else
  if (read(core->wake_read, sink, sizeof(sink)) < 0) {
  }
#endif
}

/* Native backends: the kernel keeps the registration and reports what fired. */

#if defined(__linux__)

static int native_open(duppy_core *core) {
  struct epoll_event event;
  memset(&event, 0, sizeof(event));
  event.events = EPOLLIN;
  int native = epoll_create1(EPOLL_CLOEXEC);
  core->timer = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC | TFD_NONBLOCK);
  event.data.fd = core->timer;
  if (native >= 0 && core->timer >= 0 &&
      epoll_ctl(native, EPOLL_CTL_ADD, core->timer, &event) == 0)
    return native;
  if (native >= 0)
    close(native);
  if (core->timer >= 0)
    close(core->timer);
  core->timer = -1;
  return -1;
}

static int native_set(duppy_core *core, duppy_fd fd, int interest) {
  struct epoll_event event;
  memset(&event, 0, sizeof(event));
  if (interest & DUPPY_READ)
    event.events |= EPOLLIN;
  if (interest & DUPPY_WRITE)
    event.events |= EPOLLOUT;
  event.data.fd = fd;
  if (epoll_ctl(core->native, EPOLL_CTL_MOD, fd, &event) == 0)
    return 0;
  if (errno != ENOENT)
    return -1;
  return epoll_ctl(core->native, EPOLL_CTL_ADD, fd, &event);
}

static void native_remove(duppy_core *core, duppy_fd fd) {
  epoll_ctl(core->native, EPOLL_CTL_DEL, fd, NULL);
}

/* epoll_wait counts in milliseconds, so the timeout is a timer in the set:
   arming it also clears its previous expiry. */
static int native_wait(duppy_core *core, double timeout, fd_event *events) {
  struct epoll_event fired[MAX_EVENTS];
  struct itimerspec wait;
  memset(&wait, 0, sizeof(wait));
  wait.it_value.tv_sec = (time_t)timeout;
  wait.it_value.tv_nsec =
      (long)((timeout - (double)wait.it_value.tv_sec) * 1e9);
  if (timeout > 0 && wait.it_value.tv_sec == 0 && wait.it_value.tv_nsec == 0)
    wait.it_value.tv_nsec = 1;
  timerfd_settime(core->timer, 0, &wait, NULL);
  int fired_count =
      epoll_wait(core->native, fired, MAX_EVENTS, timeout > 0 ? -1 : 0);
  int count = 0;
  for (int i = 0; i < fired_count; i++) {
    if (fired[i].data.fd == core->timer)
      continue;
    events[count].fd = fired[i].data.fd;
    events[count].flags = 0;
    if (fired[i].events & EPOLLIN)
      events[count].flags |= DUPPY_READ;
    if (fired[i].events & EPOLLOUT)
      events[count].flags |= DUPPY_WRITE;
    if (fired[i].events & (EPOLLERR | EPOLLHUP))
      events[count].flags |= FD_FAILED;
    count++;
  }
  return fired_count < 0 ? fired_count : count;
}

#elif defined(DUPPY_NATIVE)

static int native_open(duppy_core *core) {
  (void)core;
  return kqueue();
}

static int native_filter(duppy_core *core, duppy_fd fd, int filter,
                         int enable) {
  struct kevent change;
  EV_SET(&change, fd, filter, enable ? EV_ADD : EV_DELETE, 0, 0, NULL);
  if (kevent(core->native, &change, 1, NULL, 0, NULL) == 0)
    return 0;
  return (!enable && (errno == ENOENT || errno == EBADF)) ? 0 : -1;
}

static int native_set(duppy_core *core, duppy_fd fd, int interest) {
  if (native_filter(core, fd, EVFILT_READ, interest & DUPPY_READ) == -1)
    return -1;
  return native_filter(core, fd, EVFILT_WRITE, interest & DUPPY_WRITE);
}

static void native_remove(duppy_core *core, duppy_fd fd) {
  native_filter(core, fd, EVFILT_READ, 0);
  native_filter(core, fd, EVFILT_WRITE, 0);
}

static int native_wait(duppy_core *core, double timeout, fd_event *events) {
  struct kevent fired[MAX_EVENTS];
  struct timespec wait;
  wait.tv_sec = (time_t)timeout;
  wait.tv_nsec = (long)((timeout - (double)wait.tv_sec) * 1e9);
  int count = kevent(core->native, NULL, 0, fired, MAX_EVENTS, &wait);
  for (int i = 0; i < count; i++) {
    events[i].fd = (duppy_fd)fired[i].ident;
    events[i].flags = 0;
    if (fired[i].filter == EVFILT_READ)
      events[i].flags |= DUPPY_READ;
    if (fired[i].filter == EVFILT_WRITE)
      events[i].flags |= DUPPY_WRITE;
    if (fired[i].flags & (EV_ERROR | EV_EOF))
      events[i].flags |= FD_FAILED;
  }
  return count;
}

#else

static int native_open(duppy_core *core) {
  (void)core;
  return -1;
}

static int native_set(duppy_core *core, duppy_fd fd, int interest) {
  (void)core;
  (void)fd;
  (void)interest;
  return -1;
}

static void native_remove(duppy_core *core, duppy_fd fd) {
  (void)core;
  (void)fd;
}

static int native_wait(duppy_core *core, double timeout, fd_event *events) {
  (void)core;
  (void)timeout;
  (void)events;
  return -1;
}

#endif

/* The fallback hands the whole set to poll on every wait: the event thread
   snapshots it under the mutex and has to be woken whenever it changes. */
static int fallback_snapshot(duppy_core *core) {
  size_t needed = core->entry_count + 1;
  if (core->poll_capacity < needed) {
    struct pollfd *grown =
        realloc(core->poll_fds, 2 * needed * sizeof(struct pollfd));
    if (grown == NULL)
      return -1;
    core->poll_fds = grown;
    core->poll_capacity = 2 * needed;
  }
  size_t count = 0;
  core->poll_fds[count].fd = core->wake_read;
  core->poll_fds[count++].events = POLLIN;
  for (size_t bucket = 0; bucket < core->bucket_count; bucket++)
    for (fd_entry *entry = core->buckets[bucket]; entry; entry = entry->next) {
      core->poll_fds[count].fd = entry->fd;
      core->poll_fds[count].events =
          ((entry->armed & DUPPY_READ) ? POLLIN : 0) |
          ((entry->armed & DUPPY_WRITE) ? POLLOUT : 0);
      count++;
    }
  return (int)count;
}

/* ponytail: poll counts in milliseconds, so a deadline is met up to one late;
   ppoll where it exists would remove that. */
static int fallback_wait(duppy_core *core, int watched, double timeout,
                         fd_event *events) {
  int ready = poll(core->poll_fds, watched, (int)ceil(timeout * 1e3));
  if (ready < 0)
    return ready;
  int count = 0;
  for (int i = 0; i < watched && count < MAX_EVENTS; i++) {
    short fired = core->poll_fds[i].revents;
    if (fired == 0)
      continue;
    events[count].fd = core->poll_fds[i].fd;
    events[count].flags = 0;
    if (fired & POLLIN)
      events[count].flags |= DUPPY_READ;
    if (fired & POLLOUT)
      events[count].flags |= DUPPY_WRITE;
    if (fired & (POLLERR | POLLHUP | POLLNVAL))
      events[count].flags |= FD_FAILED;
    count++;
  }
  return count;
}

static int backend_set(duppy_core *core, duppy_fd fd, int interest) {
  if (!core->fallback)
    return native_set(core, fd, interest);
  if (core->sleeping)
    wake_event_thread(core);
  return 0;
}

static void backend_remove(duppy_core *core, duppy_fd fd) {
  if (!core->fallback)
    native_remove(core, fd);
}

const char *duppy_core_backend(const duppy_core *core) {
#ifdef DUPPY_NATIVE
  if (!core->fallback)
    return DUPPY_NATIVE;
#endif
  (void)core;
  return "poll";
}

/* Deadlines: a binary heap ordered by deadline, then by submission. */

static int expires_before(const task *left, const task *right) {
  if (left->deadline != right->deadline)
    return left->deadline < right->deadline;
  return left->sequence < right->sequence;
}

static void heap_place(duppy_core *core, size_t index, task *entry) {
  core->heap[index] = entry;
  entry->heap_index = index;
}

static void heap_sift_up(duppy_core *core, size_t index) {
  task *moving = core->heap[index];
  while (index > 0) {
    size_t parent = (index - 1) / 2;
    if (!expires_before(moving, core->heap[parent]))
      break;
    heap_place(core, index, core->heap[parent]);
    index = parent;
  }
  heap_place(core, index, moving);
}

static void heap_sift_down(duppy_core *core, size_t index) {
  task *moving = core->heap[index];
  for (;;) {
    size_t child = 2 * index + 1;
    if (child >= core->heap_count)
      break;
    if (child + 1 < core->heap_count &&
        expires_before(core->heap[child + 1], core->heap[child]))
      child++;
    if (!expires_before(core->heap[child], moving))
      break;
    heap_place(core, index, core->heap[child]);
    index = child;
  }
  heap_place(core, index, moving);
}

static int heap_push(duppy_core *core, task *entry) {
  if (core->heap_count == core->heap_capacity) {
    size_t capacity = core->heap_capacity ? 2 * core->heap_capacity : 64;
    task **grown = realloc(core->heap, capacity * sizeof(task *));
    if (grown == NULL)
      return -1;
    core->heap = grown;
    core->heap_capacity = capacity;
  }
  heap_place(core, core->heap_count++, entry);
  heap_sift_up(core, entry->heap_index);
  return 0;
}

static void heap_remove(duppy_core *core, task *entry) {
  size_t index = entry->heap_index;
  if (index == NOT_IN_HEAP)
    return;
  entry->heap_index = NOT_IN_HEAP;
  task *last = core->heap[--core->heap_count];
  if (last == entry)
    return;
  heap_place(core, index, last);
  heap_sift_up(core, index);
  heap_sift_down(core, last->heap_index);
}

/* Watched descriptors: a chained hash table from descriptor to the watches
   waiting on it. */

static size_t bucket_of(const duppy_core *core, duppy_fd fd) {
  uint64_t hash = (uint64_t)fd * UINT64_C(0x9E3779B97F4A7C15);
  return (size_t)(hash >> 32) & (core->bucket_count - 1);
}

static fd_entry *entry_find(const duppy_core *core, duppy_fd fd) {
  fd_entry *entry = core->buckets[bucket_of(core, fd)];
  while (entry && entry->fd != fd)
    entry = entry->next;
  return entry;
}

static void buckets_grow(duppy_core *core) {
  size_t previous_count = core->bucket_count;
  fd_entry **previous = core->buckets;
  fd_entry **grown = calloc(2 * previous_count, sizeof(fd_entry *));
  if (grown == NULL)
    return;
  core->buckets = grown;
  core->bucket_count = 2 * previous_count;
  for (size_t bucket = 0; bucket < previous_count; bucket++) {
    fd_entry *entry = previous[bucket];
    while (entry) {
      fd_entry *next = entry->next;
      size_t target = bucket_of(core, entry->fd);
      entry->next = core->buckets[target];
      core->buckets[target] = entry;
      entry = next;
    }
  }
  free(previous);
}

static fd_entry *entry_create(duppy_core *core, duppy_fd fd) {
  fd_entry *entry = calloc(1, sizeof(fd_entry));
  if (entry == NULL)
    return NULL;
  if (core->entry_count >= core->bucket_count)
    buckets_grow(core);
  size_t bucket = bucket_of(core, fd);
  entry->fd = fd;
  entry->next = core->buckets[bucket];
  core->buckets[bucket] = entry;
  core->entry_count++;
  return entry;
}

static void entry_delete(duppy_core *core, fd_entry *entry) {
  fd_entry **link = &core->buckets[bucket_of(core, entry->fd)];
  while (*link != entry)
    link = &(*link)->next;
  *link = entry->next;
  core->entry_count--;
  free(entry);
}

/* What a descriptor is armed for is the union of what its watches want, so
   dropping one of them does not stop watching for the others. */
static int entry_rearm(duppy_core *core, fd_entry *entry) {
  int wanted = 0;
  for (watch *watcher = entry->watchers; watcher; watcher = watcher->next)
    wanted |= watcher->interest;
  if (backend_set(core, entry->fd, wanted) != 0)
    return -1;
  entry->armed = wanted;
  return 0;
}

static void watch_unlink(watch *watcher) {
  fd_entry *entry = watcher->entry;
  if (watcher->previous)
    watcher->previous->next = watcher->next;
  else
    entry->watchers = watcher->next;
  if (watcher->next)
    watcher->next->previous = watcher->previous;
  watcher->entry = NULL;
}

static int watch_register(duppy_core *core, watch *watcher) {
  fd_entry *entry = entry_find(core, watcher->fd);
  if (entry == NULL)
    entry = entry_create(core, watcher->fd);
  if (entry == NULL)
    return -1;
  watcher->entry = entry;
  watcher->previous = NULL;
  watcher->next = entry->watchers;
  if (entry->watchers)
    entry->watchers->previous = watcher;
  entry->watchers = watcher;
  if (entry_rearm(core, entry) == 0)
    return 0;
  watch_unlink(watcher);
  if (entry->watchers == NULL)
    entry_delete(core, entry);
  return -1;
}

/* A descriptor closed while watches remain cannot be re-armed: they are left
   to their task's deadline. */
static void watch_unregister(duppy_core *core, watch *watcher) {
  fd_entry *entry = watcher->entry;
  if (entry == NULL)
    return;
  watch_unlink(watcher);
  if (entry->watchers == NULL) {
    backend_remove(core, entry->fd);
    entry_delete(core, entry);
  } else
    entry_rearm(core, entry);
}

static void waiting_link(duppy_core *core, task *entry) {
  entry->waiting_previous = NULL;
  entry->waiting_next = core->waiting;
  if (core->waiting)
    core->waiting->waiting_previous = entry;
  core->waiting = entry;
}

static void task_stop_waiting(duppy_core *core, task *entry) {
  for (size_t i = 0; i < entry->watch_count; i++)
    watch_unregister(core, &entry->watches[i]);
  heap_remove(core, entry);
  if (entry->waiting_previous)
    entry->waiting_previous->waiting_next = entry->waiting_next;
  else if (core->waiting == entry)
    core->waiting = entry->waiting_next;
  if (entry->waiting_next)
    entry->waiting_next->waiting_previous = entry->waiting_previous;
  entry->waiting_previous = entry->waiting_next = NULL;
}

/* Ready tasks: one queue per class and rank, and the same again per worker
   for the tasks pinned to it. */

static void queue_push(queue *target, task *entry) {
  entry->next = NULL;
  if (target->tail)
    target->tail->next = entry;
  else
    target->head = entry;
  target->tail = entry;
}

static void queue_unlink(queue *source, task *previous, task *entry) {
  if (previous)
    previous->next = entry->next;
  else
    source->head = entry->next;
  if (source->tail == entry)
    source->tail = previous;
  entry->next = NULL;
}

static int accepted_by(const task *entry, int worker_index) {
  if (worker_index >= 64)
    return entry->accepted_by == DUPPY_EVERY_WORKER;
  return (entry->accepted_by >> worker_index) & 1;
}

/* Whether a worker that is not running a handler and holds fewer threaded
   tasks could take this one: leaving it to that worker is what spreads them
   over the pool. */
static int better_placed(const duppy_core *core, const task *entry,
                         int worker_index) {
  const worker *taker = &core->workers[worker_index];
  for (int i = 0; i < core->worker_count; i++) {
    const worker *other = &core->workers[i];
    if (i != worker_index && !other->running &&
        other->blocking < taker->blocking && accepted_by(entry, i))
      return 1;
  }
  return 0;
}

/* balanced is the core when the queue holds threaded tasks, NULL otherwise. */
static task *queue_find(queue *source, int worker_index,
                        const duppy_core *balanced, task **previous) {
  *previous = NULL;
  for (task *entry = source->head; entry; entry = entry->next) {
    if (accepted_by(entry, worker_index) &&
        !(balanced && better_placed(balanced, entry, worker_index)))
      return entry;
    *previous = entry;
  }
  return NULL;
}

static int can_take(const duppy_core *core, int worker_index,
                    const task *entry) {
  if (entry->pin != DUPPY_ANY_WORKER && entry->pin != worker_index)
    return 0;
  if (!accepted_by(entry, worker_index))
    return 0;
  if (entry->task_class != DUPPY_THREADED)
    return 1;
  return core->blocking < core->slots &&
         (entry->pin != DUPPY_ANY_WORKER ||
          !better_placed(core, entry, worker_index));
}

static void wake_worker(worker *sleeper) {
  sleeper->idle = 0;
  sleeper->wake = 1;
  pthread_cond_signal(&sleeper->wake_condition);
}

static void offer(duppy_core *core, const task *entry) {
  for (int i = 0; i < core->worker_count; i++)
    if (core->workers[i].idle && can_take(core, i, entry)) {
      wake_worker(&core->workers[i]);
      return;
    }
}

static void make_ready(duppy_core *core, task *entry) {
  int rank = entry->rank;
  uint64_t bit = UINT64_C(1) << rank;
  if (entry->pin != DUPPY_ANY_WORKER) {
    worker *owner = &core->workers[entry->pin];
    switch (entry->task_class) {
    case DUPPY_IMMEDIATE:
      queue_push(&owner->pinned_immediate, entry);
      break;
    case DUPPY_DIRECT:
      queue_push(&owner->pinned_direct[rank], entry);
      owner->pinned_direct_ranks |= bit;
      break;
    case DUPPY_THREADED:
      queue_push(&owner->pinned_threaded[rank], entry);
      owner->pinned_threaded_ranks |= bit;
      break;
    }
    offer(core, entry);
    return;
  }
  switch (entry->task_class) {
  case DUPPY_IMMEDIATE: {
    /* A worker is already on its way for a queue that is not empty, and will
       take this one in the same batch. */
    int joins_a_batch = core->immediate.head != NULL &&
                        entry->accepted_by == DUPPY_EVERY_WORKER;
    queue_push(&core->immediate, entry);
    if (joins_a_batch)
      return;
    break;
  }
  case DUPPY_DIRECT:
    queue_push(&core->direct[rank], entry);
    core->direct_ranks |= bit;
    break;
  case DUPPY_THREADED:
    queue_push(&core->threaded[rank], entry);
    core->threaded_ranks |= bit;
    break;
  }
  offer(core, entry);
}

typedef struct {
  task *entry;
  task *previous;
  queue *source;
  uint64_t *ranks;
} candidate;

/* The earlier of a shared queue's first acceptable task and a pinned queue's
   first task. */
static candidate earliest(queue *shared, uint64_t *shared_ranks, queue *pinned,
                          uint64_t *pinned_ranks, int worker_index,
                          const duppy_core *balanced) {
  candidate found = {NULL, NULL, NULL, NULL};
  task *previous;
  task *from_shared = queue_find(shared, worker_index, balanced, &previous);
  task *from_pinned = pinned->head;
  if (from_shared &&
      (!from_pinned || from_shared->sequence < from_pinned->sequence)) {
    found.entry = from_shared;
    found.previous = previous;
    found.source = shared;
    found.ranks = shared_ranks;
  } else if (from_pinned) {
    found.entry = from_pinned;
    found.source = pinned;
    found.ranks = pinned_ranks;
  }
  return found;
}

static candidate find_single(duppy_core *core, int worker_index, int has_slot) {
  worker *taker = &core->workers[worker_index];
  candidate found = {NULL, NULL, NULL, NULL};
  uint64_t ranks = core->direct_ranks | taker->pinned_direct_ranks;
  if (has_slot)
    ranks |= core->threaded_ranks | taker->pinned_threaded_ranks;
  while (ranks) {
    int rank = __builtin_ctzll(ranks);
    ranks &= ranks - 1;
    found = earliest(&core->direct[rank], &core->direct_ranks,
                     &taker->pinned_direct[rank], &taker->pinned_direct_ranks,
                     worker_index, NULL);
    if (found.entry == NULL && has_slot)
      found = earliest(&core->threaded[rank], &core->threaded_ranks,
                       &taker->pinned_threaded[rank],
                       &taker->pinned_threaded_ranks, worker_index, core);
    if (found.entry)
      return found;
  }
  return found;
}

static int has_work(duppy_core *core, int worker_index) {
  worker *idler = &core->workers[worker_index];
  if (find_single(core, worker_index, core->blocking < core->slots).entry)
    return 1;
  return earliest(&core->immediate, NULL, &idler->pinned_immediate, NULL,
                  worker_index, NULL)
             .entry != NULL;
}

/* A worker woken for one task may take another, which leaves the first to
   whoever else is idle. */
static void offer_remaining(duppy_core *core) {
  for (int i = 0; i < core->worker_count; i++)
    if (core->workers[i].idle && has_work(core, i)) {
      wake_worker(&core->workers[i]);
      return;
    }
}

static void take_candidate(candidate *found) {
  queue_unlink(found->source, found->previous, found->entry);
  if (found->ranks && found->source->head == NULL)
    *found->ranks &= ~(UINT64_C(1) << found->entry->rank);
}

static size_t report(const task *entry, intptr_t *out) {
  size_t written = 0;
  out[written++] = entry->handle;
  out[written++] = entry->delay_fired;
  for (size_t i = 0; i < entry->watch_count; i++)
    out[written++] = entry->watches[i].fired;
  return written;
}

static void free_chain(task *chain) {
  while (chain) {
    task *next = chain->next;
    free(chain);
    chain = next;
  }
}

duppy_work duppy_core_take(duppy_core *core, int worker_index, intptr_t *out,
                           size_t capacity, size_t *written) {
  task *taken = NULL;
  duppy_work work = DUPPY_NONE;
  *written = 0;

  pthread_mutex_lock(&core->mutex);
  worker *taker = &core->workers[worker_index];
  taker->idle = 0;
  taker->running = 0;
  if (core->error && !core->error_reported) {
    core->error_reported = 1;
    work = DUPPY_FAILED;
  } else if (core->stopped) {
    work = DUPPY_STOPPED;
  } else {
    int has_slot = core->blocking < core->slots;
    candidate single = find_single(core, worker_index, has_slot);
    candidate batched =
        earliest(&core->immediate, NULL, &taker->pinned_immediate, NULL,
                 worker_index, NULL);
    /* A worker alternates a batch with a single task: immediate tasks that
       become ready as fast as they run would otherwise starve the rest, which
       a lone worker cannot leave to another. */
    if (batched.entry && !(taker->took_batch && single.entry)) {
      task **last = &taken;
      while (batched.entry &&
             *written + batched.entry->watch_count + 2 <= capacity) {
        take_candidate(&batched);
        *written += report(batched.entry, out + *written);
        *last = batched.entry;
        last = &batched.entry->next;
        batched = earliest(&core->immediate, NULL, &taker->pinned_immediate,
                           NULL, worker_index, NULL);
      }
      taker->took_batch = 1;
      taker->running = 1;
      work = DUPPY_BATCH;
    } else if (single.entry) {
      take_candidate(&single);
      *written = report(single.entry, out);
      taken = single.entry;
      taker->took_batch = 0;
      if (single.entry->task_class == DUPPY_THREADED) {
        taker->blocking++;
        core->blocking++;
        work = DUPPY_ONE_THREADED;
      } else {
        taker->running = 1;
        work = DUPPY_ONE_DIRECT;
      }
    } else
      taker->idle = 1;
    offer_remaining(core);
  }
  pthread_mutex_unlock(&core->mutex);

  free_chain(taken);
  return work;
}

void duppy_core_wait(duppy_core *core, int worker_index) {
  pthread_mutex_lock(&core->mutex);
  worker *sleeper = &core->workers[worker_index];
  while (!sleeper->wake && !core->stopped &&
         !(core->error && !core->error_reported))
    pthread_cond_wait(&sleeper->wake_condition, &core->mutex);
  sleeper->wake = 0;
  sleeper->idle = 0;
  pthread_mutex_unlock(&core->mutex);
}

void duppy_core_blocking_done(duppy_core *core, int worker_index) {
  pthread_mutex_lock(&core->mutex);
  worker *owner = &core->workers[worker_index];
  owner->blocking--;
  core->blocking--;
  offer_remaining(core);
  pthread_mutex_unlock(&core->mutex);
}

static void wake_every_worker(duppy_core *core) {
  for (int i = 0; i < core->worker_count; i++)
    wake_worker(&core->workers[i]);
}

static void update_slots(duppy_core *core) {
  int slots = core->max_blocking + core->reserved;
  core->slots = slots > 1 ? slots : 1;
}

int duppy_core_reserve(duppy_core *core, int delta) {
  pthread_mutex_lock(&core->mutex);
  core->reserved += delta;
  update_slots(core);
  int slots = core->slots;
  for (int i = 0; i < core->worker_count; i++)
    if (core->workers[i].idle)
      wake_worker(&core->workers[i]);
  pthread_mutex_unlock(&core->mutex);
  return slots;
}

int duppy_core_slots(duppy_core *core) {
  pthread_mutex_lock(&core->mutex);
  int slots = core->slots;
  pthread_mutex_unlock(&core->mutex);
  return slots;
}

static int fired_mask(int interest, int flags) {
  int fired = 0;
  if ((interest & DUPPY_READ) && (flags & (DUPPY_READ | FD_FAILED)))
    fired |= DUPPY_READ;
  if ((interest & DUPPY_WRITE) && (flags & (DUPPY_WRITE | FD_FAILED)))
    fired |= DUPPY_WRITE;
  return fired;
}

int duppy_core_submit(duppy_core *core, const duppy_task *request) {
  if (request->rank < 0 || request->rank >= DUPPY_RANKS ||
      request->fd_count > DUPPY_MAX_FDS ||
      request->task_class < DUPPY_IMMEDIATE ||
      request->task_class > DUPPY_THREADED) {
    errno = EINVAL;
    return -1;
  }
  task *entry =
      calloc(1, sizeof(task) + request->fd_count * sizeof(struct watch));
  if (entry == NULL) {
    errno = ENOMEM;
    return -1;
  }
  entry->handle = request->handle;
  entry->rank = request->rank;
  entry->task_class = request->task_class;
  entry->pin = request->pin;
  entry->accepted_by = request->accepted_by;
  entry->heap_index = NOT_IN_HEAP;
  entry->watch_count = request->fd_count;
  for (size_t i = 0; i < request->fd_count; i++) {
    entry->watches[i].fd = request->fds[i];
    entry->watches[i].interest = request->interests[i];
    entry->watches[i].owner = entry;
  }

  int failure = 0;
  pthread_mutex_lock(&core->mutex);
  if (core->stopped) {
    free(entry);
  } else if (entry->pin != DUPPY_ANY_WORKER &&
             (entry->pin < 0 || entry->pin >= core->worker_count ||
              !accepted_by(entry, entry->pin))) {
    failure = EINVAL;
  } else {
    entry->sequence = core->next_sequence++;
    entry->deadline =
        request->delay > 0 ? duppy_core_now() + request->delay : INFINITY;
    int unwatchable = 0;
    if (request->delay == 0)
      entry->delay_fired = 1;
    else
      for (size_t i = 0; i < entry->watch_count && !unwatchable; i++)
        if (watch_register(core, &entry->watches[i]) != 0) {
          entry->watches[i].fired = entry->watches[i].interest;
          unwatchable = 1;
        }
    if (!unwatchable && !entry->delay_fired && isfinite(entry->deadline) &&
        heap_push(core, entry) != 0)
      failure = ENOMEM;
    if (failure || unwatchable || entry->delay_fired) {
      for (size_t i = 0; i < entry->watch_count; i++)
        watch_unregister(core, &entry->watches[i]);
      if (!failure)
        make_ready(core, entry);
    } else {
      waiting_link(core, entry);
      if (core->sleeping && entry->deadline < core->sleeping_until)
        wake_event_thread(core);
    }
  }
  pthread_mutex_unlock(&core->mutex);

  if (failure) {
    free(entry);
    errno = failure;
    return -1;
  }
  return 0;
}

static void drop_waiting(duppy_core *core) {
  while (core->waiting) {
    task *entry = core->waiting;
    task_stop_waiting(core, entry);
    free(entry);
  }
}

static void drop_queue(queue *target) {
  free_chain(target->head);
  target->head = target->tail = NULL;
}

static void drop_ready(duppy_core *core) {
  drop_queue(&core->immediate);
  for (int rank = 0; rank < DUPPY_RANKS; rank++) {
    drop_queue(&core->direct[rank]);
    drop_queue(&core->threaded[rank]);
  }
  core->direct_ranks = core->threaded_ranks = 0;
  for (int i = 0; i < core->worker_count; i++) {
    worker *owner = &core->workers[i];
    drop_queue(&owner->pinned_immediate);
    for (int rank = 0; rank < DUPPY_RANKS; rank++) {
      drop_queue(&owner->pinned_direct[rank]);
      drop_queue(&owner->pinned_threaded[rank]);
    }
    owner->pinned_direct_ranks = owner->pinned_threaded_ranks = 0;
  }
}

/* One round of events: the tasks they woke leave the waiting set together,
   those woken by a descriptor first, then the expired ones by deadline. */
static void dispatch_events(duppy_core *core, const fd_event *events,
                            int count) {
  task *collected = NULL;
  task **last = &collected;
  for (int i = 0; i < count; i++) {
    if (events[i].fd == core->wake_read) {
      drain_wake(core);
      continue;
    }
    fd_entry *entry = entry_find(core, events[i].fd);
    if (entry == NULL)
      continue;
    for (watch *watcher = entry->watchers; watcher; watcher = watcher->next) {
      int fired = fired_mask(watcher->interest, events[i].flags);
      if (fired == 0)
        continue;
      watcher->fired |= fired;
      if (!watcher->owner->collected) {
        watcher->owner->collected = 1;
        *last = watcher->owner;
        last = &watcher->owner->next;
      }
    }
  }
  double now = duppy_core_now();
  while (core->heap_count && core->heap[0]->deadline <= now) {
    task *expired = core->heap[0];
    heap_remove(core, expired);
    if (!expired->collected) {
      expired->collected = 1;
      *last = expired;
      last = &expired->next;
    }
  }
  *last = NULL;
  while (collected) {
    task *entry = collected;
    collected = entry->next;
    if (entry->deadline <= now)
      entry->delay_fired = 1;
    task_stop_waiting(core, entry);
    make_ready(core, entry);
  }
}

static void *event_loop(void *argument) {
  duppy_core *core = argument;
  fd_event events[MAX_EVENTS];
#ifndef _WIN32
  sigset_t every_signal;
  sigfillset(&every_signal);
  pthread_sigmask(SIG_BLOCK, &every_signal, NULL);
#endif

  pthread_mutex_lock(&core->mutex);
  while (!core->stopped) {
    double timeout = LONGEST_WAIT;
    if (core->heap_count) {
      double remaining = core->heap[0]->deadline - duppy_core_now();
      timeout = remaining < 0 ? 0 : remaining < timeout ? remaining : timeout;
    }
    int watched = core->fallback ? fallback_snapshot(core) : 0;
    core->sleeping = 1;
    core->sleeping_until = duppy_core_now() + timeout;
    pthread_mutex_unlock(&core->mutex);

    int count = watched < 0      ? -1
                : core->fallback ? fallback_wait(core, watched, timeout, events)
                                 : native_wait(core, timeout, events);
    int failure = count < 0 && errno != EINTR ? (errno ? errno : EIO) : 0;

    pthread_mutex_lock(&core->mutex);
    core->sleeping = 0;
    if (failure) {
      core->error = failure;
      drop_waiting(core);
      wake_every_worker(core);
      break;
    }
    dispatch_events(core, events, count < 0 ? 0 : count);
  }
  pthread_mutex_unlock(&core->mutex);
  return NULL;
}

duppy_core *duppy_core_create(duppy_fd wake_read, duppy_fd wake_write,
                              int force_fallback) {
  duppy_core *core = calloc(1, sizeof(duppy_core));
  if (core == NULL)
    return NULL;
  core->wake_read = wake_read;
  core->wake_write = wake_write;
  core->timer = -1;
  core->bucket_count = 64;
  core->buckets = calloc(core->bucket_count, sizeof(fd_entry *));
  core->native = force_fallback ? -1 : native_open(core);
  core->fallback = core->native < 0;
  if (core->buckets == NULL ||
      (!core->fallback && native_set(core, wake_read, DUPPY_READ) != 0) ||
      pthread_mutex_init(&core->mutex, NULL) != 0) {
    int failure = errno ? errno : ENOMEM;
#ifndef _WIN32
    if (core->native >= 0)
      close(core->native);
    if (core->timer >= 0)
      close(core->timer);
#endif
    free(core->buckets);
    free(core);
    errno = failure;
    return NULL;
  }
  return core;
}

int duppy_core_start(duppy_core *core, int worker_count, int max_blocking) {
  int failure = 0;
  pthread_mutex_lock(&core->mutex);
  if (core->started || core->stopped || worker_count < 1)
    failure = EINVAL;
  else {
    core->workers = calloc(worker_count, sizeof(worker));
    if (core->workers == NULL)
      failure = ENOMEM;
  }
  if (!failure) {
    for (int i = 0; i < worker_count; i++)
      pthread_cond_init(&core->workers[i].wake_condition, NULL);
    core->worker_count = worker_count;
    core->max_blocking = max_blocking;
    update_slots(core);
    failure = pthread_create(&core->event_thread, NULL, event_loop, core);
    core->started = failure == 0;
  }
  pthread_mutex_unlock(&core->mutex);
  if (failure) {
    errno = failure;
    return -1;
  }
  return 0;
}

void duppy_core_stop(duppy_core *core) {
  pthread_mutex_lock(&core->mutex);
  int join = core->started && !core->stopped;
  if (!core->stopped) {
    core->stopped = 1;
    drop_waiting(core);
    drop_ready(core);
    wake_every_worker(core);
  }
  pthread_mutex_unlock(&core->mutex);
  if (join) {
    wake_event_thread(core);
    pthread_join(core->event_thread, NULL);
  }
}

int duppy_core_error(duppy_core *core) {
  pthread_mutex_lock(&core->mutex);
  int error = core->error;
  pthread_mutex_unlock(&core->mutex);
  return error;
}

void duppy_core_free(duppy_core *core) {
  drop_waiting(core);
  drop_ready(core);
  for (int i = 0; i < core->worker_count; i++)
    pthread_cond_destroy(&core->workers[i].wake_condition);
#ifndef _WIN32
  if (core->native >= 0)
    close(core->native);
  if (core->timer >= 0)
    close(core->timer);
#endif
  pthread_mutex_destroy(&core->mutex);
  free(core->workers);
  free(core->heap);
  free(core->buckets);
  free(core->poll_fds);
  free(core);
}
