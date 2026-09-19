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

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>
#include <string.h>

#define POLLSET_READ 1
#define POLLSET_WRITE 2
#define POLLSET_EXCEPT 4
#define POLLSET_MAX_EVENTS 512

typedef struct {
  int fd;
  int flags;
} pollset_event;

/* Each platform supplies these five, and the entry points below are written
   once against them. A descriptor closed by its owner leaves the kernel's set
   on its own, so removing what is not there is not an error. */

#if defined(__linux__)

#include <sys/epoll.h>

static const char *pollset_backend = "epoll";
static const int pollset_has_backend = 1;

static int pollset_create(void) { return epoll_create1(EPOLL_CLOEXEC); }

static int pollset_set(int set, int fd, int read, int write) {
  struct epoll_event ev;
  memset(&ev, 0, sizeof(ev));
  if (read)
    ev.events |= EPOLLIN;
  if (write)
    ev.events |= EPOLLOUT;
  ev.data.fd = fd;

  if (epoll_ctl(set, EPOLL_CTL_MOD, fd, &ev) == 0)
    return 0;
  if (errno != ENOENT)
    return -1;
  return epoll_ctl(set, EPOLL_CTL_ADD, fd, &ev);
}

static int pollset_remove(int set, int fd) {
  if (epoll_ctl(set, EPOLL_CTL_DEL, fd, NULL) == 0)
    return 0;
  return (errno == ENOENT || errno == EBADF || errno == EPERM) ? 0 : -1;
}

static int pollset_wait(int set, double timeout, pollset_event *out, int max) {
  struct epoll_event evs[POLLSET_MAX_EVENTS];
  int ms = timeout < 0 ? -1 : (int)(timeout * 1e3);
  int n, i;

  n = epoll_wait(set, evs, max, ms);
  if (n < 0)
    return n;

  for (i = 0; i < n; i++) {
    out[i].fd = evs[i].data.fd;
    out[i].flags = 0;
    if (evs[i].events & EPOLLIN)
      out[i].flags |= POLLSET_READ;
    if (evs[i].events & EPOLLOUT)
      out[i].flags |= POLLSET_WRITE;
    if (evs[i].events & (EPOLLERR | EPOLLHUP))
      out[i].flags |= POLLSET_EXCEPT;
  }
  return n;
}

#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) ||    \
    defined(__NetBSD__) || defined(__DragonFly__)

#include <sys/event.h>
#include <sys/time.h>
#include <sys/types.h>

static const char *pollset_backend = "kqueue";
static const int pollset_has_backend = 1;

static int pollset_create(void) { return kqueue(); }

static int pollset_filter(int set, int fd, int filter, int enable) {
  struct kevent ch;
  EV_SET(&ch, fd, filter, enable ? EV_ADD : EV_DELETE, 0, 0, NULL);
  if (kevent(set, &ch, 1, NULL, 0, NULL) == 0)
    return 0;
  return (errno == ENOENT || errno == EBADF) ? 0 : -1;
}

static int pollset_set(int set, int fd, int read, int write) {
  if (pollset_filter(set, fd, EVFILT_READ, read) == -1)
    return -1;
  return pollset_filter(set, fd, EVFILT_WRITE, write);
}

static int pollset_remove(int set, int fd) {
  if (pollset_filter(set, fd, EVFILT_READ, 0) == -1)
    return -1;
  return pollset_filter(set, fd, EVFILT_WRITE, 0);
}

static int pollset_wait(int set, double timeout, pollset_event *out, int max) {
  struct kevent evs[POLLSET_MAX_EVENTS];
  struct timespec ts;
  struct timespec *tsp = NULL;
  int n, i;

  if (timeout >= 0) {
    ts.tv_sec = (time_t)timeout;
    ts.tv_nsec = (long)((timeout - (double)ts.tv_sec) * 1e9);
    tsp = &ts;
  }

  n = kevent(set, NULL, 0, evs, max, tsp);
  if (n < 0)
    return n;

  for (i = 0; i < n; i++) {
    out[i].fd = (int)evs[i].ident;
    out[i].flags = 0;
    if (evs[i].filter == EVFILT_READ)
      out[i].flags |= POLLSET_READ;
    if (evs[i].filter == EVFILT_WRITE)
      out[i].flags |= POLLSET_WRITE;
    if (evs[i].flags & (EV_ERROR | EV_EOF))
      out[i].flags |= POLLSET_EXCEPT;
  }
  return n;
}

#else

static const char *pollset_backend = "select";
static const int pollset_has_backend = 0;

static int pollset_unsupported(void) {
  errno = ENOSYS;
  return -1;
}

static int pollset_create(void) { return pollset_unsupported(); }

static int pollset_set(int set, int fd, int read, int write) {
  (void)set;
  (void)fd;
  (void)read;
  (void)write;
  return pollset_unsupported();
}

static int pollset_remove(int set, int fd) {
  (void)set;
  (void)fd;
  return pollset_unsupported();
}

static int pollset_wait(int set, double timeout, pollset_event *out, int max) {
  (void)set;
  (void)timeout;
  (void)out;
  (void)max;
  return pollset_unsupported();
}

#endif

CAMLprim value caml_pollset_available(value unit) {
  (void)unit;
  return Val_bool(pollset_has_backend);
}

CAMLprim value caml_pollset_backend_name(value unit) {
  (void)unit;
  return caml_copy_string(pollset_backend);
}

CAMLprim value caml_pollset_create(value unit) {
  int fd = pollset_create();
  (void)unit;
  if (fd == -1)
    caml_uerror("pollset_create", Nothing);
  return Val_int(fd);
}

CAMLprim value caml_pollset_set(value _set, value _fd, value _read,
                                value _write) {
  if (pollset_set(Int_val(_set), Int_val(_fd), Bool_val(_read),
                  Bool_val(_write)) == -1)
    caml_uerror("pollset_set", Nothing);
  return Val_unit;
}

CAMLprim value caml_pollset_remove(value _set, value _fd) {
  if (pollset_remove(Int_val(_set), Int_val(_fd)) == -1)
    caml_uerror("pollset_remove", Nothing);
  return Val_unit;
}

CAMLprim value caml_pollset_wait(value _set, value _timeout) {
  CAMLparam2(_set, _timeout);
  CAMLlocal2(ret, pair);
  pollset_event evs[POLLSET_MAX_EVENTS];
  int set = Int_val(_set);
  double timeout = Double_val(_timeout);
  int n, i;

  caml_release_runtime_system();
  n = pollset_wait(set, timeout, evs, POLLSET_MAX_EVENTS);
  while (n == -1 && errno == EINTR) {
    caml_acquire_runtime_system();
    caml_process_pending_actions();
    caml_release_runtime_system();
    n = pollset_wait(set, timeout, evs, POLLSET_MAX_EVENTS);
  }
  caml_acquire_runtime_system();

  if (n == -1)
    caml_uerror("pollset_wait", Nothing);

  ret = caml_alloc(n, 0);
  for (i = 0; i < n; i++) {
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, Val_int(evs[i].fd));
    Store_field(pair, 1, Val_int(evs[i].flags));
    Store_field(ret, i, pair);
  }

  CAMLreturn(ret);
}
