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
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <errno.h>
#include <math.h> /* ceil() */

#ifdef WIN32
#define Fd_val(fd) win_CRT_fd_of_filedescr(fd)
#define Val_fd(fd) caml_failwith("Val_fd")
#else
#define Fd_val(fd) Int_val(fd)
#define Val_fd(fd) Val_int(fd)
#endif

#ifndef WIN32
#include <poll.h>

CAMLprim value caml_liquidsoap_poll(value _read, value _write, value _err,
                                    value _timeout) {
  CAMLparam4(_read, _write, _err, _timeout);
  CAMLlocal4(_pread, _pwrite, _perr, _ret);

  struct pollfd *fds;
  nfds_t nfds = 0;
  nfds_t nread = 0;
  nfds_t nwrite = 0;
  nfds_t nerr = 0;
  nfds_t write_start, write_end;
  int timeout;
  size_t last = 0;
  int n, ret;

  if (Double_val(_timeout) == -1)
    timeout = -1;
  else
    timeout = ceil(Double_val(_timeout) * 1000);

  nfds += Wosize_val(_read);
  nfds += Wosize_val(_write);
  nfds += Wosize_val(_err);

  fds = calloc(nfds, sizeof(struct pollfd));
  if (fds == NULL)
    caml_raise_out_of_memory();

  for (n = 0; n < Wosize_val(_read); n++) {
    fds[last + n].fd = Fd_val(Field(_read, n));
    fds[last + n].events = POLLIN;
  }
  last += Wosize_val(_read);
  write_start = last;

  for (n = 0; n < Wosize_val(_write); n++) {
    fds[last + n].fd = Fd_val(Field(_write, n));
    fds[last + n].events = POLLOUT;
  }
  last += Wosize_val(_write);
  write_end = last;

  for (n = 0; n < Wosize_val(_err); n++) {
    fds[last + n].fd = Fd_val(Field(_err, n));
    fds[last + n].events = POLLERR;
  }

  caml_release_runtime_system();
  ret = poll(fds, nfds, timeout);
  while (ret == -1 && errno == EINTR) {
    caml_acquire_runtime_system();
    caml_process_pending_actions();
    caml_release_runtime_system();
    ret = poll(fds, nfds, timeout);
  }
  caml_acquire_runtime_system();

  if (ret == -1) {
    free(fds);
    uerror("poll", Nothing);
  }

  for (n = 0; n < nfds; n++) {
    if (fds[n].revents & POLLIN)
      nread++;
    /* POLLHUP on a write fd means the peer closed the connection; treat it as
       write-ready so the caller can detect the error (e.g. EPIPE on next write). */
    if ((fds[n].revents & POLLOUT) ||
        ((nfds_t)n >= write_start && (nfds_t)n < write_end &&
         (fds[n].revents & POLLHUP)))
      nwrite++;
    if (fds[n].revents & POLLERR)
      nerr++;
  }

  _pread = caml_alloc_tuple(nread);
  nread = 0;

  _pwrite = caml_alloc_tuple(nwrite);
  nwrite = 0;

  _perr = caml_alloc_tuple(nerr);
  nerr = 0;

  for (n = 0; n < nfds; n++) {
    if (fds[n].revents & POLLIN) {
      Store_field(_pread, nread, Val_fd(fds[n].fd));
      nread++;
    }
    if ((fds[n].revents & POLLOUT) ||
        ((nfds_t)n >= write_start && (nfds_t)n < write_end &&
         (fds[n].revents & POLLHUP))) {
      Store_field(_pwrite, nwrite, Val_fd(fds[n].fd));
      nwrite++;
    }
    if (fds[n].revents & POLLERR) {
      Store_field(_perr, nerr, Val_fd(fds[n].fd));
      nerr++;
    }
  }

  free(fds);

  _ret = caml_alloc_tuple(3);
  Store_field(_ret, 0, _pread);
  Store_field(_ret, 1, _pwrite);
  Store_field(_ret, 2, _perr);

  CAMLreturn(_ret);
}
#else
CAMLprim value caml_liquidsoap_poll(value _read, value _write, value _err,
                                    value _timeout) {
  caml_failwith("caml_liquidsoap_poll");
}
#endif
