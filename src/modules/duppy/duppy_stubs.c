/*
 * Copyright 2010 Savonet team
 *
 * This file is part of Ocaml-duppy.
 *
 * Ocaml-duppy is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-duppy is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-duppy; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include <errno.h>
#include <math.h>

/* On native Windows platforms, many macros are not defined.  */
# if (defined _WIN32 || defined __WIN32__) && ! defined __CYGWIN__

#ifndef EWOULDBLOCK
#define EWOULDBLOCK     EAGAIN
#endif

#endif

#ifdef WIN32
#define Fd_val(fd) win_CRT_fd_of_filedescr(fd)
#define Val_fd(fd) caml_failwith("Val_fd")
#else
#define Fd_val(fd) Int_val(fd)
#define Val_fd(fd) Val_int(fd)
#endif

#ifndef WIN32
#include <poll.h>

CAMLprim value caml_poll(value _read, value _write, value _err, value _timeout) {
  CAMLparam4(_read, _write, _err, _timeout);
  CAMLlocal4(_pread, _pwrite, _perr, _ret);

  struct pollfd *fds;
  nfds_t nfds = 0;
  nfds_t nread = 0;
  nfds_t nwrite = 0;
  nfds_t nerr = 0;
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

  fds = calloc(nfds,sizeof(struct pollfd));
  if (fds == NULL) caml_raise_out_of_memory();

  for (n = 0; n < Wosize_val(_read); n++) {
    fds[last+n].fd = Fd_val(Field(_read,n));
    fds[last+n].events = POLLIN;
  }
  last += Wosize_val(_read);

  for (n = 0; n < Wosize_val(_write); n++) {
    fds[last+n].fd = Fd_val(Field(_write,n));
    fds[last+n].events = POLLOUT;
  }
  last += Wosize_val(_write);

  for (n = 0; n < Wosize_val(_err); n++) {
    fds[last+n].fd = Fd_val(Field(_err,n));
    fds[last+n].events = POLLERR;
  }

  caml_release_runtime_system();
  ret = poll(fds, nfds, timeout);
  caml_acquire_runtime_system();

  if (ret == -1) {
    free(fds);
    uerror("poll", Nothing);
  }

  for (n = 0; n < nfds; n++) {
    if (fds[n].revents & POLLIN)
      nread++;
    if (fds[n].revents & POLLOUT)
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
    if (fds[n].revents & POLLOUT) {
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
CAMLprim value caml_poll(value _read, value _write, value _err, value _timeout) {
  caml_failwith("caml_poll");
}
#endif

CAMLprim value ocaml_duppy_write_ba(value _fd, value ba, value _ofs, value _len)
{
  CAMLparam4(_fd, ba, _ofs, _len);
  int fd = Fd_val(_fd);
  long ofs = Long_val(_ofs);
  long len = Long_val(_len);
  void *buf = Caml_ba_data_val(ba);
  int ret;

  int written = 0;
  while (len > 0) {
    caml_enter_blocking_section();
    ret = write(fd, buf+ofs, len);
    caml_leave_blocking_section();
    if (ret == -1) {
      if ((errno == EAGAIN || errno == EWOULDBLOCK) && written > 0) break;
      uerror("write", Nothing);
    }
    written += ret;
    ofs += ret;
    len -= ret;
  }

  CAMLreturn(Val_long(written));
}
