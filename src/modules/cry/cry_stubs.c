#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <errno.h>

/* On native Windows platforms, many macros are not defined.  */
#if (defined _WIN32 || defined __WIN32__) && !defined __CYGWIN__

#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
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

CAMLprim value caml_cry_poll(value _read, value _write, value _err,
                             value _timeout) {
  CAMLparam3(_read, _write, _err);
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
    timeout = Double_val(_timeout) * 1000;

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

  for (n = 0; n < Wosize_val(_write); n++) {
    fds[last + n].fd = Fd_val(Field(_write, n));
    fds[last + n].events = POLLOUT;
  }
  last += Wosize_val(_write);

  for (n = 0; n < Wosize_val(_err); n++) {
    fds[last + n].fd = Fd_val(Field(_err, n));
    fds[last + n].events = POLLERR;
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
CAMLprim value caml_cry_poll(value _read, value _write, value _err,
                             value _timeout) {
  caml_failwith("caml_poll");
}
#endif
