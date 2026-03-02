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
