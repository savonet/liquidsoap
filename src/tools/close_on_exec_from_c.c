/* Inspired from closefrom() from openssh. */

#include "config.h"

#ifdef HAVE_FCNTL_CLOEXEC

#include <sys/types.h>
#include <sys/param.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif

#ifndef OPEN_MAX
# define OPEN_MAX 256
#endif

/*
 * Mark all opened file descriptor greater than lowfd as close-on-exec
 */
void close_on_exec(int fd)
{
  int flags;
  if ((flags = fcntl(fd, F_GETFD)) != -1)
    fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
  return;
}

void close_on_exec_from(int lowfd)
{
    long fd, maxfd;
#if defined(HAVE_DIRFD) && defined(HAVE_PROC_PID)
    char fdpath[PATH_MAX], *endp;
    struct dirent *dent;
    DIR *dirp;
    int len;

    /* Check for a /proc/$$/fd directory. */
    len = snprintf(fdpath, sizeof(fdpath), "/proc/%ld/fd", (long)getpid());
    if (len > 0 && (size_t)len <= sizeof(fdpath) && (dirp = opendir(fdpath))) {
  while ((dent = readdir(dirp)) != NULL) {
      fd = strtol(dent->d_name, &endp, 10);
      if (dent->d_name != endp && *endp == '\0' &&
    fd >= 0 && fd < INT_MAX && fd >= lowfd && fd != dirfd(dirp))
    close_on_exec((int) fd);
  }
  (void) closedir(dirp);
    } else
#endif
    {
  /*
   * Fall back on sysconf() or getdtablesize().  We avoid checking
   * resource limits since it is possible to open a file descriptor
   * and then drop the rlimit such that it is below the open fd.
   */
#ifdef HAVE_SYSCONF
  maxfd = sysconf(_SC_OPEN_MAX);
#else
  maxfd = getdtablesize();
#endif /* HAVE_SYSCONF */
  if (maxfd < 0)
      maxfd = OPEN_MAX;

  for (fd = lowfd; fd < maxfd; fd++)
      close_on_exec((int) fd);
    }
}
#else
void close_on_exec_from(int lowfd)
{
  return; // :-(
}
#endif /* !HAVE_FCNTL_CLOEXEC */

/* OCaml wrapper */

#include <caml/misc.h>
#include <caml/mlvalues.h>

CAMLprim value liquidsoap_close_on_exec_from(value from)
{
#ifndef WIN32
  close_on_exec_from(Int_val(from));
#endif
  return Val_unit;
}
