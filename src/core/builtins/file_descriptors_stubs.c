#include <caml/mlvalues.h>

#ifndef _WIN32
#include <sys/resource.h>
#endif

/* Soft limit on the number of open file descriptors, or -1 when there is none
   or it cannot be read. */
CAMLprim value liquidsoap_file_descriptors_limit(value unit) {
#ifdef _WIN32
  return Val_long(-1);
#else
  struct rlimit limit;
  if (getrlimit(RLIMIT_NOFILE, &limit) != 0 || limit.rlim_cur == RLIM_INFINITY)
    return Val_long(-1);
  return Val_long(limit.rlim_cur);
#endif
}
