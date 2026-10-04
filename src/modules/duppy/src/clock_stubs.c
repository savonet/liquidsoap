#include <time.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

CAMLprim value duppy_monotonic_now(value unit) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return caml_copy_double((double)ts.tv_sec + (double)ts.tv_nsec * 1e-9);
}
