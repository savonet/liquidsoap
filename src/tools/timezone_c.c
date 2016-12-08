// Operations for accessing timezone
// see https://caml.inria.fr/mantis/view.php?id=4038

#include <stddef.h>
#include <time.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value liquidsoap_get_timezone() {
  CAMLparam0();
  tzset();
  CAMLreturn(Val_long(timezone));
}
