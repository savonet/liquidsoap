#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value ocaml_srt_unix_error_of_code(value _code) {
  CAMLparam1(_code);
  CAMLreturn(caml_unix_error_of_code(Int_val(_code)));
}
