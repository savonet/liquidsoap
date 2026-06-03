#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

CAMLprim value polymorphic_variant_string_to_c_value(value _pv_name) {
  CAMLparam1(_pv_name);
  CAMLreturn(caml_copy_int64((int64_t)caml_hash_variant(String_val(_pv_name))));
}
