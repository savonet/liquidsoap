#define CAML_INTERNALS 1

#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>

// Single bigarray not registered with the GC.
CAMLprim value liquidsoap_alloc_managed_int16_ba(value _len) {
  CAMLparam1(_len);
  intnat out_size = Int_val(_len);
  void *data = malloc(out_size * caml_ba_element_size[CAML_BA_SINT16]);
  if (!data)
    caml_raise_out_of_memory();

  CAMLreturn(
      caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_SINT16, 1, data, &out_size));
}

CAMLprim value liquidsoap_cleanup_managed_int16_ba(value _ba) {
  CAMLparam1(_ba);
  free(Caml_ba_data_val(_ba));
  CAMLreturn(Val_unit);
}
