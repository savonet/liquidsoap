/* Stub of the avdevice binding, spec/avdevice.md. */

#include <caml/mlvalues.h>

#include <libavdevice/avdevice.h>

CAMLprim value ocaml_avdevice_init(value _unit) {
  (void)_unit;
  avdevice_register_all();
  return Val_unit;
}
