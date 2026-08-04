#include <caml/bigarray.h>
#include <caml/memory.h>

CAMLprim value liquidsoap_amplify_s16_ba(value _src, value _factor) {
  CAMLparam2(_src, _factor);
  double factor = Double_val(_factor);
  int16_t *data = Caml_ba_data_val(_src);
  int len = Caml_ba_array_val(_src)->dim[0];
  int i;

  for (i = 0; i < len; i++) {
    data[i] *= factor;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value liquidsoap_amplify_f32_ba(value _src, value _factor) {
  CAMLparam2(_src, _factor);
  double factor = Double_val(_factor);
  float *data = Caml_ba_data_val(_src);
  int len = Caml_ba_array_val(_src)->dim[0];
  int i;

  for (i = 0; i < len; i++) {
    data[i] *= factor;
  }

  CAMLreturn(Val_unit);
}
