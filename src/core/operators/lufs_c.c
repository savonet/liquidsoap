#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

CAMLprim value liquidsoap_lufs_process(value _iir, value _x, value _y)
{
  CAMLparam3(_iir, _x, _y);
  CAMLlocal4(_x1, _x2, _y1, _y2);
  int i;
  double tmp;
  int channels = Int_val(Field(_iir, 0));

  _x1 = Field(Field(_iir, 1), 0);
  _x2 = Field(Field(_iir, 1), 1);
  _y1 = Field(Field(_iir, 2), 0);
  _y2 = Field(Field(_iir, 2), 1);

  double a1 = Double_val(Field(_iir, 3));
  double a2 = Double_val(Field(_iir, 4));
  double b0 = Double_val(Field(_iir, 5));
  double b1 = Double_val(Field(_iir, 6));
  double b2 = Double_val(Field(_iir, 7));

  for (i = 0; i < channels; i++) {
    tmp = b0 * Double_array_field(_x, i) + b1 * Double_array_field(_x1, i) +
          b2 * Double_array_field(_x2, i) - a1 * Double_array_field(_y1, i) -
          a2 * Double_array_field(_y2, i);
    Store_double_array_field(_y, i, tmp);
  }

  Store_field(Field(_iir, 1), 0, _x);
  Store_field(Field(_iir, 1), 0, _x1);
  Store_field(Field(_iir, 2), 0, _y);
  Store_field(Field(_iir, 2), 0, _y1);

  CAMLreturn(Val_unit);
}
