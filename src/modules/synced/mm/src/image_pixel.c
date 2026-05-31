#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "image_pixel.h"

CAMLprim value caml_yuv_of_rgb(value rgb) {
  CAMLparam1(rgb);
  CAMLlocal1(ans);
  int r = Int_val(Field(rgb, 0));
  int g = Int_val(Field(rgb, 1));
  int b = Int_val(Field(rgb, 2));

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(YofRGB(r, g, b)));
  Store_field(ans, 1, Val_int(UofRGB(r, g, b)));
  Store_field(ans, 2, Val_int(VofRGB(r, g, b)));
  CAMLreturn(ans);
}

CAMLprim value caml_rgb_of_yuv(value yuv) {
  CAMLparam1(yuv);
  CAMLlocal1(ans);
  int y = Int_val(Field(yuv, 0));
  int u = Int_val(Field(yuv, 1));
  int v = Int_val(Field(yuv, 2));

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(RofYUV(y, u, v)));
  Store_field(ans, 1, Val_int(GofYUV(y, u, v)));
  Store_field(ans, 2, Val_int(BofYUV(y, u, v)));
  CAMLreturn(ans);
}
