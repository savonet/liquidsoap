#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdint.h>
#include <string.h>

CAMLprim value caml_nofpu_gain(value _x, value _buf, value _ofs, value _len)
{
  CAMLparam1(_buf);
  double x = Double_val(_x);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int32_t *buf = (int32_t*)String_val(_buf);
  int i;

  for (i = ofs; i < ofs + len; i++)
    buf[i] = x * buf[i];

  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofpu_ms(value _buf, value _ofs, value _len)
{
  CAMLparam1(_buf);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int32_t *buf = (int32_t*)String_val(_buf);
  int ms = 0;
  int i;

  for (i = ofs; i < ofs + len; i++)
    ms += buf[i] * buf[i];

  CAMLreturn(Val_int(ms));
}

CAMLprim value caml_nofpu_clear(value _buf, value _ofs, value _len)
{
  CAMLparam1(_buf);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);

  memset(String_val(_buf)+ofs*sizeof(int32_t), 0, len*sizeof(int32_t));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofpu_add(value _b1, value _o1, value _b2, value _o2, value _len)
{
  CAMLparam2(_b1, _b2);
  int o1 = Int_val(_o1);
  int o2 = Int_val(_o2);
  int len = Int_val(_len);
  int32_t *b1 = (int32_t*)String_val(_b1);
  int32_t *b2 = (int32_t*)String_val(_b2);
  int i;

  for (i = 0; i < len; i++)
    b1[i+o1] += b2[i+o2];

  CAMLreturn(Val_unit);
}

CAMLprim value caml_nofpu_to_s16le(value _buf, value _ofs, value _len)
{
  CAMLparam1(_buf);
  CAMLlocal1(ans);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int chans = Wosize_val(_buf);
  int c, i;

  ans = caml_alloc_string(chans * sizeof(int16_t) * len);
  for (i = 0; i < len; i++)
    for (c = 0; c < chans; c++)
      /* TODO: endianness */
      ((int16_t*)String_val(ans))[i*chans+c] = ((int32_t*)String_val(Field(_buf, c)))[i+ofs];

  CAMLreturn(Val_unit);
}
