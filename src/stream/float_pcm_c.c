/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************/

/**
 * Some C functions for the Float_pcm module.
 *
 * @author Samuel Mimram
 */

/* $Id: mixer_c.c 3127 2007-03-28 22:37:44Z smimram $ */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <limits.h>
#include <stdint.h>
#include "config.h"
static inline int16_t bswap_16 (int16_t x) { return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)); }

#include <assert.h>
#include <stdio.h>
#include <string.h>

/* Optimized implementation of Array.blit for float arrays.
 * See http://caml.inria.fr/mantis/view.php?id=2787
 */
CAMLprim value caml_float_array_blit(value _src, value _src_off,
                                     value _dst, value _dst_off, value _len) {
  int src_off = Int_val(_src_off) ;
  int dst_off = Int_val(_dst_off) ;
  int len = Int_val(_len) ;
  int i ;
  for (i=0 ; i<len ; i++)
    Store_double_field(_dst,dst_off+i,Double_field(_src,src_off+i)) ;
  return Val_unit ;
}

static inline short clip(double s)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return SHRT_MIN;
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return SHRT_MAX;
  }
  else
    return (s * (SHRT_MAX - 1));
}

CAMLprim value caml_float_pcm_to_s16le(value a, value _offs, value _len, value _dst, value _dst_offs)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = 2 * len * nc;
  value src;
  short *dst = (short*)String_val(_dst);

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_s16le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Field(a, c);
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = clip(Double_field(src, i + offs));
#ifdef LIQ_BIG_ENDIAN
      dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
    }
   }

  CAMLreturn(Val_int(dst_len));
}

#define s16tof(x) (((double)x)/32768)
#define u8tof(x)  (((double)x-127)/127)
#define get_u8(src,offset,nc,c,i)    u8tof(((uint8_t*)src)[offset+i*nc+c])
#ifdef LIQ_BIG_ENDIAN
#define get_s16le(src,offset,nc,c,i) s16tof(bswap_16(((int16_t*)src)[offset/2+i*nc+c]))
#else
#define get_s16le(src,offset,nc,c,i) s16tof(((int16_t*)src)[offset/2+i*nc+c])
#endif

CAMLprim value caml_float_pcm_convert_u8_native(
    value _src, value _offset, value _length,
    value _ratio, value _dst, value _dst_off)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  double ratio = Double_val(_ratio) ;
  int dst_off = Int_val(_dst_off) ;
  int dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;
  int newlen = (int)(ratio*len) ;
  int i,c ;
  int nc = Wosize_val(_dst) ;

  if (dst_off + newlen > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (ratio==1) {
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_u8(src,offset,nc,c,i)) ;
      }
    }
  }else{
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_u8(src,offset,nc,c,((int)(i/ratio)))) ;
      }
    }
  }

  CAMLreturn(Val_int(dst_off+newlen)) ;
}

CAMLprim value caml_float_pcm_convert_u8_byte(value* argv, int argn) {\
  return caml_float_pcm_convert_u8_native(argv[0],argv[1],argv[2],\
                                       argv[3],argv[4],argv[5]) ;\
}

CAMLprim value caml_float_pcm_convert_s16le_native(
    value _src, value _offset, value _length,
    value _ratio, value _dst, value _dst_off)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  double ratio = Double_val(_ratio) ;
  int dst_off = Int_val(_dst_off) ;
  int dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;
  int newlen = (int)(ratio*len) ;
  int i,c ;
  int nc = Wosize_val(_dst) ;

  if (dst_off + newlen > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (ratio==1) {
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_s16le(src,offset,nc,c,i)) ;
      }
    }
  }else{
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_s16le(src,offset,nc,c,((int)(i/ratio)))) ;
      }
    }
  }

  CAMLreturn(Val_int(dst_off+newlen)) ;
}

CAMLprim value caml_float_pcm_convert_s8le_byte(value* argv, int argn) {\
  return caml_float_pcm_convert_s16le_native(argv[0],argv[1],argv[2],\
                                       argv[3],argv[4],argv[5]) ;\
}

CAMLprim value caml_float_pcm_from_s16le(value a, value _aoffs, value _buf, value _boffs, value _len)
{
  CAMLparam2(a, _buf);
  CAMLlocal1(cbuf);
  short *buf = (short*)String_val(_buf);
  int aoffs = Int_val(_aoffs);
  int boffs = Int_val(_boffs);
  int len = Int_val(_len);
  int i, c;
  int chans = Wosize_val(a);

  if (Wosize_val(Field(a, 0)) / Double_wosize - boffs < len)
    caml_invalid_argument("from_s16le: buffer too small");

  for(c = 0; c < chans; c++)
  {
    cbuf = Field(a, c);
      for(i = 0; i < len; i++)
        Store_double_field(cbuf, i + aoffs, get_s16le(buf,boffs,chans,c,i));
  }

  CAMLreturn(Val_unit);
}
