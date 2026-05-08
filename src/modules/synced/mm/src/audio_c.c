/*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <stdint.h>

#define INT24_MAX ((1 << 23) - 1)
#define INT24_MIN (-INT24_MAX)

#ifndef Bytes_val
#define Bytes_val String_val
#endif

typedef uint8_t int24_t[3];

static inline void int24_of_int32(int32_t x, int24_t d) {
  d[0] = x;
  d[1] = x >> 8;
  d[2] = x >> 16;
}
static inline int32_t int32_of_int24(int24_t x) {
  int32_t tmp = x[0] | (x[1] << 8) | (x[2] << 16);

  return INT24_MAX < tmp ? (0xff000000 | tmp) : tmp;
}

#define bswap_16(x)                                                            \
  ((int16_t)((((int16_t)(x) & 0xff00) >> 8) | (((int16_t)(x) & 0x00ff) << 8)))

#define bswap_32(x)                                                            \
  ((int32_t)((((int32_t)(x) & 0xff000000) >> 24) |                             \
             (((int32_t)(x) & 0x00ff0000) >> 8) |                              \
             (((int32_t)(x) & 0x0000ff00) << 8) |                              \
             (((int32_t)(x) & 0x000000ff) << 24)))

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

static inline double clip(double s) {
  if (s < -1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return -1;
  } else if (s > 1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return 1;
  } else
    return s;
}

static inline int16_t s16_clip(double s) {
  if (s < -1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT16_MIN;
  } else if (s > 1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT16_MAX;
  } else
    return (s * INT16_MAX);
}

static inline int32_t s32_clip(double s) {
  if (s < -1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT32_MIN;
  } else if (s > 1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT32_MAX;
  } else
    return (s * INT32_MAX);
}

static inline void s24_clip(double s, int24_t x) {
  if (s < -1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return int24_of_int32(INT24_MIN, x);
  } else if (s > 1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return int24_of_int32(INT24_MAX, x);
  } else
    return int24_of_int32(s * INT24_MAX, x);
}

static inline uint8_t u8_clip(double s) {
  if (s < -1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return 0;
  } else if (s > 1) {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT8_MAX;
  } else
    return (s * INT8_MAX + INT8_MAX);
}

#define u8tof(x) (((double)x - INT8_MAX) / INT8_MAX)
#define s16tof(x) (((double)x) / INT16_MAX)
#define s24tof(x) (((double)int32_of_int24(x)) / INT24_MAX)
#define s32tof(x) (((double)x) / INT32_MAX)

#define get_u8(src, offset, nc, c, i)                                          \
  u8tof(((uint8_t *)src)[offset + i * nc + c])
#define get_s24le(src, offset, nc, c, i)                                       \
  s24tof(((int24_t *)src)[offset / 3 + i * nc + c])

#ifdef BIGENDIAN
#define get_s16le(src, offset, nc, c, i)                                       \
  s16tof(bswap_16(((int16_t *)src)[offset / 2 + i * nc + c]))
#define get_s16be(src, offset, nc, c, i)                                       \
  s16tof(((int16_t *)src)[offset / 2 + i * nc + c])
#define get_s32le(src, offset, nc, c, i)                                       \
  s32tof(bswap_32(((int32_t *)src)[offset / 4 + i * nc + c]))
#else
#define get_s16le(src, offset, nc, c, i)                                       \
  s16tof(((int16_t *)src)[offset / 2 + i * nc + c])
#define get_s16be(src, offset, nc, c, i)                                       \
  s16tof(bswap_16(((int16_t *)src)[offset / 2 + i * nc + c]))
#define get_s32le(src, offset, nc, c, i)                                       \
  s32tof(((int32_t *)src)[offset / 4 + i * nc + c])
#endif

CAMLprim value caml_mm_audio_to_s32le(value _src, value _src_offs, value _dst,
                                      value _dst_offs, value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(src);
  int c, i;
  int dst_offs = Int_val(_dst_offs);
  int src_offs = Int_val(_src_offs);
  int nc = Wosize_val(_src);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int32_t *dst = (int32_t *)Bytes_val(_dst);

  if (caml_string_length(_dst) < dst_offs + len * nc * 4)
    caml_invalid_argument("pcm_to_s32le: destination buffer too short");

  for (c = 0; c < nc; c++) {
    src = Field(_src, c);
    for (i = 0; i < len; i++) {
      dst[i * nc + c + dst_offs] = s32_clip(Double_field(src, i + src_offs));
#ifdef BIGENDIAN
      dst[i * nc + c + dst_offs] = bswap_32(dst[i * nc + c + dst_offs]);
#endif
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_to_fltp(value _src, value _src_offs, value _dst,
                                     value _dst_offs, value _len,
                                     value _stride) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(src);
  int c, i;
  int dst_offs = Int_val(_dst_offs);
  int src_offs = Int_val(_src_offs);
  int nc = Wosize_val(_src);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int stride = Int_val(_stride);
  float *dst = (float *)Caml_ba_data_val(_dst);

  if (stride < len)
    caml_invalid_argument("caml_mm_audio_to_fltp: invalid dst length/stride");

  if (len < dst_offs)
    caml_invalid_argument("caml_mm_audio_to_fltp: invalid dst_offset");

  if (Caml_ba_array_val(_dst)->dim[0] < stride * nc)
    caml_invalid_argument(
        "caml_mm_audio_to_fltp: destination buffer too short");

  for (c = 0; c < nc; c++) {
    src = Field(_src, c);
    for (i = 0; i < len; i++) {
      dst[c * stride + i + dst_offs] = clip(Double_field(src, i + src_offs));
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_to_fltp_bytes(value *argv, value argn) {
  return caml_mm_audio_to_fltp(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5]);
}

CAMLprim value caml_mm_audio_to_s24le(value _src, value _src_offs, value _dst,
                                      value _dst_offs, value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(src);
  int c, i;
  int dst_offs = Int_val(_dst_offs);
  int src_offs = Int_val(_src_offs);
  int nc = Wosize_val(_src);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int24_t *dst = (int24_t *)Bytes_val(_dst);

  if (caml_string_length(_dst) < dst_offs + len * nc * 3)
    caml_invalid_argument("pcm_to_s24le: destination buffer too short");

  for (c = 0; c < nc; c++) {
    src = Field(_src, c);
    for (i = 0; i < len; i++)
      s24_clip(Double_field(src, i + src_offs), dst[i * nc + c + dst_offs]);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_to_s16(value _le, value _src, value _src_offs,
                                    value _dst, value _dst_offs, value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(src);
  int little_endian = Bool_val(_le);
  int dst_offs = Int_val(_dst_offs);
  int src_offs = Int_val(_src_offs);
  int nc = Wosize_val(_src);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int16_t *dst = (int16_t *)Bytes_val(_dst);
  int c, i;

  if (caml_string_length(_dst) < dst_offs + 2 * nc * len)
    caml_invalid_argument("pcm_to_s16: destination buffer too short");

  dst = (void *)dst + dst_offs;

  if (little_endian == 1)
    for (c = 0; c < nc; c++) {
      src = Field(_src, c);
      for (i = 0; i < len; i++) {
        dst[i * nc + c] = s16_clip(Double_field(src, i + src_offs));
#ifdef BIGENDIAN
        dst[i * nc + c] = bswap_16(dst[i * nc + c]);
#endif
      }
    }
  else
    for (c = 0; c < nc; c++) {
      src = Field(_src, c);
      for (i = 0; i < len; i++) {
        dst[i * nc + c] = s16_clip(Double_field(src, i + src_offs));
#ifndef BIGENDIAN
        dst[i * nc + c] = bswap_16(dst[i * nc + c]);
#endif
      }
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_to_s16_byte(value *argv, int argn) {
  return caml_mm_audio_to_s16(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

CAMLprim value caml_mm_audio_convert_s16(value _le, value _src, value _src_offs,
                                         value _dst, value _dst_offs,
                                         value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(dst);
  int little_endian = Bool_val(_le);
  const char *src = String_val(_src);
  int src_offs = Int_val(_src_offs);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(_dst);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int i, c;

  if (src_offs + len * nc * 2 > caml_string_length(_src))
    caml_invalid_argument("convert_native: output buffer too small");

  if (little_endian == 1)
    for (c = 0; c < nc; c++) {
      dst = Field(_dst, c);
      for (i = 0; i < len; i++)
        Store_double_field(dst, i + dst_offs,
                           get_s16le(src, src_offs, nc, c, i));
    }
  else
    for (c = 0; c < nc; c++) {
      dst = Field(_dst, c);
      for (i = 0; i < len; i++)
        Store_double_field(dst, i + dst_offs,
                           get_s16be(src, src_offs, nc, c, i));
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_convert_s16_byte(value *argv, int argn) {
  return caml_mm_audio_convert_s16(argv[0], argv[1], argv[2], argv[3], argv[4],
                                   argv[5]);
}

CAMLprim value caml_mm_audio_to_u8(value _src, value _src_offs, value _dst,
                                   value _dst_offs, value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(src);
  int c, i;
  int dst_offs = Int_val(_dst_offs);
  int src_offs = Int_val(_src_offs);
  int nc = Wosize_val(_src);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  uint8_t *dst = (uint8_t *)Bytes_val(_dst);

  if (caml_string_length(_dst) < nc * (dst_offs + len))
    caml_invalid_argument("pcm_to_s16: destination buffer too short");

  dst = dst + nc * dst_offs;

  for (c = 0; c < nc; c++) {
    src = Field(_src, c);
    for (i = 0; i < len; i++) {
      dst[i * nc + c + dst_offs] = u8_clip(Double_field(src, i + src_offs));
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_of_u8(value _src, value _src_offs, value _dst,
                                   value _dst_offs, value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(dst);
  const char *src = String_val(_src);
  int src_offs = Int_val(_src_offs);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(_dst);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  assert(nc > 0);
  int i, c;

  if (len + src_offs > caml_string_length(_src))
    caml_invalid_argument("convert_native: output buffer too small");

  for (c = 0; c < nc; c++) {
    dst = Field(_dst, c);
    for (i = 0; i < len; i++)
      Store_double_field(dst, i + dst_offs, get_u8(src, src_offs, nc, c, i));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_convert_s32le(value _src, value _src_offs,
                                           value _dst, value _dst_offs,
                                           value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(dst);
  const char *src = String_val(_src);
  int src_offs = Int_val(_src_offs);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(_dst);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int i, c;

  if (caml_string_length(_src) < src_offs + len * nc * 4)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c = 0; c < nc; c++) {
    dst = Field(_dst, c);
    for (i = 0; i < len; i++)
      Store_double_field(dst, i + dst_offs, get_s32le(src, src_offs, nc, c, i));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_convert_fltp(value _src, value _src_offs,
                                          value _dst, value _dst_offs,
                                          value _len, value _stride) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(dst);
  const float *src = Caml_ba_data_val(_src);
  int src_offs = Int_val(_src_offs);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(_dst);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int stride = Int_val(_stride);
  int i, c;

  if (stride < len)
    caml_invalid_argument(
        "caml_mm_audio_convert_fltp: invalid src length/stride");

  if (len < src_offs)
    caml_invalid_argument("caml_mm_audio_convert_fltp: invalid src_offset");

  if (Caml_ba_array_val(_src)->dim[0] < stride * nc)
    caml_invalid_argument(
        "caml_mm_audio_convert_fltp: output buffer too small");

  for (c = 0; c < nc; c++) {
    dst = Field(_dst, c);
    for (i = 0; i < len; i++)
      Store_double_field(dst, i + dst_offs,
                         clip(src[stride * c + src_offs + i]));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_convert_fltp_bytes(value *argv, value argn) {
  return caml_mm_audio_convert_fltp(argv[0], argv[1], argv[2], argv[3], argv[4],
                                    argv[5]);
}

CAMLprim value caml_mm_audio_convert_s24le(value _src, value _src_offs,
                                           value _dst, value _dst_offs,
                                           value _len) {
  CAMLparam2(_src, _dst);
  CAMLlocal1(dst);
  const char *src = String_val(_src);
  int src_offs = Int_val(_src_offs);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(_dst);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Int_val(_len);
  int i, c;

  if (caml_string_length(_src) < src_offs + len * nc * 3)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c = 0; c < nc; c++) {
    dst = Field(_dst, c);
    for (i = 0; i < len; i++)
      Store_double_field(dst, i + dst_offs, get_s24le(src, src_offs, nc, c, i));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_copy_from_ba(value _src, value _dst, value _ofs,
                                          value _len) {
  CAMLparam2(_src, _dst);
  float *src = Caml_ba_data_val(_src);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int i;

  for (i = 0; i < len; i++) {
    Store_double_field(_dst, i + ofs, src[i]);
  }

  CAMLreturn(_dst);
}

CAMLprim value caml_mm_audio_copy_to_ba(value _src, value _ofs, value _len,
                                        value _dst) {
  CAMLparam2(_src, _dst);
  float *dst = Caml_ba_data_val(_dst);
  int len = Int_val(_len);
  int ofs = Int_val(_ofs);
  long i;

  for (i = 0; i < len; i++) {
    dst[i] = Double_field(_src, i + ofs);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_audio_copy_from_int16_ba(value _src, value _dst,
                                                value _ofs, value _len) {
  CAMLparam2(_src, _dst);
  int16_t *src = Caml_ba_data_val(_src);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int i;

  for (i = 0; i < len; i++) {
    Store_double_field(_dst, i + ofs, ((double)src[i]) / INT16_MAX);
  }

  CAMLreturn(_dst);
}

CAMLprim value caml_mm_audio_copy_to_int16_ba(value _src, value _ofs,
                                              value _len, value _dst) {
  CAMLparam2(_src, _dst);
  int16_t *dst = Caml_ba_data_val(_dst);
  int len = Int_val(_len);
  int ofs = Int_val(_ofs);
  long i;

  for (i = 0; i < len; i++) {
    dst[i] = Double_field(_src, i + ofs) * INT16_MAX;
  }
  CAMLreturn(Val_unit);
}
