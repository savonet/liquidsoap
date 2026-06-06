/*
 * OCaml bindings for liblame
 *
 * Copyright 2005-2006 Savonet team
 *
 * This file is part of ocaml-lame.
 *
 * ocaml-lame is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-lame is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-lame; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <assert.h>
#include <lame/lame.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#define Lame_val(v) (*(lame_global_flags **)Data_custom_val(v))

static inline int16_t bswap_16(int16_t x) {
  return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8));
}

static void finalize_lame_t(value l) {
  lame_global_flags *lgf = Lame_val(l);
  lame_close(lgf);
}

static struct custom_operations lame_ops = {
    "ocaml_lame_t",      finalize_lame_t,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_lame_init(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(l);

  lame_global_flags *lgf;
  lgf = lame_init();
#ifdef DEBUG
  printf("New encoder: %p.\n", lgf);
#endif
  l = caml_alloc_custom(&lame_ops, sizeof(lame_global_flags *), 0, 1);
  Lame_val(l) = lgf;

  CAMLreturn(l);
}

CAMLprim value ocaml_lame_frame_size(value _enc) {
  CAMLparam1(_enc);
  CAMLreturn(Val_int(lame_get_framesize(Lame_val(_enc))));
}

/***** Parameters *****/

#define BIND_SET_INT_PARAM(p)                                                  \
  CAMLprim value ocaml_lame_set_##p(value l, value v) {                        \
    CAMLparam1(l);                                                             \
    lame_set_##p(Lame_val(l), Int_val(v));                                     \
    CAMLreturn(Val_unit);                                                      \
  }

#define BIND_GET_INT_PARAM(p)                                                  \
  CAMLprim value ocaml_lame_get_##p(value l) {                                 \
    CAMLparam1(l);                                                             \
    CAMLreturn(Val_int(lame_get_##p(Lame_val(l))));                            \
  }

#define BIND_INT_PARAM(p) BIND_GET_INT_PARAM(p) BIND_SET_INT_PARAM(p)

BIND_INT_PARAM(num_samples);
BIND_INT_PARAM(in_samplerate);
BIND_INT_PARAM(num_channels);
BIND_INT_PARAM(out_samplerate);
BIND_INT_PARAM(quality);
BIND_INT_PARAM(mode);
BIND_INT_PARAM(brate);

#define BIND_FLOAT_PARAM(p)                                                    \
  CAMLprim value ocaml_lame_set_##p(value l, value v) {                        \
    CAMLparam2(l, v);                                                          \
    lame_set_##p(Lame_val(l), Double_val(v));                                  \
    CAMLreturn(Val_unit);                                                      \
  }                                                                            \
  CAMLprim value ocaml_lame_get_##p(value l) {                                 \
    CAMLparam1(l);                                                             \
    CAMLreturn(caml_copy_double(lame_get_##p(Lame_val(l))));                   \
  }

BIND_FLOAT_PARAM(compression_ratio);

CAMLprim value ocaml_lame_init_params(value l) {
  CAMLparam1(l);
  int ans = lame_init_params(Lame_val(l));
  if (ans < 0) {
    fprintf(stderr, "init_params error: %d\n", ans);
    caml_raise_constant(*caml_named_value("lame_exn_init_params_failed"));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lame_init_bitstream(value l) {
  CAMLparam1(l);
  lame_init_bitstream(Lame_val(l));
  CAMLreturn(Val_unit);
}

static vbr_mode vbr_mode_val[] = {vbr_off, vbr_rh, vbr_abr, vbr_mtrh,
                                  vbr_max_indicator};

CAMLprim value ocaml_lame_set_vbr(value l, value m) {
  CAMLparam2(l, m);
  lame_set_VBR(Lame_val(l), vbr_mode_val[Int_val(m)]);
  CAMLreturn(Val_unit);
}

BIND_INT_PARAM(VBR_q);
BIND_INT_PARAM(VBR_mean_bitrate_kbps);
BIND_INT_PARAM(VBR_min_bitrate_kbps);
BIND_INT_PARAM(VBR_max_bitrate_kbps);
BIND_INT_PARAM(VBR_hard_min);

#define BIND_SET_BOOL_PARAM(p)                                                 \
  CAMLprim value ocaml_lame_set_##p(value l, value v) {                        \
    CAMLparam1(l);                                                             \
    lame_set_##p(Lame_val(l), Int_val(v));                                     \
    CAMLreturn(Val_unit);                                                      \
  }

#define BIND_GET_BOOL_PARAM(p)                                                 \
  CAMLprim value ocaml_lame_get_##p(value l) {                                 \
    CAMLparam1(l);                                                             \
    CAMLreturn(Val_bool(lame_get_##p(Lame_val(l))));                           \
  }

#define BIND_BOOL_PARAM(p) BIND_GET_BOOL_PARAM(p) BIND_SET_BOOL_PARAM(p)

BIND_SET_BOOL_PARAM(bWriteVbrTag)
BIND_SET_BOOL_PARAM(disable_reservoir)
BIND_BOOL_PARAM(copyright)
BIND_BOOL_PARAM(original)
BIND_BOOL_PARAM(extension)

/****** Encoding ******/

static void raise_enc_err(int n) {
  switch (n) {
  case -3:
    caml_raise_constant(*caml_named_value("lame_exn_init_params_not_called"));

  case -4:
    caml_raise_constant(*caml_named_value("lame_exn_psychoacoustic_problem"));

  default:
    /* TODO: be more precise. */
    caml_raise_with_arg(*caml_named_value("lame_exn_unknown_error"),
                        Val_int(n));
  }
}

CAMLprim value ocaml_lame_encode_buffer_interleaved(value l, value _buf,
                                                    value _ofs,
                                                    value _samples) {
  CAMLparam4(l, _buf, _ofs, _samples);
  CAMLlocal1(ret);
  lame_global_flags *lgf = Lame_val(l);
  int samples = Int_val(_samples);
  int inbuf_len = caml_string_length(_buf);
  short int *inbuf = malloc(inbuf_len);
  unsigned char outbuf[LAME_MAXMP3BUFFER];
  int ans;

  memcpy(inbuf, String_val(_buf), inbuf_len);

  caml_release_runtime_system();
#ifdef BIGENDIAN
  int i;
  for (i = 0; i < inbuf_len / 2; i++)
    inbuf[i] = bswap_16(inbuf[i]);
#endif
  ans = lame_encode_buffer_interleaved(lgf, inbuf, samples, outbuf,
                                       sizeof(outbuf));
  caml_acquire_runtime_system();

  free(inbuf);

  if (ans < 0)
    raise_enc_err(ans);

  ret = caml_alloc_string(ans);
  memcpy(Bytes_val(ret), outbuf, ans);

  CAMLreturn(ret);
}

static inline double clip(double s) {
  // NaN
  if (s != s)
    return 0;

  if (s < -1) {
    return -1;
  } else if (s > 1) {
    return 1;
  } else
    return s;
}

/* Input: float arrays, floats in [-1..1]
 * Output: MP3 frames.
 * Lame wants floats in +/- SHRTMAX, we renormalize when copying to C heap.
 * TODO Figure out how to proceed when encoding mono data. */
CAMLprim value ocaml_lame_encode_buffer_float(value l, value _bufl, value _bufr,
                                              value _ofs, value _samples) {
  CAMLparam5(l, _bufl, _bufr, _ofs, _samples);
  CAMLlocal1(ret);
  lame_global_flags *lgf = Lame_val(l);
  int ofs = Int_val(_ofs);
  int samples = Int_val(_samples);
  float *inbufl = malloc(sizeof(float) * samples);
  float *inbufr = malloc(sizeof(float) * samples);
  unsigned char outbuf[LAME_MAXMP3BUFFER];
  int i, ans;

  for (i = 0; i < samples; i++) {
    inbufl[i] = 32768. * clip(Double_field(_bufl, ofs + i));
    inbufr[i] = 32768. * clip(Double_field(_bufr, ofs + i));
  }

  caml_release_runtime_system();
  ans = lame_encode_buffer_float(lgf, inbufl, inbufr, samples, outbuf,
                                 sizeof(outbuf));
  caml_acquire_runtime_system();

  free(inbufl);
  free(inbufr);

  if (ans < 0)
    raise_enc_err(ans);

  ret = caml_alloc_string(ans);
  memcpy(Bytes_val(ret), outbuf, ans);

  CAMLreturn(ret);
}

CAMLprim value ocaml_lame_encode_buffer_float_ba(value l, value _bufl,
                                                 value _bufr) {
  CAMLparam3(l, _bufl, _bufr);
  CAMLlocal1(ret);
  lame_global_flags *lgf = Lame_val(l);
  struct caml_ba_array *bal = Caml_ba_array_val(_bufl);
  struct caml_ba_array *bar = Caml_ba_array_val(_bufr);
  const float *bufl = bal->data;
  const float *bufr = bar->data;
  int samples = bal->dim[0];
  if (bar->dim[0] != samples)
    caml_failwith("Invalid argument: buffers must be of same length");

  caml_release_runtime_system();

  unsigned char outbuf[LAME_MAXMP3BUFFER];
  int ans;

  ans = lame_encode_buffer_float(lgf, bufl, bufr, samples, outbuf,
                                 sizeof(outbuf));

  caml_acquire_runtime_system();

  if (ans < 0)
    raise_enc_err(ans);

  ret = caml_alloc_string(ans);
  memcpy(Bytes_val(ret), outbuf, ans);

  CAMLreturn(ret);
}

CAMLprim value ocaml_lame_encode_flush(value l) {
  CAMLparam1(l);
  CAMLlocal1(ret);
  lame_global_flags *lgf = Lame_val(l);
  int ans;
  unsigned char outbuf[LAME_MAXMP3BUFFER];

  caml_release_runtime_system();
  ans = lame_encode_flush(lgf, outbuf, sizeof(outbuf));
  caml_acquire_runtime_system();

  if (ans < 0)
    raise_enc_err(ans);

  ret = caml_alloc_string(ans);
  memcpy(Bytes_val(ret), outbuf, ans);

  CAMLreturn(ret);
}

CAMLprim value ocaml_lame_encode_flush_nogap(value l) {
  CAMLparam1(l);
  CAMLlocal1(ret);
  lame_global_flags *lgf = Lame_val(l);
  int ans;
  unsigned char outbuf[LAME_MAXMP3BUFFER];

  caml_release_runtime_system();
  ans = lame_encode_flush_nogap(lgf, outbuf, sizeof(outbuf));
  caml_acquire_runtime_system();

  if (ans < 0)
    raise_enc_err(ans);

  ret = caml_alloc_string(ans);
  memcpy(Bytes_val(ret), outbuf, ans);

  CAMLreturn(ret);
}

/***** Id3 tags *****/

CAMLprim value ocaml_lame_id3tag_init(value l) {
  CAMLparam1(l);

  id3tag_init(Lame_val(l));

  CAMLreturn(Val_unit);
}

#define BIND_TAG_PARAM(p)                                                      \
  CAMLprim value ocaml_lame_id3tag_set_##p(value l, value v) {                 \
    CAMLparam2(l, v);                                                          \
    id3tag_set_##p(Lame_val(l), String_val(v));                                \
    CAMLreturn(Val_unit);                                                      \
  }

BIND_TAG_PARAM(title);
BIND_TAG_PARAM(artist);
BIND_TAG_PARAM(album);
BIND_TAG_PARAM(year);
BIND_TAG_PARAM(comment);
BIND_TAG_PARAM(track);
/* TODO: check for errors. */
BIND_TAG_PARAM(genre);

/***** Information *****/

BIND_GET_INT_PARAM(version);
BIND_GET_INT_PARAM(encoder_delay);
BIND_GET_INT_PARAM(framesize);
BIND_GET_INT_PARAM(mf_samples_to_encode);
BIND_GET_INT_PARAM(frameNum);
BIND_GET_INT_PARAM(totalframes);

#define BIND_GET_STRING_PARAM(p)                                               \
  CAMLprim value ocaml_lame_get_##p(value unit) {                              \
    CAMLparam1(unit);                                                          \
    CAMLreturn(caml_copy_string(get_##p()));                                   \
  }

BIND_GET_STRING_PARAM(lame_version);
BIND_GET_STRING_PARAM(lame_short_version);
BIND_GET_STRING_PARAM(lame_very_short_version);
BIND_GET_STRING_PARAM(psy_version);
BIND_GET_STRING_PARAM(lame_url);
