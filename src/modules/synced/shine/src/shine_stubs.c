/*
 * OCaml bindings for libshine
 *
 * Copyright 2005-2010 Savonet team
 *
 * This file is part of ocaml-shine.
 *
 * ocaml-shine is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-shine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-shine; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* OCaml bindings for the libshine library. */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>

#include <shine/layer3.h>

#include <string.h>

#include "config.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#define Encoder_val(v) (*((shine_t *)Data_custom_val(v)))

static void finalize_encoder(value e) { shine_close(Encoder_val(e)); }

static struct custom_operations encoder_ops = {
    "ocaml_shine_encoder",    finalize_encoder,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_shine_samples_per_pass(value e) {
  CAMLparam1(e);
  CAMLreturn(Val_int(shine_samples_per_pass(Encoder_val(e))));
}

CAMLprim value ocaml_shine_check_config(value samplerate, value bitrate) {
  CAMLparam0();

  if (shine_check_config(Int_val(samplerate), Int_val(bitrate)) < 0)
    CAMLreturn(Val_false);

  CAMLreturn(Val_true);
}

CAMLprim value ocaml_shine_init(value chans, value samplerate, value bitrate) {
  CAMLparam0();
  CAMLlocal1(ans);
  shine_config_t config;
  shine_t enc;

  shine_set_config_mpeg_defaults(&config.mpeg);

  config.wave.channels = Int_val(chans);
  config.wave.samplerate = Int_val(samplerate);
  config.mpeg.bitr = Int_val(bitrate);
  if (config.wave.channels == 1)
    config.mpeg.mode = MONO;
  else
    config.mpeg.mode = JOINT_STEREO;

  enc = shine_initialise(&config);
  if (enc == NULL)
    caml_raise_out_of_memory();

  ans = caml_alloc_custom(&encoder_ops, sizeof(shine_t), 1, 0);
  Encoder_val(ans) = enc;

  CAMLreturn(ans);
}

static inline int16_t clip(double s) {
  if (s < -1) {
    return INT16_MIN;
  } else if (s > 1) {
    return INT16_MAX;
  } else
    return (s * INT16_MAX);
}

CAMLprim value ocaml_shine_encode_float(value e, value data) {
  CAMLparam2(e, data);
  CAMLlocal2(src, ret);
  int16_t *pcm[2];
  int16_t chan1[SHINE_MAX_SAMPLES], chan2[SHINE_MAX_SAMPLES];
  int c, i;
  int written;
  unsigned char *outdata;

  shine_t enc = Encoder_val(e);
  pcm[0] = chan1, pcm[1] = chan2;

  for (c = 0; c < Wosize_val(data); c++) {
    src = Field(data, c);
    for (i = 0; i < shine_samples_per_pass(enc); i++) {
      pcm[c][i] = clip(Double_field(src, i));
    }
  }

  caml_enter_blocking_section();

  outdata = shine_encode_buffer(enc, pcm, &written);

  caml_leave_blocking_section();

  ret = caml_alloc_string(written);
  memcpy(Bytes_val(ret), outdata, written);

  CAMLreturn(ret);
}

#ifdef IS_BIGENDIAN
static inline int16_t bswap_16(int16_t x) {
  return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8));
}

void swap_buffer(int16_t *sample_buffer, int length) {
  int i;
  for (i = 0; i < length; i++)
    sample_buffer[i] = bswap_16(sample_buffer[i]);
}
#endif

CAMLprim value ocaml_shine_encode_s16le(value e, value data, value channels) {
  CAMLparam2(e, data);
  CAMLlocal1(ret);
  int16_t pcm[2 * SHINE_MAX_SAMPLES];
  int16_t *src = (int16_t *)Bytes_val(data);
  int written;
  int chans = Int_val(channels);

  unsigned char *outdata;

  shine_t enc = Encoder_val(e);

  memcpy(pcm, src, chans * shine_samples_per_pass(enc) * sizeof(int16_t));

  caml_enter_blocking_section();

#ifdef IS_BIGENDIAN
  swap_buffer(pcm, chans * shine_samples_per_pass(enc));
#endif

  outdata = shine_encode_buffer_interleaved(enc, pcm, &written);

  caml_leave_blocking_section();

  ret = caml_alloc_string(written);
  memcpy(Bytes_val(ret), outdata, written);

  CAMLreturn(ret);
}

CAMLprim value ocaml_shine_flush(value e) {
  CAMLparam1(e);
  CAMLlocal1(ret);

  int written;
  unsigned char *outdata;
  shine_t enc = Encoder_val(e);

  outdata = shine_flush(enc, &written);

  ret = caml_alloc_string(written);
  memcpy(Bytes_val(ret), outdata, written);

  CAMLreturn(ret);
}
