/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-soundtouch.
 *
 * ocaml-soundtouch is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-soundtouch is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-soundtouch; if not, write to the Free Software
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

#include <assert.h>
#include <string.h>

// Hack in order to have access to channels... (numChannels was only recently
// introduced)
#define protected public
#include <BPMDetect.h>
#include <SoundTouch.h>
#undef protected

using namespace soundtouch;

/* Declaring the functions which should be accessible on the C side. */
extern "C" {
/* flush definition in <caml/compatibility.h> messes with the call to flush()
   below. Disabling the include here. */
#define CAML_COMPATIBILITY_H

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

CAMLprim value ocaml_st_make(value unit);
CAMLprim value ocaml_st_get_version_string(value st);
CAMLprim value ocaml_st_get_version_id(value st);
CAMLprim value ocaml_st_set_channels(value st, value channels);
CAMLprim value ocaml_st_set_samplerate(value st, value samplerate);
CAMLprim value ocaml_st_set_rate(value st, value rate);
CAMLprim value ocaml_st_set_tempo(value st, value tempo);
CAMLprim value ocaml_st_set_pitch(value st, value pitch);
CAMLprim value ocaml_st_flush(value st);
CAMLprim value ocaml_st_clear(value st);
CAMLprim value ocaml_st_putsamples_ba(value _st, value samples);
CAMLprim value ocaml_st_putsamples_ni(value _st, value samples, value _ofs,
                                      value _len);
CAMLprim value ocaml_st_num_samples(value st);
CAMLprim value ocaml_st_receive_samples_ba(value _st, value samples);
CAMLprim value ocaml_st_receive_samples_ni(value st, value samples, value _ofs,
                                           value _len);

CAMLprim value ocaml_st_bpm_make(value chans, value rate);
CAMLprim value ocaml_st_bpm_putsamples_ba(value _bpm, value samples);
CAMLprim value ocaml_st_bpm_putsamples_ni(value _bpm, value samples, value _ofs,
                                          value _len);
CAMLprim value ocaml_st_bpm_get_bpm(value bpm);
}

#define ST_val(v) (*((SoundTouch **)Data_custom_val(v)))

static void finalize_st(value s) {
  SoundTouch *st = ST_val(s);

  delete st;
}

static struct custom_operations stream_ops = {
    (char *)"ocaml_st",       finalize_st,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_st_make(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);

  SoundTouch *st = new SoundTouch();

  ans = caml_alloc_custom(&stream_ops, sizeof(SoundTouch *), 1, 0);
  ST_val(ans) = st;

  CAMLreturn(ans);
}

CAMLprim value ocaml_st_get_version_string(value st) {
  return caml_copy_string(ST_val(st)->getVersionString());
}

CAMLprim value ocaml_st_get_version_id(value st) {
  return Val_int(ST_val(st)->getVersionId());
}

CAMLprim value ocaml_st_set_channels(value st, value channels) {
  ST_val(st)->setChannels(Int_val(channels));

  return Val_unit;
}

CAMLprim value ocaml_st_set_samplerate(value st, value samplerate) {
  ST_val(st)->setSampleRate(Int_val(samplerate));

  return Val_unit;
}

CAMLprim value ocaml_st_set_rate(value st, value rate) {
  ST_val(st)->setRate(Double_val(rate));

  return Val_unit;
}

CAMLprim value ocaml_st_set_tempo(value st, value tempo) {
  ST_val(st)->setTempo(Double_val(tempo));

  return Val_unit;
}

CAMLprim value ocaml_st_set_pitch(value st, value pitch) {
  ST_val(st)->setPitch(Double_val(pitch));

  return Val_unit;
}

CAMLprim value ocaml_st_flush(value st) {
  ST_val(st)->flush();

  return Val_unit;
}

CAMLprim value ocaml_st_clear(value st) {
  ST_val(st)->clear();

  return Val_unit;
}

CAMLprim value ocaml_st_putsamples_ba(value _st, value samples) {
  CAMLparam2(_st, samples);
  SoundTouch *st = ST_val(_st);
  int chans = st->channels;
  int len = Caml_ba_array_val(samples)->dim[0] / chans;
  float *buf = (float *)Caml_ba_data_val(samples);

  caml_enter_blocking_section();
  st->putSamples(buf, len);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_st_putsamples_ni(value _st, value samples, value _ofs,
                                      value _len) {
  CAMLparam2(_st, samples);
  CAMLlocal1(cbuf);
  SoundTouch *st = ST_val(_st);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int chans = Wosize_val(samples);
  float *buf;
  int i, c;

  buf = (float *)malloc(chans * len * sizeof(float));
  for (c = 0; c < chans; c++) {
    cbuf = Field(samples, c);
    for (i = 0; i < len; i++)
      buf[chans * i + c] = Double_field(cbuf, ofs + i);
  }

  caml_enter_blocking_section();
  st->putSamples(buf, len);
  caml_leave_blocking_section();

  free(buf);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_st_num_samples(value st) {
  return Val_int(ST_val(st)->numSamples());
}

CAMLprim value ocaml_st_receive_samples_ba(value _st, value samples) {
  CAMLparam2(_st, samples);
  SoundTouch *st = ST_val(_st);
  int chans = st->channels;
  int len = Caml_ba_array_val(samples)->dim[0] / chans;
  float *buf = (float *)Caml_ba_data_val(samples);
  int ret;

  caml_enter_blocking_section();
  ret = ST_val(st)->receiveSamples(buf, len);
  caml_leave_blocking_section();

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_st_receive_samples_ni(value st, value samples, value _ofs,
                                           value _len) {
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int chans = Wosize_val(samples);
  float *buf;
  value cbuf;
  int ret;
  int i, c;

  buf = (float *)malloc(chans * len * sizeof(float));
  ret = ST_val(st)->receiveSamples(buf, len);
  for (c = 0; c < chans; c++) {
    cbuf = Field(samples, c);
    for (i = 0; i < ret; i++)
      Store_double_field(cbuf, ofs + i, buf[chans * i + c]);
  }
  free(buf);

  return Val_int(ret);
}

/*****************
 * BPM detection *
 *****************/

#define BPM_val(v) (*((BPMDetect **)Data_custom_val(v)))

static void finalize_bpm(value b) {
  BPMDetect *bpm = BPM_val(b);

  delete bpm;
}

static struct custom_operations bpm_ops = {
    (char *)"ocaml_bpm",      finalize_bpm,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_st_bpm_make(value chans, value rate) {
  CAMLparam0();
  CAMLlocal1(ans);

  BPMDetect *bpm = new BPMDetect(Int_val(chans), Int_val(rate));

  ans = caml_alloc_custom(&bpm_ops, sizeof(BPMDetect *), 1, 0);
  BPM_val(ans) = bpm;

  CAMLreturn(ans);
}

CAMLprim value ocaml_st_bpm_putsamples_ba(value _bpm, value samples) {
  CAMLparam2(_bpm, samples);
  BPMDetect *bpm = BPM_val(_bpm);
  int chans = bpm->channels;
  int len = Caml_ba_array_val(samples)->dim[0] / chans;
  float *buf = (float *)Caml_ba_data_val(samples);

  caml_enter_blocking_section();
  bpm->inputSamples(buf, len);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_st_bpm_putsamples_ni(value _bpm, value samples, value _ofs,
                                          value _len) {
  CAMLparam2(_bpm, samples);
  CAMLlocal1(cbuf);
  BPMDetect *bpm = BPM_val(_bpm);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int chans = Wosize_val(samples);
  float *buf;
  int i, c;

  buf = (float *)malloc(chans * len * sizeof(float));
  for (c = 0; c < chans; c++) {
    cbuf = Field(samples, c);
    for (i = 0; i < len; i++)
      buf[chans * i + c] = Double_field(cbuf, ofs + i);
  }

  caml_enter_blocking_section();
  bpm->inputSamples(buf, len);
  caml_leave_blocking_section();

  free(buf);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_st_bpm_get_bpm(value bpm) {
  return caml_copy_double(BPM_val(bpm)->getBpm());
}
