/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-portaudio.
 *
 * ocaml-portaudio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-portaudio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-portaudio; if not, write to the Free Software
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
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include <portaudio.h>
#include <stdio.h>

typedef struct stream__t {
  PaStream *stream;
  int channels_in;
  int channels_out;
  int sample_format_in;
  int sample_format_out;
  value cb;
  int tstart;
  int tend;
} stream_t;

#define Stream_t_val(v) (*((stream_t **)Data_custom_val(v)))
#define Stream_val(v) (Stream_t_val(v))->stream

/* Check for errors. */
static int cerr(int ret) {
  if (ret >= 0)
    return ret;

  switch (ret) {
  case paUnanticipatedHostError:
    caml_raise_constant(
        *caml_named_value("portaudio_exn_unanticipated_host_error"));

  default:
    caml_raise_with_arg(*caml_named_value("portaudio_exn_error"), Val_int(ret));
  }
}

CAMLprim value ocaml_pa_get_last_host_error_info(value unit) {
  CAMLparam0();
  const PaHostErrorInfo *info = Pa_GetLastHostErrorInfo();
  CAMLlocal1(ans);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(info->errorCode));
  Store_field(ans, 1, caml_copy_string(info->errorText));

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_get_version(value unit) {
  return Val_int(Pa_GetVersion());
}

CAMLprim value ocaml_pa_get_version_text(value unit) {
  return caml_copy_string(Pa_GetVersionText());
}

CAMLprim value ocaml_pa_get_error_text(value n) {
  return caml_copy_string(Pa_GetErrorText(Int_val(n)));
}

CAMLprim value ocaml_pa_initialize(value unit) {
  cerr(Pa_Initialize());

  return Val_unit;
}

CAMLprim value ocaml_pa_terminate(value unit) {
  cerr(Pa_Terminate());

  return Val_unit;
}

CAMLprim value ocaml_pa_get_host_api_count(value unit) {
  return Val_int(cerr(Pa_GetHostApiCount()));
}

CAMLprim value ocaml_pa_get_default_host_api(value unit) {
  return Val_int(cerr(Pa_GetDefaultHostApi()));
}

CAMLprim value ocaml_pa_get_host_api_info(value id) {
  CAMLparam1(id);
  CAMLlocal1(res);
  const PaHostApiInfo *info = Pa_GetHostApiInfo(Int_val(id));
  res = caml_alloc_tuple(6);
  Field(res, 0) = Val_int(info->structVersion);
  Field(res, 1) = Val_int(info->type);
  Field(res, 2) = caml_copy_string(info->name);
  Field(res, 3) = Val_int(info->deviceCount);
  Field(res, 4) = Val_int(info->defaultOutputDevice);
  Field(res, 5) = Val_int(info->defaultInputDevice);
  CAMLreturn(res);
}

CAMLprim value ocaml_pa_get_default_input_device(value unit) {
  return Val_int(cerr(Pa_GetDefaultInputDevice()));
}

CAMLprim value ocaml_pa_get_default_output_device(value unit) {
  return Val_int(cerr(Pa_GetDefaultOutputDevice()));
}

CAMLprim value ocaml_pa_get_device_count(value unit) {
  return Val_int(cerr(Pa_GetDeviceCount()));
}

CAMLprim value ocaml_pa_get_device_info(value id) {
  CAMLparam1(id);
  CAMLlocal1(res);
  const PaDeviceInfo *info = Pa_GetDeviceInfo(Int_val(id));
  res = caml_alloc_tuple(10);
  Field(res, 0) = Val_int(info->structVersion);
  Field(res, 1) = caml_copy_string(info->name);
  Field(res, 2) = Val_int(info->hostApi);
  Field(res, 3) = Val_int(info->maxInputChannels);
  Field(res, 4) = Val_int(info->maxOutputChannels);
  Field(res, 5) = caml_copy_double(info->defaultLowInputLatency);
  Field(res, 6) = caml_copy_double(info->defaultLowOutputLatency);
  Field(res, 7) = caml_copy_double(info->defaultHighInputLatency);
  Field(res, 8) = caml_copy_double(info->defaultHighOutputLatency);
  Field(res, 9) = caml_copy_double(info->defaultSampleRate);
  CAMLreturn(res);
}

static int get_ba_type(int fmt) {
  if (fmt & paFloat32)
    return CAML_BA_FLOAT32;
  if (fmt & paInt32 || fmt & paInt24)
    return CAML_BA_INT32;
  if (fmt & paInt16)
    return CAML_BA_SINT16;
  if (fmt & paInt8)
    return CAML_BA_SINT8;

  return 0;
}

value alloc_ba_input_ni(const void *data, unsigned long frames, stream_t *st) {
  int type = get_ba_type(st->sample_format_in);

  if (st->channels_in > 0) {
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 2, (void *)data,
                              st->channels_in, frames, NULL);
  }
  return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

value alloc_ba_output_ni(void *data, unsigned long frames, stream_t *st) {
  int type = get_ba_type(st->sample_format_out);

  if (st->channels_out > 0) {
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 2, data,
                              st->channels_out, frames, NULL);
  }
  return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

value alloc_ba_input(const void *data, unsigned long frames, stream_t *st) {
  if (st->sample_format_in & paNonInterleaved)
    return alloc_ba_input_ni(data, frames, st);

  int type = get_ba_type(st->sample_format_in);
  if (st->channels_in > 0) {
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 1, (void *)data,
                              st->channels_in * frames, NULL);
  }
  return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

value alloc_ba_output(void *data, unsigned long frames, stream_t *st) {
  if (st->sample_format_out & paNonInterleaved)
    return alloc_ba_output_ni(data, frames, st);

  int type = get_ba_type(st->sample_format_out);
  if (st->channels_out > 0) {
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 1, data,
                              st->channels_out * frames, NULL);
  }
  return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

int pa_callback(const void *input_buffer, void *output_buffer,
                unsigned long frames_per_buffer,
                const PaStreamCallbackTimeInfo *time_info,
                PaStreamCallbackFlags status_flags, void *user_data) {
  stream_t *st = (stream_t *)user_data;
  int ret;

#ifdef CAML_THREADS
  if (!st->tstart && !st->tend) {
    caml_c_thread_register();
    st->tstart = 1;
  } else if (st->tstart && st->tend) {
    st->tstart = 0;
    caml_c_thread_unregister();
    return 0;
  } else if (st->tend) {
    return 0;
  }
#endif

  caml_leave_blocking_section();
  value in, out;
  in = alloc_ba_input(input_buffer, frames_per_buffer, st);
  out = alloc_ba_output(output_buffer, frames_per_buffer, st);
  ret = Int_val(caml_callback3(st->cb, in, out, Val_int(frames_per_buffer)));
  caml_enter_blocking_section();

  return ret;
}

static const int format_cst[6] = {paInt8, paInt16, paInt24, paInt32, paFloat32};

static int fmt_val(value format, int interleaved) {
  return format_cst[Int_val(format)] | (interleaved ? 0 : paNonInterleaved);
}

/* The result must be freed after use. */
static PaStreamParameters *sp_val(value vsp, int interleaved) {
  PaStreamParameters *sp = malloc(sizeof(PaStreamParameters));

  sp->channelCount = Int_val(Field(vsp, 0));
  sp->device = Int_val(Field(vsp, 1));
  sp->hostApiSpecificStreamInfo = NULL;
  sp->sampleFormat = fmt_val(Field(vsp, 2), interleaved);
  sp->suggestedLatency = Double_val(Field(vsp, 3));

  return sp;
}

static void finalize_stream(value s) {
  stream_t *st = Stream_t_val(s);

  if (st->stream)
    Pa_CloseStream(st->stream);
  caml_remove_generational_global_root(&st->cb);
  free(st);
}

static struct custom_operations stream_ops = {
    "ocaml_pa_stream",   finalize_stream,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_pa_open_stream(value inparam, value outparam,
                                    value interleaved, value rate, value frames,
                                    value flags, value cb) {
  CAMLparam5(inparam, outparam, interleaved, rate, frames);
  CAMLxparam2(flags, cb);
  CAMLlocal1(ans);
  stream_t *st;
  PaStream *stream;
  PaStreamParameters *ip = NULL, *op = NULL;
  int ret;
  PaStreamCallback *callb = NULL;

  st = malloc(sizeof(stream_t));
  memset(st, 0, sizeof(*st));
  st->tstart = 0;
  st->tend = 0;

  if (Is_block(inparam)) {
    ip = sp_val(Field(inparam, 0), Int_val(interleaved));
    st->channels_in = ip->channelCount;
    st->sample_format_in = ip->sampleFormat;
  }
  if (Is_block(outparam)) {
    op = sp_val(Field(outparam, 0), Int_val(interleaved));
    st->channels_out = op->channelCount;
    st->sample_format_out = op->sampleFormat;
  }
  if (Is_block(cb)) {
    st->cb = Field(cb, 0);
    caml_register_generational_global_root(&st->cb);
    callb = &pa_callback;
  }

  ret = Pa_OpenStream(&stream, ip, op, Double_val(rate), Int_val(frames),
                      paNoFlag, callb, st);

  if (ret < 0)
    free(st);
  if (ip != NULL)
    free(ip);
  if (op != NULL)
    free(op);

  cerr(ret);
  st->stream = stream;
  ans = caml_alloc_custom(&stream_ops, sizeof(stream_t *), 1, 0);
  Stream_t_val(ans) = st;

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_open_stream_byte(value *argv, int argc) {
  return ocaml_pa_open_stream(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5], argv[6]);
}

CAMLprim value ocaml_pa_open_default_stream(value inchans, value outchans,
                                            value fmt, value interleaved,
                                            value rate, value frames,
                                            value cb) {
  CAMLparam5(inchans, outchans, fmt, rate, frames);
  CAMLxparam1(cb);
  CAMLlocal1(ans);
  stream_t *st;
  PaStream *stream;
  int ret;
  int inc = Int_val(inchans);
  int outc = Int_val(outchans);
  int sample_rate = Int_val(rate);
  int num_frames = Int_val(frames);
  int format = fmt_val(fmt, Int_val(interleaved));
  PaStreamCallback *callb = NULL;

  st = malloc(sizeof(stream_t));
  st->channels_in = inc;
  st->channels_out = outc;
  st->sample_format_in = format;
  st->sample_format_out = format;
  st->tstart = 0;
  st->tend = 0;

  if (Is_block(cb)) {
    st->cb = Field(cb, 0);
    caml_register_generational_global_root(&st->cb);
    callb = &pa_callback;
  }

  ret = Pa_OpenDefaultStream(&stream, inc, outc, format, sample_rate,
                             num_frames, callb, st);
  if (ret < 0)
    free(st);
  cerr(ret);

  st->stream = stream;
  ans = caml_alloc_custom(&stream_ops, sizeof(stream_t *), 1, 0);
  Stream_t_val(ans) = st;

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_open_default_stream_byte(value *argv, int argc) {
  return ocaml_pa_open_default_stream(argv[0], argv[1], argv[2], argv[3],
                                      argv[4], argv[5], argv[6]);
}

CAMLprim value ocaml_pa_start_stream(value stream) {
  cerr(Pa_StartStream(Stream_val(stream)));
  caml_enter_blocking_section();
  Pa_Sleep(1);
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value ocaml_pa_stop_stream(value stream) {
  Stream_t_val(stream)->tend = 1;
  caml_enter_blocking_section();
  Pa_Sleep(1);
  cerr(Pa_StopStream(Stream_val(stream)));
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value ocaml_pa_abort_stream(value stream) {
  cerr(Pa_AbortStream(Stream_val(stream)));

  return Val_unit;
}

CAMLprim value ocaml_pa_close_stream(value stream) {
  cerr(Pa_CloseStream(Stream_val(stream)));
  Stream_val(stream) = 0;

  return Val_unit;
}

CAMLprim value ocaml_pa_sleep(value time) {
  CAMLparam1(time);
  caml_enter_blocking_section();
  Pa_Sleep(Int_val(time));
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

int get_index(int fmt, int chans, int len, int c, int i) {
  int index;
  if (fmt & paNonInterleaved) {
    index = len * c + i;
  } else {
    index = chans * i + c;
  }
  return index;
}

#define GET_BUFFER(type, elem)                                                 \
  {                                                                            \
    type *bufi = malloc(chans * len * sizeof(type));                           \
    for (c = 0; c < chans; ++c) {                                              \
      value bufc = Field(buf, c);                                              \
      for (i = 0; i < len; ++i) {                                              \
        bufi[chans * i + c] = elem;                                            \
      }                                                                        \
    }                                                                          \
    return bufi;                                                               \
  }

#define GET_BUFFER_NI(type, elem)                                              \
  {                                                                            \
    type **bufi = malloc(chans * sizeof(type *));                              \
    for (c = 0; c < chans; ++c) {                                              \
      bufi[c] = malloc(len * sizeof(type));                                    \
      value bufc = Field(buf, c);                                              \
      for (i = 0; i < len; ++i) {                                              \
        bufi[c][i] = elem;                                                     \
      }                                                                        \
    }                                                                          \
    return bufi;                                                               \
  }

void *get_buffer(int fmt, int chans, int ofs, int len, value buf) {
  int c, i;
  if (fmt & paFloat32) {
    if (fmt & paNonInterleaved)
      GET_BUFFER_NI(float, Double_field(bufc, ofs + i))
    else
      GET_BUFFER(float, Double_field(bufc, ofs + i))
  } else if (fmt & paInt32 || fmt & paInt24) {
    if (fmt & paNonInterleaved)
      GET_BUFFER_NI(int32_t, Int32_val(Field(bufc, ofs + i)))
    else
      GET_BUFFER(int32_t, Int32_val(Field(bufc, ofs + i)))
  } else if (fmt & paInt16) {
    if (fmt & paNonInterleaved)
      GET_BUFFER_NI(short, Int_val(Field(bufc, ofs + i)))
    else
      GET_BUFFER(short, Int_val(Field(bufc, ofs + i)))
  } else if (fmt & paInt8) {
    if (fmt & paNonInterleaved)
      GET_BUFFER_NI(char, Int_val(Field(bufc, ofs + i)))
    else
      GET_BUFFER(char, Int_val(Field(bufc, ofs + i)))
  } else
    return NULL;
}

void *get_read_buffer(int fmt, int chans, int len) {
  int size = 0;
  if (fmt & paFloat32) {
    size = sizeof(float);
  } else if (fmt & paInt32 || fmt & paInt24) {
    size = sizeof(int32_t);
  } else if (fmt & paInt16) {
    size = sizeof(short);
  } else if (fmt & paInt8) {
    size = sizeof(char);
  }

  if (size == 0)
    return NULL;

  if (fmt & paNonInterleaved) {
    void **buf = malloc(chans * sizeof(void *));
    int i;
    for (i = 0; i < chans; ++i)
      buf[i] = malloc(len * size);
    return buf;
  }
  return malloc(chans * len * size);
}

#define COPY_BUFFER(type, store, elem)                                         \
  type *buf = inbuf;                                                           \
  for (c = 0; c < chans; c++) {                                                \
    value bufc = Field(_buf, c);                                               \
    for (i = 0; i < len; i++)                                                  \
      store(bufc, ofs + i, elem(buf[get_index(fmt, chans, len, c, i)]));       \
  }

void ocaml_portaudio_copy_buffer(void *inbuf, int fmt, int chans, int ofs,
                                 int len, value _buf) {
  int c, i;
  if (fmt & paFloat32) {
    COPY_BUFFER(float, Store_double_field, (float))
  } else if (fmt & paInt32 || fmt & paInt24) {
    COPY_BUFFER(int32_t, Store_field, caml_copy_int32)
  } else if (fmt & paInt16) {
    COPY_BUFFER(short, Store_field, Val_int);
  } else if (fmt & paInt8) {
    COPY_BUFFER(char, Store_field, Val_int);
  }
}

CAMLprim value ocaml_pa_write_stream(value _stream, value _buf, value _ofs,
                                     value _len) {
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_out;
  int format = Stream_t_val(_stream)->sample_format_out;

  buf = get_buffer(format, chans, ofs, len, _buf);

  caml_enter_blocking_section();
  ret = Pa_WriteStream(stream, buf, len);
  caml_leave_blocking_section();
  free(buf);
  cerr(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_stream(value _stream, value _buf, value _ofs,
                                    value _len) {
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_in;
  int format = Stream_t_val(_stream)->sample_format_in;

  buf = get_read_buffer(format, chans, len);

  caml_enter_blocking_section();
  ret = Pa_ReadStream(stream, buf, len);
  caml_leave_blocking_section();

  ocaml_portaudio_copy_buffer(buf, format, chans, ofs, len, _buf);

  free(buf);
  cerr(ret);

  CAMLreturn(Val_unit);
}

void *get_buffer_ba(int fmt, int chans, int ofs, value buf) {
  int dim;
  dim = Caml_ba_array_val(buf)->dim[0];
  if (fmt & paFloat32) {
    float *bufi = Caml_ba_data_val(buf);
    return bufi + chans * ofs;
  } else if (fmt & paInt32 || fmt & paInt24) {
    int32_t *bufi = Caml_ba_data_val(buf);
    return bufi + chans * ofs;
  } else if (fmt & paInt16) {
    short *bufi = Caml_ba_data_val(buf);
    return bufi + chans * ofs;
  } else if (fmt & paInt8) {
    char *bufi = Caml_ba_data_val(buf);
    return bufi + chans * ofs;
  } else
    return NULL;
}

#define GET_BUFFER_BA(type)                                                    \
  type *bufi = Caml_ba_data_val(buf);                                          \
  type **bufo = malloc(chans * sizeof(type *));                                \
  for (c = 0; c < chans; c++) {                                                \
    bufo[c] = bufi + c * dim + ofs;                                            \
  }                                                                            \
  return bufo;

void *get_buffer_ba_ni(int fmt, int chans, int ofs, value buf) {
  int num_dims;
  int dim;
  int c;
  num_dims = Caml_ba_array_val(buf)->num_dims;
  dim = Caml_ba_array_val(buf)->dim[(num_dims - 1)];
  if (fmt & paFloat32) {
    GET_BUFFER_BA(float)
  } else if (fmt & paInt32 || fmt & paInt24) {
    GET_BUFFER_BA(int32_t)
  } else if (fmt & paInt16) {
    GET_BUFFER_BA(short)
  } else if (fmt & paInt8) {
    GET_BUFFER_BA(char)
  } else
    return NULL;
}

CAMLprim value ocaml_pa_write_stream_ba(value _stream, value _buf, value _ofs,
                                        value _len) {
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_out;
  int format = Stream_t_val(_stream)->sample_format_out;

  if (format & paNonInterleaved)
    buf = get_buffer_ba_ni(format, chans, ofs, _buf);
  else
    buf = get_buffer_ba(format, chans, ofs, _buf);

  caml_enter_blocking_section();
  ret = Pa_WriteStream(stream, buf, len);
  if (format & paNonInterleaved)
    free(buf);
  caml_leave_blocking_section();
  cerr(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_stream_ba(value _stream, value _buf, value _ofs,
                                       value _len) {
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_in;
  int format = Stream_t_val(_stream)->sample_format_in;

  if (format & paNonInterleaved)
    buf = get_buffer_ba_ni(format, chans, ofs, _buf);
  else
    buf = get_buffer_ba(format, chans, ofs, _buf);

  caml_enter_blocking_section();
  ret = Pa_ReadStream(stream, buf, len);
  if (format & paNonInterleaved)
    free(buf);
  caml_leave_blocking_section();

  cerr(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_stream_available_frames(value _stream) {
  CAMLparam1(_stream);
  int frames_available = Pa_GetStreamReadAvailable(Stream_val(_stream));
  CAMLreturn(Val_int(frames_available));
}

CAMLprim value ocaml_pa_write_stream_available_frames(value _stream) {
  CAMLparam1(_stream);
  int frames_available = Pa_GetStreamWriteAvailable(Stream_val(_stream));
  CAMLreturn(Val_int(frames_available));
}
