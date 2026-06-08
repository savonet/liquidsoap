/*
 * Copyright (C) 2003-2008 Samuel Mimram
 *           (C) 2008-2010 The Savonet Team
 *
 * This file is part of Ocaml-faad.
 *
 * Ocaml-faad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-faad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-faad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/**
 * Libfaad bindings for OCaml.
 *
 * @author Samuel Mimram
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "mp4ff.h"
#include <neaacdec.h>

static void check_err(int n) {
  if (n < 0)
    caml_raise_constant(*caml_named_value("ocaml_faad_exn_failed"));
}

#define Dec_val(v) (*(NeAACDecHandle *)Data_custom_val(v))

static void finalize_faad_dec(value l) {
  NeAACDecHandle dh = Dec_val(l);
  NeAACDecClose(dh);
}

static struct custom_operations faad_dec_ops = {
    "ocaml_faad_dec",    finalize_faad_dec,        custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_faad_open(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  NeAACDecHandle dh = NeAACDecOpen();
  NeAACDecConfigurationPtr conf = NeAACDecGetCurrentConfiguration(dh);

  conf->outputFormat = FAAD_FMT_FLOAT;
  NeAACDecSetConfiguration(dh, conf);

  ret = caml_alloc_custom(&faad_dec_ops, sizeof(NeAACDecHandle), 0, 1);
  Dec_val(ret) = dh;

  CAMLreturn(ret);
}

CAMLprim value ocaml_faad_min_bytes_per_channel(value unit) {
  return Val_int(FAAD_MIN_STREAMSIZE);
}

CAMLprim value ocaml_faad_init(value dh, value _buf, value _ofs, value _len) {
  CAMLparam2(dh, _buf);
  CAMLlocal1(ans);

  unsigned long samplerate;
  uint8_t channels;
  int32_t offset;
  int32_t pre_offset = 0;
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  unsigned char *buf = (unsigned char *)String_val(_buf);
  int i;

  /* ADTS mpeg file can be a stream and start in the middle of a
   * frame so we need to have extra loop check here */
  for (i = ofs; i < len - 1; i++) {
    if (buf[i] == 0xff && (buf[i + 1] & 0xf6) == 0xf0) {
      pre_offset = i;
      break;
    }
  }

  offset = NeAACDecInit(Dec_val(dh), buf + ofs + pre_offset, len - pre_offset,
                        &samplerate, &channels);
  check_err(offset);

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(offset + pre_offset));
  Store_field(ans, 1, Val_int(samplerate));
  Store_field(ans, 2, Val_int(channels));
  CAMLreturn(ans);
}

CAMLprim value ocaml_faad_post_seek_reset(value _dh) {
  CAMLparam1(_dh);
  NeAACDecHandle dh = Dec_val(_dh);
  NeAACDecPostSeekReset(dh, 0);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_faad_decode(value _dh, value _inbuf, value _inbufofs,
                                 value _inbuflen) {
  CAMLparam2(_dh, _inbuf);
  CAMLlocal2(ans, outbuf);
  NeAACDecFrameInfo frameInfo;
  int inbufofs = Int_val(_inbufofs);
  int inbuflen = Int_val(_inbuflen);
  unsigned char *inbuf = malloc(inbuflen);
  float *data;
  int c, i;

  memcpy(inbuf, String_val(_inbuf) + inbufofs, inbuflen);

  NeAACDecHandle dh = Dec_val(_dh);

  caml_release_runtime_system();
  data = NeAACDecDecode(dh, &frameInfo, inbuf, inbuflen);
  caml_acquire_runtime_system();

  free(inbuf);

  if (frameInfo.error > 0)
    caml_raise_with_arg(*caml_named_value("ocaml_faad_exn_error"),
                        Val_int(frameInfo.error));
  if (!data)
    caml_raise_constant(*caml_named_value("ocaml_faad_exn_failed"));

  outbuf = caml_alloc_tuple(frameInfo.channels);
  for (c = 0; c < frameInfo.channels; c++)
    Store_field(
        outbuf, c,
        caml_alloc(frameInfo.samples / frameInfo.channels * Double_wosize,
                   Double_array_tag));
  for (i = 0; i < frameInfo.samples; i++)
    Store_double_field(Field(outbuf, i % frameInfo.channels),
                       i / frameInfo.channels, data[i]);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(frameInfo.bytesconsumed));
  Store_field(ans, 1, outbuf);

  CAMLreturn(ans);
}

CAMLprim value ocaml_faad_get_error_message(value err) {
  return caml_copy_string((char *)NeAACDecGetErrorMessage(Int_val(err)));
}

/***** MP4 *****/

typedef struct {
  mp4ff_t *ff;
  mp4ff_callback_t ff_cb;
  int fd;
  value read_cb;
  value write_cb;
  value seek_cb;
  value trunc_cb;
} mp4_t;

#define Mp4_val(v) (*((mp4_t **)Data_custom_val(v)))

static void finalize_mp4(value e) {
  mp4_t *mp = Mp4_val(e);

  if (mp->ff)
    mp4ff_close(mp->ff);
  if (mp->read_cb)
    caml_remove_generational_global_root(&mp->read_cb);
  if (mp->write_cb)
    caml_remove_generational_global_root(&mp->write_cb);
  if (mp->seek_cb)
    caml_remove_generational_global_root(&mp->seek_cb);
  if (mp->trunc_cb)
    caml_remove_generational_global_root(&mp->trunc_cb);

  free(mp);
}

static struct custom_operations mp4_ops = {"ocaml_mp4_t",
                                           finalize_mp4,
                                           custom_compare_default,
                                           custom_hash_default,
                                           custom_serialize_default,
                                           custom_deserialize_default};

static uint32_t read_cb(void *user_data, void *buffer, uint32_t length) {
  mp4_t *mp = (mp4_t *)user_data;
  value ans;
  int len;
  value tmp;

  if (mp->fd != -1)
    return read(mp->fd, buffer, length);
  else {
    caml_acquire_runtime_system();

    tmp = caml_alloc_string(length);
    caml_register_generational_global_root(&tmp);

    ans = caml_callback3_exn(mp->read_cb, tmp, Val_int(0), Val_int(length));

    if (Is_exception_result(ans)) {
      ans = Extract_exception(ans);
      caml_remove_generational_global_root(&tmp);
      caml_raise(ans);
    }

    len = Int_val(ans);

    if (len > 0)
      memcpy(buffer, String_val(tmp), len);

    caml_remove_generational_global_root(&tmp);

    caml_release_runtime_system();

    return len;
  }
}

static uint32_t write_cb(void *user_data, void *buffer, uint32_t length) {
  mp4_t *mp = (mp4_t *)user_data;

  if (mp->fd != -1)
    return write(mp->fd, buffer, length);
  else
    return 0;
}

static uint32_t seek_cb(void *user_data, uint64_t position) {
  mp4_t *mp = (mp4_t *)user_data;
  value ans;
  int pos;

  if (mp->fd != -1)
    return lseek(mp->fd, position, SEEK_SET) != -1 ? 0 : -1;
  else {
    caml_acquire_runtime_system();
    ans = caml_callback(mp->seek_cb, Val_int(position));
    pos = Int_val(ans);
    caml_release_runtime_system();

    return pos;
  }
}

static uint32_t trunc_cb(void *user_data) {
  // mp4_t *mp = (mp4_t*)user_data;

  return -1;
}

CAMLprim value ocaml_faad_mp4_open_read(value metaonly, value read, value write,
                                        value seek, value trunc) {
  CAMLparam4(read, write, seek, trunc);
  CAMLlocal1(ans);

  mp4_t *mp = malloc(sizeof(mp4_t));
  mp->fd = -1;
  mp->ff_cb.read = read_cb;
  mp->read_cb = read;
  caml_register_generational_global_root(&mp->read_cb);
  if (Is_block(write)) {
    mp->ff_cb.write = write_cb;
    mp->write_cb = Field(write, 0);
    caml_register_generational_global_root(&mp->write_cb);
  } else {
    mp->ff_cb.write = NULL;
    mp->write_cb = (value)NULL;
  }
  if (Is_block(seek)) {
    mp->ff_cb.seek = seek_cb;
    mp->seek_cb = Field(seek, 0);
    caml_register_generational_global_root(&mp->seek_cb);
  } else {
    mp->ff_cb.seek = NULL;
    mp->seek_cb = (value)NULL;
  }
  if (Is_block(trunc)) {
    mp->ff_cb.truncate = trunc_cb;
    mp->trunc_cb = Field(trunc, 0);
    caml_register_generational_global_root(&mp->trunc_cb);
  } else {
    mp->ff_cb.truncate = NULL;
    mp->trunc_cb = (value)NULL;
  }
  mp->ff_cb.user_data = mp;

  caml_release_runtime_system();
  if (Bool_val(metaonly))
    mp->ff = mp4ff_open_read_metaonly(&mp->ff_cb);
  else
    mp->ff = mp4ff_open_read(&mp->ff_cb);
  caml_acquire_runtime_system();
  assert(mp->ff);

  ans = caml_alloc_custom(&mp4_ops, sizeof(mp4_t *), 1, 0);
  Mp4_val(ans) = mp;

  CAMLreturn(ans);
}

#ifdef WIN32
#define GET_FD(fh) win_CRT_fd_of_filedescr(fh)
#else
#define GET_FD(fh) Int_val(fh)
#endif

CAMLprim value ocaml_faad_mp4_open_read_fd(value metaonly, value fd) {
  CAMLparam2(metaonly, fd);
  CAMLlocal1(ans);

  mp4_t *mp = malloc(sizeof(mp4_t));
  mp->fd = GET_FD(fd);
  mp->ff_cb.read = read_cb;
  mp->read_cb = (value)NULL;
  mp->ff_cb.write = write_cb;
  mp->write_cb = (value)NULL;
  mp->ff_cb.seek = seek_cb;
  mp->seek_cb = (value)NULL;
  mp->ff_cb.truncate = trunc_cb;
  mp->trunc_cb = (value)NULL;
  mp->ff_cb.user_data = mp;

  caml_release_runtime_system();
  if (Bool_val(metaonly))
    mp->ff = mp4ff_open_read_metaonly(&mp->ff_cb);
  else
    mp->ff = mp4ff_open_read(&mp->ff_cb);
  caml_acquire_runtime_system();
  assert(mp->ff);

  ans = caml_alloc_custom(&mp4_ops, sizeof(mp4_t *), 1, 0);
  Mp4_val(ans) = mp;

  CAMLreturn(ans);
}

CAMLprim value ocaml_faad_mp4_total_tracks(value m) {
  CAMLparam1(m);
  mp4_t *mp = Mp4_val(m);
  int n;

  caml_release_runtime_system();
  n = mp4ff_total_tracks(mp->ff);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(n));
}

CAMLprim value ocaml_faad_mp4_seek(value m, value track, value offset) {
  CAMLparam1(m);
  CAMLlocal1(ret);
  int32_t toskip = 0;
  mp4_t *mp = Mp4_val(m);
  int t = Int_val(track);

  caml_release_runtime_system();
  int sample =
      mp4ff_find_sample(mp->ff, t, (int64_t)(Int_val(offset)), &toskip);
  caml_acquire_runtime_system();

  ret = caml_alloc_tuple(2);
  Field(ret, 0) = Val_int(sample);
  Field(ret, 1) = Val_int(toskip);

  CAMLreturn(ret);
}

CAMLprim value ocaml_faad_mp4_find_aac_track(value m) {
  CAMLparam1(m);
  mp4_t *mp = Mp4_val(m);

  int i, rc;
  int num_tracks;

  caml_release_runtime_system();
  num_tracks = mp4ff_total_tracks(mp->ff);
  for (i = 0; i < num_tracks; i++) {
    unsigned char *buff = NULL;
    unsigned int buff_size = 0;
    mp4AudioSpecificConfig mp4ASC;

    mp4ff_get_decoder_config(mp->ff, i, &buff, &buff_size);

    if (buff) {
      rc = NeAACDecAudioSpecificConfig(buff, buff_size, &mp4ASC);
      free(buff);
      if (rc < 0)
        continue;
      caml_acquire_runtime_system();
      CAMLreturn(Val_int(i));
    }
  }

  caml_acquire_runtime_system();
  caml_raise_constant(*caml_named_value("ocaml_faad_exn_failed"));
}

CAMLprim value ocaml_faad_mp4_init(value m, value dh, value track) {
  CAMLparam3(m, dh, track);
  CAMLlocal1(ans);
  mp4_t *mp = Mp4_val(m);
  int t = Int_val(track);
  int ret;
  long unsigned int samplerate;
  unsigned char channels;
  NeAACDecHandle dec = Dec_val(dh);

  unsigned char *mp4_buffer = NULL;
  unsigned int mp4_buffer_size = 0;

  caml_release_runtime_system();
  mp4ff_get_decoder_config(mp->ff, t, &mp4_buffer, &mp4_buffer_size);
  ret = NeAACDecInit2(dec, mp4_buffer, mp4_buffer_size, &samplerate, &channels);
  caml_acquire_runtime_system();

  free(mp4_buffer);
  check_err(ret);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(samplerate));
  Store_field(ans, 1, Val_int(channels));

  CAMLreturn(ans);
}

CAMLprim value ocaml_faad_mp4_num_samples(value m, value track) {
  CAMLparam2(m, track);
  mp4_t *mp = Mp4_val(m);
  int t = Int_val(track);
  int ans;

  caml_release_runtime_system();
  ans = mp4ff_num_samples(mp->ff, t);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ans));
}

CAMLprim value ocaml_faad_mp4_read_sample(value m, value track, value sample) {
  CAMLparam3(m, track, sample);
  CAMLlocal1(ans);
  mp4_t *mp = Mp4_val(m);
  int t = Int_val(track);
  int s = Int_val(sample);
  int8_t *buf = NULL;
  unsigned int buflen = 0;
  int ret;

  caml_release_runtime_system();
  ret = mp4ff_read_sample(mp->ff, t, s, &buf, &buflen);
  caml_acquire_runtime_system();
  check_err(ret);

  ans = caml_alloc_string(buflen);
  memcpy((void *)String_val(ans), buf, buflen);
  free(buf);

  CAMLreturn(ans);
}

// Same as Faad.decode (Faad.Mp4.read_sample) but more efficient. Share code?
CAMLprim value ocaml_faad_mp4_decode(value m, value track, value sample,
                                     value dh) {
  CAMLparam4(m, track, sample, dh);
  CAMLlocal1(outbuf);
  mp4_t *mp = Mp4_val(m);
  int t = Int_val(track);
  int s = Int_val(sample);
  NeAACDecHandle dec = Dec_val(dh);
  NeAACDecFrameInfo frameInfo;
  int8_t *inbuf = NULL;
  unsigned int inbuflen = 0;
  float *data;
  int c, i, ret;

  caml_release_runtime_system();
  ret = mp4ff_read_sample(mp->ff, t, s, &inbuf, &inbuflen);
  caml_acquire_runtime_system();

  check_err(ret);

  caml_release_runtime_system();
  data = NeAACDecDecode(dec, &frameInfo, (uint8_t *)inbuf, inbuflen);
  caml_acquire_runtime_system();

  free(inbuf);

  if (!data)
    caml_raise_constant(*caml_named_value("ocaml_faad_exn_failed"));
  if (frameInfo.error != 0)
    caml_raise_with_arg(*caml_named_value("ocaml_faad_exn_error"),
                        Val_int(frameInfo.error));

  outbuf = caml_alloc_tuple(frameInfo.channels);
  for (c = 0; c < frameInfo.channels; c++)
    Store_field(
        outbuf, c,
        caml_alloc(frameInfo.samples / frameInfo.channels * Double_wosize,
                   Double_array_tag));
  for (i = 0; i < frameInfo.samples; i++)
    Store_double_field(Field(outbuf, i % frameInfo.channels),
                       i / frameInfo.channels, data[i]);

  CAMLreturn(outbuf);
}

CAMLprim value ocaml_faad_mp4_metadata(value m) {
  CAMLparam1(m);
  CAMLlocal2(ans, v);
  mp4_t *mp = Mp4_val(m);
  int i, n;
  char *tag, *item;

  caml_release_runtime_system();
  n = mp4ff_meta_get_num_items(mp->ff);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(n);
  for (i = 0; i < n; i++) {
    tag = NULL;
    item = NULL;

    caml_release_runtime_system();
    mp4ff_meta_get_by_index(mp->ff, i, &item, &tag);
    caml_acquire_runtime_system();

    assert(item && tag);
    v = caml_alloc_tuple(2);
    Store_field(v, 0, caml_copy_string(item));
    Store_field(v, 1, caml_copy_string(tag));
    Store_field(ans, i, v);
    free(item);
    free(tag);
  }

  CAMLreturn(ans);
}
