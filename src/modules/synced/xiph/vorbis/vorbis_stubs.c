/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-vorbis.
 *
 * ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * Libvorbis bindings for OCaml.
 *
 * @author Samuel Mimram
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <vorbis/codec.h>
#include <vorbis/vorbisenc.h>
#include <vorbis/vorbisfile.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <ocaml-ogg.h>

#ifndef Bytes_val
#define Bytes_val String_val
#endif

static inline float clip(float s) {
  if (s < -1) {
    return -1;
  } else if (s > 1) {
    return 1;
  } else
    return s;
}

static void raise_err(int ret) {
  switch (ret) {
  case OV_FALSE:
    caml_raise_constant(*caml_named_value("vorbis_exn_false"));

  case OV_HOLE:
    caml_raise_constant(*caml_named_value("vorbis_exn_hole_in_data"));

  case OV_EREAD:
    caml_raise_constant(*caml_named_value("vorbis_exn_read_error"));

  case OV_EFAULT:
    caml_raise_constant(*caml_named_value("vorbis_exn_internal_fault"));

  case OV_ENOTVORBIS:
    caml_raise_constant(*caml_named_value("vorbis_exn_not_vorbis"));

  case OV_EBADHEADER:
    caml_raise_constant(*caml_named_value("vorbis_exn_bad_header"));

  case OV_EVERSION:
    caml_raise_constant(*caml_named_value("vorbis_exn_version_mismatch"));

  case OV_EBADLINK:
    caml_raise_constant(*caml_named_value("vorbis_exn_bad_link"));

  case OV_EINVAL:
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid"));

  case OV_EIMPL:
    caml_raise_constant(*caml_named_value("vorbis_exn_not_implemented"));

  case OV_ENOTAUDIO:
    caml_raise_constant(*caml_named_value("vorbis_exn_not_audio"));

  default:
    caml_raise_with_arg(*caml_named_value("vorbis_exn_unknown_error"),
                        Val_int(ret));
  }
}

/**** Decoding *****/

typedef struct {
  vorbis_dsp_state vd;
  vorbis_block vb;
  vorbis_info vi;
  vorbis_comment vc;
} decoder_t;

#define Decoder_val(v) (*((decoder_t **)Data_custom_val(v)))
#define Decoder_dsp_state_val(v) (&Decoder_val(v)->vd)
#define Decoder_info_val(v) (&Decoder_val(v)->vi)
#define Comment_val(v) (&Decoder_val(v)->vc)
#define Block_val(v) (&Decoder_val(v)->vb)

static void finalize_decoder(value e) {
  decoder_t *dec = Decoder_val(e);

  vorbis_block_clear(&dec->vb);
  vorbis_dsp_clear(&dec->vd);
  vorbis_info_clear(&dec->vi);
  vorbis_comment_clear(&dec->vc);
  free(dec);
}

static struct custom_operations decoder_ops = {
    "ocaml_decoder_t",   finalize_decoder,         custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_vorbis_val_info_of_decoder(value vorbis_t) {
  CAMLparam1(vorbis_t);
  CAMLlocal1(v);
  int i = 0;
  v = caml_alloc_tuple(7);
  vorbis_info *vi = Decoder_info_val(vorbis_t);

  Store_field(v, i++, Val_int(vi->version));
  Store_field(v, i++, Val_int(vi->channels));
  Store_field(v, i++, Val_long(vi->rate));
  Store_field(v, i++, Val_long(vi->bitrate_upper));
  Store_field(v, i++, Val_long(vi->bitrate_nominal));
  Store_field(v, i++, Val_long(vi->bitrate_lower));
  Store_field(v, i++, Val_long(vi->bitrate_window));

  CAMLreturn(v);
}

CAMLprim value ocaml_vorbis_val_comments_of_decoder(value decoder) {
  CAMLparam1(decoder);
  CAMLlocal2(ans, cmts);

  int i;
  vorbis_comment *vc = Comment_val(decoder);

  cmts = caml_alloc_tuple(vc->comments);
  for (i = 0; i < vc->comments; i++)
    Store_field(cmts, i, caml_copy_string(vc->user_comments[i]));
  ans = caml_alloc_tuple(2);
  if (vc->vendor != NULL)
    Store_field(ans, 0, caml_copy_string(vc->vendor));
  else
    Store_field(ans, 0, caml_copy_string("(null)"));
  Store_field(ans, 1, cmts);

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_check_packet(value packet) {
  CAMLparam1(packet);
  ogg_packet *op = Packet_val(packet);
  vorbis_info vi;
  vorbis_comment vc;
  vorbis_info_init(&vi);
  vorbis_comment_init(&vc);
  int ret = 1;
  if (vorbis_synthesis_headerin(&vi, &vc, op) < 0)
    ret = 0;
  vorbis_info_clear(&vi);
  vorbis_comment_clear(&vc);
  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_synthesis_init(value packet, value packet2,
                                           value packet3) {
  CAMLparam3(packet, packet2, packet3);
  CAMLlocal1(ans);
  ogg_packet *op = Packet_val(packet);
  ogg_packet *op2 = Packet_val(packet2);
  ogg_packet *op3 = Packet_val(packet3);
  int ret;

  decoder_t *vt = malloc(sizeof(decoder_t));
  if (vt == NULL)
    caml_raise_out_of_memory();

  vorbis_info_init(&vt->vi);
  vorbis_comment_init(&vt->vc);

  ret = vorbis_synthesis_headerin(&vt->vi, &vt->vc, op);
  if (ret < 0) {
    vorbis_info_clear(&vt->vi);
    vorbis_comment_clear(&vt->vc);
    free(vt);
    raise_err(ret);
  }
  ret = vorbis_synthesis_headerin(&vt->vi, &vt->vc, op2);
  if (ret < 0) {
    vorbis_info_clear(&vt->vi);
    vorbis_comment_clear(&vt->vc);
    free(vt);
    raise_err(ret);
  }
  ret = vorbis_synthesis_headerin(&vt->vi, &vt->vc, op3);
  if (ret < 0) {
    vorbis_info_clear(&vt->vi);
    vorbis_comment_clear(&vt->vc);
    free(vt);
    raise_err(ret);
  }

  vorbis_synthesis_init(&vt->vd, &vt->vi);
  vorbis_block_init(&vt->vd, &vt->vb);

  ans = caml_alloc_custom(&decoder_ops, sizeof(decoder_t *), 1, 0);
  Decoder_val(ans) = vt;

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_decode_pcm(value vorbis_state, value stream_state,
                                       value buf, value _pos, value _len) {
  CAMLparam3(vorbis_state, stream_state, buf);
  CAMLlocal2(buffer, chan);
  ogg_stream_state *os = Stream_state_val(stream_state);
  ogg_packet op;
  vorbis_block *vb = Block_val(vorbis_state);
  vorbis_dsp_state *vd = Decoder_dsp_state_val(vorbis_state);
  vorbis_info *vi = Decoder_info_val(vorbis_state);
  int pos = Int_val(_pos);
  int len = Int_val(_len);
  float **pcm;
  int samples;
  int i, j, ret;
  int total_samples = 0;

  while (1) {
    while (total_samples < len) {
      caml_release_runtime_system();
      samples = vorbis_synthesis_pcmout(vd, &pcm);
      caml_acquire_runtime_system();

      if (samples < 0)
        raise_err(samples);

      if (samples == 0)
        break;

      if (samples > len - total_samples)
        samples = len - total_samples;

      if (Wosize_val(buf) != vi->channels)
        caml_raise_constant(*caml_named_value("vorbis_exn_invalid_channels"));

      for (i = 0; i < vi->channels; i++) {
        chan = Field(buf, i);
        if (Wosize_val(chan) / Double_wosize - pos < samples)
          caml_raise_constant(*caml_named_value("vorbis_exn_invalid"));

        for (j = 0; j < samples; j++)
          Store_double_field(chan, pos + j, clip(pcm[i][j]));
      }

      pos += samples;
      total_samples += samples;

      caml_release_runtime_system();
      ret = vorbis_synthesis_read(vd, samples);
      caml_acquire_runtime_system();

      if (ret < 0)
        raise_err(ret);
    }
    if (total_samples == len)
      CAMLreturn(Val_int(total_samples));

    caml_release_runtime_system();
    ret = ogg_stream_packetout(os, &op);
    caml_acquire_runtime_system();

    /* returned values are:
     * 1: ok
     * 0: not enough data. in this case
     *    we return the number of samples
     *    decoded if > 0 and raise
     *    Ogg_not_enough_data otherwise
     * -1: out of sync */
    if (ret == 0) {
      if (total_samples > 0)
        CAMLreturn(Val_int(total_samples));
      else
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
    }
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    caml_release_runtime_system();
    ret = vorbis_synthesis(vb, &op);
    caml_acquire_runtime_system();

    if (ret == 0) {
      caml_release_runtime_system();
      ret = vorbis_synthesis_blockin(vd, vb);
      caml_acquire_runtime_system();
    }

    if (ret < 0)
      raise_err(ret);
  }

  CAMLreturn(Val_int(total_samples));
}

CAMLprim value ocaml_vorbis_decode_pcm_ba(value vorbis_state,
                                          value stream_state, value buf,
                                          value _pos, value _len) {
  CAMLparam3(vorbis_state, stream_state, buf);
  CAMLlocal2(buffer, chan);
  ogg_stream_state *os = Stream_state_val(stream_state);
  ogg_packet op;
  vorbis_block *vb = Block_val(vorbis_state);
  vorbis_dsp_state *vd = Decoder_dsp_state_val(vorbis_state);
  vorbis_info *vi = Decoder_info_val(vorbis_state);
  int pos = Int_val(_pos);
  int len = Int_val(_len);
  float **pcm;
  int samples;
  int i, j, ret;
  int total_samples = 0;

  while (1) {
    while (total_samples < len) {
      caml_release_runtime_system();
      samples = vorbis_synthesis_pcmout(vd, &pcm);
      caml_acquire_runtime_system();

      if (samples < 0)
        raise_err(samples);

      if (samples == 0)
        break;

      if (samples > len - total_samples)
        samples = len - total_samples;

      if (Wosize_val(buf) != vi->channels)
        caml_raise_constant(*caml_named_value("vorbis_exn_invalid_channels"));

      for (i = 0; i < vi->channels; i++) {
        chan = Field(buf, i);
        if (Caml_ba_array_val(chan)->dim[0] - pos < samples)
          caml_raise_constant(*caml_named_value("vorbis_exn_invalid"));

        for (j = 0; j < samples; j++)
          ((float *)Caml_ba_data_val(chan))[pos + j] = clip(pcm[i][j]);
      }
      pos += samples;
      total_samples += samples;

      caml_release_runtime_system();
      ret = vorbis_synthesis_read(vd, samples);
      caml_acquire_runtime_system();

      if (ret < 0)
        raise_err(ret);
    }

    if (total_samples == len)
      CAMLreturn(Val_int(total_samples));

    caml_release_runtime_system();
    ret = ogg_stream_packetout(os, &op);
    caml_acquire_runtime_system();

    /* returned values are:
     * 1: ok
     * 0: not enough data. in this case
     *    we return the number of samples
     *    decoded if > 0 and raise
     *    Ogg_not_enough_data otherwise
     * -1: out of sync */
    if (ret == 0) {
      if (total_samples > 0)
        CAMLreturn(Val_int(total_samples));
      else
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
    }
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    caml_release_runtime_system();
    ret = vorbis_synthesis(vb, &op);
    caml_acquire_runtime_system();

    if (ret == 0) {
      caml_release_runtime_system();
      ret = vorbis_synthesis_blockin(vd, vb);
      caml_acquire_runtime_system();
    }

    if (ret < 0)
      raise_err(ret);
  }

  CAMLreturn(Val_int(total_samples));
}

CAMLprim value ocaml_vorbis_synthesis_restart(value s) {
  CAMLparam1(s);
  vorbis_synthesis_restart(Decoder_dsp_state_val(s));
  CAMLreturn(Val_unit);
}

/***** Encoding *****/

typedef struct {
  vorbis_dsp_state vd;
  vorbis_block vb;
  vorbis_info vi;
  ogg_packet op;
} encoder_t;

#define Encoder_val(v) (*((encoder_t **)Data_custom_val(v)))
#define Enc_dsp_state_val(v) (&Encoder_val(v)->vd)

static void finalize_encoder(value e) {
  encoder_t *enc = Encoder_val(e);

  vorbis_block_clear(&enc->vb);
  vorbis_dsp_clear(&enc->vd);
  vorbis_info_clear(&enc->vi);
  free(enc);
}

static struct custom_operations encoder_ops = {
    "ocaml_vorbis_encoder",   finalize_encoder,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_vorbis_analysis_init(value channels, value rate,
                                          value max_bitrate,
                                          value nominal_bitrate,
                                          value min_bitrate) {
  encoder_t *enc = malloc(sizeof(encoder_t));
  value ans;
  int err;

  vorbis_info_init(&enc->vi);
  err = vorbis_encode_init(&enc->vi, Int_val(channels), Int_val(rate),
                           Int_val(max_bitrate), Int_val(nominal_bitrate),
                           Int_val(min_bitrate));
  if (err) {
    vorbis_info_clear(&enc->vi);
    raise_err(err);
  }
  vorbis_analysis_init(&enc->vd, &enc->vi);
  vorbis_block_init(&enc->vd, &enc->vb);
  ans = caml_alloc_custom(&encoder_ops, sizeof(encoder_t *), 1, 0);
  Encoder_val(ans) = enc;

  return ans;
}

CAMLprim value ocaml_vorbis_analysis_init_vbr(value channels, value rate,
                                              value quality) {
  encoder_t *enc = malloc(sizeof(encoder_t));
  value ans;
  int err;

  vorbis_info_init(&enc->vi);
  err = vorbis_encode_init_vbr(&enc->vi, Int_val(channels), Int_val(rate),
                               Double_val(quality));
  if (err) {
    vorbis_info_clear(&enc->vi);
    raise_err(err);
  }
  vorbis_analysis_init(&enc->vd, &enc->vi);
  vorbis_block_init(&enc->vd, &enc->vb);
  ans = caml_alloc_custom(&encoder_ops, sizeof(encoder_t *), 1, 0);
  Encoder_val(ans) = enc;

  return ans;
}

CAMLprim value ocaml_vorbis_reset(value vdsp) {
  encoder_t *enc = Encoder_val(vdsp);

  vorbis_block_clear(&enc->vb);
  vorbis_dsp_clear(&enc->vd);
  vorbis_info_clear(&enc->vi);

  vorbis_analysis_init(&enc->vd, &enc->vi);
  vorbis_block_init(&enc->vd, &enc->vb);

  return Val_unit;
}

CAMLprim value ocaml_vorbis_analysis_headerout(value vdsp, value comments) {
  CAMLparam2(vdsp, comments);
  CAMLlocal4(ret, p1, p2, p3);
  vorbis_dsp_state *vd = Enc_dsp_state_val(vdsp);

  vorbis_comment vc;
  ogg_packet header, header_comm, header_code;
  int i;

  vorbis_comment_init(&vc);
  for (i = 0; i < Wosize_val(comments); i++)
    vorbis_comment_add_tag(&vc, String_val(Field(Field(comments, i), 0)),
                           String_val(Field(Field(comments, i), 1)));
  vorbis_analysis_headerout(vd, &vc, &header, &header_comm, &header_code);
  vorbis_comment_clear(&vc);

  ret = caml_alloc_tuple(3);
  Store_field(ret, 0, value_of_packet(&header));
  Store_field(ret, 1, value_of_packet(&header_comm));
  Store_field(ret, 2, value_of_packet(&header_code));

  CAMLreturn(ret);
}

CAMLprim value ocaml_vorbis_encode_get_channels(value vdsp) {
  CAMLparam1(vdsp);
  encoder_t *enc = Encoder_val(vdsp);
  CAMLreturn(Val_int(enc->vi.channels));
}

CAMLprim value ocaml_vorbis_encode_float(value vdsp, value vogg, value data,
                                         value _offs, value _len) {
  CAMLparam3(vdsp, vogg, data);
  encoder_t *enc = Encoder_val(vdsp);
  vorbis_block *vb = &enc->vb;
  vorbis_dsp_state *vd = Enc_dsp_state_val(vdsp);
  ogg_stream_state *os = Stream_state_val(vogg);
  ogg_packet *op = &enc->op;
  int offs = Int_val(_offs);
  int len = Int_val(_len);
  float **vorbis_buffer;
  int c, i;
  value datac;
  int channels = enc->vi.channels;

  if (Wosize_val(data) != channels)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_channels"));

  vorbis_buffer = vorbis_analysis_buffer(vd, len);
  for (c = 0; c < channels; c++) {
    datac = Field(data, c);
    for (i = 0; i < len; i++)
      vorbis_buffer[c][i] = Double_field(datac, i + offs);
  }

  caml_release_runtime_system();
  vorbis_analysis_wrote(vd, len);

  /* TODO: split the encoding part? */

  while (vorbis_analysis_blockout(vd, vb) == 1) {
    /* Analysis, assume we want to use bitrate management. */
    vorbis_analysis(vb, NULL);
    vorbis_bitrate_addblock(vb);

    /* Weld packets into the bitstream. */
    while (vorbis_bitrate_flushpacket(vd, op))
      ogg_stream_packetin(os, op);
  }
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_vorbis_encode_float_ba(value vdsp, value vogg, value data,
                                            value _ofs, value _len) {
  CAMLparam3(vdsp, vogg, data);
  encoder_t *enc = Encoder_val(vdsp);
  vorbis_block *vb = &enc->vb;
  vorbis_dsp_state *vd = Enc_dsp_state_val(vdsp);
  ogg_stream_state *os = Stream_state_val(vogg);
  ogg_packet *op = &enc->op;
  float **vorbis_buffer;
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int c, i;
  value datac;
  int channels = enc->vi.channels;

  if (Wosize_val(data) != channels)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_channels"));

  if (channels == 0)
    CAMLreturn(Val_unit);

  if (Caml_ba_array_val(Field(data, 0))->dim[0] < ofs + len)
    caml_failwith("Invalid length or offset");

  vorbis_buffer = vorbis_analysis_buffer(vd, len);
  for (c = 0; c < channels; c++) {
    for (i = 0; i < len; i++) {
      vorbis_buffer[c][i] =
          ((float *)Caml_ba_data_val(Field(data, c)))[i + ofs];
    }
  }

  caml_release_runtime_system();
  vorbis_analysis_wrote(vd, len);

  /* TODO: split the encoding part? */

  while (vorbis_analysis_blockout(vd, vb) == 1) {
    /* Analysis, assume we want to use bitrate management. */
    vorbis_analysis(vb, NULL);
    vorbis_bitrate_addblock(vb);

    /* Weld packets into the bitstream. */
    while (vorbis_bitrate_flushpacket(vd, op))
      ogg_stream_packetin(os, op);
  }
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_vorbis_encode_time_of_granulepos(value v_state,
                                                      value gpos) {
  CAMLparam2(v_state, gpos);
  encoder_t *enc = Encoder_val(v_state);
  ogg_int64_t granulepos = Int64_val(gpos);
  CAMLreturn(caml_copy_nativeint(vorbis_granule_time(&enc->vd, granulepos)));
}

/***** File Decoding *****/

/* This should be malloced since we might want to register *_func as global
 * root. */
typedef struct {
  OggVorbis_File *ovf;
  int bitstream;
  value read_func;
  value seek_func;
  value tell_func;
} myvorbis_dec_file_t;

#define Decfile_val(v) (*((myvorbis_dec_file_t **)Data_custom_val(v)))

static void finalize_dec_file(value _df) {
  myvorbis_dec_file_t *df = Decfile_val(_df);

  ov_clear(df->ovf);
  free(df->ovf);
  df->ovf = NULL;
  caml_remove_global_root(&df->read_func);
  caml_remove_global_root(&df->seek_func);
  caml_remove_global_root(&df->tell_func);

  free(df);
}

static struct custom_operations decfile_ops = {
    "ocaml_vorbis_decfile",   finalize_dec_file,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

static size_t read_func_cb(void *ptr, size_t size, size_t nmemb,
                           void *datasource) {
  myvorbis_dec_file_t *df = datasource;
  value ret;
  int len;

  caml_acquire_runtime_system();
  ret = caml_callback(df->read_func, Val_int(size * nmemb));
  len = Int_val(Field(ret, 1));
  memcpy(ptr, String_val(Field(ret, 0)), len);
  caml_release_runtime_system();

  return len;
}

static int seek_func_cb(void *datasource, ogg_int64_t offset, int whence) {
  myvorbis_dec_file_t *df = datasource;
  int cmd;
  int ret;

  switch (whence) {
  case SEEK_SET:
    cmd = 0;
    break;

  case SEEK_CUR:
    cmd = 1;
    break;

  case SEEK_END:
    cmd = 2;
    break;

  default:
    assert(0);
  }
  caml_acquire_runtime_system();
  ret = Int_val(caml_callback2(df->seek_func, Val_int(offset), Val_int(cmd)));
  caml_release_runtime_system();

  return ret;
}

static long tell_func_cb(void *datasource) {
  myvorbis_dec_file_t *df = datasource;
  int ret;

  caml_acquire_runtime_system();
  ret = Int_val(caml_callback(df->tell_func, Val_unit));
  caml_release_runtime_system();

  return ret;
}

static ov_callbacks callbacks = {.read_func = read_func_cb,
                                 .seek_func = seek_func_cb,
                                 .close_func = NULL,
                                 .tell_func = tell_func_cb};

CAMLprim value ocaml_vorbis_open_dec_stream(value read_func, value seek_func,
                                            value tell_func, value params) {
  CAMLparam4(read_func, seek_func, tell_func, params);
  CAMLlocal1(block);
  int ret = 0;
  myvorbis_dec_file_t *df;

  df = malloc(sizeof(myvorbis_dec_file_t));

  df->ovf = (OggVorbis_File *)malloc(sizeof(OggVorbis_File));
  df->bitstream = 0;
  caml_register_global_root(&df->read_func);
  df->read_func = read_func;
  caml_register_global_root(&df->seek_func);
  df->seek_func = seek_func;
  caml_register_global_root(&df->tell_func);
  df->tell_func = tell_func;

  caml_release_runtime_system();
  ret = ov_open_callbacks(df, df->ovf, NULL, 0, callbacks);
  caml_acquire_runtime_system();

  if (ret < 0) {
    caml_remove_global_root(&df->tell_func);
    caml_remove_global_root(&df->seek_func);
    caml_remove_global_root(&df->read_func);
    free(df->ovf);
    free(df);
    raise_err(ret);
  }

  block = caml_alloc_custom(&decfile_ops, sizeof(myvorbis_dec_file_t *), 0, 1);
  Decfile_val(block) = df;

  CAMLreturn(block);
}

CAMLprim value ocaml_vorbis_decode(value d_f, value be_, value ss_,
                                   value signed_, value buf_, value ofs_,
                                   value len_) {
  CAMLparam2(d_f, buf_);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int ret = 0;
  int ofs = Int_val(ofs_);
  int len = Int_val(len_);
  int big_endian = Bool_val(be_);
  int sample_size = Int_val(ss_);
  int sign = Bool_val(signed_);
  char *buf;

  if (!df->ovf)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  if (ofs + len > caml_string_length(buf_))
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  /* TODO: this buffer could be allocated once when creating the decoder
   * and reused for every decoding pass. This might be useful to reduce
   * load or memory fragmentation if needed.
   */
  buf = malloc(len);

  /* We have to make sure that when a callback is called, the ocaml master lock
   * has been released.  Callbacks are responsible for taking it back if they
   * need to call ocaml code.
   */
  caml_release_runtime_system();
  ret =
      ov_read(df->ovf, buf, len, big_endian, sample_size, sign, &df->bitstream);
  caml_acquire_runtime_system();

  if (ret <= 0) {
    free(buf);
    ret ? raise_err(ret) : caml_raise_end_of_file();
  }
  memcpy(Bytes_val(buf_) + ofs, buf, ret);
  free(buf);

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decode_byte(value *argv, int argn) {
  return ocaml_vorbis_decode(argv[0], argv[1], argv[2], argv[3], argv[4],
                             argv[5], argv[6]);
}

CAMLprim value ocaml_vorbis_decode_float(value d_f, value dst, value ofs_,
                                         value len_) {
  CAMLparam2(d_f, dst);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int ret = 0;
  int ofs = Int_val(ofs_);
  int len = Int_val(len_);
  float **buf;
  int chans, c, i;

  if (!df->ovf)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  chans = df->ovf->vi->channels;

  if (chans > Wosize_val(dst))
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  if (Wosize_val(dst) < 1 ||
      Wosize_val(Field(dst, 0)) / Double_wosize - ofs < len)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));

  /* We have to make sure that when a callback is called, the ocaml master lock
   * has been released.  Callbacks are responsible for taking it back if they
   * need to call ocaml code.
   */
  caml_release_runtime_system();
  ret = ov_read_float(df->ovf, &buf, len, &df->bitstream);
  caml_acquire_runtime_system();

  if (ret <= 0)
    ret ? raise_err(ret) : caml_raise_end_of_file();

  for (c = 0; c < chans; c++)
    for (i = 0; i < ret; i++)
      Store_double_field(Field(dst, c), i + ofs, clip(buf[c][i]));

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decode_float_alloc(value d_f, value len_) {
  CAMLparam1(d_f);
  CAMLlocal2(ans, ansc);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int ret = 0;
  int len = Int_val(len_);
  float **buf;
  int chans, c, i;

  if (!df->ovf)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  chans = df->ovf->vi->channels;

  /* We have to make sure that when a callback is called, the ocaml master lock
   * has been released.  Callbacks are responsible for taking it back if they
   * need to call ocaml code.
   */
  caml_release_runtime_system();
  ret = ov_read_float(df->ovf, &buf, len, &df->bitstream);
  caml_acquire_runtime_system();

  if (ret <= 0)
    ret ? raise_err(ret) : caml_raise_end_of_file();

  ans = caml_alloc_tuple(chans);
  for (c = 0; c < chans; c++) {
    ansc = caml_alloc(ret * Double_wosize, Double_array_tag);
    Store_field(ans, c, ansc);
    for (i = 0; i < ret; i++)
      Store_double_field(ansc, i, clip(buf[c][i]));
  }

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_decode_float_ba(value d_f, value dst, value ofs_,
                                            value len_) {
  CAMLparam2(d_f, dst);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int ret = 0;
  int ofs = Int_val(ofs_);
  int len = Int_val(len_);
  float **buf;
  int chans, c, i;

  if (!df->ovf)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  chans = df->ovf->vi->channels;

  if (chans > Wosize_val(dst))
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  if (Wosize_val(dst) < 1 ||
      Caml_ba_array_val(Field(dst, 0))->dim[0] - ofs < len)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));

  /* We have to make sure that when a callback is called, the ocaml master lock
   * has been released.  Callbacks are responsible for taking it back if they
   * need to call ocaml code.
   */
  caml_release_runtime_system();
  ret = ov_read_float(df->ovf, &buf, len, &df->bitstream);
  caml_acquire_runtime_system();

  if (ret <= 0)
    ret ? raise_err(ret) : caml_raise_end_of_file();

  for (c = 0; c < chans; c++)
    for (i = 0; i < ret; i++)
      ((float *)Caml_ba_data_val(Field(dst, c)))[i + ofs] = buf[c][i];

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decode_float_alloc_ba(value d_f, value len_) {
  CAMLparam1(d_f);
  CAMLlocal2(ans, ansc);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int ret = 0;
  int len = Int_val(len_);
  float **buf;
  int chans, c, i;

  if (!df->ovf)
    caml_raise_constant(*caml_named_value("vorbis_exn_invalid_parameters"));
  chans = df->ovf->vi->channels;

  /* We have to make sure that when a callback is called, the ocaml master lock
   * has been released.  Callbacks are responsible for taking it back if they
   * need to call ocaml code.
   */
  caml_release_runtime_system();
  ret = ov_read_float(df->ovf, &buf, len, &df->bitstream);
  caml_acquire_runtime_system();

  if (ret <= 0)
    ret ? raise_err(ret) : caml_raise_end_of_file();

  ans = caml_alloc_tuple(chans);
  for (c = 0; c < chans; c++) {
    ansc = caml_ba_alloc_dims(CAML_BA_FLOAT32 | CAML_BA_C_LAYOUT, 1, NULL, ret);
    Store_field(ans, c, ansc);
    for (i = 0; i < ret; i++)
      ((float *)Caml_ba_data_val(ansc))[i] = clip(buf[c][i]);
  }

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_get_dec_file_bitstream(value d_f) {
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  return Val_int(df->bitstream);
}

CAMLprim value ocaml_vorbis_decoder_info(value d_f, value bs) {
  CAMLparam1(d_f);
  CAMLlocal1(ans);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int bitstream = Int_val(bs);
  vorbis_info *vi;

  caml_release_runtime_system();
  vi = ov_info(df->ovf, bitstream);
  caml_acquire_runtime_system();

  assert(vi);
  ans = caml_alloc_tuple(7);
  Store_field(ans, 0, Val_int(vi->version));
  Store_field(ans, 1, Val_int(vi->channels));
  Store_field(ans, 2, Val_int(vi->rate));
  Store_field(ans, 3, Val_int(vi->bitrate_upper));
  Store_field(ans, 4, Val_int(vi->bitrate_nominal));
  Store_field(ans, 5, Val_int(vi->bitrate_lower));
  Store_field(ans, 6, Val_int(vi->bitrate_window));

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_get_dec_file_comments(value d_f, value link_) {
  CAMLparam2(d_f, link_);
  CAMLlocal2(ans, cmts);

  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int link = Int_val(link_);
  int i;
  vorbis_comment *vc;

  caml_release_runtime_system();
  vc = ov_comment(df->ovf, link);
  caml_acquire_runtime_system();

  if (!vc)
    /* TODO: better error */
    caml_raise_with_arg(*caml_named_value("vorbis_exn_unknown_error"),
                        Val_int(666));

  cmts = caml_alloc_tuple(vc->comments);
  for (i = 0; i < vc->comments; i++)
    Store_field(cmts, i, caml_copy_string(vc->user_comments[i]));
  ans = caml_alloc_tuple(2);
  if (vc->vendor != NULL)
    Store_field(ans, 0, caml_copy_string(vc->vendor));
  else
    Store_field(ans, 0, caml_copy_string("(null)"));
  Store_field(ans, 1, cmts);

  CAMLreturn(ans);
}

CAMLprim value ocaml_vorbis_decoder_bitrate(value d_f, value bs) {
  CAMLparam1(d_f);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int bitstream = Int_val(bs);
  long ret;

  caml_release_runtime_system();
  ret = ov_bitrate(df->ovf, bitstream);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decoder_time_total(value d_f, value bs) {
  CAMLparam1(d_f);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int bitstream = Int_val(bs);
  double ret;

  caml_release_runtime_system();
  ret = ov_time_total(df->ovf, bitstream);
  caml_acquire_runtime_system();

  CAMLreturn(caml_copy_double(ret));
}

CAMLprim value ocaml_vorbis_decoder_pcm_total(value d_f, value bs) {
  CAMLparam1(d_f);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int bitstream = Int_val(bs);
  ogg_int64_t ret;

  caml_release_runtime_system();
  ret = ov_pcm_total(df->ovf, bitstream);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decoder_streams(value d_f) {
  CAMLparam1(d_f);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  long ret;

  caml_release_runtime_system();
  ret = ov_streams(df->ovf);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_vorbis_decoder_serialnumber(value d_f, value bs) {
  CAMLparam1(d_f);
  myvorbis_dec_file_t *df = Decfile_val(d_f);
  int bitstream = Int_val(bs);
  long ret;

  caml_release_runtime_system();
  ret = ov_serialnumber(df->ovf, bitstream);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* Ogg skeleton interface */

/* Wrappers */
static void write32le(unsigned char *ptr, ogg_uint32_t v) {
  ptr[0] = v & 0xff;
  ptr[1] = (v >> 8) & 0xff;
  ptr[2] = (v >> 16) & 0xff;
  ptr[3] = (v >> 24) & 0xff;
}

static void write64le(unsigned char *ptr, ogg_int64_t v) {
  ogg_uint32_t hi = v >> 32;
  ptr[0] = v & 0xff;
  ptr[1] = (v >> 8) & 0xff;
  ptr[2] = (v >> 16) & 0xff;
  ptr[3] = (v >> 24) & 0xff;
  ptr[4] = hi & 0xff;
  ptr[5] = (hi >> 8) & 0xff;
  ptr[6] = (hi >> 16) & 0xff;
  ptr[7] = (hi >> 24) & 0xff;
}

/* Values from http://xiph.org/ogg/doc/skeleton.html */
#define FISBONE_IDENTIFIER "fisbone\0"
#define FISBONE_MESSAGE_HEADER_OFFSET 44
#define FISBONE_SIZE 52

/* Code from theorautils.c in ffmpeg2theora */
CAMLprim value ocaml_vorbis_skeleton_fisbone(value serial, value samplerate,
                                             value start, value content) {
  CAMLparam4(serial, samplerate, start, content);
  CAMLlocal1(packet);
  ogg_packet op;
  int len = FISBONE_SIZE + caml_string_length(content);

  memset(&op, 0, sizeof(op));
  op.packet = malloc(len);
  if (op.packet == NULL)
    caml_raise_out_of_memory();

  memset(op.packet, 0, len);
  /* it will be the fisbone packet for the vorbis audio */
  memcpy(op.packet, FISBONE_IDENTIFIER, 8); /* identifier */
  write32le(
      op.packet + 8,
      FISBONE_MESSAGE_HEADER_OFFSET); /* offset of the message header fields */
  write32le(op.packet + 12,
            Nativeint_val(serial)); /* serialno of the vorbis stream */
  write32le(op.packet + 16, 3);     /* number of header packet */
  /* granulerate, temporal resolution of the bitstream in Hz */
  write64le(op.packet + 20,
            (ogg_int64_t)Int64_val(samplerate)); /* granulerate numerator */
  write64le(op.packet + 28, (ogg_int64_t)1);     /* granulerate denominator */
  write64le(op.packet + 36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet + 44, 2); /* preroll, for vorbis its 2 */
  *(op.packet + 48) = 0;        /* granule shift, always 0 for vorbis */
  memcpy(op.packet + FISBONE_SIZE, String_val(content),
         caml_string_length(content));

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}
