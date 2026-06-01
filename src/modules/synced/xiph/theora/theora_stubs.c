/*
 * Copyright 2007-2009 Samuel Mimram and Romain Beauxis
 *
 * This file is part of ocaml-theora.
 *
 * ocaml-theora is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-theora is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-theora; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <theora/theoradec.h>
#include <theora/theoraenc.h>

#include <ocaml-ogg.h>

#ifndef Bytes_val
#define Byles_val String_val
#endif

/***** Error handling ******/

static void check_err(int n) {
  switch (n) {
  case 0:
    return;

  case TH_EFAULT:
    caml_raise_constant(*caml_named_value("theora_exn_fault"));

  case TH_EINVAL:
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  case TH_EVERSION:
    caml_raise_constant(*caml_named_value("theora_exn_version"));
  case TH_EBADPACKET:
    caml_raise_constant(*caml_named_value("theora_exn_bad_packet"));
  case TH_ENOTFORMAT:
    caml_raise_constant(*caml_named_value("theora_exn_notformat"));
  case TH_EBADHEADER:
    caml_raise_constant(*caml_named_value("theora_exn_bad_header"));
  case TH_EIMPL:
    caml_raise_constant(*caml_named_value("theora_exn_not_implemented"));

  case TH_DUPFRAME:
    caml_raise_constant(*caml_named_value("theora_exn_dup_frame"));

  default:
    caml_raise_with_arg(*caml_named_value("theora_exn_unknown"), Val_int(n));
  }
}

/***** Version *****/

CAMLprim value ocaml_theora_version_string(value unit) {
  return caml_copy_string(th_version_string());
}

CAMLprim value ocaml_theora_version_number(value unit) {
  return Val_int(th_version_number());
}

/***** Helper functions *****/

static th_colorspace cs_of_val(value v) {
  switch (Int_val(v)) {
  case 0:
    return TH_CS_UNSPECIFIED;

  case 1:
    return TH_CS_ITU_REC_470M;

  case 2:
    return TH_CS_ITU_REC_470BG;

  case 3:
    return TH_CS_NSPACES;

  default:
    assert(0);
  }
}

static value val_of_cs(th_colorspace c) {
  switch (c) {
  case TH_CS_UNSPECIFIED:
    return Int_val(0);

  case TH_CS_ITU_REC_470M:
    return Int_val(1);

  case TH_CS_ITU_REC_470BG:
    return Int_val(2);

  case TH_CS_NSPACES:
    return Int_val(3);

  default:
    assert(0);
  }
}

static th_pixel_fmt pf_of_val(value v) {
  switch (Int_val(v)) {
  case 0:
    return TH_PF_420;

  case 1:
    return TH_PF_RSVD;

  case 2:
    return TH_PF_422;

  case 3:
    return TH_PF_444;

  default:
    assert(0);
  }
}

static value val_of_pf(th_pixel_fmt p) {
  switch (p) {
  case TH_PF_420:
    return Int_val(0);

  case TH_PF_RSVD:
    return Int_val(1);

  case TH_PF_422:
    return Int_val(2);

  case TH_PF_444:
    return Int_val(3);

  default:
    assert(0);
  }
}

/* ti is *not* allocated: codec_setup may be allocated by
 * theora_info_init and its memory lost. You better check what you need */
static th_info *info_of_val(value v, th_info *ti) {
  int i = 0;

  ti->frame_width = Int_val(Field(v, i++));
  ti->frame_height = Int_val(Field(v, i++));
  ti->pic_width = Int_val(Field(v, i++));
  ti->pic_height = Int_val(Field(v, i++));
  ti->pic_x = Int_val(Field(v, i++));
  ti->pic_y = Int_val(Field(v, i++));
  ti->colorspace = cs_of_val(Field(v, i++));
  ti->pixel_fmt = pf_of_val(Field(v, i++));
  ti->target_bitrate = Int_val(Field(v, i++));
  ti->quality = Int_val(Field(v, i++));
  ti->keyframe_granule_shift = Int_val(Field(v, i++));
  ti->version_major = Int_val(Field(v, i++));
  ti->version_minor = Int_val(Field(v, i++));
  ti->version_subminor = Int_val(Field(v, i++));
  ti->fps_numerator = Int_val(Field(v, i++));
  ti->fps_denominator = Int_val(Field(v, i++));
  ti->aspect_numerator = Int_val(Field(v, i++));
  ti->aspect_denominator = Int_val(Field(v, i++));

  return ti;
}

static value val_of_info(th_info *ti) {
  CAMLparam0();
  CAMLlocal1(v);
  int i = 0;
  v = caml_alloc_tuple(18);
  Store_field(v, i++, Val_int(ti->frame_width));
  Store_field(v, i++, Val_int(ti->frame_height));
  Store_field(v, i++, Val_int(ti->pic_width));
  Store_field(v, i++, Val_int(ti->pic_height));
  Store_field(v, i++, Val_int(ti->pic_x));
  Store_field(v, i++, Val_int(ti->pic_y));
  Store_field(v, i++, val_of_cs(ti->colorspace));
  Store_field(v, i++, val_of_pf(ti->pixel_fmt));
  Store_field(v, i++, Val_int(ti->target_bitrate));
  Store_field(v, i++, Val_int(ti->quality));
  Store_field(v, i++, Val_int(ti->keyframe_granule_shift));
  Store_field(v, i++, Val_int(ti->version_major));
  Store_field(v, i++, Val_int(ti->version_minor));
  Store_field(v, i++, Val_int(ti->version_subminor));
  Store_field(v, i++, Val_int(ti->fps_numerator));
  Store_field(v, i++, Val_int(ti->fps_denominator));
  Store_field(v, i++, Val_int(ti->aspect_numerator));
  Store_field(v, i++, Val_int(ti->aspect_denominator));

  CAMLreturn(v);
}

static void yuv_of_val(value v, th_ycbcr_buffer buffer) {
  int i = 0;
  struct caml_ba_array *ba;

  /* Y plane */
  buffer[0].width = Int_val(Field(v, i++));
  buffer[0].height = Int_val(Field(v, i++));
  buffer[0].stride = Int_val(Field(v, i++));
  ba = Caml_ba_array_val(Field(v, i++));
  if (ba->dim[0] < buffer[0].stride * buffer[0].height)
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  buffer[0].data = (unsigned char *)ba->data;

  /* Cb plane */
  buffer[1].width = Int_val(Field(v, i++));
  buffer[1].height = Int_val(Field(v, i++));
  buffer[1].stride = Int_val(Field(v, i++));
  ba = Caml_ba_array_val(Field(v, i++));
  if (ba->dim[0] < buffer[1].stride * buffer[1].height)
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  buffer[1].data = (unsigned char *)ba->data;

  /* Cr plane */
  buffer[2].width = Int_val(Field(v, i++));
  buffer[2].height = Int_val(Field(v, i++));
  buffer[2].stride = Int_val(Field(v, i++));
  ba = Caml_ba_array_val(Field(v, i++));
  if (ba->dim[0] < buffer[2].stride * buffer[2].height)
    caml_raise_constant(*caml_named_value("theora_exn_inval"));
  buffer[2].data = (unsigned char *)ba->data;

  return;
}

/* The result must be freed afterwards! */
/* This should not be called in a blocking section. */
static value val_of_yuv(th_ycbcr_buffer buffer) {
  CAMLparam0();
  CAMLlocal4(ret, y, u, v);
  int i = 0;
  intnat len;
  ret = caml_alloc_tuple(12);
  unsigned char *data;

  /* Y plane */
  Store_field(ret, i++, Val_int(buffer[0].width));
  Store_field(ret, i++, Val_int(buffer[0].height));
  Store_field(ret, i++, Val_int(buffer[0].stride));
  len = buffer[0].stride * buffer[0].height;
  y = caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, &len);
  data = Caml_ba_data_val(y);
  memcpy(data, buffer[0].data, len);
  Store_field(ret, i++, y);

  /* Cb plane */
  Store_field(ret, i++, Val_int(buffer[1].width));
  Store_field(ret, i++, Val_int(buffer[1].height));
  Store_field(ret, i++, Val_int(buffer[1].stride));
  len = buffer[1].stride * buffer[1].height;
  u = caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, &len);
  data = Caml_ba_data_val(u);
  memcpy(data, buffer[1].data, len);
  Store_field(ret, i++, u);

  /* Cr plane */
  Store_field(ret, i++, Val_int(buffer[2].width));
  Store_field(ret, i++, Val_int(buffer[2].height));
  Store_field(ret, i++, Val_int(buffer[2].stride));
  len = buffer[2].stride * buffer[2].height;
  v = caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, &len);
  data = Caml_ba_data_val(v);
  memcpy(data, buffer[2].data, len);
  Store_field(ret, i++, v);

  CAMLreturn(ret);
}

CAMLprim value ocaml_theora_default_granuleshift(value unit) {
  CAMLparam0();
  th_info info;
  th_info_init(&info);
  int ret = info.keyframe_granule_shift;
  th_info_clear(&info);
  CAMLreturn(Int_val(ret));
}

CAMLprim value ocaml_theora_ogg_packet_iskeyframe(value _op) {
  CAMLparam1(_op);
  ogg_packet *op = Packet_val(_op);

  CAMLreturn(Val_int(th_packet_iskeyframe(op)));
}

/** Encoding API **/

typedef struct enc_state_t {
  th_enc_ctx *ts;
  th_info ti;
  th_comment tc;
  ogg_int64_t granulepos;
  ogg_int64_t packetno;
} enc_state_t;

#define Theora_enc_state_val(v) (*((enc_state_t **)Data_custom_val(v)))

static void finalize_enc_state(value s) {
  enc_state_t *state = Theora_enc_state_val(s);
  th_encode_free(state->ts);
  th_info_clear(&state->ti);
  th_comment_clear(&state->tc);
  free(state);
}

static struct custom_operations enc_state_ops = {
    "ocaml_enc_theora_state", finalize_enc_state,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

/* Thanks you
 * http://www.linux-nantes.org/~fmonnier/ocaml/ocaml-wrapping-c.php#ref_option
 */
#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)

CAMLprim value ocaml_theora_encode_init(value info, value params,
                                        value comments) {
  CAMLparam3(info, params, comments);
  CAMLlocal2(ans, c);
  enc_state_t *state = malloc(sizeof(enc_state_t));
  th_info_init(&state->ti);
  info_of_val(info, &state->ti);
  th_comment_init(&state->tc);
  int i;
  for (i = 0; i < Wosize_val(comments); i++)
    th_comment_add_tag(&state->tc,
                       (char *)Bytes_val(Field(Field(comments, i), 0)),
                       (char *)Bytes_val(Field(Field(comments, i), 1)));
  state->ts = th_encode_alloc(&state->ti);
  if (state->ts == NULL) {
    th_info_clear(&state->ti);
    th_comment_clear(&state->tc);
    free(state);
    check_err(TH_EINVAL);
  }
  state->granulepos = 0;
  state->packetno = 0;

  /* Apply settings */
  int j = 0;
  int v;
  c = Field(params, j++);
  if (c != Val_none) {
    v = Int_val(Some_val(c));
    check_err(th_encode_ctl(state->ts, TH_ENCCTL_SET_KEYFRAME_FREQUENCY_FORCE,
                            &v, sizeof(int)));
  }
  c = Field(params, j++);
  if (c != Val_none) {
    v = 0;
    if (Some_val(c) == Val_true)
      v = 1;
    check_err(th_encode_ctl(state->ts, TH_ENCCTL_SET_VP3_COMPATIBLE, &v,
                            sizeof(int)));
  }
  c = Field(params, j++);
  if (c != Val_none) {
    if (Some_val(c) == Val_true) {
      v = TH_RATECTL_CAP_UNDERFLOW;
      check_err(
          th_encode_ctl(state->ts, TH_ENCCTL_SET_RATE_FLAGS, &v, sizeof(int)));
    }
  }
  c = Field(params, j++);
  if (c != Val_none) {
    v = Int_val(Some_val(c));
    check_err(
        th_encode_ctl(state->ts, TH_ENCCTL_SET_RATE_BUFFER, &v, sizeof(int)));
  }
  c = Field(params, j++);
  if (c != Val_none) {
    v = Int_val(Some_val(c));
    check_err(th_encode_ctl(state->ts, TH_ENCCTL_SET_SPLEVEL, &v, sizeof(int)));
  }

  ans = caml_alloc_custom(&enc_state_ops, sizeof(enc_state_t *), 1, 0);
  Theora_enc_state_val(ans) = state;

  CAMLreturn(ans);
}

CAMLprim value ocaml_theora_encoder_frame_of_granulepos(value t_state,
                                                        value gpos) {
  CAMLparam2(t_state, gpos);
  enc_state_t *state = Theora_enc_state_val(t_state);
  CAMLreturn(caml_copy_int64(th_granule_frame(state->ts, Int64_val(gpos))));
}

CAMLprim value ocaml_theora_encode_header(value t_state, value o_stream_state) {
  CAMLparam2(t_state, o_stream_state);
  enc_state_t *state = Theora_enc_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret;

  ret = th_encode_flushheader(state->ts, &state->tc, &op);
  if (ret < 0)
    check_err(ret);
  if (ret == 0)
    CAMLreturn(Val_true);
  else {
    state->granulepos = op.granulepos;
    state->packetno = op.packetno;
    ogg_stream_packetin(os, &op);
    CAMLreturn(Val_false);
  }
}

CAMLprim value ocaml_theora_encode_buffer(value t_state, value o_stream_state,
                                          value frame) {
  CAMLparam3(t_state, o_stream_state, frame);
  CAMLlocal1(v);
  enc_state_t *state = Theora_enc_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  th_ycbcr_buffer yb;
  ogg_packet op;
  int ret = 1;
  if (ogg_stream_eos(os))
    caml_raise_constant(*caml_named_value("theora_exn_end_of_file"));

  /* Encode the theora packet. */
  yuv_of_val(frame, yb);

  caml_enter_blocking_section();
  ret = th_encode_ycbcr_in(state->ts, yb);
  caml_leave_blocking_section();
  if (ret != 0)
    /* TODO:
     * \retval OC_EINVAL Encoder is not ready, or is finished.
     * \retval -1 The size of the given frame differs from those previously
     * input */
    check_err(ret);

  ret = 1;
  while (ret > 0) {
    ret = th_encode_packetout(state->ts, 0, &op);
    if (ret > 0) {
      state->granulepos = op.granulepos;
      state->packetno = op.packetno;
      ogg_stream_packetin(os, &op);
    }
  }
  if (ret < 0)
    check_err(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_theora_encode_eos(value t_state, value o_stream_state) {
  CAMLparam2(t_state, o_stream_state);
  enc_state_t *state = Theora_enc_state_val(t_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret;
  ogg_int64_t iframe;
  ogg_int64_t pframe;

  /* TODO: a proper eos should be achieved using an empty ogg page with the
   * eos marker.. */

  /* Try to grab a packet */
  ret = th_encode_packetout(state->ts, 1, &op);
  if (ret <= 0) {
    check_err(ret);
    /* No packet was produced: we bake our own ! */
    op.packet = (unsigned char *)NULL;
    op.bytes = 0;
    op.b_o_s = 0;
    op.e_o_s = 1;
    /* Set the granulepos as a new frame */
    iframe = state->granulepos >> state->ti.keyframe_granule_shift;
    pframe = state->granulepos & ~iframe;
    op.granulepos = (iframe << state->ti.keyframe_granule_shift) | (pframe + 1);
    op.packetno = state->packetno + 1;
  }
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

/** Decoding API **/

typedef struct dec_state_t {
  th_dec_ctx *ts;
  th_info ti;
  th_comment tc;
  th_setup_info *tsi;
  int init;
  ogg_packet init_packet;
} dec_state_t;

#define Theora_dec_state_val(v) (*((dec_state_t **)Data_custom_val(v)))

static void finalize_dec_state(value s) {
  dec_state_t *state = Theora_dec_state_val(s);
  if (state->ts != NULL)
    th_decode_free(state->ts);
  th_info_clear(&state->ti);
  th_comment_clear(&state->tc);
  if (state->tsi != NULL)
    th_setup_free(state->tsi);
  free(state);
}

static struct custom_operations dec_state_ops = {
    "ocaml_dec_theora_state", finalize_dec_state,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value caml_theora_check(value packet) {
  CAMLparam1(packet);
  ogg_packet *op = Packet_val(packet);

  th_info ti;
  th_comment tc;
  th_setup_info *tsi = NULL;
  th_comment_init(&tc);
  th_info_init(&ti);
  int ret;

  ret = th_decode_headerin(&ti, &tc, &tsi, op);

  th_comment_clear(&tc);
  th_info_clear(&ti);
  if (tsi != NULL)
    th_setup_free(tsi);

  if (ret > 0)
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

CAMLprim value ocaml_theora_create_dec(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  dec_state_t *state = malloc(sizeof(dec_state_t));
  if (state == NULL)
    caml_raise_out_of_memory();
  th_comment_init(&state->tc);
  th_info_init(&state->ti);
  state->ts = NULL;
  state->tsi = NULL;
  state->init_packet.packet = NULL;
  ret = caml_alloc_custom(&dec_state_ops, sizeof(dec_state_t *), 1, 0);
  Theora_dec_state_val(ret) = state;

  CAMLreturn(ret);
}

CAMLprim value ocaml_theora_dec_headerin(value decoder, value packet) {
  CAMLparam1(packet);
  CAMLlocal4(ret, t, comment, tmp);
  dec_state_t *state = Theora_dec_state_val(decoder);
  ogg_packet *op = Packet_val(packet);
  int v;

  v = th_decode_headerin(&state->ti, &state->tc, &state->tsi, op);
  if (v < 0)
    caml_raise_constant(*caml_named_value("theora_exn_inval"));

  if (v == 0) {
    /* Keep this packet for the first YUV decoding.. */
    memcpy(&state->init_packet, op, sizeof(ogg_packet));
    state->init = 1;
    comment = caml_alloc_tuple(state->tc.comments + 1);
    Store_field(comment, 0, caml_copy_string(state->tc.vendor));
    if (state->tc.comments) {
      int i;
      int len;
      for (i = 0; i < state->tc.comments; i++) {
        if (state->tc.user_comments[i]) {
          len = state->tc.comment_lengths[i];
          tmp = caml_alloc_string(len);
          memcpy(Bytes_val(tmp), state->tc.user_comments[i], len);
          Store_field(comment, i + 1, tmp);
        }
      }
    }
    state->ts = th_decode_alloc(&state->ti, state->tsi);
    ret = caml_alloc_tuple(2);
    Store_field(ret, 0, val_of_info(&state->ti));
    Store_field(ret, 1, comment);

    CAMLreturn(ret);
  } else {
    caml_raise_constant(*caml_named_value("theora_exn_not_enough_data"));
  }
}

CAMLprim value ocaml_theora_decode_YUVout(value decoder, value _os) {
  CAMLparam2(decoder, _os);
  ogg_stream_state *os = Stream_state_val(_os);
  dec_state_t *state = Theora_dec_state_val(decoder);
  th_ycbcr_buffer yb;
  ogg_packet op;
  int ret;

  if (state->init != 1) {
    ret = ogg_stream_packetout(os, &op);
    if (ret == 0)
      caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    /* TODO: use the third argument (granulepos of the decoded packet) */
    check_err(th_decode_packetin(state->ts, &op, NULL));
  } else {
    check_err(th_decode_packetin(state->ts, &state->init_packet, NULL));
    state->init = 0;
  }

  caml_enter_blocking_section();
  th_decode_ycbcr_out(state->ts, yb);
  caml_leave_blocking_section();

  CAMLreturn(val_of_yuv(yb));
}

CAMLprim value ocaml_theora_decoder_frame_of_granulepos(value t_state,
                                                        value gpos) {
  CAMLparam2(t_state, gpos);
  dec_state_t *state = Theora_dec_state_val(t_state);
  CAMLreturn(caml_copy_int64(th_granule_frame(state->ts, Int64_val(gpos))));
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
CAMLprim value ocaml_theora_skeleton_fisbone(value serial, value info,
                                             value start, value content) {
  CAMLparam4(serial, info, start, content);
  CAMLlocal1(packet);
  ogg_packet op;
  th_info ti;
  info_of_val(info, &ti);
  int len = FISBONE_SIZE + caml_string_length(content);

  memset(&op, 0, sizeof(op));
  op.packet = malloc(len);
  if (op.packet == NULL)
    caml_raise_out_of_memory();

  memset(op.packet, 0, len);
  /* it will be the fisbone packet for the theora video */
  memcpy(op.packet, FISBONE_IDENTIFIER, 8); /* identifier */
  write32le(
      op.packet + 8,
      FISBONE_MESSAGE_HEADER_OFFSET); /* offset of the message header fields */
  write32le(op.packet + 12,
            Nativeint_val(serial)); /* serialno of the theora stream */
  write32le(op.packet + 16, 3);     /* number of header packets */
  /* granulerate, temporal resolution of the bitstream in samples/microsecond */
  write64le(op.packet + 20,
            (ogg_int64_t)ti.fps_numerator); /* granulrate numerator */
  write64le(op.packet + 28,
            (ogg_int64_t)ti.fps_denominator); /* granulrate denominator */
  write64le(op.packet + 36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet + 44, 0);                  /* preroll, for theora its 0 */
  *(op.packet + 48) = ti.keyframe_granule_shift; /* granule shift */
  memcpy(op.packet + FISBONE_SIZE, String_val(content),
         caml_string_length(content)); /* message header field */

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}
