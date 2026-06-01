#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#define caml_acquire_runtime_system caml_leave_blocking_section
#define caml_release_runtime_system caml_enter_blocking_section

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <ocaml-ogg.h>
#include <ogg/ogg.h>
#include <opus.h>
#include <opus_multistream.h>

#include "config.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#ifdef BIGENDIAN
// code from bits/byteswap.h (C) 1997, 1998 Free Software Foundation, Inc.
#define int32le_to_native(x)                                                   \
  ((((x)&0xff000000) >> 24) | (((x)&0x00ff0000) >> 8) |                        \
   (((x)&0x0000ff00) << 8) | (((x)&0x000000ff) << 24))
#define int16le_to_native(x) ((((x) >> 8) & 0xff) | (((x)&0xff) << 8))
#else
#define int32le_to_native(x) x
#define int16le_to_native(x) x
#endif

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

/* polymorphic variant utility macro */
#define get_var(x) caml_hash_variant(#x)

/* Macros to convert variants to controls. */
#define set_ctl(tag, variant, handle, fn, name, v)                             \
  if (tag == get_var(variant)) {                                               \
    check(fn(handle, name(Int_val(v))));                                       \
    CAMLreturn(Val_unit);                                                      \
  }

#define set_value_ctl(tag, variant, handle, fn, name, v, convert)              \
  if (tag == get_var(variant)) {                                               \
    check(fn(handle, name(convert(v))));                                       \
    CAMLreturn(Val_unit);                                                      \
  }

#define get_ctl(tag, variant, handle, fn, name, v, type)                       \
  if (tag == get_var(variant)) {                                               \
    type name = (type)Int_val(Field(v, 0));                                    \
    check(fn(handle, name(&name)));                                            \
    Store_field(v, 0, Val_int(name));                                          \
    CAMLreturn(Val_unit);                                                      \
  }

#define get_value_ctl(tag, variant, handle, fn, name, v, type, convert)        \
  if (tag == get_var(variant)) {                                               \
    type name = (type)Int_val(Field(v, 0));                                    \
    check(fn(handle, name(&name)));                                            \
    Store_field(v, 0, convert(name));                                          \
    CAMLreturn(Val_unit);                                                      \
  }

static void check(int ret) {
  if (ret < 0)
    switch (ret) {
    case OPUS_BAD_ARG:
      caml_invalid_argument("opus");

    case OPUS_BUFFER_TOO_SMALL:
      caml_raise_constant(*caml_named_value("opus_exn_buffer_too_small"));

    case OPUS_INTERNAL_ERROR:
      caml_raise_constant(*caml_named_value("opus_exn_internal_error"));

    case OPUS_INVALID_PACKET:
      caml_raise_constant(*caml_named_value("opus_exn_invalid_packet"));

    case OPUS_UNIMPLEMENTED:
      caml_raise_constant(*caml_named_value("opus_exn_unimplemented"));

    case OPUS_INVALID_STATE:
      caml_raise_constant(*caml_named_value("opus_exn_invalid_state"));

    case OPUS_ALLOC_FAIL:
      caml_raise_constant(*caml_named_value("opus_exn_alloc_fail"));

    default:
      caml_failwith("Unknown opus error");
    }
}

CAMLprim value ocaml_opus_version_string(value unit) {
  CAMLparam0();
  CAMLreturn(caml_copy_string(opus_get_version_string()));
}

/***** Decoder ******/

#define Dec_val(v) (*(OpusDecoder **)Data_custom_val(v))

static void finalize_dec(value v) {
  OpusDecoder *dec = Dec_val(v);
  opus_decoder_destroy(dec);
}

static struct custom_operations dec_ops = {
    "ocaml_opus_dec",         finalize_dec,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_opus_decoder_create(value _sr, value _chans) {
  CAMLparam0();
  CAMLlocal1(ans);
  opus_int32 sr = Int_val(_sr);
  int chans = Int_val(_chans);
  int ret = 0;
  OpusDecoder *dec;

  dec = opus_decoder_create(sr, chans, &ret);

  check(ret);
  ans = caml_alloc_custom(&dec_ops, sizeof(OpusDecoder *), 0, 1);
  Dec_val(ans) = dec;
  CAMLreturn(ans);
}

CAMLprim value ocaml_opus_packet_check_header(value packet) {
  CAMLparam1(packet);
  ogg_packet *op = Packet_val(packet);
  int ans = 0;

  if (op->bytes >= 8 && !memcmp(op->packet, "OpusHead", 8))
    ans = 1;

  CAMLreturn(Val_bool(ans));
}

CAMLprim value ocaml_opus_decoder_channels(value packet) {
  CAMLparam1(packet);
  ogg_packet *op = Packet_val(packet);
  uint8_t *data = op->packet;
  uint8_t version = *(data + 8);

  if (op->bytes <= 10 || memcmp(op->packet, "OpusHead", 8))
    caml_invalid_argument("Wrong header data.");

  if (version != 1)
    caml_invalid_argument("Wrong header version.");

  uint8_t ret = *(data + 9);

  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_opus_comments(value packet) {
  CAMLparam1(packet);
  CAMLlocal2(ans, comments);
  ogg_packet *op = Packet_val(packet);
  if (!(op->bytes >= 8 && !memcmp(op->packet, "OpusTags", 8)))
    check(OPUS_INVALID_PACKET);
  ans = caml_alloc_tuple(2);

  int off = 8;
  /* Vendor */

  if (off + 4 > op->bytes)
    check(OPUS_INVALID_PACKET);

  opus_int32 vendor_length =
      int32le_to_native(*((opus_int32 *)(op->packet + off)));
  off += 4;

  if (off + vendor_length > op->bytes)
    check(OPUS_INVALID_PACKET);

  Store_field(ans, 0, caml_alloc_string(vendor_length));
  memcpy(Bytes_val(Field(ans, 0)), op->packet + off, vendor_length);

  off += vendor_length;

  /* Comments */

  if (off + 4 > op->bytes)
    check(OPUS_INVALID_PACKET);

  opus_int32 comments_length =
      int32le_to_native(*((opus_int32 *)(op->packet + off)));
  off += 4;

  comments = caml_alloc_tuple(comments_length);
  Store_field(ans, 1, comments);
  opus_int32 i, len;
  for (i = 0; i < comments_length; i++) {
    if (off + 4 > op->bytes)
      check(OPUS_INVALID_PACKET);

    len = int32le_to_native(*((opus_int32 *)(op->packet + off)));
    off += 4;

    if (off + len > op->bytes)
      check(OPUS_INVALID_PACKET);

    Store_field(comments, i, caml_alloc_string(len));
    memcpy(Bytes_val(Field(comments, i)), op->packet + off, len);
    off += len;
  }

  CAMLreturn(ans);
}

static opus_int32 bandwidth_of_value(value v) {
  if (v == get_var(Auto))
    return OPUS_AUTO;
  if (v == get_var(Narrow_band))
    return OPUS_BANDWIDTH_NARROWBAND;
  if (v == get_var(Medium_band))
    return OPUS_BANDWIDTH_MEDIUMBAND;
  if (v == get_var(Wide_band))
    return OPUS_BANDWIDTH_WIDEBAND;
  if (v == get_var(Super_wide_band))
    return OPUS_BANDWIDTH_SUPERWIDEBAND;
  if (v == get_var(Full_band))
    return OPUS_BANDWIDTH_FULLBAND;

  caml_failwith("Unknown opus error");
}

static value value_of_bandwidth(opus_int32 a) {
  switch (a) {
  case OPUS_AUTO:
    return get_var(Auto);
  case OPUS_BANDWIDTH_NARROWBAND:
    return get_var(Narrow_band);
  case OPUS_BANDWIDTH_MEDIUMBAND:
    return get_var(Medium_band);
  case OPUS_BANDWIDTH_WIDEBAND:
    return get_var(Wide_band);
  case OPUS_BANDWIDTH_SUPERWIDEBAND:
    return get_var(Super_wide_band);
  case OPUS_BANDWIDTH_FULLBAND:
    return get_var(Full_band);
  default:
    caml_failwith("Unknown opus error");
  }
}

CAMLprim value ocaml_opus_decoder_ctl(value ctl, value _dec) {
  CAMLparam2(_dec, ctl);
  CAMLlocal2(tag, v);
  OpusDecoder *dec = Dec_val(_dec);
  if (Is_long(ctl)) {
    // Only ctl without argument here is reset state..
    opus_decoder_ctl(dec, OPUS_RESET_STATE);
    CAMLreturn(Val_unit);
  } else {
    v = Field(ctl, 1);
    tag = Field(ctl, 0);

    /* Generic controls. */
    get_ctl(tag, Get_final_range, dec, opus_decoder_ctl, OPUS_GET_FINAL_RANGE,
            v, opus_uint32);
    get_ctl(tag, Get_pitch, dec, opus_decoder_ctl, OPUS_GET_PITCH, v,
            opus_int32);
    get_value_ctl(tag, Get_bandwidth, dec, opus_decoder_ctl, OPUS_GET_BANDWIDTH,
                  v, opus_int32, value_of_bandwidth);
    set_ctl(tag, Set_lsb_depth, dec, opus_decoder_ctl, OPUS_SET_LSB_DEPTH, v);
    get_ctl(tag, Get_lsb_depth, dec, opus_decoder_ctl, OPUS_GET_LSB_DEPTH, v,
            opus_int32);
#ifdef OPUS_SET_PHASE_INVERSION_DISABLED
    set_ctl(tag, Set_phase_inversion_disabled, dec, opus_decoder_ctl,
            OPUS_SET_PHASE_INVERSION_DISABLED, v);
#endif

    /* Decoder controls. */
    get_ctl(tag, Get_gain, dec, opus_decoder_ctl, OPUS_GET_GAIN, v, opus_int32);
    set_ctl(tag, Set_gain, dec, opus_decoder_ctl, OPUS_SET_GAIN, v);
  }

  caml_failwith("Unknown opus error");
}

CAMLprim value ocaml_opus_decoder_decode_float(value _dec, value _os, value buf,
                                               value _ofs, value _len,
                                               value _fec) {
  CAMLparam3(_dec, _os, buf);
  CAMLlocal1(chan);
  ogg_stream_state *os = Stream_state_val(_os);
  ogg_packet op;
  OpusDecoder *dec = Dec_val(_dec);
  int decode_fec = Int_val(_fec);

  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int total_samples = 0;
  int ret;

  int chans = Wosize_val(buf);
  float *pcm = malloc(chans * len * sizeof(float));
  if (pcm == NULL)
    caml_raise_out_of_memory();

  int i, c;

  while (total_samples < len) {
    ret = ogg_stream_packetout(os, &op);
    /* returned values are:
     * 1: ok
     * 0: not enough data. in this case
     *    we return the number of samples
     *    decoded if > 0 and raise
     *    Ogg_not_enough_data otherwise
     * -1: out of sync */
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    if (ret == 0) {
      free(pcm);
      if (total_samples > 0) {
        CAMLreturn(Val_int(total_samples));
      } else {
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
      }
    }

    if (chans != opus_packet_get_nb_channels(op.packet))
      caml_invalid_argument("Wrong number of channels.");

    caml_release_runtime_system();
    ret = opus_decode_float(dec, op.packet, op.bytes, pcm, len, decode_fec);
    caml_acquire_runtime_system();

    if (ret < 0) {
      free(pcm);
      check(ret);
    }
    for (c = 0; c < chans; c++) {
      chan = Field(buf, c);
      for (i = 0; i < ret; i++)
        Store_double_field(chan, ofs + total_samples + i,
                           clip(pcm[i * chans + c]));
    }
    total_samples += ret;
    len -= ret;
  }

  free(pcm);
  CAMLreturn(Val_int(total_samples));
}

CAMLprim value ocaml_opus_decoder_decode_float_byte(value *argv, int argn) {
  return ocaml_opus_decoder_decode_float(argv[0], argv[1], argv[2], argv[3],
                                         argv[4], argv[5]);
}

CAMLprim value ocaml_opus_decoder_decode_float_ba(value _dec, value _os,
                                                  value buf, value _ofs,
                                                  value _len, value _fec) {
  CAMLparam3(_dec, _os, buf);
  CAMLlocal1(chan);
  ogg_stream_state *os = Stream_state_val(_os);
  ogg_packet op;
  OpusDecoder *dec = Dec_val(_dec);
  int decode_fec = Int_val(_fec);

  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  int total_samples = 0;
  int ret;

  int chans = Wosize_val(buf);
  float *pcm = malloc(chans * len * sizeof(float));
  if (pcm == NULL)
    caml_raise_out_of_memory();

  int i, c;

  while (total_samples < len) {
    ret = ogg_stream_packetout(os, &op);
    /* returned values are:
     * 1: ok
     * 0: not enough data. in this case
     *    we return the number of samples
     *    decoded if > 0 and raise
     *    Ogg_not_enough_data otherwise
     * -1: out of sync */
    if (ret == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

    if (ret == 0) {
      free(pcm);
      if (total_samples > 0) {
        CAMLreturn(Val_int(total_samples));
      } else {
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
      }
    }

    if (chans != opus_packet_get_nb_channels(op.packet))
      caml_invalid_argument("Wrong number of channels.");

    caml_release_runtime_system();
    ret = opus_decode_float(dec, op.packet, op.bytes, pcm, len, decode_fec);
    caml_acquire_runtime_system();

    if (ret < 0) {
      free(pcm);
      check(ret);
    }
    for (c = 0; c < chans; c++) {
      chan = Field(buf, c);
      for (i = 0; i < ret; i++)
        ((float *)Caml_ba_data_val(chan))[ofs + total_samples + i] =
            pcm[i * chans + c];
    }
    total_samples += ret;
    len -= ret;
  }

  free(pcm);
  CAMLreturn(Val_int(total_samples));
}

CAMLprim value ocaml_opus_decoder_decode_float_ba_byte(value *argv, int argn) {
  return ocaml_opus_decoder_decode_float_ba(argv[0], argv[1], argv[2], argv[3],
                                            argv[4], argv[5]);
}

/***** Encoder *****/

typedef struct encoder_t {
  OpusEncoder *encoder;
  int samplerate_ratio;
  ogg_int64_t granulepos;
  ogg_int64_t packetno;
} encoder_t;

#define Enc_val(v) (*(encoder_t **)Data_custom_val(v))

static void finalize_enc(value v) {
  encoder_t *enc = Enc_val(v);
  opus_encoder_destroy(enc->encoder);
  free(enc);
}

static struct custom_operations enc_ops = {
    "ocaml_opus_enc",         finalize_enc,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

static opus_int32 application_of_value(value v) {
  if (v == get_var(Voip))
    return OPUS_APPLICATION_VOIP;
  if (v == get_var(Audio))
    return OPUS_APPLICATION_AUDIO;
  if (v == get_var(Restricted_lowdelay))
    return OPUS_APPLICATION_RESTRICTED_LOWDELAY;

  caml_failwith("Unknown opus error");
}

static value value_of_application(opus_int32 a) {
  switch (a) {
  case OPUS_APPLICATION_VOIP:
    return get_var(Voip);
  case OPUS_APPLICATION_AUDIO:
    return get_var(Audio);
  case OPUS_APPLICATION_RESTRICTED_LOWDELAY:
    return get_var(Restricted_lowdelay);
  default:
    caml_failwith("Unknown opus error");
  }
}

static unsigned char header_packet[19] = {
    /* Identifier. */
    'O', 'p', 'u', 's', 'H', 'e', 'a', 'd',
    /* version, channels count, pre-skip (16 bits, unsigned,
     *                                    little endian) */
    1, 2, 0, 0,
    /* Samperate (32 bits, unsigned, little endian) */
    0, 0, 0, 0,
    /* output gain (16 bits, signed, little endian), channels mapping family,
     * stream count (always 0 in this implementation) */
    0, 0, 0};

static void pack_header(ogg_packet *op, opus_int32 sr, int channels,
                        opus_int16 pre_skip, opus_int16 gain) {
  op->bytes = sizeof(header_packet);
  op->packet = header_packet;

  /* Now fill data. */
  op->packet[9] = channels;
  opus_int16 pre_skip_native = int16le_to_native(pre_skip);
  memcpy(op->packet + 10, &pre_skip_native, sizeof(opus_int16));
  opus_int32 sr_native = int32le_to_native(sr);
  memcpy(op->packet + 12, &sr_native, sizeof(opus_int32));
  opus_int16 gain_native = int16le_to_native(gain);
  memcpy(op->packet + 16, &gain_native, sizeof(opus_int16));

  op->b_o_s = 1;
  op->e_o_s = op->granulepos = op->packetno = 0;
}

static void pack_comments(ogg_packet *op, char *vendor, value comments) {
  int i;
  long pos = 0;
  opus_int32 vendor_length = strlen(vendor);
  char *comment;
  opus_int32 comments_len = Wosize_val(comments);
  opus_int32 comment_length;

  op->bytes = 8 + 4 + vendor_length + 4;

  for (i = 0; i < Wosize_val(comments); i++)
    op->bytes += 4 + caml_string_length(Field(comments, i));

  op->packet = malloc(op->bytes);
  if (op->packet == NULL)
    caml_raise_out_of_memory();

  /* Identifier. */
  memcpy(op->packet, "OpusTags", 8);
  pos += 8;

  /* Vendor. */
  opus_int32 vendor_length_native = int32le_to_native(vendor_length);
  memcpy(op->packet + 8, &vendor_length_native, sizeof(opus_int32));
  memcpy(op->packet + 12, vendor, vendor_length);
  pos += 4 + vendor_length;

  /* Comments length. */
  memcpy(op->packet + pos, &comments_len, sizeof(opus_int32));
  pos += 4;

  /* Comments. */
  for (i = 0; i < comments_len; i++) {
    comment = (char *)Bytes_val(Field(comments, i));
    comment_length = caml_string_length(Field(comments, i));
    opus_int32 comment_length_native = int32le_to_native(comment_length);
    memcpy(op->packet + pos, &comment_length_native, sizeof(opus_int32));
    memcpy(op->packet + pos + 4, comment, comment_length);
    pos += 4 + comment_length;
  }

  op->e_o_s = op->b_o_s = op->granulepos = 0;
  op->packetno = 1;
}

CAMLprim value ocaml_opus_encoder_create(value _skip, value _comments,
                                         value _gain, value _sr, value _chans,
                                         value _application) {
  CAMLparam0();
  CAMLlocal2(_enc, ans);
  opus_int32 sr = Int_val(_sr);
  int chans = Int_val(_chans);
  int ret = 0;
  int app = application_of_value(_application);
  encoder_t *enc = malloc(sizeof(encoder_t));
  if (enc == NULL)
    caml_raise_out_of_memory();
  /* First encoded packet is the third one. */
  enc->packetno = 1;
  enc->granulepos = 0;
  /* Value samplerates are: 48000, 24000, 16000, 12000, 8000
   * so this value is always an integer. */
  enc->samplerate_ratio = 48000 / sr;

  ogg_packet header;
  pack_header(&header, sr, chans, Int_val(_skip), Int_val(_gain));

  ogg_packet comments;
  pack_comments(&comments, "ocaml-opus by the Savonet Team.", _comments);

  enc->encoder = opus_encoder_create(sr, chans, app, &ret);

  check(ret);
  _enc = caml_alloc_custom(&enc_ops, sizeof(encoder_t *), 0, 1);
  Enc_val(_enc) = enc;

  ans = caml_alloc_tuple(3);

  Store_field(ans, 0, _enc);
  Store_field(ans, 1, value_of_packet(&header));
  Store_field(ans, 2, value_of_packet(&comments));

  free(comments.packet);

  CAMLreturn(ans);
}

CAMLprim value ocaml_opus_encoder_create_byte(value *argv, int argn) {
  return ocaml_opus_encoder_create(argv[0], argv[1], argv[2], argv[3], argv[4],
                                   argv[5]);
}

static opus_int32 bitrate_of_value(value v) {
  if (Is_long(v)) {
    if (v == get_var(Auto))
      return OPUS_AUTO;
    if (v == get_var(Bitrate_max))
      return OPUS_BITRATE_MAX;
  } else {
    if (Field(v, 0) == get_var(Bitrate))
      return Int_val(Field(v, 1));
  }

  caml_failwith("Unknown opus error");
}

CAMLprim value value_of_bitrate(opus_int32 a) {
  CAMLparam0();
  CAMLlocal1(ret);
  switch (a) {
  case OPUS_AUTO:
    CAMLreturn(get_var(Auto));
  case OPUS_BITRATE_MAX:
    CAMLreturn(get_var(Voice));
  default:
    ret = caml_alloc_tuple(2);
    Store_field(ret, 0, get_var(Bitrate));
    Store_field(ret, 1, Val_int(a));
    CAMLreturn(ret);
  }
}

static opus_int32 signal_of_value(value v) {
  if (v == get_var(Auto))
    return OPUS_AUTO;
  if (v == get_var(Voice))
    return OPUS_SIGNAL_VOICE;
  if (v == get_var(Music))
    return OPUS_SIGNAL_MUSIC;

  caml_failwith("Unknown opus error");
}

static value value_of_signal(opus_int32 a) {
  switch (a) {
  case OPUS_AUTO:
    return get_var(Auto);
  case OPUS_SIGNAL_VOICE:
    return get_var(Voice);
  case OPUS_SIGNAL_MUSIC:
    return get_var(Music);
  default:
    caml_failwith("Unknown opus error");
  }
}

CAMLprim value ocaml_opus_encoder_ctl(value ctl, value _enc) {
  CAMLparam2(_enc, ctl);
  CAMLlocal2(tag, v);
  encoder_t *handler = Enc_val(_enc);
  OpusEncoder *enc = handler->encoder;
  if (Is_long(ctl)) {
    // Only ctl without argument here is reset state..
    opus_encoder_ctl(enc, OPUS_RESET_STATE);
    CAMLreturn(Val_unit);
  } else {
    v = Field(ctl, 1);
    tag = Field(ctl, 0);

    /* Generic controls. */
    get_ctl(tag, Get_final_range, enc, opus_encoder_ctl, OPUS_GET_FINAL_RANGE,
            v, opus_uint32);
    get_ctl(tag, Get_pitch, enc, opus_encoder_ctl, OPUS_GET_PITCH, v,
            opus_int32);
    get_value_ctl(tag, Get_bandwidth, enc, opus_encoder_ctl, OPUS_GET_BANDWIDTH,
                  v, opus_int32, value_of_bandwidth);
    set_ctl(tag, Set_lsb_depth, enc, opus_encoder_ctl, OPUS_SET_LSB_DEPTH, v);
    get_ctl(tag, Get_lsb_depth, enc, opus_encoder_ctl, OPUS_GET_LSB_DEPTH, v,
            opus_int32);
#ifdef OPUS_SET_PHASE_INVERSION_DISABLED
    set_ctl(tag, Set_phase_inversion_disabled, enc, opus_encoder_ctl,
            OPUS_SET_PHASE_INVERSION_DISABLED, v);
#endif

    /* Encoder controls. */
    set_ctl(tag, Set_complexity, enc, opus_encoder_ctl, OPUS_SET_COMPLEXITY, v);
    get_ctl(tag, Get_complexity, enc, opus_encoder_ctl, OPUS_GET_COMPLEXITY, v,
            opus_int32);
    set_ctl(tag, Set_vbr, enc, opus_encoder_ctl, OPUS_SET_VBR, v);
    get_ctl(tag, Get_vbr, enc, opus_encoder_ctl, OPUS_GET_VBR, v, opus_int32);
    set_ctl(tag, Set_vbr_constraint, enc, opus_encoder_ctl,
            OPUS_SET_VBR_CONSTRAINT, v);
    get_ctl(tag, Get_vbr_constraint, enc, opus_encoder_ctl,
            OPUS_GET_VBR_CONSTRAINT, v, opus_int32);
    set_ctl(tag, Set_force_channels, enc, opus_encoder_ctl,
            OPUS_SET_FORCE_CHANNELS, v);
    get_ctl(tag, Get_force_channels, enc, opus_encoder_ctl,
            OPUS_GET_FORCE_CHANNELS, v, opus_int32);
    get_ctl(tag, Get_samplerate, enc, opus_encoder_ctl, OPUS_GET_SAMPLE_RATE, v,
            opus_int32);
    get_ctl(tag, Get_lookhead, enc, opus_encoder_ctl, OPUS_GET_LOOKAHEAD, v,
            opus_int32);
    set_ctl(tag, Set_inband_fec, enc, opus_encoder_ctl, OPUS_SET_INBAND_FEC, v);
    get_ctl(tag, Get_inband_fec, enc, opus_encoder_ctl, OPUS_GET_INBAND_FEC, v,
            opus_int32);
    set_ctl(tag, Set_packet_loss_perc, enc, opus_encoder_ctl,
            OPUS_SET_PACKET_LOSS_PERC, v);
    get_ctl(tag, Get_packet_loss_perc, enc, opus_encoder_ctl,
            OPUS_GET_PACKET_LOSS_PERC, v, opus_int32);
    set_ctl(tag, Set_dtx, enc, opus_encoder_ctl, OPUS_SET_DTX, v);
    get_ctl(tag, Get_dtx, enc, opus_encoder_ctl, OPUS_GET_DTX, v, opus_int32);

    /* These guys have polynmorphic variant as argument.. */
    set_value_ctl(tag, Set_bitrate, enc, opus_encoder_ctl, OPUS_SET_BITRATE, v,
                  bitrate_of_value);
    get_value_ctl(tag, Get_bitrate, enc, opus_encoder_ctl, OPUS_GET_BITRATE, v,
                  opus_int32, value_of_bitrate);
    set_value_ctl(tag, Set_max_bandwidth, enc, opus_encoder_ctl,
                  OPUS_SET_MAX_BANDWIDTH, v, bandwidth_of_value);
    get_value_ctl(tag, Get_max_bandwidth, enc, opus_encoder_ctl,
                  OPUS_GET_MAX_BANDWIDTH, v, opus_int32, value_of_bandwidth);
    set_value_ctl(tag, Set_bandwidth, enc, opus_encoder_ctl, OPUS_SET_BANDWIDTH,
                  v, bandwidth_of_value);
    set_value_ctl(tag, Set_signal, enc, opus_encoder_ctl, OPUS_SET_SIGNAL, v,
                  signal_of_value);
    get_value_ctl(tag, Get_signal, enc, opus_encoder_ctl, OPUS_GET_SIGNAL, v,
                  opus_int32, value_of_signal);
    set_value_ctl(tag, Set_application, enc, opus_encoder_ctl,
                  OPUS_SET_APPLICATION, v, application_of_value);
    get_value_ctl(tag, Get_application, enc, opus_encoder_ctl,
                  OPUS_GET_APPLICATION, v, opus_int32, value_of_application);
  }

  caml_failwith("Unknown opus error");
}

CAMLprim value ocaml_opus_encode_float(value _frame_size, value _enc, value _os,
                                       value buf, value _off, value _len) {
  CAMLparam3(_enc, buf, _os);
  encoder_t *handler = Enc_val(_enc);
  OpusEncoder *enc = handler->encoder;
  ogg_stream_state *os = Stream_state_val(_os);
  ogg_packet op;
  int off = Int_val(_off);
  int len = Int_val(_len);
  int frame_size = Int_val(_frame_size);

  if (len < frame_size)
    caml_raise_constant(*caml_named_value("opus_exn_buffer_too_small"));

  int chans = Wosize_val(buf);
  /* This is the recommended value */
  int max_data_bytes = 4000;
  unsigned char *data = malloc(max_data_bytes);
  if (data == NULL)
    caml_raise_out_of_memory();
  float *pcm = malloc(chans * frame_size * sizeof(float));
  if (pcm == NULL)
    caml_raise_out_of_memory();
  int i, j, c;
  int ret;
  int loops = len / frame_size;
  for (i = 0; i < loops; i++) {
    for (j = 0; j < frame_size; j++)
      for (c = 0; c < chans; c++)
        pcm[chans * j + c] =
            clip(Double_field(Field(buf, c), off + j + i * frame_size));

    caml_release_runtime_system();
    ret = opus_encode_float(enc, pcm, frame_size, data, max_data_bytes);
    caml_acquire_runtime_system();

    if (ret < 0) {
      free(pcm);
      free(data);
      check(ret);
    }

    /* From the documentation: If the return value is 1 byte,
     * then the packet does not need to be transmitted (DTX). */
    if (ret < 2)
      continue;

    handler->granulepos += frame_size * handler->samplerate_ratio;
    handler->packetno++;

    op.bytes = ret;
    op.packet = data;
    op.b_o_s = op.e_o_s = 0;
    op.packetno = handler->packetno;
    op.granulepos = handler->granulepos;

    if (ogg_stream_packetin(os, &op) != 0) {
      free(pcm);
      free(data);
      caml_raise_constant(*caml_named_value("ogg_exn_internal_error"));
    }
  }
  free(pcm);
  free(data);

  CAMLreturn(Val_int(loops * frame_size));
}

CAMLprim value ocaml_opus_encode_float_byte(value *argv, int argn) {
  return ocaml_opus_encode_float(argv[0], argv[1], argv[2], argv[3], argv[4],
                                 argv[5]);
}

CAMLprim value ocaml_opus_encode_float_ba(value _frame_size, value _enc,
                                          value _os, value buf, value _ofs,
                                          value _len) {
  CAMLparam3(_enc, buf, _os);
  encoder_t *handler = Enc_val(_enc);
  OpusEncoder *enc = handler->encoder;
  ogg_stream_state *os = Stream_state_val(_os);
  ogg_packet op;
  int len = Int_val(_len);
  int ofs = Int_val(_ofs);
  int chans = Wosize_val(buf);
  int frame_size = Int_val(_frame_size);

  if (chans == 0)
    CAMLreturn(Val_int(0));

  if (Caml_ba_array_val(Field(buf, 0))->dim[0] < ofs + len)
    caml_failwith("Invalid length or offset!");

  if (len < frame_size)
    caml_raise_constant(*caml_named_value("opus_exn_buffer_too_small"));

  /* This is the recommended value */
  int max_data_bytes = 4000;
  unsigned char *data = malloc(max_data_bytes);
  if (data == NULL)
    caml_raise_out_of_memory();
  float *pcm = malloc(chans * frame_size * sizeof(float));
  if (pcm == NULL)
    caml_raise_out_of_memory();
  int i, j, c;
  int ret;
  int loops = len / frame_size;
  for (i = 0; i < loops; i++) {
    for (j = 0; j < frame_size; j++)
      for (c = 0; c < chans; c++)
        pcm[chans * j + c] = ((float *)Caml_ba_data_val(
            Field(buf, c)))[j + i * frame_size + ofs];

    caml_release_runtime_system();
    ret = opus_encode_float(enc, pcm, frame_size, data, max_data_bytes);
    caml_acquire_runtime_system();

    if (ret < 0) {
      free(pcm);
      free(data);
      check(ret);
    }

    /* From the documentation: If the return value is 1 byte,
     * then the packet does not need to be transmitted (DTX). */
    if (ret < 2)
      continue;

    handler->granulepos += frame_size * handler->samplerate_ratio;
    handler->packetno++;

    op.bytes = ret;
    op.packet = data;
    op.b_o_s = op.e_o_s = 0;
    op.packetno = handler->packetno;
    op.granulepos = handler->granulepos;

    if (ogg_stream_packetin(os, &op) != 0) {
      free(pcm);
      free(data);
      caml_raise_constant(*caml_named_value("ogg_exn_internal_error"));
    }
  }
  free(pcm);
  free(data);

  CAMLreturn(Val_int(loops * frame_size));
}

CAMLprim value ocaml_opus_encode_float_ba_byte(value *argv, int argn) {
  return ocaml_opus_encode_float_ba(argv[0], argv[1], argv[2], argv[3], argv[4],
                                    argv[5]);
}

CAMLprim value ocaml_opus_encode_eos(value _os, value _enc) {
  CAMLparam2(_os, _enc);
  ogg_stream_state *os = Stream_state_val(_os);
  ogg_packet op;
  encoder_t *handler = Enc_val(_enc);
  handler->packetno++;

  op.bytes = 0;
  op.packet = NULL;
  op.b_o_s = 0;
  op.e_o_s = 1;
  op.packetno = handler->packetno;
  op.granulepos = handler->granulepos;

  if (ogg_stream_packetin(os, &op) != 0)
    caml_raise_constant(*caml_named_value("ogg_exn_internal_error"));
  ;

  CAMLreturn(Val_unit);
}
