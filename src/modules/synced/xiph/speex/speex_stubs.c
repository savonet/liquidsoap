/*
  Copyright 2003-2008 Savonet team

  This file is part of Ocaml-speex.

  Ocaml-speex is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Ocaml-speex is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Ocaml-speex; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <ocaml-ogg.h>
#include <ogg/ogg.h>

#include <speex/speex.h>
#include <speex/speex_bits.h>
#include <speex/speex_callbacks.h>
#include <speex/speex_header.h>
#include <speex/speex_stereo.h>

#include <string.h>

#ifndef Bytes_val
#define Bytes_val String_val
#endif

/* Comment API */

/* This is stolen from speexenc.c
 * It should definitely be part of the library.. */

/*
 Comments will be stored in the Vorbis style.
 It is describled in the "Structure" section of
    http://www.xiph.org/ogg/vorbis/doc/v-comment.html

The comment header is decoded as follows:
  1) [vendor_length] = read an unsigned integer of 32 bits
  2) [vendor_string] = read a UTF-8 vector as [vendor_length] octets
  3) [user_comment_list_length] = read an unsigned integer of 32 bits
  4) iterate [user_comment_list_length] times {
     5) [length] = read an unsigned integer of 32 bits
     6) this iteration's user comment = read a UTF-8 vector as [length] octets
     }
  7) [framing_bit] = read a single bit as boolean
  8) if ( [framing_bit]  unset or end of packet ) then ERROR
  9) done.

  If you have troubles, please write to ymnk@jcraft.com.
 */

#define readint(buf, base)                                                     \
  (((buf[base + 3] << 24) & 0xff000000) | ((buf[base + 2] << 16) & 0xff0000) | \
   ((buf[base + 1] << 8) & 0xff00) | (buf[base] & 0xff))
#define writeint(buf, base, val)                                               \
  do {                                                                         \
    buf[base + 3] = ((val) >> 24) & 0xff;                                      \
    buf[base + 2] = ((val) >> 16) & 0xff;                                      \
    buf[base + 1] = ((val) >> 8) & 0xff;                                       \
    buf[base] = (val)&0xff;                                                    \
  } while (0)

void comment_init(char **comments, int *length, char *vendor_string) {
  int vendor_length = strlen(vendor_string);
  int user_comment_list_length = 0;
  int len = 4 + vendor_length + 4;
  char *p = (char *)malloc(len);
  if (p == NULL)
    caml_raise_out_of_memory();
  writeint(p, 0, vendor_length);
  memcpy(p + 4, vendor_string, vendor_length);
  writeint(p, 4 + vendor_length, user_comment_list_length);
  *length = len;
  *comments = p;
}
void comment_add(char **comments, int *length, char *val) {
  char *p = *comments;
  int vendor_length = readint(p, 0);
  int user_comment_list_length = readint(p, 4 + vendor_length);
  int val_len = strlen(val);
  int len = (*length) + 4 + val_len;

  p = (char *)realloc(p, len);
  if (p == NULL) {
    caml_failwith("realloc");
  }

  writeint(p, *length, val_len);         /* length of comment */
  memcpy(p + *length + 4, val, val_len); /* comment */
  writeint(p, 4 + vendor_length, user_comment_list_length + 1);

  *comments = p;
  *length = len;
}

CAMLprim value caml_speex_comments_of_packet(value o_packet) {
  CAMLparam1(o_packet);
  CAMLlocal2(ret, tmp);
  ogg_packet *op = Packet_val(o_packet);
  char *c = (char *)op->packet;
  int length = op->bytes;
  int len, i, nb_fields;
  char *end;

  if (length < 8)
    caml_failwith("Invalid comments raw length");

  end = c + length;
  len = readint(c, 0);
  c += 4;
  if (len < 0 || c + len > end)
    caml_failwith("Invalid comments raw data");
  tmp = caml_alloc_string(len);
  memcpy(Bytes_val(tmp), c, len);
  c += len;
  if (c + 4 > end)
    caml_failwith("Invalid comments raw data");
  nb_fields = readint(c, 0);
  ret = caml_alloc_tuple(nb_fields + 1);
  Store_field(ret, 0, tmp);
  c += 4;
  for (i = 0; i < nb_fields; i++) {
    if (c + 4 > end)
      caml_failwith("Invalid comments raw data");
    len = readint(c, 0);
    c += 4;
    if (len < 0 || c + len > end)
      caml_failwith("Invalid comments raw data");
    tmp = caml_alloc_string(len);
    memcpy(Bytes_val(tmp), c, len);
    Store_field(ret, i + 1, tmp);
    c += len;
  }

  CAMLreturn(ret);
}

#undef readint
#undef writeint

/* Mode API */

#define Mode_val(v) (*((SpeexMode **)Data_abstract_val(v)))

static inline value value_of_speex_mode(value v, const SpeexMode *s) {
  v = caml_alloc(1, Abstract_tag);
  *((const SpeexMode **)Data_abstract_val(v)) = s;
  return v;
}

CAMLprim value caml_speex_get_mode(value i) {
  CAMLparam0();
  CAMLlocal1(ret);
  CAMLreturn(value_of_speex_mode(ret, speex_lib_get_mode(Int_val(i))));
}

/* Header API */

static SpeexHeader *header_of_value(value v, SpeexHeader *header) {
  int i = 0;
  value tmp;
  tmp = Field(v, i++);
  if (caml_string_length(tmp) > SPEEX_HEADER_STRING_LENGTH)
    caml_invalid_argument("wrong argument: speex_string too long");
  memcpy(header->speex_string, String_val(tmp), caml_string_length(tmp));
  tmp = Field(v, i++);
  if (caml_string_length(tmp) > SPEEX_HEADER_VERSION_LENGTH)
    caml_invalid_argument("wrong argument: speex_version too long");
  memcpy(header->speex_version, String_val(tmp), caml_string_length(tmp));
#define shv(x) header->x = Int_val(Field(v, i++))
  shv(speex_version_id);
  shv(header_size);
  shv(rate);
  header->mode = Int_val(caml_callback(
      *caml_named_value("caml_speex_int_of_mode"), Field(v, i++)));
  shv(mode_bitstream_version);
  shv(nb_channels);
  shv(bitrate);
  shv(frame_size);
  Store_field(v, i++, Val_bool(header->vbr));
  shv(frames_per_packet);
  shv(extra_headers);

  return header;
}

CAMLprim value value_of_header(SpeexHeader *header) {
  CAMLparam0();
  CAMLlocal2(ret, tmp);
  ret = caml_alloc_tuple(13);
  int i = 0;
  tmp = caml_alloc_string(SPEEX_HEADER_STRING_LENGTH);
  memcpy(Bytes_val(tmp), header->speex_string, SPEEX_HEADER_STRING_LENGTH);
  Store_field(ret, i++, tmp);
  tmp = caml_alloc_string(SPEEX_HEADER_VERSION_LENGTH);
  memcpy(Bytes_val(tmp), header->speex_version, SPEEX_HEADER_VERSION_LENGTH);
  Store_field(ret, i++, tmp);
#define svh(v) Store_field(ret, i++, Val_int(header->v));
  svh(speex_version_id);
  svh(header_size);
  svh(rate);
  Store_field(ret, i++,
              caml_callback(*caml_named_value("caml_speex_mode_of_int"),
                            Val_int(header->mode)));
  svh(mode_bitstream_version);
  svh(nb_channels);
  svh(bitrate);
  svh(frame_size);
  Store_field(ret, i++, Val_bool(header->vbr));
  svh(frames_per_packet);
  svh(extra_headers);

  CAMLreturn(ret);
}

CAMLprim value caml_speex_init_header(value rate, value chans, value mode,
                                      value fpp, value vbr) {
  CAMLparam1(mode);
  struct SpeexMode *m = Mode_val(mode);
  SpeexHeader header;
  speex_init_header(&header, Int_val(rate), 1, m);
  header.frames_per_packet = Int_val(fpp);
  header.vbr = Int_val(vbr);
  header.nb_channels = Int_val(chans);
  CAMLreturn(value_of_header(&header));
}

CAMLprim value caml_speex_encode_header(value v, value o_comments) {
  CAMLparam2(v, o_comments);
  CAMLlocal1(ret);
  ogg_packet op;
  int packet_size;
  SpeexHeader header;
  char *vendor_string =
      "ocaml-speex by the savonet team (http://savonet.sf.net/)";
  char *comments;
  int comments_length;
  int i;
  ret = caml_alloc_tuple(2);

  unsigned char *data = (unsigned char *)speex_header_to_packet(
      header_of_value(v, &header), &packet_size);
  op.packet = data;
  op.bytes = packet_size;
  op.b_o_s = 1;
  op.e_o_s = 0;
  op.granulepos = 0;
  op.packetno = 0;
  Store_field(ret, 0, value_of_packet(&op));
  free(data);

  /* Comment Packet */
  comment_init(&comments, &comments_length, vendor_string);
  for (i = 0; i < Wosize_val(o_comments); i++)
    comment_add(&comments, &comments_length,
                (char *)Bytes_val(Field(o_comments, i)));

  op.packet = (unsigned char *)comments;
  op.bytes = comments_length;
  op.b_o_s = 0;
  op.e_o_s = 0;
  op.granulepos = 0;
  op.packetno = 1;
  Store_field(ret, 1, value_of_packet(&op));

  free(comments);
  CAMLreturn(ret);
}

CAMLprim value caml_speex_header_of_packet(value packet) {
  CAMLparam1(packet);
  CAMLlocal1(ret);
  ogg_packet *op = Packet_val(packet);

  if (op->bytes < sizeof(SpeexHeader))
    caml_invalid_argument("not a speex header");

  caml_enter_blocking_section();
  SpeexHeader *header = speex_packet_to_header((char *)op->packet, op->bytes);
  caml_leave_blocking_section();

  if (header == NULL)
    caml_invalid_argument("not a speex header");
  ret = value_of_header(header);
  speex_header_free(header);
  CAMLreturn(ret);
}

/* Encoder API */

typedef struct cenc_t {
  int position;
  SpeexBits bits;
  void *enc;
  int fpp;
} cenc_t;

#define Enc_val(v) (*((cenc_t **)Data_custom_val(v)))

static void finalize_speex_enc(value v) {
  cenc_t *enc = Enc_val(v);
  speex_bits_destroy(&enc->bits);
  speex_encoder_destroy(enc->enc);
  free(enc);
}

static struct custom_operations speex_enc_ops = {
    "ocaml_speex_enc",   finalize_speex_enc,       custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_speex_enc_init(value m, value fpp) {
  CAMLparam1(m);
  CAMLlocal1(ret);
  cenc_t *cen = malloc(sizeof(cenc_t));
  if (cen == NULL)
    caml_raise_out_of_memory();
  SpeexMode *mode = Mode_val(m);
  void *enc = speex_encoder_init(mode);
  if (enc == NULL)
    caml_raise_out_of_memory();
  speex_bits_init(&cen->bits);
  cen->enc = enc;
  cen->position = 0;
  cen->fpp = Int_val(fpp);
  ret = caml_alloc_custom(&speex_enc_ops, sizeof(cenc_t *), 1, 0);
  Enc_val(ret) = cen;

  CAMLreturn(ret);
}

CAMLprim value ocaml_speex_encoder_ctl_get(value e, value n) {
  CAMLparam1(e);
  cenc_t *cenc = Enc_val(e);
  void *enc = cenc->enc;
  int ret;
  if (speex_encoder_ctl(enc, Int_val(n), &ret) == -2)
    caml_invalid_argument("wrong argument in speex_encoder_ctl");
  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_speex_encoder_ctl_set(value e, value n, value x) {
  CAMLparam1(e);
  cenc_t *cenc = Enc_val(e);
  void *enc = cenc->enc;
  int arg = Int_val(x);
  if (speex_encoder_ctl(enc, Int_val(n), &arg) == -2)
    caml_invalid_argument("wrong argument in speex_encoder_ctl");
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_speex_encode_page(value e_state, value o_chans,
                                       value o_stream_state, value feed) {
  CAMLparam3(e_state, feed, o_stream_state);
  CAMLlocal2(v, ret);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  cenc_t *cenc = Enc_val(e_state);
  void *enc = cenc->enc;
  ogg_page og;
  ogg_packet op;
  int state = cenc->position - 1;
  int len;
  int frame_size;
  int fpp = cenc->fpp;
  int chans = Int_val(o_chans);
  speex_encoder_ctl(enc, SPEEX_GET_FRAME_SIZE, &frame_size);
  int i;
  float *data = malloc(sizeof(float) * frame_size * chans);
  if (data == NULL)
    caml_raise_out_of_memory();
  char *cbits = malloc(sizeof(char) * frame_size * chans);
  if (cbits == NULL) {
    free(data);
    caml_raise_out_of_memory();
  }
  int nbBytes;

  /* Is there an audio page flushed? If not, work until there is. */
  do {

    if (ogg_stream_eos(os)) {
      free(data);
      free(cbits);
      caml_raise_constant(*caml_named_value("ocaml_speex_eos_exn"));
    }

    /* Read and process more audio. */
    v = caml_callback_exn(feed, Val_unit);
    if Is_exception_result (v) {
      free(data);
      free(cbits);
      cenc->position = state + 1;
      caml_raise(Extract_exception(v));
    }

    len = Wosize_val(v) / Double_wosize;
    if (len != frame_size * chans) {
      free(data);
      free(cbits);
      cenc->position = state + 1;
      caml_raise_constant(*caml_named_value("ocaml_speex_invfrlen_exn"));
    }
    for (i = 0; i < len; i++)
      data[i] = Double_field(v, i);

    caml_enter_blocking_section();
    if (chans == 2)
      speex_encode_stereo(data, frame_size, &cenc->bits);
    speex_encode(enc, data, &cenc->bits);
    caml_leave_blocking_section();

    state++;

    if ((state + 1) % fpp != 0)
      continue;

    speex_bits_insert_terminator(&cenc->bits);
    nbBytes = speex_bits_write(&cenc->bits, cbits, frame_size * fpp);
    speex_bits_reset(&cenc->bits);
    op.packet = (unsigned char *)cbits;
    op.bytes = nbBytes;
    op.b_o_s = 0;
    op.e_o_s = 0;
    op.granulepos = (state + 1) * frame_size;
    op.packetno = 2 + state / fpp;

    /* Put the packet in the ogg stream. */
    ogg_stream_packetin(os, &op);
  } while (ogg_stream_pageout(os, &og) <= 0);

  cenc->position = state + 1;
  ret = value_of_page(&og);
  free(data);
  free(cbits);
  CAMLreturn(ret);
}

CAMLprim value ocaml_speex_encode_page_int(value e_state, value o_chans,
                                           value o_stream_state, value feed) {
  CAMLparam3(e_state, feed, o_stream_state);
  CAMLlocal2(v, ret);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  cenc_t *cenc = Enc_val(e_state);
  void *enc = cenc->enc;
  ogg_page og;
  ogg_packet op;
  int state = cenc->position - 1;
  int len;
  int chans = Int_val(o_chans);
  int frame_size;
  int fpp = cenc->fpp;
  speex_encoder_ctl(enc, SPEEX_GET_FRAME_SIZE, &frame_size);
  int i;
  spx_int16_t *data = malloc(sizeof(spx_int16_t) * frame_size * chans);
  if (data == NULL)
    caml_raise_out_of_memory();
  char *cbits = malloc(sizeof(char) * frame_size * chans);
  if (cbits == NULL) {
    free(data);
    caml_raise_out_of_memory();
  }
  int nbBytes;

  /* Is there an audio page flushed? If not, work until there is. */
  do {
    if (ogg_stream_eos(os)) {
      free(data);
      free(cbits);
      caml_raise_constant(*caml_named_value("ocaml_speex_eos_exn"));
    }

    /* Read and process more audio. */
    v = caml_callback_exn(feed, Val_unit);
    if Is_exception_result (v) {
      free(data);
      free(cbits);
      cenc->position = state + 1;
      caml_raise(Extract_exception(v));
    }

    len = Wosize_val(v);
    if (len != frame_size * chans) {
      free(data);
      free(cbits);
      cenc->position = state + 1;
      caml_raise_constant(*caml_named_value("ocaml_speex_invfrlen_exn"));
    }

    for (i = 0; i < len; i++)
      data[i] = Int_val(Field(v, i));

    caml_enter_blocking_section();
    if (chans == 2)
      speex_encode_stereo_int(data, frame_size, &cenc->bits);
    speex_encode_int(enc, data, &cenc->bits);
    caml_leave_blocking_section();

    state++;

    if ((state + 1) % fpp != 0)
      continue;

    speex_bits_insert_terminator(&cenc->bits);
    nbBytes = speex_bits_write(&cenc->bits, cbits, frame_size);
    speex_bits_reset(&cenc->bits);
    op.packet = (unsigned char *)cbits;
    op.bytes = nbBytes;
    op.b_o_s = 0;
    op.e_o_s = 0;
    op.granulepos = (state + 1) * frame_size;
    op.packetno = 2 + state / fpp;

    /* Put the packet in the ogg stream. */
    ogg_stream_packetin(os, &op);
  } while (ogg_stream_pageout(os, &og) <= 0);

  cenc->position = state + 1;
  ret = value_of_page(&og);
  free(data);
  free(cbits);
  CAMLreturn(ret);
}

CAMLprim value ocaml_speex_encoder_eos(value v, value o_stream_state) {
  CAMLparam2(v, o_stream_state);
  ogg_packet op;
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  cenc_t *cenc = Enc_val(v);
  int state = cenc->position - 1;
  void *enc = cenc->enc;
  int frame_size;
  speex_encoder_ctl(enc, SPEEX_GET_FRAME_SIZE, &frame_size);

  op.packet = (unsigned char *)NULL;
  op.bytes = 0;
  op.b_o_s = 0;
  op.e_o_s = 1;
  op.granulepos = (state + 1) * frame_size;
  op.packetno = 2 + state;
  ogg_stream_packetin(os, &op);

  CAMLreturn(Val_unit);
}

/* Decoder API */

typedef struct cdec_t {
  SpeexStereoState *stereo;
  SpeexBits bits;
  void *dec;
} cdec_t;

#define Dec_val(v) (*((cdec_t **)Data_custom_val(v)))

static void finalize_speex_dec(value v) {
  cdec_t *cdec = Dec_val(v);
  speex_stereo_state_destroy(cdec->stereo);
  speex_bits_destroy(&cdec->bits);
  speex_decoder_destroy(cdec->dec);
  free(cdec);
}

static struct custom_operations speex_dec_ops = {
    "ocaml_speex_dec",   finalize_speex_dec,       custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_speex_dec_init(value m) {
  CAMLparam1(m);
  CAMLlocal1(ret);
  SpeexMode *mode = Mode_val(m);
  void *dec = speex_decoder_init(mode);
  if (dec == NULL)
    caml_raise_out_of_memory();
  SpeexStereoState *stereo = speex_stereo_state_init();
  if (stereo == NULL)
    caml_raise_out_of_memory();
  cdec_t *cdec = malloc(sizeof(cdec_t));
  if (cdec == NULL)
    caml_raise_out_of_memory();
  cdec->dec = dec;
  speex_bits_init(&cdec->bits);
  cdec->stereo = stereo;

  SpeexCallback callback;

  callback.callback_id = SPEEX_INBAND_STEREO;
  callback.func = speex_std_stereo_request_handler;
  callback.data = stereo;
  speex_decoder_ctl(dec, SPEEX_SET_HANDLER, &callback);

  ret = caml_alloc_custom(&speex_dec_ops, sizeof(cdec_t *), 1, 0);
  Dec_val(ret) = cdec;

  CAMLreturn(ret);
}

CAMLprim value ocaml_speex_decoder_ctl_get(value e, value n) {
  CAMLparam1(e);
  cdec_t *cdec = Dec_val(e);
  void *dec = cdec->dec;
  int ret;
  if (speex_decoder_ctl(dec, Int_val(n), &ret) == -2)
    caml_invalid_argument("wrong argument in speex_decoder_ctl");
  CAMLreturn(Val_int(ret));
}

CAMLprim value ocaml_speex_decoder_ctl_set(value e, value n, value x) {
  CAMLparam1(e);
  cdec_t *cdec = Dec_val(e);
  void *dec = cdec->dec;
  int arg = Int_val(x);
  if (speex_decoder_ctl(dec, Int_val(n), &arg) == -2)
    caml_invalid_argument("wrong argument in speex_decoder_ctl");
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_speex_decoder_decode(value e, value o_chans, value s,
                                          value add) {
  CAMLparam3(e, s, add);
  CAMLlocal2(ret, v);
  ogg_stream_state *os = Stream_state_val(s);
  cdec_t *cdec = Dec_val(e);
  void *dec = cdec->dec;
  SpeexStereoState *stereo = cdec->stereo;
  int chans = Int_val(o_chans);
  int err;
  ogg_packet op;
  int frame_size;
  speex_decoder_ctl(dec, SPEEX_GET_FRAME_SIZE, &frame_size);
  float *out = malloc(sizeof(float) * frame_size * chans);
  if (out == NULL)
    caml_raise_out_of_memory();
  int i;
  int n;

  while (1) {
    err = ogg_stream_packetout(os, &op);
    if (err <= 0) {
      free(out);
      if (err == 0)
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
      else
        caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));
    }

    /* Copy Ogg packet to Speex bitstream */
    caml_enter_blocking_section();
    speex_bits_read_from(&cdec->bits, (char *)op.packet, op.bytes);
    caml_leave_blocking_section();

    while (1) {
      caml_enter_blocking_section();
      n = speex_decode(dec, &cdec->bits, out);
      caml_leave_blocking_section();

      if (n == -1)
        break;

      if (chans == 2)
        speex_decode_stereo(out, frame_size, stereo);
      ret = caml_alloc(frame_size * chans * Double_wosize, Double_array_tag);
      for (i = 0; i < frame_size * chans; i++)
        Store_double_field(ret, i, out[i]);
      v = caml_callback_exn(add, ret);
      if Is_exception_result (v) {
        free(out);
        caml_raise(Extract_exception(v));
      }
    };
  }

  free(out);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_speex_decoder_decode_int(value e, value o_chans, value s,
                                              value add) {
  CAMLparam3(e, s, add);
  CAMLlocal2(ret, v);
  ogg_stream_state *os = Stream_state_val(s);
  cdec_t *cdec = Dec_val(e);
  void *dec = cdec->dec;
  SpeexStereoState *stereo = cdec->stereo;
  int chans = Int_val(o_chans);
  int err;
  ogg_packet op;
  int frame_size;
  speex_decoder_ctl(dec, SPEEX_GET_FRAME_SIZE, &frame_size);
  spx_int16_t *out = malloc(sizeof(spx_int16_t) * frame_size * chans);
  if (out == NULL)
    caml_raise_out_of_memory();
  int i;
  int n;

  while (1) {
    err = ogg_stream_packetout(os, &op);
    if (err <= 0) {
      free(out);
      if (err == 0)
        caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));
      else
        caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));
    }

    /* Copy Ogg packet to Speex bitstream */
    speex_bits_read_from(&cdec->bits, (char *)op.packet, op.bytes);

    while (1) {
      caml_enter_blocking_section();
      n = speex_decode_int(dec, &cdec->bits, out);
      caml_leave_blocking_section();

      if (n == -1)
        break;

      if (chans == 2)
        speex_decode_stereo_int(out, frame_size, stereo);
      ret = caml_alloc_tuple(frame_size * chans);
      for (i = 0; i < frame_size * chans; i++)
        Store_field(ret, i, Int_val(out[i]));
      v = caml_callback_exn(add, ret);
      if Is_exception_result (v) {
        free(out);
        caml_raise(Extract_exception(v));
      }
    };
  }

  free(out);
  CAMLreturn(Val_unit);
}

/* Ogg skeleton support */

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

/* Values from speexenc.c in speexenc */
CAMLprim value ocaml_speex_skeleton_fisbone(value serial, value _header,
                                            value start, value content) {
  CAMLparam4(serial, _header, start, content);
  CAMLlocal1(packet);
  ogg_packet op;
  SpeexHeader header;
  header_of_value(_header, &header);
  int len = FISBONE_SIZE + caml_string_length(content);

  memset(&op, 0, sizeof(op));
  op.packet = malloc(len);
  if (op.packet == NULL)
    caml_raise_out_of_memory();

  memset(op.packet, 0, len);
  /* it will be the fisbone packet for the speex audio */
  memcpy(op.packet, FISBONE_IDENTIFIER, 8); /* identifier */
  write32le(
      op.packet + 8,
      FISBONE_MESSAGE_HEADER_OFFSET); /* offset of the message header fields */
  write32le(op.packet + 12,
            Nativeint_val(serial)); /* serialno of the vorbis stream */
  write32le(op.packet + 16,
            2 + header.extra_headers); /* number of header packet */
  /* granulerate, temporal resolution of the bitstream in Hz */
  write64le(op.packet + 20,
            (ogg_int64_t)header.rate);       /* granulerate numerator */
  write64le(op.packet + 28, (ogg_int64_t)1); /* granulerate denominator */
  write64le(op.packet + 36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet + 44, 3); /* preroll, for speex its 3 */
  *(op.packet + 48) = 0;        /* granule shift, always 0 for speex */
  memcpy(op.packet + FISBONE_SIZE, String_val(content),
         caml_string_length(content));

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}
