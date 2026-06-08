/*
 * OCaml bindings for libfdk-aac
 *
 * Copyright 2013 Savonet team
 *
 * This file is part of ocaml-fdk-aac.
 *
 * ocaml-fdk-aac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-fdk-aac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-fdk-aac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* OCaml bindings for the libfdk-aac library. */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>

#include <fdk-aac/aacenc_lib.h>

#include <stdint.h>
#include <string.h>

/* Not all errors seem to be used
 * so we only check the one that
 * appear relevant.. */
static void check_for_err(int ret) {
  switch (ret) {
  case AACENC_OK:
    return;

  case AACENC_INVALID_HANDLE:
    caml_raise_constant(*caml_named_value("fdkaac_exn_invalid_handle"));
    break;

  case AACENC_MEMORY_ERROR:
    caml_raise_out_of_memory();
    break;

  case AACENC_UNSUPPORTED_PARAMETER:
    caml_raise_constant(*caml_named_value("fdkaac_exn_unsupported_parameter"));
    break;

  case AACENC_INVALID_CONFIG:
    caml_raise_constant(*caml_named_value("fdkaac_exn_invalid_config"));
    break;

  case AACENC_INIT_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(0));
    break;

  case AACENC_INIT_AAC_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(1));
    break;

  case AACENC_INIT_SBR_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(2));
    break;

  case AACENC_INIT_TP_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(3));
    break;

  case AACENC_INIT_META_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(4));
    break;

  case AACENC_ENCODE_ERROR:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_error"), Val_int(5));
    break;

  case AACENC_ENCODE_EOF:
    caml_raise_constant(*caml_named_value("fdkaac_exn_encode_eof"));
    break;

  default:
    caml_raise_with_arg(*caml_named_value("fdkaac_exn_unknown_error"),
                        Val_int(ret));
    break;
  }
}

#define Encoder_val(v) (*((HANDLE_AACENCODER *)Data_custom_val(v)))

static void finalize_encoder(value e) {
  HANDLE_AACENCODER enc = Encoder_val(e);
  aacEncClose(&enc);
}

static struct custom_operations encoder_ops = {
    "ocaml_fdkaac_encoder",   finalize_encoder,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

#include <stdio.h>

CAMLprim value ocaml_fdkaac_init_enc(value chans) {
  CAMLparam1(chans);
  CAMLlocal1(ans);

  int channels = Int_val(chans);
  int mode;

  HANDLE_AACENCODER enc;

  int ret = aacEncOpen(&enc, 0x0, channels);
  if (ret != AACENC_OK) {
    free(enc);
    check_for_err(ret);
  }

  switch (channels) {
  case 1:
    mode = MODE_1;
    break;
  case 2:
    mode = MODE_2;
    break;
  case 3:
    mode = MODE_1_2;
    break;
  case 4:
    mode = MODE_1_2_1;
    break;
  case 5:
    mode = MODE_1_2_2;
    break;
  case 6:
    mode = MODE_1_2_2_1;
    break;
  default:
    free(enc);
    caml_raise_constant(*caml_named_value("fdkaac_exn_unsupported_parameter"));
  }

  ret = aacEncoder_SetParam(enc, AACENC_CHANNELMODE, mode);

  if (ret != AACENC_OK) {
    free(enc);
    check_for_err(ret);
  }

  ret = aacEncoder_SetParam(enc, AACENC_CHANNELORDER, 1);

  if (ret != AACENC_OK) {
    free(enc);
    check_for_err(ret);
  }

  ans = caml_alloc_custom(&encoder_ops, sizeof(HANDLE_AACENCODER), 1, 0);
  Encoder_val(ans) = enc;

  CAMLreturn(ans);
}

CAMLprim value ocaml_fdkaac_encode(value e, value buf, value ofs, value len) {
  CAMLparam4(e, buf, ofs, len);
  CAMLlocal1(ret);

  HANDLE_AACENCODER enc = Encoder_val(e);

  AACENC_BufDesc inBuf = {0};
  AACENC_BufDesc outBuf = {0};
  AACENC_InArgs inArgs = {0};
  AACENC_OutArgs outArgs = {0};

  // Input buffer
  int bufid = IN_AUDIO_DATA;
  int buflen = Int_val(len);
  int bufelsize = 2; // S16LE
  int offset = Int_val(ofs);
  void *bufs;

  inBuf.numBufs = 1;
  inBuf.bufferIdentifiers = &bufid;
  inBuf.bufSizes = &buflen;
  inBuf.bufElSizes = &bufelsize;

  bufs = malloc(buflen);

  if (bufs == NULL)
    caml_raise_out_of_memory();

  memcpy(bufs, String_val(buf) + offset, buflen);

  inBuf.bufs = &bufs;

  inArgs.numInSamples = buflen / bufelsize;

  // Output buffer
  uint8_t outbuf[20480];
  int outid = OUT_BITSTREAM_DATA;
  int outlen = sizeof(outbuf);
  int outelsize = 1;
  void *outptr = &outbuf;

  outBuf.numBufs = 1;
  outBuf.bufferIdentifiers = &outid;
  outBuf.bufSizes = &outlen;
  outBuf.bufElSizes = &outelsize;
  outBuf.bufs = &outptr;

  caml_enter_blocking_section();
  int err = aacEncEncode(enc, &inBuf, &outBuf, &inArgs, &outArgs);
  caml_leave_blocking_section();

  free(bufs);
  check_for_err(err);

  ret = caml_alloc_string(outArgs.numOutBytes);
  memcpy(Bp_val(ret), outbuf, outArgs.numOutBytes);

  CAMLreturn(ret);
}

CAMLprim value ocaml_fdkaac_flush(value e) {
  CAMLparam1(e);
  CAMLlocal1(ret);

  HANDLE_AACENCODER enc = Encoder_val(e);

  AACENC_BufDesc inBuf = {0};
  AACENC_BufDesc outBuf = {0};
  AACENC_InArgs inArgs = {0};
  AACENC_OutArgs outArgs = {0};

  inArgs.numInSamples = -1;

  // Output buffer
  uint8_t outbuf[20480];
  int outid = OUT_BITSTREAM_DATA;
  int outlen = sizeof(outbuf);
  int outelsize = 1;
  void *outptr = &outbuf;

  outBuf.bufferIdentifiers = &outid;
  outBuf.bufSizes = &outlen;
  outBuf.bufElSizes = &outelsize;
  outBuf.bufs = &outptr;

  caml_enter_blocking_section();
  int err = aacEncEncode(enc, &inBuf, &outBuf, &inArgs, &outArgs);
  caml_leave_blocking_section();

  check_for_err(err);

  ret = caml_alloc_string(outArgs.numOutBytes);
  memcpy(Bp_val(ret), outbuf, outArgs.numOutBytes);

  CAMLreturn(ret);
}

CAMLprim value ocaml_fdkaac_set_param(value e, value p, value v) {
  CAMLparam3(e, p, v);

  HANDLE_AACENCODER enc = Encoder_val(e);

  AACENC_PARAM param = Int_val(p);

  check_for_err(aacEncoder_SetParam(enc, param, Int_val(v)));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_fdkaac_get_param(value e, value p) {
  CAMLparam2(e, p);
  int ans;

  HANDLE_AACENCODER enc = Encoder_val(e);

  AACENC_PARAM param = Int_val(p);

  ans = aacEncoder_GetParam(enc, param);

  CAMLreturn(Val_int(ans));
}

CAMLprim value ocaml_fdkaac_info(value e) {
  CAMLparam1(e);
  CAMLlocal1(ans);
  HANDLE_AACENCODER enc = Encoder_val(e);
  AACENC_InfoStruct info = {0};

  // Apparently we need to do this before being able to access info...
  check_for_err(aacEncEncode(enc, NULL, NULL, NULL, NULL));
  check_for_err(aacEncInfo(enc, &info));

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(info.inputChannels));
  Store_field(ans, 1, Val_int(info.frameLength));
  CAMLreturn(ans);
}
