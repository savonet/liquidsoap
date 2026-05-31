/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-ogg.
 *
 * ocaml-ogg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ogg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ogg; if not, write to the Free Software
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
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <caml/signals.h>

#include <ogg/ogg.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "ocaml-ogg.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

/** Page manipulation **/

value value_of_page(ogg_page *op) {
  CAMLparam0();
  CAMLlocal3(v, header, body);
  header = caml_alloc_string(op->header_len);
  memcpy(Bytes_val(header), op->header, op->header_len);

  body = caml_alloc_string(op->body_len);
  memcpy(Bytes_val(body), op->body, op->body_len);

  v = caml_alloc_tuple(2);
  Store_field(v, 0, header);
  Store_field(v, 1, body);

  CAMLreturn(v);
}

ogg_page *page_of_value(value v, ogg_page *page) {
  page->header = (unsigned char *)String_val(Field(v, 0));
  page->header_len = caml_string_length(Field(v, 0));

  page->body = (unsigned char *)String_val(Field(v, 1));
  page->body_len = caml_string_length(Field(v, 1));

  return page;
}

CAMLprim value ocaml_ogg_page_serialno(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(caml_copy_nativeint(ogg_page_serialno(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_eos(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_eos(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_bos(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_bos(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_packets(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_int(ogg_page_packets(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_continued(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_continued(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_version(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_int(ogg_page_version(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_granulepos(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(caml_copy_int64(ogg_page_granulepos(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_pageno(value page) {
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(caml_copy_nativeint(ogg_page_pageno(page_of_value(page, &op))));
}

CAMLprim value ocaml_ogg_page_checksum_set(value page) {
  CAMLparam1(page);
  ogg_page op;
  ogg_page_checksum_set(page_of_value(page, &op));
  CAMLreturn(Val_unit);
}

/***** Sync state *****/

static void finalize_sync_state(value s) {
  ogg_sync_destroy(Sync_state_val(s));
}

static struct custom_operations sync_state_ops = {
    "ocaml_ogg_sync_state",   finalize_sync_state,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_ogg_sync_init() {
  CAMLparam0();
  CAMLlocal1(sync);
  ogg_sync_state *oy = malloc(sizeof(ogg_sync_state));

  ogg_sync_init(oy);
  sync = caml_alloc_custom(&sync_state_ops, sizeof(ogg_sync_state *), 1, 0);
  Sync_state_val(sync) = oy;

  CAMLreturn(sync);
}

CAMLprim value ocaml_ogg_sync_reset(value oy) {
  CAMLparam1(oy);
  ogg_sync_reset(Sync_state_val(oy));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_sync_pageseek(value cb, value oy) {
  CAMLparam2(cb, oy);
  CAMLlocal1(bytes);
  ogg_sync_state *sync = Sync_state_val(oy);
  int read;
  int len = 4096;
  ogg_page page;
  int err = ogg_sync_pageseek(sync, &page);

  bytes = caml_alloc_string(len);

  while (err <= 0) {
    read = Int_val(caml_callback3(cb, bytes, Val_int(0), Val_int(len)));
    if (read == 0)
      caml_raise_constant(*caml_named_value("ogg_exn_eos"));

    char *buffer = ogg_sync_buffer(sync, read);
    memcpy(buffer, String_val(bytes), read);
    ogg_sync_wrote(sync, read);
    err = ogg_sync_pageseek(sync, &page);
  }

  CAMLreturn(value_of_page(&page));
}

CAMLprim value ocaml_ogg_sync_read(value cb, value oy) {
  CAMLparam2(cb, oy);
  CAMLlocal2(ret, bytes);
  ogg_sync_state *sync = Sync_state_val(oy);
  int read;
  int len = 4096;
  ogg_page page;
  int ans = ogg_sync_pageout(sync, &page);

  bytes = caml_alloc_string(len);

  while (ans != 1) {
    if (ans == -1)
      caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));
    read = Int_val(caml_callback3(cb, bytes, Val_int(0), Val_int(len)));
    if (read == 0)
      caml_raise_constant(*caml_named_value("ogg_exn_eos"));

    char *buffer = ogg_sync_buffer(sync, read);
    memcpy(buffer, String_val(bytes), read);
    ogg_sync_wrote(sync, read);
    ans = ogg_sync_pageout(sync, &page);
  }

  CAMLreturn(value_of_page(&page));
}

/***** Stream state ******/

static void finalize_stream_state(value s) {
  // This also free the argument
  ogg_stream_destroy(Stream_state_val(s));
}

static struct custom_operations stream_state_ops = {
    "ocaml_ogg_stream_state", finalize_stream_state,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

static void finalize_packet(value s) {
  ogg_packet *op = Packet_val(s);
  free(op->packet);
  free(op);
}

static inline ogg_packet *copy_packet(ogg_packet *op) {
  ogg_packet *nop = malloc(sizeof(ogg_packet));
  if (nop == NULL)
    caml_raise_out_of_memory();
  nop->packet = malloc(op->bytes);
  memcpy(nop->packet, op->packet, op->bytes);
  nop->bytes = op->bytes;
  nop->b_o_s = op->b_o_s;
  nop->e_o_s = op->e_o_s;
  nop->granulepos = op->granulepos;
  nop->packetno = op->packetno;

  return nop;
}

static struct custom_operations packet_ops = {
    "ocaml_ogg_packet",  finalize_packet,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

value value_of_packet(ogg_packet *op) {
  CAMLparam0();
  CAMLlocal1(packet);
  packet = caml_alloc_custom_mem(&packet_ops, sizeof(ogg_packet *), op->bytes);
  Packet_val(packet) = copy_packet(op);
  CAMLreturn(packet);
}

CAMLprim value ocaml_ogg_stream_init(value serial) {
  CAMLparam0();
  CAMLlocal1(ans);
  ogg_stream_state *os = malloc(sizeof(ogg_stream_state));

  ogg_stream_init(os, Nativeint_val(serial));
  ans = caml_alloc_custom(&stream_state_ops, sizeof(ogg_stream_state *), 1, 0);
  Stream_state_val(ans) = os;

  CAMLreturn(ans);
}

CAMLprim value ocaml_ogg_stream_eos(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  CAMLreturn(Val_bool(ogg_stream_eos(os)));
}

// libogg does not offer any API to generate a final, empty page.
// However, some (most!) codecs need a synchronous way to end their
// logical bitstream without having to submit an empty packet so we
// hack it away..
CAMLprim value ocaml_ogg_stream_terminate(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page page;

  ogg_packet op;
  op.packet = (unsigned char *)NULL;
  op.bytes = 0;
  op.b_o_s = 0;
  op.e_o_s = 1;
  op.granulepos = os->granulepos + 1;
  op.packetno = os->packetno + 1;
  ogg_stream_packetin(os, &op);

  if (!ogg_stream_pageout(os, &page))
    caml_raise_constant(*caml_named_value("ogg_exn_bad_data"));

  page.header[26] = 0;
  page.header_len = 27;
  page.body = NULL;
  page.body_len = 0;

  ogg_page_checksum_set(&page);
  CAMLreturn(value_of_page(&page));
}

CAMLprim value ocaml_ogg_stream_pageout(value o_stream_state, value fill) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page og;
  int ret;

#ifdef HAVE_PAGEOUT_FILL
  if (fill != Val_unit)
    ret = ogg_stream_pageout_fill(os, &og, Int_val(fill));
  else
#endif
    ret = ogg_stream_pageout(os, &og);

  if (!ret)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  CAMLreturn(value_of_page(&og));
}

CAMLprim value ocaml_ogg_stream_pagein(value o_stream_state, value page) {
  CAMLparam2(o_stream_state, page);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page op;

  if (ogg_stream_pagein(os, page_of_value(page, &op)) != 0)
    caml_raise_constant(*caml_named_value("ogg_exn_bad_data"));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_stream_packetin(value o_stream_state, value packet) {
  CAMLparam2(o_stream_state, packet);
  ogg_stream_state *os = Stream_state_val(o_stream_state);

  if (ogg_stream_packetin(os, Packet_val(packet)) != 0)
    caml_raise_constant(*caml_named_value("ogg_exn_bad_data"));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_stream_packetout(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetout(os, &op);

  if (ret == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  if (ret == -1)
    caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

  CAMLreturn(value_of_packet(&op));
}

CAMLprim value ocaml_ogg_stream_packet_advance(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetout(os, &op);

  if (ret == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  if (ret == -1)
    caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_stream_packetpeek(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetpeek(os, &op);

  if (ret == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  if (ret == -1)
    caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

  CAMLreturn(value_of_packet(&op));
}

CAMLprim value ocaml_ogg_stream_granulepospeek(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetpeek(os, &op);

  if (ret == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  if (ret == -1)
    caml_raise_constant(*caml_named_value("ogg_exn_out_of_sync"));

  CAMLreturn(caml_copy_int64(op.granulepos));
}

CAMLprim value ocaml_ogg_stream_packet_granulepos(value _op) {
  CAMLparam1(_op);
  ogg_packet *op = Packet_val(_op);

  CAMLreturn(caml_copy_int64(op->granulepos));
}

CAMLprim value ocaml_ogg_flush_stream(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page og;

  if (!ogg_stream_flush(os, &og))
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  CAMLreturn(value_of_page(&og));
}

CAMLprim value ocaml_ogg_stream_serialno(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);

  CAMLreturn(caml_copy_nativeint((intnat)os->serialno));
}

/* Ogg skeleton helpers */

/* Values from http://xiph.org/ogg/doc/skeleton.html */
#define SKELETON_VERSION_MAJOR 3
#define SKELETON_VERSION_MINOR 0
#define FISHEAD_IDENTIFIER "fishead\0"

/* Wrappers */
static void write16le(unsigned char *ptr, ogg_uint16_t v) {
  ptr[0] = v & 0xff;
  ptr[1] = (v >> 8) & 0xff;
}

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

/* Code from theorautils.c in ffmpeg2theora */
CAMLprim value ocaml_ogg_skeleton_fishead(value pres_num, value pres_den,
                                          value base_num, value base_den,
                                          value time) {
  CAMLparam0();
  CAMLlocal1(packet);
  ogg_packet op;

  memset(&op, 0, sizeof(op));

  op.packet = malloc(64);
  if (op.packet == NULL)
    caml_raise_out_of_memory();

  memset(op.packet, 0, 64);
  memcpy(op.packet, FISHEAD_IDENTIFIER, 8);          /* identifier */
  write16le(op.packet + 8, SKELETON_VERSION_MAJOR);  /* version major */
  write16le(op.packet + 10, SKELETON_VERSION_MINOR); /* version minor */
  write64le(op.packet + 12,
            (ogg_int64_t)Int64_val(pres_num)); /* presentationtime numerator */
  write64le(op.packet + 20, (ogg_int64_t)Int64_val(
                                pres_den)); /* presentationtime denominator */
  write64le(op.packet + 28,
            (ogg_int64_t)Int64_val(base_num)); /* basetime numerator */
  write64le(op.packet + 36,
            (ogg_int64_t)Int64_val(base_den)); /* basetime denominator */
  /* both the numerator are zero hence handled by the memset */
  write32le(op.packet + 44,
            Int32_val(time)); /* UTC time, set to zero for now */

  op.b_o_s = 1;  /* its the first packet of the stream */
  op.e_o_s = 0;  /* its not the last packet of the stream */
  op.bytes = 64; /* length of the packet in bytes */

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}

/* Code from theorautils.c from ffmpeg2theora */
CAMLprim value ocaml_ogg_skeleton_eos(value v) {
  CAMLparam0();
  ogg_packet op;

  /* build the e_o_s packet */
  memset(&op, 0, sizeof(op));
  op.b_o_s = 0;
  op.e_o_s = 1; /* its the e_o_s packet */
  op.granulepos = 0;
  op.bytes = 0; /* e_o_s packet is an empty packet */

  CAMLreturn(value_of_packet(&op));
}
