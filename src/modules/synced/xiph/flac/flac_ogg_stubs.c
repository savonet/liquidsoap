/* This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * Chunks of this code have been borrowed and influenced
 * by flac/decode.c and the flac XMMS plugin.
 *
 */

#include <memory.h>
#include <ogg/ogg.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <ocaml-ogg.h>

#include "flac_stubs.h"

/* C.f. http://flac.sourceforge.net/ogg_mapping.html */
CAMLprim value ocaml_flac_decoder_check_ogg(value v) {
  CAMLparam1(v);
  ogg_packet *p = Packet_val(v);
  unsigned char *h = p->packet;
  if (p->bytes < 9 ||
      /* FLAC */
      h[0] != 0x7f || h[1] != 'F' || h[2] != 'L' || h[3] != 'A' || h[4] != 'C')
    CAMLreturn(Val_false);

  CAMLreturn(Val_true);
}

CAMLprim value ocaml_flac_decoder_packet_data(value v) {
  CAMLparam1(v);
  CAMLlocal1(ans);
  ogg_packet *p = Packet_val(v);

  ans = caml_alloc_string(p->bytes);
  memcpy((char *)String_val(ans), p->packet, p->bytes);
  CAMLreturn(ans);
}

/* Encoder */

CAMLprim value ocaml_flac_encoder_ogg_init(value _enc, value _serialno) {
  CAMLparam2(_enc, _serialno);

  intnat serialno = Nativeint_val(_serialno);

  ocaml_flac_encoder *enc = Encoder_val(_enc);

  caml_release_runtime_system();
  FLAC__stream_encoder_set_ogg_serial_number(enc->encoder, serialno);
  FLAC__stream_encoder_init_ogg_stream(enc->encoder, NULL, enc_write_callback,
                                       NULL, NULL, NULL,
                                       (void *)&enc->callbacks);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
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
CAMLprim value ocaml_flac_skeleton_fisbone(value serial, value samplerate,
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
  write32le(op.packet + 16, 2);     /* number of header packet, 2 for now. */
  /* granulerate, temporal resolution of the bitstream in Hz */
  write64le(op.packet + 20,
            (ogg_int64_t)Int64_val(samplerate)); /* granulerate numerator */
  write64le(op.packet + 28, (ogg_int64_t)1);     /* granulerate denominator */
  write64le(op.packet + 36, (ogg_int64_t)Int64_val(start)); /* start granule */
  write32le(op.packet + 44, 2); /* preroll, for flac its 2 ??? */
  *(op.packet + 48) = 0;        /* granule shift, always 0 for flac */
  memcpy(op.packet + FISBONE_SIZE, String_val(content),
         caml_string_length(content));

  op.b_o_s = 0;
  op.e_o_s = 0;
  op.bytes = len;

  packet = value_of_packet(&op);
  free(op.packet);
  CAMLreturn(packet);
}
