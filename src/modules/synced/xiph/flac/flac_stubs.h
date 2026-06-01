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

#include <pthread.h>

#include <FLAC/format.h>
#include <FLAC/metadata.h>
#include <FLAC/stream_decoder.h>
#include <FLAC/stream_encoder.h>

#include <caml/mlvalues.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
value flac_Val_some(value v);

/* Decoder */

typedef struct ocaml_flac_decoder_callbacks {
  /* This is used for callback from caml. */
  value read_cb;
  value seek_cb;
  value tell_cb;
  value length_cb;
  value eof_cb;
  value write_cb;
  value output;
  value buffer;
  int buflen;
  FLAC__StreamMetadata_StreamInfo *info;
  FLAC__StreamMetadata *meta;
} ocaml_flac_decoder_callbacks;

typedef struct ocaml_flac_decoder {
  FLAC__StreamDecoder *decoder;
  ocaml_flac_decoder_callbacks callbacks;
} ocaml_flac_decoder;

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder **)Data_custom_val(v)))

void dec_metadata_callback(const FLAC__StreamDecoder *decoder,
                           const FLAC__StreamMetadata *metadata,
                           void *client_data);

FLAC__StreamDecoderWriteStatus
dec_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame,
                   const FLAC__int32 *const buffer[], void *client_data);

void dec_error_callback(const FLAC__StreamDecoder *decoder,
                        FLAC__StreamDecoderErrorStatus status,
                        void *client_data);

/* Encoder */

typedef struct ocaml_flac_encoder_callbacks {
  value write_cb;
  value seek_cb;
  value tell_cb;
  value buffer;
  int buflen;
} ocaml_flac_encoder_callbacks;

typedef struct ocaml_flac_encoder {
  FLAC__StreamEncoder *encoder;
  FLAC__StreamMetadata *meta;
  FLAC__int32 **buf;
  FLAC__int32 *lines;
  ocaml_flac_encoder_callbacks callbacks;
} ocaml_flac_encoder;

/* Caml abstract value containing the decoder. */
#define Encoder_val(v) (*((ocaml_flac_encoder **)Data_custom_val(v)))

value ocaml_flac_encoder_alloc(value comments, value seek, value tell,
                               value write, value params);

FLAC__StreamEncoderWriteStatus
enc_write_callback(const FLAC__StreamEncoder *encoder,
                   const FLAC__byte buffer[], size_t bytes, unsigned samples,
                   unsigned current_frame, void *client_data);

/* Threads management */
void ocaml_flac_register_thread();
