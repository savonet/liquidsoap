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
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "flac_config.h"
#include "flac_stubs.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#ifndef INT24_MAX
#define INT24_MAX 0x007fffffL
#endif

/* Thank you
 * http://www.linux-nantes.org/~fmonnier/ocaml/ocaml-wrapping-c.php#ref_option
 */
value flac_Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

/* Threads management. */
static pthread_key_t ocaml_c_thread_key;
static pthread_once_t ocaml_c_thread_key_once = PTHREAD_ONCE_INIT;

static void ocaml_flac_on_thread_exit(void *key) { caml_c_thread_unregister(); }

static void ocaml_flac_make_key() {
  pthread_key_create(&ocaml_c_thread_key, ocaml_flac_on_thread_exit);
}

void ocaml_flac_register_thread() {
  static int initialized = 1;

  pthread_once(&ocaml_c_thread_key_once, ocaml_flac_make_key);

  if (caml_c_thread_register() && !pthread_getspecific(ocaml_c_thread_key))
    pthread_setspecific(ocaml_c_thread_key, (void *)&initialized);
}

/* Convenience functions */
#ifdef BIGENDIAN
static inline int16_t bswap_16(int16_t x) {
  return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8));
}
#endif

static inline int16_t clip(double s) {
  if (s < -1)
    return INT16_MIN;

  if (s > 1)
    return INT16_MAX;

  return (s * INT16_MAX);
}

CAMLprim value caml_flac_float_to_s16le(value a) {
  CAMLparam1(a);
  CAMLlocal1(ans);
  int c, i;
  int nc = Wosize_val(a);
  if (nc == 0)
    CAMLreturn(caml_copy_string(""));
  int len = Wosize_val(Field(a, 0)) / Double_wosize;
  ans = caml_alloc_string(2 * len * nc);
  int16_t *dst = (int16_t *)String_val(ans);

  for (c = 0; c < nc; c++) {
    for (i = 0; i < len; i++) {
      dst[i * nc + c] = clip(Double_field(Field(a, c), i));
#ifdef BIGENDIAN
      dst[i * nc + c] = bswap_16(dst[i * nc + c]);
#endif
    }
  }

  CAMLreturn(ans);
}

#define s16tof(x) (((double)x) / INT16_MAX)
#ifdef BIGENDIAN
#define get_s16le(src, nc, c, i) s16tof(bswap_16(((int16_t *)src)[i * nc + c]))
#else
#define get_s16le(src, nc, c, i) s16tof(((int16_t *)src)[i * nc + c])
#endif

CAMLprim value caml_flac_s16le_to_float(value _src, value _chans) {
  CAMLparam1(_src);
  CAMLlocal1(ans);
  char *src = (char *)Bytes_val(_src);
  int chans = Int_val(_chans);
  int samples = caml_string_length(_src) / (2 * chans);
  int i, c;

  ans = caml_alloc_tuple(chans);
  for (c = 0; c < chans; c++)
    Store_field(ans, c, caml_alloc(samples * Double_wosize, Double_array_tag));

  for (c = 0; c < chans; c++)
    for (i = 0; i < samples; i++)
      Store_double_field(Field(ans, c), i, get_s16le(src, chans, c, i));

  CAMLreturn(ans);
}

/* Decoder */

/* polymorphic variant utility macros */
#define get_var(x) caml_hash_variant(#x)

static value val_of_state(int s) {
  switch (s) {
  case FLAC__STREAM_DECODER_SEARCH_FOR_METADATA:
    return get_var(Search_for_metadata);
  case FLAC__STREAM_DECODER_READ_METADATA:
    return get_var(Read_metadata);
  case FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC:
    return get_var(Search_for_frame_sync);
  case FLAC__STREAM_DECODER_READ_FRAME:
    return get_var(Read_frame);
  case FLAC__STREAM_DECODER_END_OF_STREAM:
    return get_var(End_of_stream);
  case FLAC__STREAM_DECODER_OGG_ERROR:
    return get_var(Ogg_error);
  case FLAC__STREAM_DECODER_SEEK_ERROR:
    return get_var(Seek_error);
  case FLAC__STREAM_DECODER_ABORTED:
    return get_var(Aborted);
  case FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR:
    return get_var(Memory_allocation_error);
  case FLAC__STREAM_DECODER_UNINITIALIZED:
    return get_var(Uninitialized);
  default:
    return get_var(Unknown);
  }
}

static value raise_exn_of_error(FLAC__StreamDecoderErrorStatus e) {
  switch (e) {
  case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC:
    caml_raise_constant(*caml_named_value("flac_dec_exn_lost_sync"));
  case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER:
    caml_raise_constant(*caml_named_value("flac_dec_exn_bad_header"));
  case FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH:
    caml_raise_constant(*caml_named_value("flac_dec_exn_crc_mismatch"));
  case FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM:
    caml_raise_constant(*caml_named_value("flac_dec_exn_unparseable_stream"));
  default:
    caml_raise_constant(*caml_named_value("flac_exn_internal"));
  }
}

/* Caml abstract value containing the decoder. */
#define Decoder_val(v) (*((ocaml_flac_decoder **)Data_custom_val(v)))

CAMLprim value ocaml_flac_cleanup_decoder(value e) {
  ocaml_flac_decoder *dec = Decoder_val(e);

  caml_remove_generational_global_root(&dec->callbacks.read_cb);
  caml_remove_generational_global_root(&dec->callbacks.seek_cb);
  caml_remove_generational_global_root(&dec->callbacks.tell_cb);
  caml_remove_generational_global_root(&dec->callbacks.eof_cb);
  caml_remove_generational_global_root(&dec->callbacks.length_cb);
  caml_remove_generational_global_root(&dec->callbacks.write_cb);
  caml_remove_generational_global_root(&dec->callbacks.buffer);
  caml_remove_generational_global_root(&dec->callbacks.output);

  return Val_unit;
}

static void finalize_decoder(value e) {
  ocaml_flac_decoder *dec = Decoder_val(e);

  FLAC__stream_decoder_delete(dec->decoder);
  if (dec->callbacks.info != NULL)
    free(dec->callbacks.info);
  if (dec->callbacks.meta != NULL)
    FLAC__metadata_object_delete(dec->callbacks.meta);

  free(dec);
}

static struct custom_operations decoder_ops = {
    "ocaml_flac_decoder", finalize_decoder,         custom_compare_default,
    custom_hash_default,  custom_serialize_default, custom_deserialize_default};

/* start all the callbacks here. */
void dec_metadata_callback(const FLAC__StreamDecoder *decoder,
                           const FLAC__StreamMetadata *metadata,
                           void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;
  switch (metadata->type) {
  case FLAC__METADATA_TYPE_STREAMINFO:
    if (callbacks->info != NULL) {
      caml_acquire_runtime_system();
      caml_raise_constant(*caml_named_value("flac_exn_internal"));
    }
    callbacks->info = malloc(sizeof(FLAC__StreamMetadata_StreamInfo));
    if (callbacks->info == NULL) {
      // This callback is run in non-blocking mode
      caml_acquire_runtime_system();
      caml_raise_out_of_memory();
    }
    memcpy(callbacks->info, &metadata->data.stream_info,
           sizeof(FLAC__StreamMetadata_StreamInfo));
    break;
  case FLAC__METADATA_TYPE_VORBIS_COMMENT:
    if (callbacks->meta != NULL) {
      caml_acquire_runtime_system();
      caml_raise_constant(*caml_named_value("flac_exn_internal"));
    }
    callbacks->meta = FLAC__metadata_object_clone(metadata);
    if (callbacks->meta == NULL) {
      caml_acquire_runtime_system();
      caml_raise_out_of_memory();
    }
    break;
  default:
    break;
  }
  return;
}

void dec_error_callback(const FLAC__StreamDecoder *decoder,
                        FLAC__StreamDecoderErrorStatus status,
                        void *client_data) {
  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  raise_exn_of_error(status);
  return;
}

static FLAC__StreamDecoderSeekStatus
dec_seek_callback(const FLAC__StreamDecoder *decoder,
                  FLAC__uint64 absolute_byte_offset, void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  if (callbacks->seek_cb == Val_none)
    return FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  caml_callback(callbacks->seek_cb, caml_copy_int64(absolute_byte_offset));
  caml_release_runtime_system();

  return FLAC__STREAM_DECODER_SEEK_STATUS_OK;
}

static FLAC__StreamDecoderTellStatus
dec_tell_callback(const FLAC__StreamDecoder *decoder,
                  FLAC__uint64 *absolute_byte_offset, void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  if (callbacks->tell_cb == Val_none)
    return FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  value ret = caml_callback(callbacks->tell_cb, Val_unit);
  *absolute_byte_offset = (FLAC__uint64)Int64_val(ret);
  caml_release_runtime_system();

  return FLAC__STREAM_DECODER_TELL_STATUS_OK;
}

static FLAC__StreamDecoderLengthStatus
dec_length_callback(const FLAC__StreamDecoder *decoder,
                    FLAC__uint64 *stream_length, void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  if (callbacks->length_cb == Val_none)
    return FLAC__STREAM_DECODER_LENGTH_STATUS_UNSUPPORTED;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  value ret = caml_callback(callbacks->length_cb, Val_unit);
  *stream_length = (FLAC__uint64)Int64_val(ret);
  caml_release_runtime_system();

  return FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
}

static FLAC__bool dec_eof_callback(const FLAC__StreamDecoder *decoder,
                                   void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  if (callbacks->eof_cb == Val_none)
    return false;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  value ret = caml_callback(callbacks->eof_cb, Val_unit);
  caml_release_runtime_system();

  if (ret == Val_true)
    return true;

  return false;
}

FLAC__StreamDecoderReadStatus static dec_read_callback(
    const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes,
    void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();

  int readlen = *bytes;
  if (callbacks->buflen < readlen)
    readlen = callbacks->buflen;

  value ret = caml_callback3(callbacks->read_cb, callbacks->buffer, Val_int(0),
                             Val_int(readlen));

  memcpy(buffer, String_val(callbacks->buffer), Int_val(ret));
  *bytes = Int_val(ret);

  caml_release_runtime_system();

  if (*bytes == 0)
    return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
  else
    return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

static inline double sample_to_double(FLAC__int32 x, unsigned bps) {
  switch (bps) {
  case 8:
    return (((double)x) / INT8_MAX);
  case 16:
    return (((double)x) / INT16_MAX);
  case 24:
    return (((double)x) / INT24_MAX);
  default:
    return (((double)x) / INT32_MAX);
  }
}

FLAC__StreamDecoderWriteStatus
dec_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame,
                   const FLAC__int32 *const buffer[], void *client_data) {
  ocaml_flac_decoder_callbacks *callbacks =
      (ocaml_flac_decoder_callbacks *)client_data;
  int samples = frame->header.blocksize;
  int channels = frame->header.channels;
  int bps = frame->header.bits_per_sample;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();

  int c, i;
  for (c = 0; c < channels; c++) {
    Store_field(callbacks->output, c,
                caml_alloc(samples * Double_wosize, Double_array_tag));
    for (i = 0; i < samples; i++)
      Store_double_field(Field(callbacks->output, c), i,
                         sample_to_double(buffer[c][i], bps));
  }

  caml_callback(callbacks->write_cb, callbacks->output);

  caml_release_runtime_system();

  return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

#define Some_or_none(v) (v == Val_none ? Val_none : Some_val(v))

CAMLprim value ocaml_flac_decoder_alloc_native(value seek, value tell,
                                               value length, value eof,
                                               value read, value write,
                                               value u) {
  CAMLparam5(seek, tell, length, eof, read);
  CAMLxparam1(write);
  CAMLlocal1(ans);

  // Initialize things
  ocaml_flac_decoder *dec = malloc(sizeof(ocaml_flac_decoder));
  if (dec == NULL)
    caml_raise_out_of_memory();

  dec->decoder = FLAC__stream_decoder_new();

  dec->callbacks.seek_cb = Some_or_none(seek);
  caml_register_generational_global_root(&dec->callbacks.seek_cb);

  dec->callbacks.tell_cb = Some_or_none(tell);
  caml_register_generational_global_root(&dec->callbacks.tell_cb);

  dec->callbacks.length_cb = Some_or_none(length);
  caml_register_generational_global_root(&dec->callbacks.length_cb);

  dec->callbacks.eof_cb = Some_or_none(eof);
  caml_register_generational_global_root(&dec->callbacks.eof_cb);

  dec->callbacks.write_cb = write;
  caml_register_generational_global_root(&dec->callbacks.write_cb);

  dec->callbacks.read_cb = read;
  caml_register_generational_global_root(&dec->callbacks.read_cb);

  dec->callbacks.buflen = 1024;
  dec->callbacks.buffer = caml_alloc_string(dec->callbacks.buflen);
  caml_register_generational_global_root(&dec->callbacks.buffer);

  dec->callbacks.output = Val_none;
  caml_register_generational_global_root(&dec->callbacks.output);

  dec->callbacks.info = NULL;
  dec->callbacks.meta = NULL;

  // Accept vorbis comments
  FLAC__stream_decoder_set_metadata_respond(dec->decoder,
                                            FLAC__METADATA_TYPE_VORBIS_COMMENT);

  // Fill custom value
  ans = caml_alloc_custom(&decoder_ops, sizeof(ocaml_flac_decoder *), 1, 0);
  Decoder_val(ans) = dec;

  CAMLreturn(ans);
}

CAMLprim value ocaml_flac_decoder_alloc_bytecode(value *argv, int argn) {
  return ocaml_flac_decoder_alloc_native(argv[0], argv[1], argv[2], argv[3],
                                         argv[4], argv[5], argv[6]);
}

CAMLprim value ocaml_flac_decoder_init(value _dec) {
  CAMLparam1(_dec);

  ocaml_flac_decoder *dec = Decoder_val(_dec);

  // Initialize decoder
  caml_release_runtime_system();
  FLAC__stream_decoder_init_stream(
      dec->decoder, dec_read_callback, dec_seek_callback, dec_tell_callback,
      dec_length_callback, dec_eof_callback, dec_write_callback,
      dec_metadata_callback, dec_error_callback, (void *)&dec->callbacks);
  FLAC__stream_decoder_process_until_end_of_metadata(dec->decoder);
  caml_acquire_runtime_system();

  caml_modify_generational_global_root(
      &dec->callbacks.output, caml_alloc_tuple(dec->callbacks.info->channels));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_flac_decoder_state(value d) {
  CAMLparam1(d);

  ocaml_flac_decoder *dec = Decoder_val(d);

  int ret = FLAC__stream_decoder_get_state(dec->decoder);

  CAMLreturn(val_of_state(ret));
}

CAMLprim value ocaml_flac_decoder_info(value d) {
  CAMLparam1(d);
  CAMLlocal4(ret, m, i, tmp);
  ocaml_flac_decoder *dec = Decoder_val(d);
  FLAC__StreamMetadata_StreamInfo *info = dec->callbacks.info;
  if (info == NULL)
    caml_raise_constant(*caml_named_value("flac_exn_internal"));

  // Info block
  i = caml_alloc_tuple(5);
  Store_field(i, 0, Val_int(info->sample_rate));
  Store_field(i, 1, Val_int(info->channels));
  Store_field(i, 2, Val_int(info->bits_per_sample));
  Store_field(i, 3, caml_copy_int64(info->total_samples));
  tmp = caml_alloc_string(16);
  memcpy(Bytes_val(tmp), info->md5sum, 16);
  Store_field(i, 4, tmp);

  // Comments block
  if (dec->callbacks.meta != NULL) {
    m = caml_alloc_tuple(2);
    FLAC__StreamMetadata_VorbisComment coms =
        dec->callbacks.meta->data.vorbis_comment;
    // First comment is vendor string
    if (coms.vendor_string.entry != NULL)
      Store_field(m, 0, caml_copy_string((char *)coms.vendor_string.entry));
    else
      Store_field(m, 0, caml_copy_string(""));
    // Now the other metadata
    tmp = caml_alloc_tuple(coms.num_comments);
    int i;
    for (i = 0; i < coms.num_comments; i++)
      Store_field(tmp, i, caml_copy_string((char *)coms.comments[i].entry));
    Store_field(m, 1, tmp);
    m = flac_Val_some(m);
  } else
    m = Val_none;

  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, i);
  Store_field(ret, 1, m);

  CAMLreturn(ret);
}

CAMLprim value ocaml_flac_decoder_process(value d) {
  CAMLparam1(d);

  ocaml_flac_decoder *dec = Decoder_val(d);

  // Process one frame
  caml_release_runtime_system();
  FLAC__stream_decoder_process_single(dec->decoder);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_flac_decoder_seek(value d, value pos) {
  CAMLparam2(d, pos);

  FLAC__uint64 offset = Int64_val(pos);
  FLAC_API FLAC__bool ret;

  ocaml_flac_decoder *dec = Decoder_val(d);

  caml_release_runtime_system();
  ret = FLAC__stream_decoder_seek_absolute(dec->decoder, offset);
  caml_acquire_runtime_system();

  if (ret == true)
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

CAMLprim value ocaml_flac_decoder_reset(value d) {
  CAMLparam1(d);

  FLAC_API FLAC__bool ret;

  ocaml_flac_decoder *dec = Decoder_val(d);

  caml_release_runtime_system();
  ret = FLAC__stream_decoder_reset(dec->decoder);
  caml_acquire_runtime_system();

  if (ret == true)
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

CAMLprim value ocaml_flac_decoder_flush(value d) {
  CAMLparam1(d);

  FLAC_API FLAC__bool ret;

  ocaml_flac_decoder *dec = Decoder_val(d);

  caml_release_runtime_system();
  ret = FLAC__stream_decoder_flush(dec->decoder);
  caml_acquire_runtime_system();

  if (ret == true)
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

/* Encoder */

CAMLprim value ocaml_flac_cleanup_encoder(value e) {
  ocaml_flac_encoder *enc = Encoder_val(e);

  caml_remove_generational_global_root(&enc->callbacks.write_cb);
  caml_remove_generational_global_root(&enc->callbacks.seek_cb);
  caml_remove_generational_global_root(&enc->callbacks.tell_cb);
  caml_remove_generational_global_root(&enc->callbacks.buffer);

  return Val_unit;
}

static void finalize_encoder(value e) {
  ocaml_flac_encoder *enc = Encoder_val(e);
  if (enc->encoder != NULL)
    FLAC__stream_encoder_delete(enc->encoder);
  if (enc->meta != NULL)
    FLAC__metadata_object_delete(enc->meta);
  if (enc->buf != NULL)
    free(enc->buf);
  if (enc->lines != NULL)
    free(enc->lines);

  free(enc);
}

static struct custom_operations encoder_ops = {
    "ocaml_flac_encoder", finalize_encoder,         custom_compare_default,
    custom_hash_default,  custom_serialize_default, custom_deserialize_default};

FLAC__StreamEncoderWriteStatus
enc_write_callback(const FLAC__StreamEncoder *encoder,
                   const FLAC__byte buffer[], size_t bytes, unsigned samples,
                   unsigned current_frame, void *client_data)

{
  int pos, len;
  ocaml_flac_encoder_callbacks *callbacks =
      (ocaml_flac_encoder_callbacks *)client_data;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();

  pos = 0;
  while (pos < bytes) {
    len = bytes - pos;

    if (callbacks->buflen < len)
      len = callbacks->buflen;

    memcpy(Bytes_val(callbacks->buffer), buffer + pos, len);
    caml_callback2(callbacks->write_cb, callbacks->buffer, Val_int(len));

    pos += len;
  }

  caml_release_runtime_system();

  return FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
}

FLAC__StreamEncoderSeekStatus
enc_seek_callback(const FLAC__StreamEncoder *encoder,
                  FLAC__uint64 absolute_byte_offset, void *client_data) {
  ocaml_flac_encoder_callbacks *callbacks =
      (ocaml_flac_encoder_callbacks *)client_data;

  if (callbacks->seek_cb == Val_none)
    return FLAC__STREAM_ENCODER_SEEK_STATUS_UNSUPPORTED;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  caml_callback(callbacks->seek_cb, caml_copy_int64(absolute_byte_offset));
  caml_release_runtime_system();

  return FLAC__STREAM_ENCODER_SEEK_STATUS_OK;
}

static FLAC__StreamEncoderTellStatus
enc_tell_callback(const FLAC__StreamEncoder *decoder,
                  FLAC__uint64 *absolute_byte_offset, void *client_data) {
  ocaml_flac_encoder_callbacks *callbacks =
      (ocaml_flac_encoder_callbacks *)client_data;

  if (callbacks->tell_cb == Val_none)
    return FLAC__STREAM_ENCODER_TELL_STATUS_UNSUPPORTED;

  ocaml_flac_register_thread();
  caml_acquire_runtime_system();
  *absolute_byte_offset =
      (FLAC__uint64)Int64_val(caml_callback(callbacks->tell_cb, Val_unit));
  caml_release_runtime_system();

  return FLAC__STREAM_ENCODER_TELL_STATUS_OK;
}

CAMLprim value
ocaml_flac_encoder_vorbiscomment_entry_name_is_legal(value name) {
  CAMLparam1(name);
  CAMLreturn(Val_bool(
      FLAC__format_vorbiscomment_entry_name_is_legal(String_val(name))));
}

CAMLprim value
ocaml_flac_encoder_vorbiscomment_entry_value_is_legal(value _value) {
  CAMLparam1(_value);
  CAMLreturn(Val_bool(FLAC__format_vorbiscomment_entry_value_is_legal(
      (const FLAC__byte *)String_val(_value), caml_string_length(_value))));
}

CAMLprim value ocaml_flac_encoder_alloc(value comments, value seek, value tell,
                                        value write, value params) {
  CAMLparam5(comments, seek, tell, write, params);
  CAMLlocal1(ret);

  FLAC__StreamEncoder *enc = FLAC__stream_encoder_new();
  if (enc == NULL)
    caml_raise_out_of_memory();

  FLAC__stream_encoder_set_channels(enc, Int_val(Field(params, 0)));
  FLAC__stream_encoder_set_bits_per_sample(enc, Int_val(Field(params, 1)));
  FLAC__stream_encoder_set_sample_rate(enc, Int_val(Field(params, 2)));
  if (Field(params, 3) != Val_none)
    FLAC__stream_encoder_set_compression_level(
        enc, Int_val(Some_val(Field(params, 3))));

  if (Field(params, 4) != Val_none)
    FLAC__stream_encoder_set_total_samples_estimate(
        enc, Int64_val(Some_val(Field(params, 4))));

  ocaml_flac_encoder *caml_enc = malloc(sizeof(ocaml_flac_encoder));
  if (caml_enc == NULL) {
    FLAC__stream_encoder_delete(enc);
    caml_raise_out_of_memory();
  }

  caml_enc->encoder = enc;

  caml_enc->callbacks.seek_cb = Some_or_none(seek);
  caml_register_generational_global_root(&caml_enc->callbacks.seek_cb);

  caml_enc->callbacks.tell_cb = Some_or_none(tell);
  caml_register_generational_global_root(&caml_enc->callbacks.tell_cb);

  caml_enc->callbacks.write_cb = write;
  caml_register_generational_global_root(&caml_enc->callbacks.write_cb);

  caml_enc->callbacks.buflen = 1024;
  caml_enc->callbacks.buffer = caml_alloc_string(caml_enc->callbacks.buflen);
  caml_register_generational_global_root(&caml_enc->callbacks.buffer);

  caml_enc->buf = NULL;
  caml_enc->lines = NULL;

  // Fill custom value
  ret = caml_alloc_custom(&encoder_ops, sizeof(ocaml_flac_encoder *), 1, 0);
  Encoder_val(ret) = caml_enc;

  /* Metadata */
  caml_enc->meta =
      FLAC__metadata_object_new(FLAC__METADATA_TYPE_VORBIS_COMMENT);

  if (caml_enc->meta == NULL) {
    FLAC__stream_encoder_delete(enc);
    caml_raise_out_of_memory();
  }
  FLAC__StreamMetadata_VorbisComment_Entry entry;

  /* Vendor string is ignored by libFLAC.. */
  int i;
  for (i = 0; i < Wosize_val(comments); i++) {
    if (!FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair(
            &entry, String_val(Field(Field(comments, i), 0)),
            String_val(Field(Field(comments, i), 1))))
      caml_raise_constant(*caml_named_value("flac_enc_exn_invalid_metadata"));

    FLAC__metadata_object_vorbiscomment_append_comment(caml_enc->meta, entry,
                                                       true);
  }
  FLAC__stream_encoder_set_metadata(enc, &caml_enc->meta, 1);

  CAMLreturn(ret);
}

CAMLprim value ocaml_flac_encoder_init(value _enc) {
  CAMLparam1(_enc);

  ocaml_flac_encoder *enc = Encoder_val(_enc);

  caml_release_runtime_system();
  FLAC__stream_encoder_init_stream(enc->encoder, enc_write_callback,
                                   enc_seek_callback, enc_tell_callback, NULL,
                                   (void *)&enc->callbacks);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

static inline FLAC__int32 sample_from_double(double x, unsigned bps) {
  if (x < -1) {
    x = -1;
  } else if (x > 1) {
    x = 1;
  }

  switch (bps) {
  case 8:
    return x * INT8_MAX;
  case 16:
    return x * INT16_MAX;
  case 24:
    return x * INT24_MAX;
  default:
    return x * INT32_MAX;
  }
}

CAMLprim value ocaml_flac_encoder_process(value _enc, value data, value bps) {
  CAMLparam2(_enc, data);

  ocaml_flac_encoder *enc = Encoder_val(_enc);

  int chans = Wosize_val(data);
  int samples = Wosize_val(Field(data, 0)) / Double_wosize;
  int i;
  int c;

  if (enc->buf != NULL)
    free(enc->buf);
  if (enc->lines != NULL)
    free(enc->lines);

  enc->buf = malloc(chans * sizeof(FLAC__int32 *));
  if (enc->buf == NULL)
    caml_raise_out_of_memory();
  enc->lines = malloc(chans * samples * sizeof(FLAC__int32));
  enc->buf[0] = enc->lines;
  if (enc->lines == NULL)
    caml_raise_out_of_memory();
  for (c = 0; c < chans; c++) {
    if (c > 0)
      enc->buf[c] = enc->buf[c - 1] + samples;
    for (i = 0; i < samples; i++)
      enc->buf[c][i] =
          sample_from_double(Double_field(Field(data, c), i), Int_val(bps));
  }

  caml_release_runtime_system();
  FLAC__stream_encoder_process(enc->encoder,
                               (const FLAC__int32 *const *)enc->buf, samples);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_flac_encoder_finish(value _enc) {
  CAMLparam1(_enc);

  ocaml_flac_encoder *enc = Encoder_val(_enc);

  caml_release_runtime_system();
  FLAC__stream_encoder_finish(enc->encoder);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}
