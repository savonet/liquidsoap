/* Doubles for the avcodec conformance tests: FFmpeg's own answers, read
   beside the binding. */

#include <string.h>

#include "avcodec_stubs.h"

static const ocaml_ffmpeg_variant_table *test_family_table(int family) {
  switch (family) {
  case 0:
    return ocaml_avcodec_audio_id_table();
  case 1:
    return ocaml_avcodec_video_id_table();
  case 2:
    return ocaml_avcodec_subtitle_id_table();
  default:
    return ocaml_avcodec_unknown_id_table();
  }
}

static const enum AVMediaType test_family_types[] = {
    AVMEDIA_TYPE_AUDIO, AVMEDIA_TYPE_VIDEO, AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_DATA};

/* The names of the codecs FFmpeg registers for a family and a direction,
   whose identifier is in the family. */
CAMLprim value test_registered_codecs(value _family, value _encoder) {
  CAMLparam2(_family, _encoder);
  CAMLlocal2(_names, _cell);
  const ocaml_ffmpeg_variant_table *table = test_family_table(Int_val(_family));
  const AVCodec *codec;
  void *iterator = NULL;
  value _id;

  _names = Val_emptylist;

  while ((codec = av_codec_iterate(&iterator))) {
    int direction = Bool_val(_encoder) ? av_codec_is_encoder(codec)
                                       : av_codec_is_decoder(codec);

    if (!direction || codec->type != test_family_types[Int_val(_family)] ||
        !ocaml_avutil_find_variant(table, codec->id, &_id))
      continue;

    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, caml_copy_string(codec->name));
    Store_field(_cell, 1, _names);
    _names = _cell;
  }

  CAMLreturn(_names);
}

CAMLprim value test_id_roundtrip(value _family, value _id) {
  const ocaml_ffmpeg_variant_table *table = test_family_table(Int_val(_family));

  return ocaml_avutil_variant_of_constant(
      table, ocaml_avutil_constant_of_variant(table, _id));
}

/* The number of supported values FFmpeg counts for a codec; the kinds are
   in the order of the test's list. */
CAMLprim value test_supported_count(value _codec, value _kind) {
  static const enum AVCodecConfig kinds[] = {
      AV_CODEC_CONFIG_PIX_FORMAT,     AV_CODEC_CONFIG_FRAME_RATE,
      AV_CODEC_CONFIG_SAMPLE_RATE,    AV_CODEC_CONFIG_SAMPLE_FORMAT,
      AV_CODEC_CONFIG_CHANNEL_LAYOUT, AV_CODEC_CONFIG_COLOR_RANGE,
      AV_CODEC_CONFIG_COLOR_SPACE};
  const void *configs = NULL;
  int count = 0;

  avcodec_get_supported_config(NULL, Codec_val(_codec), kinds[Int_val(_kind)],
                               0, &configs, &count);

  return Val_int(configs ? count : 0);
}

/* The dictionary FFmpeg unpacks from the metadata side data of a packet. */
CAMLprim value test_unpack_metadata(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(_pairs);
  AVDictionary *dictionary = NULL;
  size_t size = 0;
  const uint8_t *data = av_packet_get_side_data(
      Packet_val(_packet), AV_PKT_DATA_STRINGS_METADATA, &size);
  int error = av_packet_unpack_dictionary(data, size, &dictionary);

  if (!data || error < 0) {
    av_dict_free(&dictionary);
    ocaml_avutil_raise_failure("FFmpeg rejects the packed dictionary");
  }

  _pairs = ocaml_avutil_pairs_of_dictionary(dictionary);
  av_dict_free(&dictionary);

  CAMLreturn(_pairs);
}

CAMLprim value test_parameters_frame_rate(value _parameters) {
  return ocaml_avutil_value_of_rational(
      CodecParameters_val(_parameters)->framerate);
}

CAMLprim value test_zero_frame(value _frame) {
  AVFrame *frame = Frame_val(_frame);

  for (int i = 0; i < AV_NUM_DATA_POINTERS; i++) {
    if (frame->buf[i])
      memset(frame->buf[i]->data, 0, frame->buf[i]->size);
  }

  return Val_unit;
}

CAMLprim value test_wrap_null_codec_objects(value _kind) {
  switch (Int_val(_kind)) {
  case 0:
    return ocaml_avcodec_wrap_codec(NULL);
  case 1:
    return ocaml_avcodec_copy_parameters(NULL);
  default:
    return ocaml_avcodec_wrap_packet(NULL);
  }
}
