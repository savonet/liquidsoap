/* Stubs of the avcodec binding, spec/avcodec.md.

   A decoder, an encoder and a bitstream filter instance are custom blocks
   pointing at a native record that holds the FFmpeg context and its use
   guard. Avcodec drives the send and receive steps below: each takes the
   guard for one FFmpeg call and releases the runtime lock around it. */

#include <limits.h>
#include <string.h>

#include "avcodec_stubs.h"

#include <libavcodec/bsf.h>
#include <libavutil/mem.h>
#include <libavutil/pixdesc.h>
#include <libavutil/replaygain.h>

#include "codec_capabilities_stubs.h"
#include "codec_id_stubs.h"
#include "codec_properties_stubs.h"
#include "hw_config_method_stubs.h"

#define TABLE_LENGTH(table) (sizeof(table) / sizeof((table)[0]))

static const ocaml_ffmpeg_variant_entry packet_flag_entries[] = {
    {PVV_Keyframe, AV_PKT_FLAG_KEY},
    {PVV_Corrupt, AV_PKT_FLAG_CORRUPT},
    {PVV_Discard, AV_PKT_FLAG_DISCARD},
    {PVV_Trusted, AV_PKT_FLAG_TRUSTED},
    {PVV_Disposable, AV_PKT_FLAG_DISPOSABLE},
};
static const ocaml_ffmpeg_variant_table packet_flag_table = {
    packet_flag_entries, TABLE_LENGTH(packet_flag_entries),
    "Avcodec.Packet.flag"};

const ocaml_ffmpeg_variant_table *ocaml_avcodec_audio_id_table(void) {
  return codec_id_audio_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avcodec_video_id_table(void) {
  return codec_id_video_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avcodec_subtitle_id_table(void) {
  return codec_id_subtitle_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avcodec_unknown_id_table(void) {
  return codec_id_unknown_table();
}

/* Avcodec.family: an identifier family and the media type of its codecs. */
enum {
  FAMILY_AUDIO,
  FAMILY_VIDEO,
  FAMILY_SUBTITLE,
  FAMILY_UNKNOWN,
  FAMILY_ALL
};

static const ocaml_ffmpeg_variant_table *family_table(value _family) {
  switch (Int_val(_family)) {
  case FAMILY_AUDIO:
    return codec_id_audio_table();
  case FAMILY_VIDEO:
    return codec_id_video_table();
  case FAMILY_SUBTITLE:
    return codec_id_subtitle_table();
  case FAMILY_UNKNOWN:
    return codec_id_unknown_table();
  default:
    return codec_id_codec_id_table();
  }
}

static enum AVMediaType family_media_type(value _family) {
  switch (Int_val(_family)) {
  case FAMILY_AUDIO:
    return AVMEDIA_TYPE_AUDIO;
  case FAMILY_VIDEO:
    return AVMEDIA_TYPE_VIDEO;
  case FAMILY_SUBTITLE:
    return AVMEDIA_TYPE_SUBTITLE;
  default:
    return AVMEDIA_TYPE_DATA;
  }
}

static value some_string(const char *text) {
  if (!text)
    return Val_none;

  return caml_alloc_some(caml_copy_string(text));
}

static value some_int64_unless(int64_t number, int64_t absent) {
  if (number == absent)
    return Val_none;

  return caml_alloc_some(caml_copy_int64(number));
}

static int64_t int64_of_option(value _number, int64_t absent) {
  return Is_some(_number) ? Int64_val(Some_val(_number)) : absent;
}

static value list_of_array(value _array) {
  CAMLparam1(_array);
  CAMLlocal2(_list, _cell);

  _list = Val_emptylist;

  for (mlsize_t i = Wosize_val(_array); i > 0; i--) {
    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, Field(_array, i - 1));
    Store_field(_cell, 1, _list);
    _list = _cell;
  }

  CAMLreturn(_list);
}

CAMLprim value ocaml_avcodec_version(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_version);
  unsigned version = avcodec_version();

  _version = caml_alloc_tuple(3);
  Store_field(_version, 0, Val_int(AV_VERSION_MAJOR(version)));
  Store_field(_version, 1, Val_int(AV_VERSION_MINOR(version)));
  Store_field(_version, 2, Val_int(AV_VERSION_MICRO(version)));

  CAMLreturn(_version);
}

CAMLprim value ocaml_avcodec_flag_qscale(value _unit) {
  (void)_unit;
  return Val_int(AV_CODEC_FLAG_QSCALE);
}

value ocaml_avcodec_wrap_codec(const AVCodec *codec) {
  value _codec;

  if (!codec)
    ocaml_avutil_raise_failure("null codec");

  _codec = caml_alloc(1, Abstract_tag);
  Codec_val(_codec) = codec;

  return _codec;
}

static int codec_matches(const AVCodec *codec, value _family, value _encoder) {
  return codec->type == family_media_type(_family) &&
         (Bool_val(_encoder) ? av_codec_is_encoder(codec)
                             : av_codec_is_decoder(codec));
}

/* The registered codecs of a family and a direction whose identifier has a
   constructor in the family, in FFmpeg's order. */
CAMLprim value ocaml_avcodec_codecs(value _encoder, value _family) {
  CAMLparam2(_family, _encoder);
  CAMLlocal1(_codecs);
  const ocaml_ffmpeg_variant_table *table = family_table(_family);
  const AVCodec *codec;
  void *iterator = NULL;
  mlsize_t count = 0, index = 0;
  value _id;

  while ((codec = av_codec_iterate(&iterator))) {
    if (codec_matches(codec, _family, _encoder) &&
        ocaml_avutil_find_variant(table, codec->id, &_id))
      count++;
  }

  _codecs = caml_alloc_tuple(count);
  iterator = NULL;

  while ((codec = av_codec_iterate(&iterator))) {
    if (codec_matches(codec, _family, _encoder) &&
        ocaml_avutil_find_variant(table, codec->id, &_id))
      Store_field(_codecs, index++, ocaml_avcodec_wrap_codec(codec));
  }

  CAMLreturn(list_of_array(_codecs));
}

static value found_codec(const AVCodec *codec, value _family, value _encoder) {
  if (!codec || codec->type != family_media_type(_family))
    ocaml_avutil_raise_error(Bool_val(_encoder) ? AVERROR_ENCODER_NOT_FOUND
                                                : AVERROR_DECODER_NOT_FOUND);

  return ocaml_avcodec_wrap_codec(codec);
}

CAMLprim value ocaml_avcodec_find_by_name(value _encoder, value _family,
                                          value _name) {
  CAMLparam3(_family, _encoder, _name);
  const AVCodec *codec = Bool_val(_encoder)
                             ? avcodec_find_encoder_by_name(String_val(_name))
                             : avcodec_find_decoder_by_name(String_val(_name));

  CAMLreturn(found_codec(codec, _family, _encoder));
}

CAMLprim value ocaml_avcodec_find_by_id(value _family, value _encoder,
                                        value _id) {
  CAMLparam3(_family, _encoder, _id);
  enum AVCodecID id = (enum AVCodecID)ocaml_avutil_constant_of_variant(
      family_table(_family), _id);
  const AVCodec *codec =
      Bool_val(_encoder) ? avcodec_find_encoder(id) : avcodec_find_decoder(id);

  CAMLreturn(found_codec(codec, _family, _encoder));
}

CAMLprim value ocaml_avcodec_codec_name(value _codec) {
  CAMLparam1(_codec);
  const char *name = Codec_val(_codec)->name;

  CAMLreturn(caml_copy_string(name ? name : ""));
}

CAMLprim value ocaml_avcodec_codec_description(value _codec) {
  CAMLparam1(_codec);
  const char *long_name = Codec_val(_codec)->long_name;

  CAMLreturn(caml_copy_string(long_name ? long_name : ""));
}

CAMLprim value ocaml_avcodec_codec_id(value _family, value _codec) {
  return ocaml_avutil_variant_of_constant(family_table(_family),
                                          Codec_val(_codec)->id);
}

CAMLprim value ocaml_avcodec_string_of_id(value _family, value _id) {
  CAMLparam2(_family, _id);
  enum AVCodecID id = (enum AVCodecID)ocaml_avutil_constant_of_variant(
      family_table(_family), _id);

  CAMLreturn(caml_copy_string(avcodec_get_name(id)));
}

CAMLprim value ocaml_avcodec_capabilities(value _codec) {
  return ocaml_avutil_flags_of_mask(codec_capabilities_table(),
                                    Codec_val(_codec)->capabilities);
}

static int hw_config_values(const AVCodecHWConfig *config, value *_pixel_format,
                            value *_device_type) {
  return ocaml_avutil_find_variant(ocaml_avutil_pixel_format_table(),
                                   config->pix_fmt, _pixel_format) &&
         ocaml_avutil_find_variant(ocaml_avutil_hw_device_type_table(),
                                   config->device_type, _device_type);
}

CAMLprim value ocaml_avcodec_hw_configs(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(_configs, _config);
  const AVCodec *codec = Codec_val(_codec);
  const AVCodecHWConfig *config;
  mlsize_t count = 0, index = 0;
  value _pixel_format, _device_type;

  for (int i = 0; (config = avcodec_get_hw_config(codec, i)); i++) {
    if (hw_config_values(config, &_pixel_format, &_device_type))
      count++;
  }

  _configs = caml_alloc_tuple(count);

  for (int i = 0; (config = avcodec_get_hw_config(codec, i)); i++) {
    if (!hw_config_values(config, &_pixel_format, &_device_type))
      continue;

    _config = caml_alloc_tuple(3);
    Store_field(_config, 0, _pixel_format);
    Store_field(
        _config, 1,
        ocaml_avutil_flags_of_mask(hw_config_method_table(), config->methods));
    Store_field(_config, 2, _device_type);
    Store_field(_configs, index++, _config);
  }

  CAMLreturn(list_of_array(_configs));
}

static value string_list(const char *const *strings) {
  CAMLparam0();
  CAMLlocal1(_strings);
  mlsize_t count = 0;

  while (strings && strings[count])
    count++;

  _strings = caml_alloc_tuple(count);
  for (mlsize_t i = 0; i < count; i++)
    Store_field(_strings, i, caml_copy_string(strings[i]));

  CAMLreturn(list_of_array(_strings));
}

static value profile_list(const AVProfile *profiles) {
  CAMLparam0();
  CAMLlocal2(_profiles, _profile);
  mlsize_t count = 0;

  while (profiles && profiles[count].profile != AV_PROFILE_UNKNOWN)
    count++;

  _profiles = caml_alloc_tuple(count);
  for (mlsize_t i = 0; i < count; i++) {
    _profile = caml_alloc_tuple(2);
    Store_field(_profile, 0, Val_int(profiles[i].profile));
    Store_field(_profile, 1,
                caml_copy_string(profiles[i].name ? profiles[i].name : ""));
    Store_field(_profiles, i, _profile);
  }

  CAMLreturn(list_of_array(_profiles));
}

static value descriptor_option(enum AVCodecID id) {
  CAMLparam0();
  CAMLlocal1(_descriptor);
  const AVCodecDescriptor *descriptor = avcodec_descriptor_get(id);

  if (!descriptor)
    CAMLreturn(Val_none);

  _descriptor = caml_alloc_tuple(6);
  Store_field(_descriptor, 0, Val_MediaType(descriptor->type));
  Store_field(_descriptor, 1,
              caml_copy_string(descriptor->name ? descriptor->name : ""));
  Store_field(_descriptor, 2, some_string(descriptor->long_name));
  Store_field(
      _descriptor, 3,
      ocaml_avutil_flags_of_mask(codec_properties_table(), descriptor->props));
  Store_field(_descriptor, 4, string_list(descriptor->mime_types));
  Store_field(_descriptor, 5, profile_list(descriptor->profiles));

  CAMLreturn(caml_alloc_some(_descriptor));
}

CAMLprim value ocaml_avcodec_descriptor(value _family, value _id) {
  CAMLparam2(_family, _id);
  CAMLreturn(descriptor_option((enum AVCodecID)ocaml_avutil_constant_of_variant(
      family_table(_family), _id)));
}

/* The supported values of one kind a codec declares: FFmpeg counts them, no
   terminator is walked to. Raises on failure. */
static int supported_configs(value _codec, enum AVCodecConfig kind,
                             const void **configs) {
  int count = 0;
  int error = avcodec_get_supported_config(NULL, Codec_val(_codec), kind, 0,
                                           configs, &count);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  return *configs ? count : 0;
}

/* A list of the constructors a table has for [count] enumeration values;
   values with none are left out. */
static value variant_list(const ocaml_ffmpeg_variant_table *table,
                          const int *constants, int count) {
  CAMLparam0();
  CAMLlocal1(_variants);
  mlsize_t found = 0, index = 0;
  value _variant;

  for (int i = 0; i < count; i++)
    found += ocaml_avutil_find_variant(table, constants[i], &_variant);

  _variants = caml_alloc_tuple(found);
  for (int i = 0; i < count; i++) {
    if (ocaml_avutil_find_variant(table, constants[i], &_variant))
      Store_field(_variants, index++, _variant);
  }

  CAMLreturn(list_of_array(_variants));
}

#define SUPPORTED_ENUMERATION(name, kind, table)                               \
  CAMLprim value ocaml_avcodec_supported_##name(value _codec) {                \
    CAMLparam1(_codec);                                                        \
    const void *configs;                                                       \
    int count = supported_configs(_codec, kind, &configs);                     \
                                                                               \
    CAMLreturn(variant_list(table, configs, count));                           \
  }

SUPPORTED_ENUMERATION(sample_formats, AV_CODEC_CONFIG_SAMPLE_FORMAT,
                      ocaml_avutil_sample_format_table())
SUPPORTED_ENUMERATION(pixel_formats, AV_CODEC_CONFIG_PIX_FORMAT,
                      ocaml_avutil_pixel_format_table())
SUPPORTED_ENUMERATION(color_spaces, AV_CODEC_CONFIG_COLOR_SPACE,
                      ocaml_avutil_color_space_table())
SUPPORTED_ENUMERATION(color_ranges, AV_CODEC_CONFIG_COLOR_RANGE,
                      ocaml_avutil_color_range_table())

CAMLprim value ocaml_avcodec_supported_sample_rates(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal1(_rates);
  const void *configs;
  int count = supported_configs(_codec, AV_CODEC_CONFIG_SAMPLE_RATE, &configs);
  const int *rates = configs;

  _rates = caml_alloc_tuple(count);
  for (int i = 0; i < count; i++)
    Store_field(_rates, i, Val_int(rates[i]));

  CAMLreturn(list_of_array(_rates));
}

CAMLprim value ocaml_avcodec_supported_frame_rates(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal1(_rates);
  const void *configs;
  int count = supported_configs(_codec, AV_CODEC_CONFIG_FRAME_RATE, &configs);
  const AVRational *rates = configs;

  _rates = caml_alloc_tuple(count);
  for (int i = 0; i < count; i++)
    Store_field(_rates, i, ocaml_avutil_value_of_rational(rates[i]));

  CAMLreturn(list_of_array(_rates));
}

CAMLprim value ocaml_avcodec_supported_channel_layouts(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal1(_layouts);
  const void *configs;
  int count =
      supported_configs(_codec, AV_CODEC_CONFIG_CHANNEL_LAYOUT, &configs);
  const AVChannelLayout *layouts = configs;

  _layouts = caml_alloc_tuple(count);
  for (int i = 0; i < count; i++)
    Store_field(_layouts, i, ocaml_avutil_copy_channel_layout(&layouts[i]));

  CAMLreturn(list_of_array(_layouts));
}

static void finalize_parameters(value _parameters) {
  avcodec_parameters_free(&CodecParameters_val(_parameters));
}

static struct custom_operations parameters_operations = {
    "ocaml_avcodec_parameters", finalize_parameters,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/* Stores in [_parameters], a registered local of the caller, a handle
   owning blank parameters, and returns them for FFmpeg to fill. */
static AVCodecParameters *alloc_parameters(value *_parameters) {
  AVCodecParameters *parameters;

  *_parameters = caml_alloc_custom(&parameters_operations,
                                   sizeof(AVCodecParameters *), 0, 1);
  CodecParameters_val(*_parameters) = NULL;

  parameters = avcodec_parameters_alloc();
  if (!parameters)
    caml_raise_out_of_memory();
  CodecParameters_val(*_parameters) = parameters;

  return parameters;
}

value ocaml_avcodec_copy_parameters(const AVCodecParameters *source) {
  CAMLparam0();
  CAMLlocal1(_parameters);
  int error;

  if (!source)
    ocaml_avutil_raise_failure("null codec parameters");

  error = avcodec_parameters_copy(alloc_parameters(&_parameters), source);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_parameters);
}

CAMLprim value ocaml_avcodec_parameters_id(value _family, value _parameters) {
  return ocaml_avutil_variant_of_constant(
      family_table(_family), CodecParameters_val(_parameters)->codec_id);
}

CAMLprim value ocaml_avcodec_parameters_descriptor(value _parameters) {
  CAMLparam1(_parameters);
  CAMLreturn(descriptor_option(CodecParameters_val(_parameters)->codec_id));
}

CAMLprim value ocaml_avcodec_parameters_channel_layout(value _parameters) {
  CAMLparam1(_parameters);
  CAMLreturn(ocaml_avutil_copy_channel_layout(
      &CodecParameters_val(_parameters)->ch_layout));
}

CAMLprim value ocaml_avcodec_parameters_nb_channels(value _parameters) {
  return Val_int(CodecParameters_val(_parameters)->ch_layout.nb_channels);
}

CAMLprim value ocaml_avcodec_parameters_sample_format(value _parameters) {
  return Val_SampleFormat(CodecParameters_val(_parameters)->format);
}

CAMLprim value ocaml_avcodec_parameters_bit_rate(value _parameters) {
  return Val_long(CodecParameters_val(_parameters)->bit_rate);
}

CAMLprim value ocaml_avcodec_parameters_sample_rate(value _parameters) {
  return Val_int(CodecParameters_val(_parameters)->sample_rate);
}

CAMLprim value ocaml_avcodec_parameters_width(value _parameters) {
  return Val_int(CodecParameters_val(_parameters)->width);
}

CAMLprim value ocaml_avcodec_parameters_height(value _parameters) {
  return Val_int(CodecParameters_val(_parameters)->height);
}

CAMLprim value ocaml_avcodec_parameters_sample_aspect_ratio(value _parameters) {
  return ocaml_avutil_value_of_rational(
      CodecParameters_val(_parameters)->sample_aspect_ratio);
}

CAMLprim value ocaml_avcodec_parameters_pixel_format(value _parameters) {
  CAMLparam1(_parameters);
  int pixel_format = CodecParameters_val(_parameters)->format;

  if (pixel_format == AV_PIX_FMT_NONE)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(Val_PixelFormat(pixel_format)));
}

static void finalize_packet(value _packet) {
  av_packet_free(&Packet_val(_packet));
}

static struct custom_operations packet_operations = {
    "ocaml_avcodec_packet",     finalize_packet,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

value ocaml_avcodec_wrap_packet(AVPacket *packet) {
  value _packet;

  if (!packet)
    ocaml_avutil_raise_failure("null packet");

  _packet = caml_alloc_custom_mem(&packet_operations, sizeof(AVPacket *),
                                  packet->size > 0 ? packet->size : 0);
  Packet_val(_packet) = packet;

  return _packet;
}

CAMLprim value ocaml_avcodec_packet_create(value _content) {
  CAMLparam1(_content);
  size_t size = caml_string_length(_content);
  AVPacket *packet;
  int error;

  if (size > INT_MAX)
    ocaml_avutil_raise_failure("packet content too large");

  packet = av_packet_alloc();
  if (!packet)
    caml_raise_out_of_memory();

  error = av_new_packet(packet, (int)size);
  if (error < 0) {
    av_packet_free(&packet);
    ocaml_avutil_raise_error(error);
  }
  memcpy(packet->data, String_val(_content), size);

  CAMLreturn(ocaml_avcodec_wrap_packet(packet));
}

CAMLprim value ocaml_avcodec_packet_dup(value _packet) {
  CAMLparam1(_packet);
  AVPacket *packet = av_packet_alloc();
  int error;

  if (!packet)
    caml_raise_out_of_memory();

  error = av_packet_ref(packet, Packet_val(_packet));
  if (error < 0) {
    av_packet_free(&packet);
    ocaml_avutil_raise_error(error);
  }

  CAMLreturn(ocaml_avcodec_wrap_packet(packet));
}

CAMLprim value ocaml_avcodec_packet_content(value _packet) {
  CAMLparam1(_packet);
  const AVPacket *packet = Packet_val(_packet);

  CAMLreturn(caml_alloc_initialized_string(packet->size > 0 ? packet->size : 0,
                                           (const char *)packet->data));
}

CAMLprim value ocaml_avcodec_packet_flags(value _packet) {
  return ocaml_avutil_flags_of_mask(&packet_flag_table,
                                    Packet_val(_packet)->flags);
}

CAMLprim value ocaml_avcodec_packet_set_flags(value _packet, value _flags) {
  Packet_val(_packet)->flags =
      (int)ocaml_avutil_mask_of_flags(&packet_flag_table, _flags);
  return Val_unit;
}

CAMLprim value ocaml_avcodec_packet_size(value _packet) {
  return Val_int(Packet_val(_packet)->size);
}

CAMLprim value ocaml_avcodec_packet_stream_index(value _packet) {
  return Val_int(Packet_val(_packet)->stream_index);
}

CAMLprim value ocaml_avcodec_packet_set_stream_index(value _packet,
                                                     value _index) {
  Packet_val(_packet)->stream_index =
      ocaml_avutil_int_of_value(_index, "stream index");
  return Val_unit;
}

#define PACKET_TIME(field, absent)                                             \
  CAMLprim value ocaml_avcodec_packet_##field(value _packet) {                 \
    CAMLparam1(_packet);                                                       \
    CAMLreturn(some_int64_unless(Packet_val(_packet)->field, absent));         \
  }                                                                            \
                                                                               \
  CAMLprim value ocaml_avcodec_packet_set_##field(value _packet,               \
                                                  value _number) {             \
    Packet_val(_packet)->field = int64_of_option(_number, absent);             \
    return Val_unit;                                                           \
  }

PACKET_TIME(pts, AV_NOPTS_VALUE)
PACKET_TIME(dts, AV_NOPTS_VALUE)
PACKET_TIME(duration, 0)
PACKET_TIME(pos, -1)

/* The payload of a packed dictionary for a (string * string) list, in a
   buffer of [*size] bytes the caller frees. Raises. */
static uint8_t *pack_dictionary(value _pairs, size_t *size) {
  AVDictionary *dictionary = ocaml_avutil_dictionary_of_pairs(_pairs);
  uint8_t *payload = av_packet_pack_dictionary(dictionary, size);

  av_dict_free(&dictionary);
  if (!payload && _pairs != Val_emptylist)
    caml_raise_out_of_memory();
  if (!payload)
    *size = 0;

  return payload;
}

static void add_side_data(AVPacket *packet, enum AVPacketSideDataType type,
                          const uint8_t *payload, size_t size) {
  uint8_t *data = av_packet_new_side_data(packet, type, size);

  if (!data)
    caml_raise_out_of_memory();
  if (size > 0)
    memcpy(data, payload, size);
}

CAMLprim value ocaml_avcodec_packet_add_side_data(value _packet,
                                                  value _side_data) {
  CAMLparam2(_packet, _side_data);
  AVPacket *packet = Packet_val(_packet);
  value _tag = Field(_side_data, 0);
  value _payload = Field(_side_data, 1);

  if (_tag == PVV_Replaygain) {
    AVReplayGain gain;

    gain.track_gain = ocaml_avutil_int_of_value(Field(_payload, 0), "gain");
    gain.track_peak = (uint32_t)Long_val(Field(_payload, 1));
    gain.album_gain = ocaml_avutil_int_of_value(Field(_payload, 2), "gain");
    gain.album_peak = (uint32_t)Long_val(Field(_payload, 3));
    add_side_data(packet, AV_PKT_DATA_REPLAYGAIN, (const uint8_t *)&gain,
                  sizeof(gain));
  } else {
    size_t size;
    uint8_t *payload = pack_dictionary(_payload, &size);
    uint8_t *data = av_packet_new_side_data(packet,
                                            _tag == PVV_Strings_metadata
                                                ? AV_PKT_DATA_STRINGS_METADATA
                                                : AV_PKT_DATA_METADATA_UPDATE,
                                            size);

    if (data && size > 0)
      memcpy(data, payload, size);
    av_free(payload);
    if (!data)
      caml_raise_out_of_memory();
  }

  CAMLreturn(Val_unit);
}

/* The pairs of a packed dictionary; a final string with no terminator is
   accepted. */
static value unpack_dictionary(const uint8_t *payload, size_t size) {
  CAMLparam0();
  CAMLlocal4(_pairs, _last, _cell, _pair);
  size_t position = 0;

  _pairs = Val_emptylist;

  while (position < size) {
    const uint8_t *key = payload + position;
    const uint8_t *key_end = memchr(key, 0, size - position);
    const uint8_t *content, *content_end;

    if (!key_end)
      break;
    content = key_end + 1;
    content_end = memchr(content, 0, payload + size - content);
    if (!content_end)
      content_end = payload + size;

    _pair = caml_alloc_tuple(2);
    Store_field(_pair, 0,
                caml_alloc_initialized_string(key_end - key, (char *)key));
    Store_field(
        _pair, 1,
        caml_alloc_initialized_string(content_end - content, (char *)content));
    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, _pair);
    Store_field(_cell, 1, Val_emptylist);
    if (_pairs == Val_emptylist)
      _pairs = _cell;
    else
      Store_field(_last, 1, _cell);
    _last = _cell;

    position = content_end - payload + 1;
  }

  CAMLreturn(_pairs);
}

static int is_supported_side_data(const AVPacketSideData *side_data) {
  switch (side_data->type) {
  case AV_PKT_DATA_REPLAYGAIN:
    return side_data->size >= sizeof(AVReplayGain);
  case AV_PKT_DATA_STRINGS_METADATA:
  case AV_PKT_DATA_METADATA_UPDATE:
    return 1;
  default:
    return 0;
  }
}

static value side_data_value(const AVPacketSideData *side_data) {
  CAMLparam0();
  CAMLlocal2(_side_data, _payload);
  value _tag;

  if (side_data->type == AV_PKT_DATA_REPLAYGAIN) {
    AVReplayGain gain;

    memcpy(&gain, side_data->data, sizeof(gain));
    _tag = PVV_Replaygain;
    _payload = caml_alloc_tuple(4);
    Store_field(_payload, 0, Val_long(gain.track_gain));
    Store_field(_payload, 1, Val_long(gain.track_peak));
    Store_field(_payload, 2, Val_long(gain.album_gain));
    Store_field(_payload, 3, Val_long(gain.album_peak));
  } else {
    _tag = side_data->type == AV_PKT_DATA_STRINGS_METADATA
               ? PVV_Strings_metadata
               : PVV_Metadata_update;
    _payload = unpack_dictionary(side_data->data, side_data->size);
  }

  _side_data = caml_alloc_tuple(2);
  Store_field(_side_data, 0, _tag);
  Store_field(_side_data, 1, _payload);

  CAMLreturn(_side_data);
}

CAMLprim value ocaml_avcodec_packet_side_data(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(_entries);
  const AVPacket *packet = Packet_val(_packet);
  mlsize_t count = 0, index = 0;

  for (int i = 0; i < packet->side_data_elems; i++)
    count += is_supported_side_data(&packet->side_data[i]);

  _entries = caml_alloc_tuple(count);
  for (int i = 0; i < packet->side_data_elems; i++) {
    if (is_supported_side_data(&packet->side_data[i]))
      Store_field(_entries, index++, side_data_value(&packet->side_data[i]));
  }

  CAMLreturn(list_of_array(_entries));
}

/* The states of spec/avcodec.md §2.4, in the order of Avcodec.state. */
enum { CODEC_OPEN, CODEC_DRAINING, CODEC_DRAINED };

typedef struct {
  AVCodecContext *context;
  ocaml_avutil_guard guard;
  atomic_int state;
} codec_handle;

#define CodecHandle_val(v) (*(codec_handle **)Data_custom_val(v))

static void finalize_codec_handle(value _handle) {
  codec_handle *handle = CodecHandle_val(_handle);

  if (handle) {
    avcodec_free_context(&handle->context);
    av_free(handle);
  }
}

static struct custom_operations codec_handle_operations = {
    "ocaml_avcodec_codec_context", finalize_codec_handle,
    custom_compare_default,        custom_hash_default,
    custom_serialize_default,      custom_deserialize_default,
    custom_compare_ext_default,    custom_fixed_length_default};

/* Stores in [_handle], a registered local of the caller, a decoder or
   encoder value owning a context allocated for [codec], not opened. */
static codec_handle *alloc_codec_handle(value *_handle, const AVCodec *codec) {
  codec_handle *handle;

  *_handle =
      caml_alloc_custom(&codec_handle_operations, sizeof(codec_handle *), 0, 1);
  CodecHandle_val(*_handle) = NULL;

  handle = av_mallocz(sizeof(*handle));
  if (!handle)
    caml_raise_out_of_memory();
  CodecHandle_val(*_handle) = handle;

  handle->context = avcodec_alloc_context3(codec);
  if (!handle->context)
    caml_raise_out_of_memory();

  return handle;
}

int ocaml_avcodec_open(AVCodecContext *context, const AVCodec *codec,
                       AVDictionary **options) {
  int error;

  context->thread_count = 0;

  caml_release_runtime_system();
  error = avcodec_open2(context, codec, options);
  caml_acquire_runtime_system();

  return error;
}

CAMLprim value ocaml_avcodec_create_decoder(value _parameters, value _codec) {
  CAMLparam2(_parameters, _codec);
  CAMLlocal1(_decoder);
  const AVCodec *codec = Codec_val(_codec);
  codec_handle *handle = alloc_codec_handle(&_decoder, codec);
  int error = 0;

  if (Is_some(_parameters))
    error = avcodec_parameters_to_context(
        handle->context, CodecParameters_val(Some_val(_parameters)));
  if (error >= 0)
    error = ocaml_avcodec_open(handle->context, codec, NULL);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_decoder);
}

/* Opens the encoder of [_encoder] with the caller's option bindings and
   returns (encoder, unused keys). */
static value open_encoder(value _encoder, value _options,
                          const AVCodec *codec) {
  CAMLparam2(_encoder, _options);
  CAMLlocal2(_result, _unused);
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  int error =
      ocaml_avcodec_open(CodecHandle_val(_encoder)->context, codec, &options);

  if (error < 0) {
    av_dict_free(&options);
    ocaml_avutil_raise_error(error);
  }

  _unused = ocaml_avutil_unused_options(&options);
  _result = caml_alloc_tuple(2);
  Store_field(_result, 0, _encoder);
  Store_field(_result, 1, _unused);

  CAMLreturn(_result);
}

int ocaml_avcodec_set_audio_encoding(AVCodecContext *context,
                                     const AVChannelLayout *layout,
                                     int sample_rate,
                                     enum AVSampleFormat sample_format,
                                     AVRational time_base) {
  context->sample_fmt = sample_format;
  context->sample_rate = sample_rate;
  context->time_base = time_base;

  return av_channel_layout_copy(&context->ch_layout, layout);
}

CAMLprim value ocaml_avcodec_create_audio_encoder(value _options, value _layout,
                                                  value _sample_rate,
                                                  value _sample_format,
                                                  value _time_base,
                                                  value _codec) {
  CAMLparam5(_options, _layout, _sample_rate, _sample_format, _time_base);
  CAMLxparam1(_codec);
  CAMLlocal1(_encoder);
  const AVCodec *codec = Codec_val(_codec);
  enum AVSampleFormat sample_format = SampleFormat_val(_sample_format);
  int sample_rate = ocaml_avutil_int_of_value(_sample_rate, "sample rate");
  AVRational time_base = ocaml_avutil_rational_of_value(_time_base);
  codec_handle *handle = alloc_codec_handle(&_encoder, codec);
  int error = ocaml_avcodec_set_audio_encoding(
      handle->context, ChannelLayout_val(_layout), sample_rate, sample_format,
      time_base);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(open_encoder(_encoder, _options, codec));
}

CAMLprim value ocaml_avcodec_create_audio_encoder_bytecode(value *arguments,
                                                           int count) {
  (void)count;
  return ocaml_avcodec_create_audio_encoder(arguments[0], arguments[1],
                                            arguments[2], arguments[3],
                                            arguments[4], arguments[5]);
}

int ocaml_avcodec_set_video_encoding(AVCodecContext *context,
                                     enum AVPixelFormat pixel_format, int width,
                                     int height, AVRational time_base,
                                     AVRational frame_rate,
                                     value _hardware_context) {
  context->pix_fmt = pixel_format;
  context->width = width;
  context->height = height;
  context->time_base = time_base;
  if (frame_rate.num != 0)
    context->framerate = frame_rate;

  if (Is_some(_hardware_context)) {
    value _context = Some_val(_hardware_context);
    AVBufferRef *reference = av_buffer_ref(HwContext_val(Field(_context, 1)));

    if (!reference)
      return AVERROR(ENOMEM);
    if (Field(_context, 0) == PVV_Device_context)
      context->hw_device_ctx = reference;
    else
      context->hw_frames_ctx = reference;
  }

  return 0;
}

CAMLprim value ocaml_avcodec_create_video_encoder(
    value _options, value _frame_rate, value _hardware_context,
    value _pixel_format, value _width, value _height, value _time_base,
    value _codec) {
  CAMLparam5(_options, _frame_rate, _hardware_context, _pixel_format, _width);
  CAMLxparam3(_height, _time_base, _codec);
  CAMLlocal1(_encoder);
  const AVCodec *codec = Codec_val(_codec);
  enum AVPixelFormat pixel_format = PixelFormat_val(_pixel_format);
  int width = ocaml_avutil_int_of_value(_width, "width");
  int height = ocaml_avutil_int_of_value(_height, "height");
  AVRational time_base = ocaml_avutil_rational_of_value(_time_base);
  AVRational frame_rate = {0, 1};
  codec_handle *handle;
  int error;

  if (Is_some(_frame_rate))
    frame_rate = ocaml_avutil_rational_of_value(Some_val(_frame_rate));

  handle = alloc_codec_handle(&_encoder, codec);
  error = ocaml_avcodec_set_video_encoding(handle->context, pixel_format, width,
                                           height, time_base, frame_rate,
                                           _hardware_context);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(open_encoder(_encoder, _options, codec));
}

CAMLprim value ocaml_avcodec_create_video_encoder_bytecode(value *arguments,
                                                           int count) {
  (void)count;
  return ocaml_avcodec_create_video_encoder(
      arguments[0], arguments[1], arguments[2], arguments[3], arguments[4],
      arguments[5], arguments[6], arguments[7]);
}

static codec_handle *shared_codec(value _codec) {
  codec_handle *handle = CodecHandle_val(_codec);

  if (!ocaml_avutil_guard_try_shared(&handle->guard))
    ocaml_avutil_raise_in_use();

  return handle;
}

static codec_handle *exclusive_codec(value _codec) {
  codec_handle *handle = CodecHandle_val(_codec);

  if (!ocaml_avutil_guard_try_exclusive(&handle->guard))
    ocaml_avutil_raise_in_use();

  return handle;
}

CAMLprim value ocaml_avcodec_codec_state(value _codec) {
  return Val_int(atomic_load(&CodecHandle_val(_codec)->state));
}

CAMLprim value ocaml_avcodec_encoder_parameters(value _encoder) {
  CAMLparam1(_encoder);
  CAMLlocal1(_parameters);
  AVCodecParameters *parameters = alloc_parameters(&_parameters);
  codec_handle *handle = shared_codec(_encoder);
  int error = avcodec_parameters_from_context(parameters, handle->context);

  ocaml_avutil_guard_release_shared(&handle->guard);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_parameters);
}

CAMLprim value ocaml_avcodec_encoder_time_base(value _encoder) {
  codec_handle *handle = shared_codec(_encoder);
  AVRational time_base = handle->context->time_base;

  ocaml_avutil_guard_release_shared(&handle->guard);

  return ocaml_avutil_value_of_rational(time_base);
}

CAMLprim value ocaml_avcodec_encoder_frame_size(value _encoder) {
  codec_handle *handle = shared_codec(_encoder);
  int frame_size = handle->context->frame_size;

  ocaml_avutil_guard_release_shared(&handle->guard);

  return Val_int(frame_size);
}

CAMLprim value ocaml_avcodec_decoder_sample_format(value _decoder) {
  codec_handle *handle = shared_codec(_decoder);
  enum AVSampleFormat sample_format = handle->context->sample_fmt;

  ocaml_avutil_guard_release_shared(&handle->guard);

  return Val_SampleFormat(sample_format);
}

/* The result of a send: true when accepted, false when the codec wants its
   output read first. Raises on any other failure. */
static value send_result(int error) {
  if (error == AVERROR(EAGAIN))
    return Val_false;
  if (error < 0)
    ocaml_avutil_raise_error(error);

  return Val_true;
}

/* [_packet] is a packet option: None signals the end of the stream, and
   the decoder is draining once FFmpeg accepted it. */
CAMLprim value ocaml_avcodec_send_packet(value _decoder, value _packet) {
  CAMLparam2(_decoder, _packet);
  codec_handle *handle = exclusive_codec(_decoder);
  const AVPacket *packet =
      Is_some(_packet) ? Packet_val(Some_val(_packet)) : NULL;
  int error;

  caml_release_runtime_system();
  error = avcodec_send_packet(handle->context, packet);
  caml_acquire_runtime_system();

  if (error >= 0 && !packet)
    atomic_store(&handle->state, CODEC_DRAINING);
  ocaml_avutil_guard_release_exclusive(&handle->guard);

  CAMLreturn(send_result(error));
}

/* The pool frame gets the pixel data and the properties, timestamps among
   them: av_hwframe_transfer_data copies no property. */
static int send_uploaded_frame(AVCodecContext *context, const AVFrame *frame) {
  AVFrame *uploaded = av_frame_alloc();
  int error;

  if (!uploaded)
    return AVERROR(ENOMEM);

  error = av_hwframe_get_buffer(context->hw_frames_ctx, uploaded, 0);
  if (error >= 0)
    error = av_hwframe_transfer_data(uploaded, frame, 0);
  if (error >= 0)
    error = av_frame_copy_props(uploaded, frame);
  if (error >= 0)
    error = avcodec_send_frame(context, uploaded);

  av_frame_free(&uploaded);

  return error;
}

int ocaml_avcodec_encoder_send(AVCodecContext *context, const AVFrame *frame) {
  if (frame && context->hw_frames_ctx)
    return send_uploaded_frame(context, frame);

  return avcodec_send_frame(context, frame);
}

/* As ocaml_avcodec_send_packet, with a frame option and an encoder. */
CAMLprim value ocaml_avcodec_send_frame(value _encoder, value _frame) {
  CAMLparam2(_encoder, _frame);
  codec_handle *handle = exclusive_codec(_encoder);
  const AVFrame *frame = Is_some(_frame) ? Frame_val(Some_val(_frame)) : NULL;
  int error;

  caml_release_runtime_system();
  error = ocaml_avcodec_encoder_send(handle->context, frame);
  caml_acquire_runtime_system();

  if (error >= 0 && !frame)
    atomic_store(&handle->state, CODEC_DRAINING);
  ocaml_avutil_guard_release_exclusive(&handle->guard);

  CAMLreturn(send_result(error));
}

/* Ends a receive: returns 1 with something received and 0 when the codec
   has nothing ready, drained once it reports the end of the stream. Raises
   on any other failure. The guard is released in every case. */
static int received(codec_handle *handle, int error) {
  if (error == AVERROR_EOF)
    atomic_store(&handle->state, CODEC_DRAINED);
  ocaml_avutil_guard_release_exclusive(&handle->guard);

  if (error == AVERROR(EAGAIN) || error == AVERROR_EOF)
    return 0;
  if (error < 0)
    ocaml_avutil_raise_error(error);

  return 1;
}

CAMLprim value ocaml_avcodec_receive_frame(value _decoder) {
  CAMLparam1(_decoder);
  codec_handle *handle = exclusive_codec(_decoder);
  AVFrame *frame = av_frame_alloc();
  int error = AVERROR(ENOMEM);

  if (frame) {
    caml_release_runtime_system();
    error = avcodec_receive_frame(handle->context, frame);
    caml_acquire_runtime_system();
  }
  if (error < 0)
    av_frame_free(&frame);

  if (!received(handle, error))
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_avutil_wrap_frame(frame)));
}

CAMLprim value ocaml_avcodec_receive_packet(value _encoder) {
  CAMLparam1(_encoder);
  codec_handle *handle = exclusive_codec(_encoder);
  AVPacket *packet = av_packet_alloc();
  int error = AVERROR(ENOMEM);

  if (packet) {
    caml_release_runtime_system();
    error = avcodec_receive_packet(handle->context, packet);
    caml_acquire_runtime_system();
  }
  if (error < 0)
    av_packet_free(&packet);

  if (!received(handle, error))
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_avcodec_wrap_packet(packet)));
}

typedef struct {
  AVBSFContext *context;
  ocaml_avutil_guard guard;
} filter_handle;

#define FilterHandle_val(v) (*(filter_handle **)Data_custom_val(v))

static void finalize_filter_handle(value _handle) {
  filter_handle *handle = FilterHandle_val(_handle);

  if (handle) {
    av_bsf_free(&handle->context);
    av_free(handle);
  }
}

static struct custom_operations filter_handle_operations = {
    "ocaml_avcodec_bitstream_filter", finalize_filter_handle,
    custom_compare_default,           custom_hash_default,
    custom_serialize_default,         custom_deserialize_default,
    custom_compare_ext_default,       custom_fixed_length_default};

static value filter_codec_ids(const enum AVCodecID *ids) {
  int count = 0;

  while (ids && ids[count] != AV_CODEC_ID_NONE)
    count++;

  return variant_list(codec_id_codec_id_table(), (const int *)ids, count);
}

/* The Avcodec.BitstreamFilter.filter records of every registered filter. */
CAMLprim value ocaml_avcodec_bitstream_filters(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal2(_filters, _filter);
  const AVBitStreamFilter *filter;
  void *iterator = NULL;
  mlsize_t count = 0;

  while (av_bsf_iterate(&iterator))
    count++;

  _filters = caml_alloc_tuple(count);
  iterator = NULL;

  for (mlsize_t i = 0; i < count; i++) {
    filter = av_bsf_iterate(&iterator);
    _filter = caml_alloc_tuple(3);
    Store_field(_filter, 0, caml_copy_string(filter->name ? filter->name : ""));
    Store_field(_filter, 1, filter_codec_ids(filter->codec_ids));
    Store_field(_filter, 2, ocaml_avutil_wrap_option_class(filter->priv_class));
    Store_field(_filters, i, _filter);
  }

  CAMLreturn(list_of_array(_filters));
}

/* Returns (instance, output parameters, unused option keys). */
CAMLprim value ocaml_avcodec_bitstream_filter_init(value _options, value _name,
                                                   value _parameters) {
  CAMLparam3(_options, _name, _parameters);
  CAMLlocal4(_result, _filter, _output, _unused);
  const AVBitStreamFilter *filter = av_bsf_get_by_name(String_val(_name));
  AVCodecParameters *output = alloc_parameters(&_output);
  AVDictionary *options;
  filter_handle *handle;
  int error;

  if (!filter)
    ocaml_avutil_raise_error(AVERROR_BSF_NOT_FOUND);

  _filter = caml_alloc_custom(&filter_handle_operations,
                              sizeof(filter_handle *), 0, 1);
  FilterHandle_val(_filter) = NULL;
  handle = av_mallocz(sizeof(*handle));
  if (!handle)
    caml_raise_out_of_memory();
  FilterHandle_val(_filter) = handle;

  error = av_bsf_alloc(filter, &handle->context);
  if (error >= 0)
    error = avcodec_parameters_copy(handle->context->par_in,
                                    CodecParameters_val(_parameters));
  if (error < 0)
    ocaml_avutil_raise_error(error);

  options = ocaml_avutil_dictionary_of_options(_options);
  error = av_opt_set_dict2(handle->context, &options, AV_OPT_SEARCH_CHILDREN);
  if (error >= 0) {
    caml_release_runtime_system();
    error = av_bsf_init(handle->context);
    caml_acquire_runtime_system();
  }
  if (error >= 0)
    error = avcodec_parameters_copy(output, handle->context->par_out);
  if (error < 0) {
    av_dict_free(&options);
    ocaml_avutil_raise_error(error);
  }

  _unused = ocaml_avutil_unused_options(&options);
  _result = caml_alloc_tuple(3);
  Store_field(_result, 0, _filter);
  Store_field(_result, 1, _output);
  Store_field(_result, 2, _unused);

  CAMLreturn(_result);
}

static filter_handle *exclusive_filter(value _filter) {
  filter_handle *handle = FilterHandle_val(_filter);

  if (!ocaml_avutil_guard_try_exclusive(&handle->guard))
    ocaml_avutil_raise_in_use();

  return handle;
}

/* av_bsf_send_packet moves the content of the packet it is given, so the
   filter gets a reference of its own. [_packet] is a packet option, None
   for the end of the stream. */
CAMLprim value ocaml_avcodec_bitstream_filter_send(value _filter,
                                                   value _packet) {
  CAMLparam2(_filter, _packet);
  filter_handle *handle = exclusive_filter(_filter);
  AVPacket *reference = NULL;
  int error = 0;

  if (Is_some(_packet)) {
    reference = av_packet_alloc();
    error = reference ? av_packet_ref(reference, Packet_val(Some_val(_packet)))
                      : AVERROR(ENOMEM);
  }

  if (error >= 0) {
    caml_release_runtime_system();
    error = av_bsf_send_packet(handle->context, reference);
    caml_acquire_runtime_system();
  }

  av_packet_free(&reference);
  ocaml_avutil_guard_release_exclusive(&handle->guard);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_bitstream_filter_receive(value _filter) {
  CAMLparam1(_filter);
  filter_handle *handle = exclusive_filter(_filter);
  AVPacket *packet = av_packet_alloc();
  int error = AVERROR(ENOMEM);

  if (packet) {
    caml_release_runtime_system();
    error = av_bsf_receive_packet(handle->context, packet);
    caml_acquire_runtime_system();
  }

  ocaml_avutil_guard_release_exclusive(&handle->guard);
  if (error < 0) {
    av_packet_free(&packet);
    ocaml_avutil_raise_error(error);
  }

  CAMLreturn(ocaml_avcodec_wrap_packet(packet));
}
