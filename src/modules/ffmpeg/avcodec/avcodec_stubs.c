#define CAML_NAME_SPACE 1

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "avcodec_stubs.h"
#include "avutil_stubs.h"
#include "codec_capabilities_stubs.h"
#include "codec_id_stubs.h"
#include "codec_properties_stubs.h"
#include "hw_config_method_stubs.h"
#include "media_types_stubs.h"

#include <libavcodec/bsf.h>
#include <libavutil/replaygain.h>

#ifndef AV_PKT_FLAG_DISPOSABLE
#define AV_PKT_FLAG_DISPOSABLE 0x0010
#endif

value ocaml_avcodec_init(value unit) {
#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(58, 9, 100)
  avcodec_register_all();
#endif
  return Val_unit;
}

CAMLprim value ocaml_avcodec_flag_qscale(value unit) {
  return Val_int(AV_CODEC_FLAG_QSCALE);
}

CAMLprim value ocaml_avcodec_subtitle_codec_id_to_AVCodecID(value _codec_id) {
  return Val_int(SubtitleCodecID_val(_codec_id));
}

/***** AVCodecContext *****/

static AVCodecContext *create_AVCodecContext(AVCodecParameters *params,
                                             const AVCodec *codec) {
  AVCodecContext *codec_context;
  int ret = 0;

  codec_context = avcodec_alloc_context3(codec);

  if (!codec_context)
    caml_raise_out_of_memory();

  if (params)
    ret = avcodec_parameters_to_context(codec_context, params);

  if (ret < 0) {
    avcodec_free_context(&codec_context);
    ocaml_avutil_raise_error(ret);
  }

  // Open the codec
  caml_release_runtime_system();
  ret = avcodec_open2(codec_context, codec, NULL);
  caml_acquire_runtime_system();

  if (ret < 0) {
    avcodec_free_context(&codec_context);
    ocaml_avutil_raise_error(ret);
  }

  return codec_context;
}

/***** AVCodecParameters *****/

static void finalize_codec_parameters(value v) {
  struct AVCodecParameters *codec_parameters = CodecParameters_val(v);
  avcodec_parameters_free(&codec_parameters);
}

static struct custom_operations codec_parameters_ops = {
    "ocaml_avcodec_parameters", finalize_codec_parameters,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default};

void value_of_codec_parameters_copy(AVCodecParameters *src, value *pvalue) {
  if (!src)
    Fail("Failed to get codec parameters");

  AVCodecParameters *dst = avcodec_parameters_alloc();

  if (!dst)
    caml_raise_out_of_memory();

  int ret = avcodec_parameters_copy(dst, src);

  if (ret < 0) {
    avcodec_parameters_free(&dst);
    ocaml_avutil_raise_error(ret);
  }

  *pvalue = caml_alloc_custom(&codec_parameters_ops,
                              sizeof(AVCodecParameters *), 0, 1);
  CodecParameters_val(*pvalue) = dst;
}

/***** AVPacket *****/

static void finalize_packet(value v) {
  struct AVPacket *packet = Packet_val(v);
  av_packet_free(&packet);
}

static struct custom_operations packet_ops = {
    "ocaml_packet",      finalize_packet,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

value value_of_ffmpeg_packet(value *ret, AVPacket *packet) {
  if (!packet)
    Fail("Empty packet");

  int size = 0;

  if (packet->buf)
    size = packet->buf->size;

  *ret = caml_alloc_custom_mem(&packet_ops, sizeof(AVPacket *), size);
  Packet_val(*ret) = packet;
  return *ret;
}

CAMLprim value ocaml_avcodec_create_packet(value _data) {
  CAMLparam1(_data);
  CAMLlocal1(ret);
  AVPacket *packet;
  int len = caml_string_length(_data);

  packet = av_packet_alloc();
  if (!packet)
    caml_raise_out_of_memory();

  int err = av_new_packet(packet, len);
  if (err != 0) {
    av_freep(packet);
    ocaml_avutil_raise_error(err);
  }

  memcpy(packet->data, String_val(_data), len);

  CAMLreturn(value_of_ffmpeg_packet(&ret, packet));
}

CAMLprim value ocaml_avcodec_packet_content(value _packet) {
  CAMLparam1(_packet);
  AVPacket *packet = Packet_val(_packet);
  CAMLreturn(caml_alloc_initialized_string(packet->size, (char *)packet->data));
}

CAMLprim value ocaml_avcodec_packet_add_side_data(value _packet,
                                                  value _side_data) {
  CAMLparam2(_packet, _side_data);
  AVPacket *packet = Packet_val(_packet);
  enum AVPacketSideDataType type;
  uint8_t *data;
  AVReplayGain *replay_gain;
  size_t len;

  switch (Field(_side_data, 0)) {
  case PVV_Metadata_update:
    type = AV_PKT_DATA_METADATA_UPDATE;
    break;
  case PVV_Strings_metadata:
    type = AV_PKT_DATA_STRINGS_METADATA;
    break;
  case PVV_Replaygain:
    type = AV_PKT_DATA_REPLAYGAIN;
    break;
  default:
    Fail("Invalid value");
  }

  switch (type) {
  case AV_PKT_DATA_METADATA_UPDATE:
  case AV_PKT_DATA_STRINGS_METADATA:
    len = caml_string_length(Field(_side_data, 1));
    data = av_malloc(len);
    if (!data)
      caml_raise_out_of_memory();
    memcpy(data, String_val(Field(_side_data, 1)), len);
    av_packet_add_side_data(packet, type, data, len);
    break;
  case AV_PKT_DATA_REPLAYGAIN:
    len = sizeof(AVReplayGain);
    data = av_malloc(len);
    if (!data)
      caml_raise_out_of_memory();
    replay_gain = (AVReplayGain *)data;
    replay_gain->track_gain = Int_val(Field(Field(_side_data, 1), 0));
    replay_gain->track_peak = Int_val(Field(Field(_side_data, 1), 1));
    replay_gain->album_gain = Int_val(Field(Field(_side_data, 1), 2));
    replay_gain->album_peak = Int_val(Field(Field(_side_data, 1), 3));
    av_packet_add_side_data(packet, type, data, len);
    break;
  default:
    Fail("Invalid value");
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_packet_side_data(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal4(ret, tmp, tmp2, type);
  AVPacket *packet = Packet_val(_packet);
  int len = 0;
  int i;
  AVReplayGain *replay_gain;

  for (i = 0; i < packet->side_data_elems; i++) {
    switch (packet->side_data[i].type) {
    case AV_PKT_DATA_METADATA_UPDATE:
    case AV_PKT_DATA_STRINGS_METADATA:
    case AV_PKT_DATA_REPLAYGAIN:
      len++;
    default:
      break;
    }
  }

  ret = caml_alloc_tuple(len);

  for (i = 0; i < len; i++) {
    switch (packet->side_data[i].type) {
    case AV_PKT_DATA_METADATA_UPDATE:
    case AV_PKT_DATA_STRINGS_METADATA:
      type = packet->side_data[i].type == AV_PKT_DATA_METADATA_UPDATE
                 ? PVV_Metadata_update
                 : PVV_Strings_metadata;

      tmp = caml_alloc_initialized_string(packet->side_data[i].size,
                                          (char *)packet->side_data[i].data);

      tmp2 = caml_alloc_tuple(2);
      Store_field(tmp2, 0, type);
      Store_field(tmp2, 1, tmp);

      Store_field(ret, i, tmp2);
      break;

    case AV_PKT_DATA_REPLAYGAIN:
      if (packet->side_data[i].size < sizeof(AVReplayGain))
        Fail("Invalid side_data");
      replay_gain = (AVReplayGain *)packet->side_data[i].data;

      tmp = caml_alloc_tuple(4);
      Store_field(tmp, 0, Val_int(replay_gain->track_gain));
      Store_field(tmp, 1, Val_int(replay_gain->track_peak));
      Store_field(tmp, 2, Val_int(replay_gain->album_gain));
      Store_field(tmp, 3, Val_int(replay_gain->album_peak));

      tmp2 = caml_alloc_tuple(2);
      Store_field(tmp2, 0, PVV_Replaygain);
      Store_field(tmp2, 1, tmp);

      Store_field(ret, i, tmp2);
      break;

    default:
      break;
    }
  }

  CAMLreturn(ret);
};

CAMLprim value ocaml_avcodec_packet_dup(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ret);

  AVPacket *packet = av_packet_alloc();

  if (!packet)
    caml_raise_out_of_memory();

  av_packet_ref(packet, Packet_val(_packet));

  ret = caml_alloc_custom(&packet_ops, sizeof(AVPacket *), 0, 1);
  Packet_val(ret) = packet;

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_get_flags(value _packet) {
  CAMLparam1(_packet);
  CAMLreturn(Val_int(Packet_val(_packet)->flags));
}

CAMLprim value ocaml_avcodec_get_packet_stream_index(value _packet) {
  CAMLparam1(_packet);
  CAMLreturn(Val_int(Packet_val(_packet)->stream_index));
}

CAMLprim value ocaml_avcodec_set_packet_stream_index(value _packet,
                                                     value _index) {
  CAMLparam2(_packet, _index);
  Packet_val(_packet)->stream_index = Int_val(_index);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_get_packet_pts(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ret);
  AVPacket *packet = Packet_val(_packet);

  if (packet->pts == AV_NOPTS_VALUE)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, caml_copy_int64(packet->pts));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_set_packet_pts(value _packet, value _pts) {
  CAMLparam2(_packet, _pts);
  AVPacket *packet = Packet_val(_packet);

  if (_pts == Val_none)
    packet->pts = AV_NOPTS_VALUE;
  else
    packet->pts = Int64_val(Field(_pts, 0));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_get_packet_duration(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ret);
  AVPacket *packet = Packet_val(_packet);

  if (packet->duration == 0)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, caml_copy_int64(packet->duration));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_set_packet_duration(value _packet,
                                                 value _duration) {
  CAMLparam2(_packet, _duration);
  AVPacket *packet = Packet_val(_packet);

  if (_duration == Val_none)
    packet->duration = 0;
  else
    packet->duration = Int64_val(Field(_duration, 0));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_get_packet_position(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ret);
  AVPacket *packet = Packet_val(_packet);

  if (packet->pos == -1)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, caml_copy_int64(packet->pos));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_set_packet_position(value _packet,
                                                 value _position) {
  CAMLparam2(_packet, _position);
  AVPacket *packet = Packet_val(_packet);

  if (_position == Val_none)
    packet->pos = -1;
  else
    packet->pos = Int64_val(Field(_position, 0));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_get_packet_dts(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ret);
  AVPacket *packet = Packet_val(_packet);

  if (packet->dts == AV_NOPTS_VALUE)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, caml_copy_int64(packet->dts));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_set_packet_dts(value _packet, value _dts) {
  CAMLparam2(_packet, _dts);
  AVPacket *packet = Packet_val(_packet);

  if (_dts == Val_none)
    packet->dts = AV_NOPTS_VALUE;
  else
    packet->dts = Int64_val(Field(_dts, 0));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_get_packet_size(value _packet) {
  CAMLparam1(_packet);
  CAMLreturn(Val_int(Packet_val(_packet)->size));
}

CAMLprim value ocaml_avcodec_packet_to_bytes(value _packet) {
  CAMLparam1(_packet);
  CAMLlocal1(ans);
  struct AVPacket *packet = Packet_val(_packet);

  ans = caml_alloc_string(packet->size);

  memcpy((uint8_t *)String_val(ans), packet->data, packet->size);

  CAMLreturn(ans);
}

/***** codec_context_t *****/

typedef struct {
  const AVCodec *codec;
  AVCodecContext *codec_context;
  // output
  int flushed;
} codec_context_t;

#define CodecContext_val(v) (*(codec_context_t **)Data_custom_val(v))

static void finalize_codec_context(value v) {
  codec_context_t *ctx = CodecContext_val(v);
  if (ctx->codec_context)
    avcodec_free_context(&ctx->codec_context);

  av_free(ctx);
}

static struct custom_operations codec_context_ops = {
    "ocaml_codec_context",    finalize_codec_context,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_avcodec_create_decoder(value _params, value _codec) {
  CAMLparam2(_params, _codec);
  CAMLlocal1(ans);
  const AVCodec *codec = AvCodec_val(_codec);
  AVCodecParameters *params = NULL;

  if (_params != Val_none)
    params = CodecParameters_val(Field(_params, 0));

  codec_context_t *ctx = (codec_context_t *)av_mallocz(sizeof(codec_context_t));
  if (!ctx)
    caml_raise_out_of_memory();

  ans = caml_alloc_custom(&codec_context_ops, sizeof(codec_context_t *), 0, 1);
  CodecContext_val(ans) = ctx;

  ctx->codec = codec;
  ctx->codec_context = create_AVCodecContext(params, ctx->codec);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_sample_format(value _ctx) {
  CAMLparam1(_ctx);
  codec_context_t *ctx = CodecContext_val(_ctx);
  CAMLreturn(Val_SampleFormat(ctx->codec_context->sample_fmt));
}

CAMLprim value ocaml_avcodec_encoder_params(value _encoder) {
  CAMLparam1(_encoder);
  CAMLlocal1(ans);
  AVCodecParameters *params = avcodec_parameters_alloc();

  if (!params)
    caml_raise_out_of_memory();

  codec_context_t *ctx = CodecContext_val(_encoder);

  int err = avcodec_parameters_from_context(params, ctx->codec_context);

  if (err < 0) {
    avcodec_parameters_free(&params);
    ocaml_avutil_raise_error(err);
  }

  value_of_codec_parameters_copy(params, &ans);

  avcodec_parameters_free(&params);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_encoder_time_base(value _encoder) {
  CAMLparam1(_encoder);
  CAMLlocal1(ans);
  codec_context_t *ctx = CodecContext_val(_encoder);

  value_of_rational(&ctx->codec_context->time_base, &ans);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_create_audio_encoder(value _sample_fmt,
                                                  value _codec,
                                                  value _channel_layout,
                                                  value _opts) {
  CAMLparam4(_sample_fmt, _codec, _channel_layout, _opts);
  CAMLlocal3(ret, ans, unused);
  const AVCodec *codec = AvCodec_val(_codec);

  AVDictionary *options = NULL;
  char *key, *val;
  int len = Wosize_val(_opts);
  int i, err, count;

  for (i = 0; i < len; i++) {
    // Dictionaries copy key/values by default!
    key = (char *)Bytes_val(Field(Field(_opts, i), 0));
    val = (char *)Bytes_val(Field(Field(_opts, i), 1));
    err = av_dict_set(&options, key, val, 0);
    if (err < 0) {
      av_dict_free(&options);
      ocaml_avutil_raise_error(err);
    }
  }

  codec_context_t *ctx = (codec_context_t *)av_mallocz(sizeof(codec_context_t));
  if (!ctx) {
    av_dict_free(&options);
    caml_raise_out_of_memory();
  }

  ans = caml_alloc_custom(&codec_context_ops, sizeof(codec_context_t *), 0, 1);
  CodecContext_val(ans) = ctx;

  ctx->codec = codec;

  ctx->codec_context = avcodec_alloc_context3(codec);

  if (!ctx->codec_context) {
    av_dict_free(&options);
    caml_raise_out_of_memory();
  }

  ctx->codec_context->sample_fmt = Int_val(_sample_fmt);

  err = av_channel_layout_copy(&ctx->codec_context->ch_layout,
                               AVChannelLayout_val(_channel_layout));
  if (err < 0) {
    av_dict_free(&options);
    ocaml_avutil_raise_error(err);
  }

  // Open the codec
  caml_release_runtime_system();
  err = avcodec_open2(ctx->codec_context, ctx->codec, &options);
  caml_acquire_runtime_system();

  if (err < 0) {
    av_dict_free(&options);
    ocaml_avutil_raise_error(err);
  }

  // Return unused keys
  count = av_dict_count(options);

  unused = caml_alloc_tuple(count);
  AVDictionaryEntry *entry = NULL;
  for (i = 0; i < count; i++) {
    entry = av_dict_get(options, "", entry, AV_DICT_IGNORE_SUFFIX);
    Store_field(unused, i, caml_copy_string(entry->key));
  }

  av_dict_free(&options);

  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, ans);
  Store_field(ret, 1, unused);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_create_video_encoder(value _device_context,
                                                  value _frame_context,
                                                  value _pix_fmt, value _codec,
                                                  value _opts) {
  CAMLparam4(_device_context, _frame_context, _pix_fmt, _codec);
  CAMLxparam1(_opts);
  CAMLlocal3(ret, ans, unused);
  const AVCodec *codec = AvCodec_val(_codec);

  AVBufferRef *device_ctx = NULL;
  AVBufferRef *frame_ctx = NULL;

  if (_device_context != Val_none)
    device_ctx = BufferRef_val(Some_val(_device_context));

  if (_frame_context != Val_none)
    frame_ctx = BufferRef_val(Some_val(_frame_context));

  AVDictionary *options = NULL;
  char *key, *val;
  int len = Wosize_val(_opts);
  int i, err, count;

  for (i = 0; i < len; i++) {
    // Dictionaries copy key/values by default!
    key = (char *)Bytes_val(Field(Field(_opts, i), 0));
    val = (char *)Bytes_val(Field(Field(_opts, i), 1));
    err = av_dict_set(&options, key, val, 0);
    if (err < 0) {
      av_dict_free(&options);
      ocaml_avutil_raise_error(err);
    }
  }

  codec_context_t *ctx = (codec_context_t *)av_mallocz(sizeof(codec_context_t));
  if (!ctx) {
    av_dict_free(&options);
    caml_raise_out_of_memory();
  }

  ans = caml_alloc_custom(&codec_context_ops, sizeof(codec_context_t *), 0, 1);
  CodecContext_val(ans) = ctx;

  ctx->codec = codec;
  ctx->codec_context = avcodec_alloc_context3(codec);

  if (!ctx->codec_context) {
    av_dict_free(&options);
    caml_raise_out_of_memory();
  }

  ctx->codec_context->pix_fmt = Int_val(_pix_fmt);

  if (device_ctx) {
    ctx->codec_context->hw_device_ctx = av_buffer_ref(device_ctx);
    if (!ctx->codec_context->hw_device_ctx) {
      av_dict_free(&options);
      caml_raise_out_of_memory();
    }
  }

  if (frame_ctx) {
    ctx->codec_context->hw_frames_ctx = av_buffer_ref(frame_ctx);
    if (!ctx->codec_context->hw_frames_ctx) {
      av_dict_free(&options);
      caml_raise_out_of_memory();
    }
  }

  // Open the codec
  caml_release_runtime_system();
  err = avcodec_open2(ctx->codec_context, ctx->codec, &options);
  caml_acquire_runtime_system();

  if (err < 0) {
    av_dict_free(&options);
    ocaml_avutil_raise_error(err);
  }

  // Return unused keys
  count = av_dict_count(options);

  unused = caml_alloc_tuple(count);
  AVDictionaryEntry *entry = NULL;
  for (i = 0; i < count; i++) {
    entry = av_dict_get(options, "", entry, AV_DICT_IGNORE_SUFFIX);
    Store_field(unused, i, caml_copy_string(entry->key));
  }

  av_dict_free(&options);

  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, ans);
  Store_field(ret, 1, unused);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_frame_size(value _ctx) {
  CAMLparam1(_ctx);
  codec_context_t *ctx = CodecContext_val(_ctx);
  CAMLreturn(Val_int(ctx->codec_context->frame_size));
}

CAMLprim value ocaml_avcodec_send_packet(value _ctx, value _packet) {
  CAMLparam2(_ctx, _packet);
  codec_context_t *ctx = CodecContext_val(_ctx);
  AVPacket *packet = _packet ? Packet_val(_packet) : NULL;

  // send the packet with the compressed data to the decoder
  caml_release_runtime_system();
  int ret = avcodec_send_packet(ctx->codec_context, packet);
  caml_acquire_runtime_system();

  if (ret < 0)
    ocaml_avutil_raise_error(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_receive_frame(value _ctx) {
  CAMLparam1(_ctx);
  CAMLlocal2(val_frame, ans);
  codec_context_t *ctx = CodecContext_val(_ctx);
  int ret = 0;

  AVFrame *frame = av_frame_alloc();
  AVFrame *hw_frame = NULL;
  AVFrame *effective_frame = frame;

  if (!frame) {
    caml_raise_out_of_memory();
  }

  caml_release_runtime_system();
  ret = avcodec_receive_frame(ctx->codec_context, frame);
  caml_acquire_runtime_system();

  if (ret < 0 && ret != AVERROR(EAGAIN)) {
    av_frame_free(&frame);
    ocaml_avutil_raise_error(ret);
  }

  if (ret == AVERROR(EAGAIN)) {
    av_frame_free(&frame);
    CAMLreturn(Val_none);
  }

  if (ctx->codec_context->hw_frames_ctx) {
    hw_frame = av_frame_alloc();
    if (!hw_frame) {
      av_frame_free(&frame);
      caml_raise_out_of_memory();
    }

    ret = av_hwframe_transfer_data(hw_frame, frame, 0);
    if (ret < 0) {
      av_frame_free(&frame);
      av_frame_free(&hw_frame);
      ocaml_avutil_raise_error(ret);
    }

    av_frame_free(&frame);
    effective_frame = hw_frame;
  }

  ans = caml_alloc_tuple(1);
  value_of_frame(&val_frame, effective_frame);
  Store_field(ans, 0, val_frame);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_flush_decoder(value _ctx) {
  ocaml_avcodec_send_packet(_ctx, 0);
  return Val_unit;
}

static void send_frame(codec_context_t *ctx, AVFrame *frame) {
  int ret;
  AVFrame *hw_frame = NULL;

  if (ctx->flushed)
    ocaml_avutil_raise_error(AVERROR_EOF);

  ctx->flushed = !frame;

  if (ctx->codec_context->hw_frames_ctx && frame) {
    hw_frame = av_frame_alloc();

    if (!hw_frame) {
      caml_raise_out_of_memory();
    }

    ret = av_hwframe_get_buffer(ctx->codec_context->hw_frames_ctx, hw_frame, 0);

    if (ret < 0) {
      av_frame_free(&hw_frame);
      ocaml_avutil_raise_error(ret);
    }

    if (!hw_frame->hw_frames_ctx) {
      caml_raise_out_of_memory();
    }

    ret = av_hwframe_transfer_data(hw_frame, frame, 0);

    if (ret < 0) {
      av_frame_free(&hw_frame);
      ocaml_avutil_raise_error(ret);
    }

    frame = hw_frame;
  }

  caml_release_runtime_system();
  ret = avcodec_send_frame(ctx->codec_context, frame);
  caml_acquire_runtime_system();

  if (hw_frame)
    av_frame_free(&hw_frame);

  if (ret < 0)
    ocaml_avutil_raise_error(ret);
}

CAMLprim value ocaml_avcodec_send_frame(value _ctx, value _frame) {
  CAMLparam2(_ctx, _frame);
  CAMLlocal1(val_packet);
  codec_context_t *ctx = CodecContext_val(_ctx);
  AVFrame *frame = _frame ? Frame_val(_frame) : NULL;

  send_frame(ctx, frame);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_receive_packet(value _ctx) {
  CAMLparam1(_ctx);
  CAMLlocal2(val_packet, ans);
  codec_context_t *ctx = CodecContext_val(_ctx);
  int ret = 0;
  AVPacket *packet = av_packet_alloc();

  if (!packet)
    caml_raise_out_of_memory();

  caml_release_runtime_system();
  ret = avcodec_receive_packet(ctx->codec_context, packet);
  caml_acquire_runtime_system();

  if (ret < 0) {
    av_packet_free(&packet);

    if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
      CAMLreturn(Val_none);

    ocaml_avutil_raise_error(ret);
  }

  ans = caml_alloc_tuple(1);
  val_packet = value_of_ffmpeg_packet(&val_packet, packet);
  Store_field(ans, 0, val_packet);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_flush_encoder(value _ctx) {
  ocaml_avcodec_send_frame(_ctx, 0);
  return Val_unit;
}

/**** codec ****/

static const AVCodec *find_encoder_by_name(const char *name,
                                           enum AVMediaType type) {
  const AVCodec *codec = avcodec_find_encoder_by_name(name);

  if (!codec || codec->type != type)
    ocaml_avutil_raise_error(AVERROR_ENCODER_NOT_FOUND);

  return codec;
}

static const AVCodec *find_encoder(enum AVCodecID id, enum AVMediaType type) {
  const AVCodec *codec = avcodec_find_encoder(id);

  if (!codec || codec->type != type)
    ocaml_avutil_raise_error(AVERROR_ENCODER_NOT_FOUND);

  return codec;
}

static const AVCodec *find_decoder_by_name(const char *name,
                                           enum AVMediaType type) {
  const AVCodec *codec = avcodec_find_decoder_by_name(name);

  if (!codec || codec->type != type)
    ocaml_avutil_raise_error(AVERROR_DECODER_NOT_FOUND);

  return codec;
}

static const AVCodec *find_decoder(enum AVCodecID id, enum AVMediaType type) {
  const AVCodec *codec = avcodec_find_decoder(id);

  if (!codec || codec->type != type)
    ocaml_avutil_raise_error(AVERROR_DECODER_NOT_FOUND);

  return codec;
}

CAMLprim value ocaml_avcodec_parameters_get_bit_rate(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_int(CodecParameters_val(_cp)->bit_rate));
}

/**** Audio codec ID ****/

CAMLprim value ocaml_avcodec_get_audio_codec_id(value _codec) {
  CAMLparam1(_codec);
  const AVCodec *codec = AvCodec_val(_codec);
  CAMLreturn(Val_AudioCodecID(codec->id));
}

CAMLprim value ocaml_avcodec_get_video_codec_id(value _codec) {
  CAMLparam1(_codec);
  const AVCodec *codec = AvCodec_val(_codec);
  CAMLreturn(Val_VideoCodecID(codec->id));
}

CAMLprim value ocaml_avcodec_get_subtitle_codec_id(value _codec) {
  CAMLparam1(_codec);
  const AVCodec *codec = AvCodec_val(_codec);
  CAMLreturn(Val_SubtitleCodecID(codec->id));
}

CAMLprim value ocaml_avcodec_get_audio_codec_id_name(value _codec_id) {
  CAMLparam1(_codec_id);
  CAMLreturn(caml_copy_string(
      avcodec_get_name((enum AVCodecID)AudioCodecID_val(_codec_id))));
}

CAMLprim value ocaml_avcodec_get_codec_id_name(value _codec_id) {
  CAMLparam1(_codec_id);
  CAMLreturn(caml_copy_string(
      avcodec_get_name((enum AVCodecID)CodecID_val(_codec_id))));
}

CAMLprim value ocaml_avcodec_find_audio_encoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder_by_name(String_val(_name), AVMEDIA_TYPE_AUDIO)));
}

CAMLprim value ocaml_avcodec_find_audio_encoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder(AudioCodecID_val(_id), AVMEDIA_TYPE_AUDIO)));
}

CAMLprim value ocaml_avcodec_find_audio_decoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder_by_name(String_val(_name), AVMEDIA_TYPE_AUDIO)));
}

CAMLprim value ocaml_avcodec_find_audio_decoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder(AudioCodecID_val(_id), AVMEDIA_TYPE_AUDIO)));
}

CAMLprim value ocaml_avcodec_name(value _codec) {
  CAMLparam1(_codec);
  CAMLreturn(caml_copy_string(AvCodec_val(_codec)->name));
}

CAMLprim value ocaml_avcodec_capabilities(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal1(ret);
  const AVCodec *codec = AvCodec_val(_codec);
  int i, len;

  len = 0;
  for (i = 0; i < AV_CODEC_CAP_T_TAB_LEN; i++)
    if (codec->capabilities & AV_CODEC_CAP_T_TAB[i][1])
      len++;

  ret = caml_alloc_tuple(len);

  len = 0;
  for (i = 0; i < AV_CODEC_CAP_T_TAB_LEN; i++)
    if (codec->capabilities & AV_CODEC_CAP_T_TAB[i][1])
      Store_field(ret, len++, Val_int(AV_CODEC_CAP_T_TAB[i][0]));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_descriptor(enum AVCodecID id) {
  CAMLparam0();
  CAMLlocal3(ret, tmp, _profile);
  const AVCodecDescriptor *descriptor = avcodec_descriptor_get(id);
  int i, len;
  char **p;
  struct AVProfile *profile;

  if (!descriptor)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(6);

  Store_field(ret, 0, Val_MediaTypes(descriptor->type));
  Store_field(ret, 1, caml_copy_string(descriptor->name));

  if (descriptor->long_name) {
    tmp = caml_alloc_tuple(1);
    Store_field(tmp, 0, caml_copy_string(descriptor->long_name));
    Store_field(ret, 2, tmp);
  } else
    Store_field(ret, 2, Val_none);

  len = 0;
  for (i = 0; i < AV_CODEC_PROP_T_TAB_LEN; i++) {
    if (descriptor->props & AV_CODEC_PROP_T_TAB[i][1])
      len++;
  }

  tmp = caml_alloc_tuple(len);
  len = 0;
  for (i = 0; i < AV_CODEC_PROP_T_TAB_LEN; i++) {
    if (descriptor->props & AV_CODEC_PROP_T_TAB[i][1]) {
      Store_field(tmp, len, AV_CODEC_PROP_T_TAB[i][0]);
      len++;
    }
  }
  Store_field(ret, 3, tmp);

  len = 0;
  p = (char **)descriptor->mime_types;
  while (p && *p) {
    len++;
    p++;
  }

  tmp = caml_alloc_tuple(len);
  len = 0;
  p = (char **)descriptor->mime_types;
  while (p && *p) {
    Store_field(tmp, len, caml_copy_string(*p));
    len++;
    p++;
  }
  Store_field(ret, 4, tmp);

  len = 0;
  profile = (struct AVProfile *)descriptor->profiles;
  while (profile && profile->profile != AV_PROFILE_UNKNOWN) {
    len++;
    profile++;
  }

  tmp = caml_alloc_tuple(len);
  len = 0;
  profile = (struct AVProfile *)descriptor->profiles;
  while (profile && profile->profile != AV_PROFILE_UNKNOWN) {
    _profile = caml_alloc_tuple(2);
    Store_field(_profile, 0, Val_int(profile->profile));
    Store_field(_profile, 1, caml_copy_string(profile->name));
    Store_field(tmp, len, _profile);
    len++;
    profile++;
  }
  Store_field(ret, 5, tmp);

  tmp = caml_alloc_tuple(1);
  Store_field(tmp, 0, ret);

  CAMLreturn(tmp);
}

CAMLprim value ocaml_avcodec_params_descriptor(value _params) {
  CAMLparam1(_params);
  CAMLreturn(ocaml_avcodec_descriptor(CodecParameters_val(_params)->codec_id));
}

value ocaml_avcodec_audio_descriptor(value _codec_id) {
  return ocaml_avcodec_descriptor(AudioCodecID_val(_codec_id));
}

value ocaml_avcodec_video_descriptor(value _codec_id) {
  return ocaml_avcodec_descriptor(VideoCodecID_val(_codec_id));
}

value ocaml_avcodec_subtitle_descriptor(value _codec_id) {
  return ocaml_avcodec_descriptor(SubtitleCodecID_val(_codec_id));
}

CAMLprim value ocaml_avcodec_hw_methods(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal5(ret, tmp1, cons1, tmp2, cons2);
  const AVCodec *codec = AvCodec_val(_codec);
  int n, i = 0;
  const AVCodecHWConfig *config = avcodec_get_hw_config(codec, i);

  if (!config)
    CAMLreturn(Val_int(0));

  cons1 = Val_int(0);
  do {
    ret = caml_alloc_tuple(2);
    Store_field(ret, 1, cons1);

    tmp1 = caml_alloc_tuple(3);
    Store_field(tmp1, 0, Val_PixelFormat(config->pix_fmt));

    tmp2 = Val_int(0);
    cons2 = Val_int(0);
    for (n = 0; n < AV_CODEC_HW_CONFIG_METHOD_T_TAB_LEN; n++) {
      if (config->methods & AV_CODEC_HW_CONFIG_METHOD_T_TAB[n][1]) {
        tmp2 = caml_alloc_tuple(2);
        Store_field(tmp2, 0, AV_CODEC_HW_CONFIG_METHOD_T_TAB[n][0]);
        Store_field(tmp2, 1, cons2);
        cons2 = tmp2;
      }
    }
    Store_field(tmp1, 1, tmp2);

    Store_field(tmp1, 2, Val_HwDeviceType(config->device_type));

    Store_field(ret, 0, tmp1);
    cons1 = ret;
    i++;
    config = avcodec_get_hw_config(codec, i);
  } while (config);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_get_supported_channel_layouts(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal3(list, cons, ch_layout);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const AVChannelLayout *ch_layouts = NULL;

#if LIBAVCODEC_VERSION_INT <= AV_VERSION_INT(61, 13, 100)
  ch_layouts = codec->ch_layouts;
#else
  int err;

  err =
      avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_CHANNEL_LAYOUT,
                                   0, (const void **)&ch_layouts, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (ch_layouts) {
    for (i = 0; ch_layouts[i].nb_channels != 0; i++) {
      value_of_channel_layout(&ch_layout, &ch_layouts[i]);
      List_add(list, cons, ch_layout);
    }
  }

  CAMLreturn(list);
}

CAMLprim value ocaml_avcodec_get_supported_sample_formats(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(list, cons);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const enum AVSampleFormat *sample_fmts = NULL;

#if LIBAVCODEC_VERSION_INT <= AV_VERSION_INT(61, 13, 100)
  sample_fmts = codec->sample_fmts;
#else
  int err;

  err = avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_SAMPLE_FORMAT,
                                     0, (const void **)&sample_fmts, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (sample_fmts) {
    for (i = 0; sample_fmts[i] != -1; i++)
      List_add(list, cons, Val_SampleFormat(sample_fmts[i]));
  }

  CAMLreturn(list);
}

CAMLprim value ocaml_avcodec_get_supported_sample_rates(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(list, cons);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const int *supported_samplerates = NULL;

#if LIBAVCODEC_VERSION_INT <= AV_VERSION_INT(61, 13, 100)
  supported_samplerates = codec->supported_samplerates;
#else
  int err;

  err =
      avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_SAMPLE_RATE, 0,
                                   (const void **)&supported_samplerates, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (supported_samplerates) {
    for (i = 0; supported_samplerates[i] != 0; i++)
      List_add(list, cons, Val_int(supported_samplerates[i]));
  }

  CAMLreturn(list);
}

/**** Audio codec parameters ****/
CAMLprim value ocaml_avcodec_parameters_get_audio_codec_id(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_AudioCodecID(CodecParameters_val(_cp)->codec_id));
}

CAMLprim value ocaml_avcodec_parameters_get_channel_layout(value _cp) {
  CAMLparam1(_cp);
  CAMLlocal1(_ch_layout);
  AVCodecParameters *cp = CodecParameters_val(_cp);

  value_of_channel_layout(&_ch_layout, &cp->ch_layout);

  CAMLreturn(_ch_layout);
}

CAMLprim value ocaml_avcodec_parameters_get_nb_channels(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_int(CodecParameters_val(_cp)->ch_layout.nb_channels));
}

CAMLprim value ocaml_avcodec_parameters_get_sample_format(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(
      Val_SampleFormat((enum AVSampleFormat)CodecParameters_val(_cp)->format));
}

CAMLprim value ocaml_avcodec_parameters_get_sample_rate(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_int(CodecParameters_val(_cp)->sample_rate));
}

/**** Video codec ID ****/

CAMLprim value ocaml_avcodec_get_video_codec_id_name(value _codec_id) {
  CAMLparam1(_codec_id);
  CAMLreturn(caml_copy_string(avcodec_get_name(VideoCodecID_val(_codec_id))));
}

CAMLprim value ocaml_avcodec_find_video_decoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder_by_name(String_val(_name), AVMEDIA_TYPE_VIDEO)));
}

CAMLprim value ocaml_avcodec_find_video_decoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder(VideoCodecID_val(_id), AVMEDIA_TYPE_VIDEO)));
}

CAMLprim value ocaml_avcodec_find_video_encoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder_by_name(String_val(_name), AVMEDIA_TYPE_VIDEO)));
}

CAMLprim value ocaml_avcodec_find_video_encoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder(VideoCodecID_val(_id), AVMEDIA_TYPE_VIDEO)));
}

CAMLprim value ocaml_avcodec_get_supported_frame_rates(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal3(list, cons, val);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const AVRational *supported_framerates = NULL;

#if LIBAVCODEC_VERSION_INT <= AV_VERSION_INT(61, 13, 100)
  supported_framerates = codec->supported_framerates;
#else
  int err;

  err =
      avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_FRAME_RATE, 0,
                                   (const void **)&supported_framerates, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (supported_framerates) {
    for (i = 0; supported_framerates[i].num != 0; i++) {
      value_of_rational(&supported_framerates[i], &val);
      List_add(list, cons, val);
    }
  }

  CAMLreturn(list);
}

CAMLprim value ocaml_avcodec_get_supported_pixel_formats(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(list, cons);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const enum AVPixelFormat *pix_fmts = NULL;

#if LIBAVCODEC_VERSION_INT <= AV_VERSION_INT(61, 13, 100)
  pix_fmts = codec->pix_fmts;
#else
  int err;

  err = avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_PIX_FORMAT, 0,
                                     (const void **)&pix_fmts, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (pix_fmts) {
    for (i = 0; pix_fmts[i] != -1; i++)
      List_add(list, cons, Val_PixelFormat(pix_fmts[i]));
  }

  CAMLreturn(list);
}

CAMLprim value ocaml_avcodec_get_supported_color_spaces(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(list, cons);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const enum AVColorSpace *color_spaces = NULL;

#if LIBAVCODEC_VERSION_INT > AV_VERSION_INT(61, 13, 100)
  int err;

  err = avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_COLOR_SPACE,
                                     0, (const void **)&color_spaces, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (color_spaces) {
    for (i = 0; color_spaces[i] != -1; i++)
      List_add(list, cons, Val_ColorSpace(color_spaces[i]));
  }

  CAMLreturn(list);
}

CAMLprim value ocaml_avcodec_get_supported_color_ranges(value _codec) {
  CAMLparam1(_codec);
  CAMLlocal2(list, cons);
  int i;
  List_init(list);
  const AVCodec *codec = AvCodec_val(_codec);
  const enum AVColorRange *color_ranges = NULL;

#if LIBAVCODEC_VERSION_INT > AV_VERSION_INT(61, 13, 100)
  int err;

  err = avcodec_get_supported_config(NULL, codec, AV_CODEC_CONFIG_COLOR_RANGE,
                                     0, (const void **)&color_ranges, NULL);

  if (err < 0)
    ocaml_avutil_raise_error(err);
#endif

  if (color_ranges) {
    for (i = 0; color_ranges[i] != -1; i++)
      List_add(list, cons, Val_ColorRange(color_ranges[i]));
  }

  CAMLreturn(list);
}

/**** Video codec parameters ****/
CAMLprim value ocaml_avcodec_parameters_get_video_codec_id(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_VideoCodecID(CodecParameters_val(_cp)->codec_id));
}

CAMLprim value ocaml_avcodec_parameters_get_width(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_int(CodecParameters_val(_cp)->width));
}

CAMLprim value ocaml_avcodec_parameters_get_height(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_int(CodecParameters_val(_cp)->height));
}

CAMLprim value ocaml_avcodec_parameters_get_sample_aspect_ratio(value _cp) {
  CAMLparam1(_cp);
  CAMLlocal1(ans);

  value_of_rational(&CodecParameters_val(_cp)->sample_aspect_ratio, &ans);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_parameters_get_pixel_format(value _cp) {
  CAMLparam1(_cp);
  CAMLlocal1(ret);
  enum AVPixelFormat f = CodecParameters_val(_cp)->format;

  if (f == AV_PIX_FMT_NONE)
    CAMLreturn(Val_none);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, Val_PixelFormat(f));

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_parameters_get_pixel_aspect(value _cp) {
  CAMLparam1(_cp);
  CAMLlocal2(ret, ans);
  const AVRational pixel_aspect = CodecParameters_val(_cp)->sample_aspect_ratio;

  if (pixel_aspect.num == 0)
    CAMLreturn(Val_none);

  value_of_rational(&pixel_aspect, &ans);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, ans);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avcodec_parameters_video_copy(value _codec_id,
                                                   value _width, value _height,
                                                   value _sample_aspect_ratio,
                                                   value _pixel_format,
                                                   value _bit_rate, value _cp) {
  CAMLparam5(_codec_id, _width, _height, _sample_aspect_ratio, _pixel_format);
  CAMLxparam2(_bit_rate, _cp);
  CAMLlocal1(ans);

  value_of_codec_parameters_copy(CodecParameters_val(_cp), &ans);

  AVCodecParameters *dst = CodecParameters_val(ans);

  dst->codec_id = VideoCodecID_val(_codec_id);
  dst->width = Int_val(_width);
  dst->height = Int_val(_height);
  dst->sample_aspect_ratio.num = Int_val(Field(_sample_aspect_ratio, 0));
  dst->sample_aspect_ratio.den = Int_val(Field(_sample_aspect_ratio, 1));
  dst->format = PixelFormat_val(_pixel_format);
  dst->bit_rate = Int_val(_bit_rate);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_parameters_video_copy_byte(value *argv, int argn) {
  return ocaml_avcodec_parameters_video_copy(argv[0], argv[1], argv[2], argv[3],
                                             argv[4], argv[5], argv[7]);
}

/**** Unknown codec ID *****/

CAMLprim value ocaml_avcodec_get_unknown_codec_id_name(value _codec_id) {
  CAMLparam1(_codec_id);
  CAMLreturn(caml_copy_string(avcodec_get_name(UnknownCodecID_val(_codec_id))));
}

CAMLprim value ocaml_avcodec_parameters_get_unknown_codec_id(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_UnknownCodecID(CodecParameters_val(_cp)->codec_id));
}

/**** Subtitle codec ID ****/

CAMLprim value ocaml_avcodec_get_subtitle_codec_id_name(value _codec_id) {
  CAMLparam1(_codec_id);
  CAMLreturn(
      caml_copy_string(avcodec_get_name(SubtitleCodecID_val(_codec_id))));
}

CAMLprim value ocaml_avcodec_find_subtitle_decoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder_by_name(String_val(_name), AVMEDIA_TYPE_SUBTITLE)));
}

CAMLprim value ocaml_avcodec_find_subtitle_decoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_decoder(SubtitleCodecID_val(_id), AVMEDIA_TYPE_SUBTITLE)));
}

CAMLprim value ocaml_avcodec_find_subtitle_encoder_by_name(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder_by_name(String_val(_name), AVMEDIA_TYPE_SUBTITLE)));
}

CAMLprim value ocaml_avcodec_find_subtitle_encoder(value _id) {
  CAMLparam1(_id);
  CAMLlocal1(ret);
  CAMLreturn(value_of_avcodec(
      &ret, find_encoder(SubtitleCodecID_val(_id), AVMEDIA_TYPE_SUBTITLE)));
}

/**** Subtitle codec parameters ****/
CAMLprim value ocaml_avcodec_parameters_get_subtitle_codec_id(value _cp) {
  CAMLparam1(_cp);
  CAMLreturn(Val_SubtitleCodecID(CodecParameters_val(_cp)->codec_id));
}

CAMLprim value ocaml_avcodec_parameters_subtitle_copy(value _codec_id,
                                                      value _cp) {
  CAMLparam2(_codec_id, _cp);
  CAMLlocal1(ans);

  value_of_codec_parameters_copy(CodecParameters_val(_cp), &ans);

  AVCodecParameters *dst = CodecParameters_val(ans);

  dst->codec_id = SubtitleCodecID_val(_codec_id);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_int_of_flag(value _flag) {
  CAMLparam1(_flag);

  switch (_flag) {
  case PVV_Keyframe:
    CAMLreturn(Val_int(AV_PKT_FLAG_KEY));
  case PVV_Corrupt:
    CAMLreturn(Val_int(AV_PKT_FLAG_CORRUPT));
  case PVV_Discard:
    CAMLreturn(Val_int(AV_PKT_FLAG_DISCARD));
  case PVV_Trusted:
    CAMLreturn(Val_int(AV_PKT_FLAG_TRUSTED));
  case PVV_Disposable:
    CAMLreturn(Val_int(AV_PKT_FLAG_DISPOSABLE));
  default:
    caml_failwith("Invalid flag type!");
  }
}

CAMLprim value ocaml_avcodec_get_next_codec(value h) {
  CAMLparam1(h);
  CAMLlocal5(_tmp, _id, _h, _ans, _ret);
  void *s;
  const AVCodec *codec;
  enum AVCodecID id = VALUE_NOT_FOUND;
  int i;

  if (h == Val_int(0)) {
    s = NULL;
  } else {
    s = AvObj_val(Field(h, 0));
  }

  codec = av_codec_iterate(&s);

  if (!codec) {
    CAMLreturn(Val_int(0));
  }

  for (i = 0; i < AV_CODEC_ID_AUDIO_TAB_LEN; i++) {
    if (codec->id == AV_CODEC_ID_AUDIO_TAB[i][1])
      id = AV_CODEC_ID_AUDIO_TAB[i][0];
  }

  for (i = 0; i < AV_CODEC_ID_VIDEO_TAB_LEN; i++) {
    if (codec->id == AV_CODEC_ID_VIDEO_TAB[i][1])
      id = AV_CODEC_ID_VIDEO_TAB[i][0];
  }

  for (i = 0; i < AV_CODEC_ID_SUBTITLE_TAB_LEN; i++) {
    if (codec->id == AV_CODEC_ID_SUBTITLE_TAB[i][1])
      id = AV_CODEC_ID_SUBTITLE_TAB[i][0];
  }

  if (id == VALUE_NOT_FOUND)
    _id = Val_int(0);
  else {
    _id = caml_alloc_tuple(1);
    Store_field(_id, 0, id);
  }

  _h = caml_alloc_tuple(1);
  Store_field(_h, 0, value_of_avobj(&_tmp, s));

  _ans = caml_alloc_tuple(4);
  Store_field(_ans, 0, value_of_avcodec(&_tmp, codec));
  Store_field(_ans, 1, _id);
  Store_field(_ans, 2, Val_bool(av_codec_is_encoder(codec)));
  Store_field(_ans, 3, _h);

  _ret = caml_alloc_tuple(1);
  Store_field(_ret, 0, _ans);

  CAMLreturn(_ret);
}

CAMLprim value ocaml_avcodec_get_name(value codec) {
  CAMLparam1(codec);
  CAMLreturn(caml_copy_string(AvCodec_val(codec)->name));
}

CAMLprim value ocaml_avcodec_get_description(value _codec) {
  CAMLparam1(_codec);
  const AVCodec *codec = AvCodec_val(_codec);

  if (!codec->long_name)
    CAMLreturn(caml_copy_string(""));

  CAMLreturn(caml_copy_string(codec->long_name));
}

#define BsfCursor_val(v) (*(void **)Data_abstract_val(v))

CAMLprim value ocaml_avcodec_bsf_next(value _cursor) {
  CAMLparam1(_cursor);
  CAMLlocal2(ans, tmp);
  int len;
  enum AVCodecID *codec_id;
  void *cursor = NULL;
  if (_cursor != Val_none)
    cursor = BsfCursor_val(Field(_cursor, 0));

  const AVBitStreamFilter *filter = av_bsf_iterate(&cursor);

  if (!filter)
    CAMLreturn(Val_none);

  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, caml_copy_string(filter->name));

  len = 0;
  codec_id = (enum AVCodecID *)filter->codec_ids;
  while (codec_id && *codec_id != AV_CODEC_ID_NONE) {
    codec_id++;
    len++;
  }

  tmp = caml_alloc_tuple(len);
  len = 0;
  codec_id = (enum AVCodecID *)filter->codec_ids;
  while (codec_id && *codec_id != AV_CODEC_ID_NONE) {
    Store_field(tmp, len, Val_CodecID(*codec_id));
    codec_id++;
    len++;
  }
  Store_field(ans, 1, tmp);
  Store_field(ans, 2, value_of_avclass(&tmp, filter->priv_class));

  tmp = caml_alloc(1, Abstract_tag);
  BsfCursor_val(tmp) = cursor;

  Store_field(ans, 3, tmp);

  tmp = caml_alloc_tuple(1);
  Store_field(tmp, 0, ans);

  CAMLreturn(tmp);
}

#define BsfFilter_val(v) (*(AVBSFContext **)Data_custom_val(v))

static void finalize_bsf_filter(value v) {
  AVBSFContext *filter = BsfFilter_val(v);
  av_bsf_free(&filter);
}

static struct custom_operations bsf_filter_ops = {
    "bsf_filter_parameters",  finalize_bsf_filter,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_avcodec_bsf_init(value _opts, value _name, value _params) {
  CAMLparam3(_opts, _name, _params);
  CAMLlocal3(tmp, ans, unused);
  AVCodecParameters *params = CodecParameters_val(_params);
  AVBSFContext *bsf;
  const AVBitStreamFilter *filter;
  AVDictionary *options = NULL;
  int ret;

  filter = av_bsf_get_by_name(String_val(_name));

  if (!filter) {
    caml_raise_not_found();
  }

  char *key, *val;
  int len = Wosize_val(_opts);
  int i, err, count;

  for (i = 0; i < len; i++) {
    // Dictionaries copy key/values by default!
    key = (char *)Bytes_val(Field(Field(_opts, i), 0));
    val = (char *)Bytes_val(Field(Field(_opts, i), 1));
    err = av_dict_set(&options, key, val, 0);
    if (err < 0) {
      av_dict_free(&options);
      ocaml_avutil_raise_error(err);
    }
  }

  ret = av_bsf_alloc(filter, &bsf);
  if (ret < 0) {
    ocaml_avutil_raise_error(ret);
  }

  ret = avcodec_parameters_copy(bsf->par_in, params);
  if (ret < 0) {
    av_bsf_free(&bsf);
    ocaml_avutil_raise_error(ret);
  }

  ret = av_opt_set_dict(bsf, &options);
  if (ret < 0) {
    av_bsf_free(&bsf);
    ocaml_avutil_raise_error(ret);
  }

  caml_release_runtime_system();
  ret = av_bsf_init(bsf);
  caml_acquire_runtime_system();

  if (ret < 0) {
    av_bsf_free(&bsf);
    ocaml_avutil_raise_error(ret);
  }

  // Return unused keys
  count = av_dict_count(options);

  unused = caml_alloc_tuple(count);
  AVDictionaryEntry *entry = NULL;
  for (i = 0; i < count; i++) {
    entry = av_dict_get(options, "", entry, AV_DICT_IGNORE_SUFFIX);
    Store_field(unused, i, caml_copy_string(entry->key));
  }

  av_dict_free(&options);

  tmp = caml_alloc_custom(&bsf_filter_ops, sizeof(AVBSFContext *), 0, 1);
  BsfFilter_val(tmp) = bsf;

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, tmp);

  value_of_codec_parameters_copy(bsf->par_out, &tmp);
  Store_field(ans, 1, tmp);

  Store_field(ans, 2, unused);

  CAMLreturn(ans);
}

CAMLprim value ocaml_avcodec_bsf_send_packet(value _filter, value _packet) {
  CAMLparam2(_filter, _packet);
  int ret;
  AVPacket *packet = Packet_val(_packet);
  AVBSFContext *bsf = BsfFilter_val(_filter);

  caml_release_runtime_system();
  ret = av_bsf_send_packet(bsf, packet);
  caml_acquire_runtime_system();

  if (ret < 0) {
    ocaml_avutil_raise_error(ret);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_bsf_send_eof(value _filter) {
  CAMLparam1(_filter);
  int ret;
  AVBSFContext *bsf = BsfFilter_val(_filter);

  caml_release_runtime_system();
  ret = av_bsf_send_packet(bsf, NULL);
  caml_acquire_runtime_system();

  if (ret < 0) {
    ocaml_avutil_raise_error(ret);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avcodec_bsf_receive_packet(value _filter) {
  CAMLparam1(_filter);
  CAMLlocal1(ans);
  int ret;
  AVPacket *packet;

  packet = av_packet_alloc();

  if (!packet) {
    caml_raise_out_of_memory();
  }

  caml_release_runtime_system();
  ret = av_bsf_receive_packet(BsfFilter_val(_filter), packet);
  caml_acquire_runtime_system();

  if (ret < 0) {
    av_packet_free(&packet);
    ocaml_avutil_raise_error(ret);
  }

  CAMLreturn(value_of_ffmpeg_packet(&ans, packet));
}

CAMLprim value ocaml_avcodec_version(value unit) {
  (void)unit;
  return Val_int(avcodec_version());
}
