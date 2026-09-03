#ifndef _AVCODEC_STUBS_H_
#define _AVCODEC_STUBS_H_

#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <libavcodec/avcodec.h>

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(60, 26, 100)
#define AV_PROFILE_UNKNOWN FF_PROFILE_UNKNOWN
#define AV_LEVEL_UNKNOWN FF_LEVEL_UNKNOWN
#endif

/* libavcodec runs a codec on a single thread unless told otherwise, ffmpeg's
   own tools raise that to automatic before opening one: do the same. Codecs
   that thread badly opt out through their own defaults, and [options], which
   avcodec_open2 applies itself, still wins over ours. */
static inline int ocaml_avcodec_open_codec(AVCodecContext *codec_context,
                                           const AVCodec *codec,
                                           AVDictionary **options) {
  int ret;

  codec_context->thread_count = 0;

  caml_release_runtime_system();
  ret = avcodec_open2(codec_context, codec, options);
  caml_acquire_runtime_system();

  return ret;
}

/***** AVCodec *****/

#define AvCodec_val(v) (*(const AVCodec **)Data_abstract_val(v))

static inline value value_of_avcodec(value *ret, const AVCodec *avcodec) {
  *ret = caml_alloc(1, Abstract_tag);
  AvCodec_val(*ret) = avcodec;
  return *ret;
}

/***** Codec parameters *****/

#define CodecParameters_val(v)                                                 \
  (*(struct AVCodecParameters **)Data_custom_val(v))

void value_of_codec_parameters_copy(AVCodecParameters *src, value *pvalue);

/***** Packet *****/

#define Packet_val(v) (*(struct AVPacket **)Data_custom_val(v))

value value_of_ffmpeg_packet(value *val_packet, AVPacket *packet);

/**** Audio codec ID ****/

enum AVCodecID AudioCodecID_val(value v);

value Val_AudioCodecID(enum AVCodecID id);

/**** Video codec ID ****/

enum AVCodecID VideoCodecID_val(value v);

value Val_VideoCodecID(enum AVCodecID id);

/**** Subtitle codec ID ****/

enum AVCodecID SubtitleCodecID_val(value v);

value Val_SubtitleCodecID(enum AVCodecID id);

/**** Unknown codec ID ****/

enum AVCodecID UnknownCodecID_val(value v);

value Val_UnknownCodecID(enum AVCodecID id);

#endif // _AVCODEC_STUBS_H_
