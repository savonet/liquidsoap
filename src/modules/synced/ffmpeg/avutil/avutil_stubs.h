#ifndef _AVUTIL_STUBS_H_
#define _AVUTIL_STUBS_H_

#include <stdio.h>

#include <caml/mlvalues.h>

#include <libavcodec/avcodec.h>
#include <libavutil/channel_layout.h>
#include <libavutil/frame.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>
#include <libavutil/samplefmt.h>

#include "polymorphic_variant_values_stubs.h"

/* AV_OPT_TYPE_FLAG_ARRAY was added in libavutil 59.1.100 (FFmpeg 7) as an
   enum member, not a preprocessor macro, so #ifdef cannot be used. */
#if LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(59, 1, 100)
#define HAVE_AV_OPT_TYPE_FLAG_ARRAY
#endif

#define Val_none Val_int(0)

#ifndef Some_val
#define Some_val(v) Field(v, 0)
#endif

#define ERROR_MSG_SIZE 256
#define EXN_ERROR "ffmpeg_exn_error"

#define Fail(...)                                                              \
  {                                                                            \
    snprintf(ocaml_av_exn_msg, ERROR_MSG_SIZE, __VA_ARGS__);                   \
    caml_callback(*caml_named_value("ffmpeg_exn_failure"),                     \
                  caml_copy_string(ocaml_av_exn_msg));                         \
  }

void ocaml_avutil_raise_error(int err);

extern char ocaml_av_exn_msg[];

#define List_init(list) (list) = Val_emptylist

#define List_add(list, cons, val)                                              \
  {                                                                            \
    (cons) = caml_alloc(2, 0);                                                 \
    Store_field((cons), 0, (val));                                             \
    Store_field((cons), 1, (list));                                            \
    (list) = (cons);                                                           \
  }

/***** Global initialisation *****/

void ocaml_ffmpeg_register_thread();

/**** AVRational ****/
#define rational_of_value(v)                                                   \
  ((AVRational){Int_val(Field((v), 0)), Int_val(Field((v), 1))})

void value_of_rational(const AVRational *r, value *pv);

/**** Time format ****/

int64_t second_fractions_of_time_format(value time_format);

/**** Channel layout ****/

#define AVChannelLayout_val(v) (*(struct AVChannelLayout **)Data_custom_val(v))
void value_of_channel_layout(value *v, const AVChannelLayout *);

/**** Sample format ****/

#define Sample_format_val(v) (Int_val(v))

enum AVSampleFormat SampleFormat_val(value v);

enum AVSampleFormat AVSampleFormat_of_Sample_format(int i);

value Val_SampleFormat(enum AVSampleFormat sf);
enum caml_ba_kind bigarray_kind_of_AVSampleFormat(enum AVSampleFormat sf);

/**** Color space ****/

enum AVColorSpace ColorSpace_val(value);

value Val_ColorSpace(enum AVColorSpace csp);

/**** Color range ****/

enum AVColorRange ColorRange_val(value);

value Val_ColorRange(enum AVColorRange cr);

/**** Color primaries ****/

enum AVColorPrimaries ColorPrimaries_val(value);

value Val_ColorPrimaries(enum AVColorPrimaries cp);

/**** Color transfer characteristic ****/

enum AVColorTransferCharacteristic ColorTrc_val(value);

value Val_ColorTrc(enum AVColorTransferCharacteristic ct);

/**** Chroma location ****/

enum AVChromaLocation ChromaLocation_val(value);

value Val_ChromaLocation(enum AVChromaLocation cl);

/**** Pixel format ****/

int PixelFormat_val(value);

value Val_PixelFormat(enum AVPixelFormat pf);

/**** Buffer Ref ****/

#define BufferRef_val(v) (*(AVBufferRef **)Data_custom_val(v))

/**** Device Type ****/

enum AVHWDeviceType HwDeviceType_val(value v);

value Val_HwDeviceType(enum AVHWDeviceType t);

/***** AVFrame *****/

#define Frame_val(v) (*(struct AVFrame **)Data_custom_val(v))

void value_of_frame(value *ret, AVFrame *frame);

/***** AVSubtitle *****/
#define Subtitle_val(v) (*(struct AVSubtitle **)Data_custom_val(v))

void value_of_subtitle(value *ret, AVSubtitle *subtitle);

/***** AVPixelFormat *****/

#define AvPixFmtDescriptor_val(v)                                              \
  (*(const AVPixFmtDescriptor **)Data_abstract_val(v))

static inline value
value_of_avpixfmtdescriptor(value ret,
                            const AVPixFmtDescriptor *avpixfmtdescriptor) {
  ret = caml_alloc(1, Abstract_tag);
  AvPixFmtDescriptor_val(ret) = avpixfmtdescriptor;
  return ret;
}

/****** AVOptions ******/

#define AvClass_val(v) (*(const AVClass **)Data_abstract_val(v))

static inline value value_of_avclass(value *ret, const AVClass *avclass) {
  *ret = caml_alloc(1, Abstract_tag);
  AvClass_val(*ret) = avclass;
  return *ret;
}

#define AvOptions_val(v) (*(const struct AVOption **)Data_abstract_val(v))

static inline value value_of_avoptions(value *ret,
                                       const struct AVOption *avoptions) {
  *ret = caml_alloc(1, Abstract_tag);
  AvOptions_val(*ret) = avoptions;
  return *ret;
}

#define AvObj_val(v) (*(void **)Data_abstract_val(v))

static inline value value_of_avobj(value *ret, void *avobj) {
  *ret = caml_alloc(1, Abstract_tag);
  AvObj_val(*ret) = avobj;
  return *ret;
}

#endif // _AVUTIL_STUBS_H_
