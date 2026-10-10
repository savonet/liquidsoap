/* Doubles for the avutil conformance tests: what the tests need from
   FFmpeg's side of the boundary, and from the services avutil gives the
   stubs of other libraries. */

#include <pthread.h>
#include <string.h>

#include "avutil_stubs.h"

#include <libavutil/avstring.h>
#include <libavutil/error.h>
#include <libavutil/eval.h>
#include <libavutil/imgutils.h>
#include <libavutil/log.h>
#include <libavutil/mem.h>
#include <libavutil/pixdesc.h>

static const int error_codes[] = {
    AVERROR_BSF_NOT_FOUND,
    AVERROR_DECODER_NOT_FOUND,
    AVERROR_DEMUXER_NOT_FOUND,
    AVERROR_ENCODER_NOT_FOUND,
    AVERROR_EOF,
    AVERROR_EXIT,
    AVERROR_FILTER_NOT_FOUND,
    AVERROR_INVALIDDATA,
    AVERROR_MUXER_NOT_FOUND,
    AVERROR_OPTION_NOT_FOUND,
    AVERROR_PATCHWELCOME,
    AVERROR_PROTOCOL_NOT_FOUND,
    AVERROR_STREAM_NOT_FOUND,
    AVERROR_BUG,
    AVERROR(EAGAIN),
    AVERROR_UNKNOWN,
    AVERROR_EXPERIMENTAL,
    AVERROR_EXTERNAL,
};

#define ERROR_CODE_COUNT (sizeof(error_codes) / sizeof(error_codes[0]))

/* (code, FFmpeg's text) for each code of spec/avutil.md §5.1, in the
   table's order, then AVERROR_EXTERNAL. */
CAMLprim value test_error_codes(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal2(_codes, _code);
  char text[AV_ERROR_MAX_STRING_SIZE];

  _codes = caml_alloc_tuple(ERROR_CODE_COUNT);

  for (size_t i = 0; i < ERROR_CODE_COUNT; i++) {
    _code = caml_alloc_tuple(2);
    Store_field(_code, 0, Val_int(error_codes[i]));
    Store_field(_code, 1,
                caml_copy_string(
                    av_make_error_string(text, sizeof(text), error_codes[i])));
    Store_field(_codes, i, _code);
  }

  CAMLreturn(_codes);
}

CAMLprim value test_raise_error(value _code) {
  ocaml_avutil_raise_error(Int_val(_code));
}

CAMLprim value test_wrap_null_frame(value _unit) {
  (void)_unit;
  return ocaml_avutil_wrap_frame(NULL);
}

CAMLprim value test_wrap_null_subtitle(value _unit) {
  (void)_unit;
  return ocaml_avutil_wrap_subtitle(NULL);
}

CAMLprim value test_pixel_format_of_id(value _id) {
  return Val_PixelFormat(Int_val(_id));
}

CAMLprim value test_media_types(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_types);
  static const enum AVMediaType types[] = {
      AVMEDIA_TYPE_UNKNOWN, AVMEDIA_TYPE_VIDEO,    AVMEDIA_TYPE_AUDIO,
      AVMEDIA_TYPE_DATA,    AVMEDIA_TYPE_SUBTITLE, AVMEDIA_TYPE_ATTACHMENT};

  _types = caml_alloc_tuple(6);
  for (int i = 0; i < 6; i++)
    Store_field(_types, i, Val_MediaType(types[i]));

  CAMLreturn(_types);
}

CAMLprim value test_time_format_units(value _time_format) {
  return Val_long(ocaml_avutil_time_format_units(_time_format));
}

CAMLprim value test_rational_roundtrip(value _rational) {
  return ocaml_avutil_value_of_rational(
      ocaml_avutil_rational_of_value(_rational));
}

CAMLprim value test_bigarray_kind(value _sample_format) {
  return Val_int(ocaml_avutil_bigarray_kind_of_sample_format(
      SampleFormat_val(_sample_format)));
}

CAMLprim value test_bigarray_kinds(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_kinds);

  _kinds = caml_alloc_tuple(6);
  Store_field(_kinds, 0, Val_int(CAML_BA_UINT8));
  Store_field(_kinds, 1, Val_int(CAML_BA_SINT16));
  Store_field(_kinds, 2, Val_int(CAML_BA_INT32));
  Store_field(_kinds, 3, Val_int(CAML_BA_INT64));
  Store_field(_kinds, 4, Val_int(CAML_BA_FLOAT32));
  Store_field(_kinds, 5, Val_int(CAML_BA_FLOAT64));

  CAMLreturn(_kinds);
}

CAMLprim value test_parse_double(value _text) {
  return caml_copy_double(av_strtod(String_val(_text), NULL));
}

CAMLprim value test_options_dictionary(value _bindings) {
  CAMLparam1(_bindings);
  CAMLlocal1(_pairs);
  AVDictionary *dictionary = ocaml_avutil_dictionary_of_options(_bindings);

  _pairs = ocaml_avutil_pairs_of_dictionary(dictionary);
  av_dict_free(&dictionary);

  CAMLreturn(_pairs);
}

/* The option-table protocol with a consumer that takes one key. */
CAMLprim value test_unused_options(value _bindings, value _consumed) {
  CAMLparam2(_bindings, _consumed);
  AVDictionary *dictionary = ocaml_avutil_dictionary_of_options(_bindings);

  av_dict_set(&dictionary, String_val(_consumed), NULL, 0);

  CAMLreturn(ocaml_avutil_unused_options(&dictionary));
}

CAMLprim value test_log(value _level, value _message) {
  av_log(NULL, Int_val(_level), "%s", String_val(_message));
  return Val_unit;
}

typedef struct {
  int thread;
  int count;
} logging_thread;

static void *run_logging_thread(void *argument) {
  logging_thread *thread = argument;

  for (int i = 0; i < thread->count; i++)
    av_log(NULL, AV_LOG_INFO, "thread %d message %d\n", thread->thread, i);

  return NULL;
}

/* Logs [count] messages from each of [threads] threads the runtime does not
   know, and returns when they are all done. */
CAMLprim value test_log_from_threads(value _threads, value _count) {
  int threads = Int_val(_threads);
  int count = Int_val(_count);
  pthread_t handles[16];
  logging_thread arguments[16];

  if (threads > 16)
    threads = 16;

  caml_release_runtime_system();
  for (int i = 0; i < threads; i++) {
    arguments[i].thread = i;
    arguments[i].count = count;
    pthread_create(&handles[i], NULL, run_logging_thread, &arguments[i]);
  }
  for (int i = 0; i < threads; i++)
    pthread_join(handles[i], NULL);
  caml_acquire_runtime_system();

  return Val_unit;
}

static void *run_registered_thread(void *argument) {
  value *_closure = argument;

  ocaml_avutil_register_thread();
  caml_acquire_runtime_system();
  caml_callback_exn(*_closure, Val_unit);
  caml_release_runtime_system();

  return NULL;
}

/* Calls [_closure] from a thread created here, through the registration
   service, and joins the thread. */
CAMLprim value test_call_from_c_thread(value _closure) {
  CAMLparam1(_closure);
  pthread_t thread;

  caml_release_runtime_system();
  pthread_create(&thread, NULL, run_registered_thread, &_closure);
  pthread_join(thread, NULL);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* Sets on a video frame, through FFmpeg, the properties the binding
   reads. */
CAMLprim value test_set_video_properties(value _frame) {
  AVFrame *frame = Frame_val(_frame);

  frame->sample_aspect_ratio = (AVRational){4, 3};
  frame->colorspace = AVCOL_SPC_BT709;
  frame->color_range = AVCOL_RANGE_JPEG;
  frame->color_primaries = AVCOL_PRI_BT2020;
  frame->color_trc = AVCOL_TRC_SMPTE2084;
  frame->chroma_location = AVCHROMA_LOC_TOPLEFT;
  frame->pts = 1234;
  frame->pkt_dts = 5678;
  frame->duration = 90;
  frame->best_effort_timestamp = 4321;
  av_dict_set(&frame->metadata, "native", "value", 0);

  return Val_unit;
}

CAMLprim value test_plane_size(value _frame, value _plane) {
  const AVFrame *frame = Frame_val(_frame);
  ptrdiff_t linesizes[4];
  size_t sizes[4];

  for (int i = 0; i < 4; i++)
    linesizes[i] = frame->linesize[i];
  av_image_fill_plane_sizes(sizes, frame->format, frame->height, linesizes);

  return Val_long(sizes[Int_val(_plane)]);
}

/* A second frame over the buffers of [_frame]. */
CAMLprim value test_share_frame(value _frame) {
  CAMLparam1(_frame);
  AVFrame *shared = av_frame_clone(Frame_val(_frame));

  if (!shared)
    caml_raise_out_of_memory();

  CAMLreturn(ocaml_avutil_wrap_frame(shared));
}

CAMLprim value test_touch_frame(value _frame) {
  AVFrame *frame = Frame_val(_frame);

  for (int i = 0; i < AV_NUM_DATA_POINTERS; i++) {
    if (frame->buf[i])
      memset(frame->buf[i]->data, 1, frame->buf[i]->size);
  }

  return Val_unit;
}

/* A frame of a hardware format that holds no data. */
CAMLprim value test_hardware_frame(value _unit) {
  AVFrame *frame = av_frame_alloc();

  (void)_unit;
  if (!frame)
    caml_raise_out_of_memory();
  frame->format = AV_PIX_FMT_VAAPI;
  frame->width = 16;
  frame->height = 16;

  return ocaml_avutil_wrap_frame(frame);
}

/* An object with one option of each type and a child object, standing for
   the option-bearing objects of the dependent libraries. */

typedef struct {
  const AVClass *class;
  int child_only;
} option_child;

typedef struct {
  const AVClass *class;
  int int_option;
  int64_t int64_option;
  uint64_t uint64_option;
  int flags_option;
  double double_option;
  float float_option;
  char *string_option;
  AVRational rational_option;
  int width, height;
  enum AVPixelFormat pixel_option;
  enum AVSampleFormat sample_option;
  AVRational rate_option;
  int64_t duration_option;
  uint8_t color_option[4];
  AVChannelLayout layout_option;
  int bool_option;
  AVDictionary *dict_option;
  unsigned uint_option;
  int *array_option;
  unsigned array_option_count;
  option_child child;
  int closed;
} option_owner;

#define CHILD_OFFSET(field) offsetof(option_child, field)
#define OWNER_OFFSET(field) offsetof(option_owner, field)

static const AVOption child_options[] = {
    {"child_only",
     "only on the child",
     CHILD_OFFSET(child_only),
     AV_OPT_TYPE_INT,
     {.i64 = 7},
     0,
     100,
     0,
     NULL},
    {0},
};

static const AVClass child_class = {
    .class_name = "test child",
    .item_name = av_default_item_name,
    .option = child_options,
    .version = LIBAVUTIL_VERSION_INT,
};

static const AVOption owner_options[] = {
    {"int_option",
     "an integer",
     OWNER_OFFSET(int_option),
     AV_OPT_TYPE_INT,
     {.i64 = 3},
     -10,
     10,
     0,
     "mode"},
    {"low", "low mode", 0, AV_OPT_TYPE_CONST, {.i64 = 1}, 0, 0, 0, "mode"},
    {"high", "high mode", 0, AV_OPT_TYPE_CONST, {.i64 = 2}, 0, 0, 0, "mode"},
    {"int64_option",
     "",
     OWNER_OFFSET(int64_option),
     AV_OPT_TYPE_INT64,
     {.i64 = 5},
     (double)INT64_MIN,
     (double)INT64_MAX,
     AV_OPT_FLAG_ENCODING_PARAM | AV_OPT_FLAG_AUDIO_PARAM,
     NULL},
    {"uint64_option",
     NULL,
     OWNER_OFFSET(uint64_option),
     AV_OPT_TYPE_UINT64,
     {.i64 = 6},
     0,
     (double)UINT64_MAX,
     0,
     NULL},
    {"flags_option",
     "flags",
     OWNER_OFFSET(flags_option),
     AV_OPT_TYPE_FLAGS,
     {.i64 = 1},
     0,
     3,
     0,
     "fl"},
    {"first", "", 0, AV_OPT_TYPE_CONST, {.i64 = 1}, 0, 0, 0, "fl"},
    {"second", "", 0, AV_OPT_TYPE_CONST, {.i64 = 2}, 0, 0, 0, "fl"},
    {"double_option",
     "",
     OWNER_OFFSET(double_option),
     AV_OPT_TYPE_DOUBLE,
     {.dbl = 0.5},
     -1,
     1,
     0,
     "level"},
    {"half", "", 0, AV_OPT_TYPE_CONST, {.dbl = 0.5}, 0, 0, 0, "level"},
    {"float_option",
     "",
     OWNER_OFFSET(float_option),
     AV_OPT_TYPE_FLOAT,
     {.dbl = 0.25},
     -1,
     1,
     0,
     NULL},
    {"string_option",
     "",
     OWNER_OFFSET(string_option),
     AV_OPT_TYPE_STRING,
     {.str = "default"},
     0,
     0,
     0,
     NULL},
    {"rational_option",
     "",
     OWNER_OFFSET(rational_option),
     AV_OPT_TYPE_RATIONAL,
     {.dbl = 0.5},
     0,
     10,
     0,
     NULL},
    {"size_option",
     "",
     OWNER_OFFSET(width),
     AV_OPT_TYPE_IMAGE_SIZE,
     {.str = "320x240"},
     0,
     0,
     0,
     NULL},
    {"pixel_option",
     "",
     OWNER_OFFSET(pixel_option),
     AV_OPT_TYPE_PIXEL_FMT,
     {.i64 = AV_PIX_FMT_YUV420P},
     -1,
     INT_MAX,
     0,
     NULL},
    {"sample_option",
     "",
     OWNER_OFFSET(sample_option),
     AV_OPT_TYPE_SAMPLE_FMT,
     {.i64 = AV_SAMPLE_FMT_NONE},
     -1,
     INT_MAX,
     0,
     NULL},
    {"rate_option",
     "",
     OWNER_OFFSET(rate_option),
     AV_OPT_TYPE_VIDEO_RATE,
     {.str = "25"},
     0,
     INT_MAX,
     0,
     NULL},
    {"duration_option",
     "",
     OWNER_OFFSET(duration_option),
     AV_OPT_TYPE_DURATION,
     {.i64 = 1000},
     0,
     (double)INT64_MAX,
     0,
     NULL},
    {"color_option",
     "",
     OWNER_OFFSET(color_option),
     AV_OPT_TYPE_COLOR,
     {.str = "red"},
     0,
     0,
     0,
     NULL},
    {"layout_option",
     "",
     OWNER_OFFSET(layout_option),
     AV_OPT_TYPE_CHLAYOUT,
     {.str = "stereo"},
     0,
     0,
     0,
     NULL},
    {"bool_option",
     "",
     OWNER_OFFSET(bool_option),
     AV_OPT_TYPE_BOOL,
     {.i64 = 1},
     -1,
     1,
     0,
     NULL},
    {"auto_bool_option",
     "",
     OWNER_OFFSET(bool_option),
     AV_OPT_TYPE_BOOL,
     {.i64 = -1},
     -1,
     1,
     0,
     NULL},
    {"dict_option",
     "",
     OWNER_OFFSET(dict_option),
     AV_OPT_TYPE_DICT,
     {.str = NULL},
     0,
     0,
     0,
     NULL},
    {"uint_option",
     "a type the bindings do not list",
     OWNER_OFFSET(uint_option),
     AV_OPT_TYPE_UINT,
     {.i64 = 1},
     0,
     10,
     0,
     NULL},
    {"array_option",
     "",
     OWNER_OFFSET(array_option),
     AV_OPT_TYPE_INT | AV_OPT_TYPE_FLAG_ARRAY,
     {.arr = NULL},
     0,
     100,
     0,
     NULL},
    {0},
};

static void *owner_child_next(void *object, void *previous) {
  option_owner *owner = object;

  return previous ? NULL : &owner->child;
}

static const AVClass *owner_child_class_iterate(void **iterator) {
  const AVClass *child = *iterator ? NULL : &child_class;

  *iterator = (void *)1;

  return child;
}

static const AVClass owner_class = {
    .class_name = "test owner",
    .item_name = av_default_item_name,
    .option = owner_options,
    .version = LIBAVUTIL_VERSION_INT,
    .child_next = owner_child_next,
    .child_class_iterate = owner_child_class_iterate,
};

#define OptionOwner_val(v) (*(option_owner **)Data_custom_val(v))

static _Atomic int finalized_owners = 0;

static void finalize_option_owner(value _owner) {
  option_owner *owner = OptionOwner_val(_owner);

  if (owner) {
    av_opt_free(owner);
    av_free(owner);
    finalized_owners++;
  }
}

static struct custom_operations option_owner_operations = {
    "test_option_owner",        finalize_option_owner,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static void *acquire_option_owner(value _owner) {
  option_owner *owner = OptionOwner_val(_owner);

  if (owner->closed)
    ocaml_avutil_raise_failure("Container closed!");

  return owner;
}

static void release_option_owner(value _owner) { (void)_owner; }

static const ocaml_avutil_option_access option_owner_access = {
    acquire_option_owner, release_option_owner};

CAMLprim value test_option_class(value _unit) {
  (void)_unit;
  return ocaml_avutil_wrap_option_class(&owner_class);
}

CAMLprim value test_no_option_class(value _unit) {
  (void)_unit;
  return ocaml_avutil_wrap_option_class(NULL);
}

/* An Options.obj over a fresh owner whose options are set, through FFmpeg,
   to values that differ from their defaults; [_closed] makes its acquire
   raise the closed error. */
CAMLprim value test_option_object(value _closed) {
  CAMLparam1(_closed);
  CAMLlocal1(_owner);
  option_owner *owner;

  _owner =
      caml_alloc_custom(&option_owner_operations, sizeof(option_owner *), 0, 1);
  OptionOwner_val(_owner) = NULL;

  owner = av_mallocz(sizeof(*owner));
  if (!owner)
    caml_raise_out_of_memory();
  OptionOwner_val(_owner) = owner;

  owner->class = &owner_class;
  owner->child.class = &child_class;
  av_opt_set_defaults(owner);
  av_opt_set_defaults(&owner->child);
  owner->closed = Bool_val(_closed);

  av_opt_set_int(owner, "int_option", -4, 0);
  av_opt_set_int(owner, "int64_option", INT64_MAX, 0);
  av_opt_set_double(owner, "double_option", 0.75, 0);
  av_opt_set(owner, "string_option", "changed", 0);
  av_opt_set_q(owner, "rational_option", (AVRational){3, 4}, 0);
  av_opt_set_image_size(owner, "size_option", 640, 360, 0);
  av_opt_set_pixel_fmt(owner, "pixel_option", AV_PIX_FMT_RGB24, 0);
  av_opt_set_sample_fmt(owner, "sample_option", AV_SAMPLE_FMT_FLTP, 0);
  av_opt_set_video_rate(owner, "rate_option", (AVRational){30000, 1001}, 0);
  av_opt_set(owner, "layout_option", "FR+FL", 0);
  av_opt_set(owner, "dict_option", "first=1:second=2", 0);
  av_opt_set_int(owner, "child_only", 42, AV_OPT_SEARCH_CHILDREN);

  CAMLreturn(ocaml_avutil_option_object(_owner, &option_owner_access));
}

CAMLprim value test_finalized_option_owners(value _unit) {
  (void)_unit;
  return Val_int(finalized_owners);
}

/* Planted defects: each must make its run fail in the configuration that
   is meant to catch it. */

CAMLprim value control_overflow(value _offset) {
  volatile char *buffer = av_malloc(64);

  buffer[Int_val(_offset)] = 1;
  av_free((void *)buffer);

  return Val_unit;
}

CAMLprim value control_leak(value _unit) {
  volatile char *buffer = av_malloc(1 << 20);

  (void)_unit;
  buffer[0] = 1;
  buffer = NULL;

  return Val_unit;
}

/* [_text] is not registered: the second allocation moves it. */
CAMLprim value control_unrooted(value _unit) {
  value _text = caml_copy_string("a value held across an allocation");
  value _pair = caml_alloc_tuple(2);

  (void)_unit;
  Store_field(_pair, 0, _text);
  Store_field(_pair, 1, Val_int(0));

  return _pair;
}
