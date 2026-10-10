/* Stubs of the avutil binding, spec/avutil.md.

   Handles are custom blocks holding one pointer to a native object whose
   address never moves. A constructor allocates its handle first, with a
   null pointer, wherever it can: the native object then has an owner from
   the moment it exists and every later failure may raise at once. */

#include <limits.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "avutil_stubs.h"

#include <libavcodec/avcodec.h>
#include <libavutil/display.h>
#include <libavutil/eval.h>
#include <libavutil/imgutils.h>
#include <libavutil/log.h>
#include <libavutil/mem.h>
#include <libavutil/parseutils.h>
#include <libavutil/pixdesc.h>

#include "channel_layout_stubs.h"
#include "chroma_location_stubs.h"
#include "color_primaries_stubs.h"
#include "color_range_stubs.h"
#include "color_space_stubs.h"
#include "color_trc_stubs.h"
#include "frame_side_data_type_stubs.h"
#include "hw_device_type_stubs.h"
#include "media_types_stubs.h"
#include "pixel_format_flag_stubs.h"
#include "pixel_format_stubs.h"
#include "sample_format_stubs.h"
#include "side_data_prop_stubs.h"
#include "subtitle_flag_stubs.h"
#include "subtitle_type_stubs.h"

#define LOG_LINE_MAX 1024
#define VIDEO_FRAME_ALIGN 32
#define SUBTITLE_PLANES 4

#define TABLE_LENGTH(table) (sizeof(table) / sizeof((table)[0]))

#ifdef OCAML_FFMPEG_GC_STRESS
#include <caml/minor_gc.h>

void caml_finish_major_cycle(int force_compaction);

void ocaml_avutil_gc_stress(void) {
  (caml_minor_collection)();
  caml_finish_major_cycle(1);
}
#endif

static const ocaml_ffmpeg_variant_entry error_entries[] = {
    {PVV_Bsf_not_found, AVERROR_BSF_NOT_FOUND},
    {PVV_Decoder_not_found, AVERROR_DECODER_NOT_FOUND},
    {PVV_Demuxer_not_found, AVERROR_DEMUXER_NOT_FOUND},
    {PVV_Encoder_not_found, AVERROR_ENCODER_NOT_FOUND},
    {PVV_Eof, AVERROR_EOF},
    {PVV_Exit, AVERROR_EXIT},
    {PVV_Filter_not_found, AVERROR_FILTER_NOT_FOUND},
    {PVV_Invalid_data, AVERROR_INVALIDDATA},
    {PVV_Muxer_not_found, AVERROR_MUXER_NOT_FOUND},
    {PVV_Option_not_found, AVERROR_OPTION_NOT_FOUND},
    {PVV_Patch_welcome, AVERROR_PATCHWELCOME},
    {PVV_Protocol_not_found, AVERROR_PROTOCOL_NOT_FOUND},
    {PVV_Stream_not_found, AVERROR_STREAM_NOT_FOUND},
    {PVV_Bug, AVERROR_BUG},
    {PVV_Eagain, AVERROR(EAGAIN)},
    {PVV_Unknown, AVERROR_UNKNOWN},
    {PVV_Experimental, AVERROR_EXPERIMENTAL},
};
static const ocaml_ffmpeg_variant_table error_table = {
    error_entries, TABLE_LENGTH(error_entries), "Avutil.error"};

static const ocaml_ffmpeg_variant_entry log_level_entries[] = {
    {PVV_Quiet, AV_LOG_QUIET},     {PVV_Panic, AV_LOG_PANIC},
    {PVV_Fatal, AV_LOG_FATAL},     {PVV_Error, AV_LOG_ERROR},
    {PVV_Warning, AV_LOG_WARNING}, {PVV_Info, AV_LOG_INFO},
    {PVV_Verbose, AV_LOG_VERBOSE}, {PVV_Debug, AV_LOG_DEBUG},
    {PVV_Trace, AV_LOG_TRACE},
};
static const ocaml_ffmpeg_variant_table log_level_table = {
    log_level_entries, TABLE_LENGTH(log_level_entries), "Avutil.Log.level"};

static const ocaml_ffmpeg_variant_entry option_flag_entries[] = {
    {PVV_Encoding_param, AV_OPT_FLAG_ENCODING_PARAM},
    {PVV_Decoding_param, AV_OPT_FLAG_DECODING_PARAM},
    {PVV_Audio_param, AV_OPT_FLAG_AUDIO_PARAM},
    {PVV_Video_param, AV_OPT_FLAG_VIDEO_PARAM},
    {PVV_Subtitle_param, AV_OPT_FLAG_SUBTITLE_PARAM},
    {PVV_Export, AV_OPT_FLAG_EXPORT},
    {PVV_Readonly, AV_OPT_FLAG_READONLY},
    {PVV_Bsf_param, AV_OPT_FLAG_BSF_PARAM},
    {PVV_Runtime_param, AV_OPT_FLAG_RUNTIME_PARAM},
    {PVV_Filtering_param, AV_OPT_FLAG_FILTERING_PARAM},
    {PVV_Deprecated, AV_OPT_FLAG_DEPRECATED},
    {PVV_Child_consts, AV_OPT_FLAG_CHILD_CONSTS},
};
static const ocaml_ffmpeg_variant_table option_flag_table = {
    option_flag_entries, TABLE_LENGTH(option_flag_entries),
    "Avutil.Options.flag"};

static const ocaml_ffmpeg_variant_entry option_type_entries[] = {
    {PVV_Flags, AV_OPT_TYPE_FLAGS},
    {PVV_Int, AV_OPT_TYPE_INT},
    {PVV_Int64, AV_OPT_TYPE_INT64},
    {PVV_UInt64, AV_OPT_TYPE_UINT64},
    {PVV_Duration, AV_OPT_TYPE_DURATION},
    {PVV_Double, AV_OPT_TYPE_DOUBLE},
    {PVV_Float, AV_OPT_TYPE_FLOAT},
    {PVV_Rational, AV_OPT_TYPE_RATIONAL},
    {PVV_String, AV_OPT_TYPE_STRING},
    {PVV_Binary, AV_OPT_TYPE_BINARY},
    {PVV_Dict, AV_OPT_TYPE_DICT},
    {PVV_Image_size, AV_OPT_TYPE_IMAGE_SIZE},
    {PVV_Video_rate, AV_OPT_TYPE_VIDEO_RATE},
    {PVV_Color, AV_OPT_TYPE_COLOR},
    {PVV_Pixel_fmt, AV_OPT_TYPE_PIXEL_FMT},
    {PVV_Sample_fmt, AV_OPT_TYPE_SAMPLE_FMT},
    {PVV_Channel_layout, AV_OPT_TYPE_CHLAYOUT},
    {PVV_Bool, AV_OPT_TYPE_BOOL},
    {PVV_Const, AV_OPT_TYPE_CONST},
};
static const ocaml_ffmpeg_variant_table option_type_table = {
    option_type_entries, TABLE_LENGTH(option_type_entries),
    "Avutil.Options.ground"};

void ocaml_avutil_raise_error(int error_code) {
  CAMLparam0();
  CAMLlocal1(_error);

  if (!ocaml_avutil_find_variant(&error_table, error_code, &_error)) {
    _error = caml_alloc_tuple(2);
    Store_field(_error, 0, PVV_Other);
    Store_field(_error, 1, Val_int(error_code));
  }

  caml_raise_with_arg(*caml_named_value("ocaml_avutil_error"), _error);
  CAMLnoreturn;
}

void ocaml_avutil_raise_failure(const char *format, ...) {
  CAMLparam0();
  CAMLlocal2(_message, _error);
  char message[512];
  va_list arguments;

  va_start(arguments, format);
  vsnprintf(message, sizeof(message), format, arguments);
  va_end(arguments);

  _message = caml_copy_string(message);
  _error = caml_alloc_tuple(2);
  Store_field(_error, 0, PVV_Failure);
  Store_field(_error, 1, _message);

  caml_raise_with_arg(*caml_named_value("ocaml_avutil_error"), _error);
  CAMLnoreturn;
}

void ocaml_avutil_raise_closed(void) {
  ocaml_avutil_raise_failure("Container closed!");
}

void ocaml_avutil_raise_failed(void) {
  ocaml_avutil_raise_failure("Object failed!");
}

void ocaml_avutil_raise_in_use(void) {
  ocaml_avutil_raise_failure("Object in use!");
}

CAMLprim value ocaml_avutil_string_of_error(value _error) {
  CAMLparam1(_error);
  char message[AV_ERROR_MAX_STRING_SIZE];
  int error_code;

  if (Is_block(_error))
    error_code = Int_val(Field(_error, 1));
  else
    error_code = (int)ocaml_avutil_constant_of_variant(&error_table, _error);

  CAMLreturn(caml_copy_string(
      av_make_error_string(message, sizeof(message), error_code)));
}

int ocaml_avutil_int_of_value(value _number, const char *name) {
  intnat number = Long_val(_number);

  if (number < INT_MIN || number > INT_MAX)
    ocaml_avutil_raise_failure("%s is out of range", name);

  return (int)number;
}

int ocaml_avutil_find_variant(const ocaml_ffmpeg_variant_table *table,
                              int64_t constant, value *_variant) {
  for (size_t i = 0; i < table->length; i++) {
    if (table->entries[i].constant == constant) {
      *_variant = table->entries[i].variant;
      return 1;
    }
  }

  return 0;
}

value ocaml_avutil_variant_of_constant(const ocaml_ffmpeg_variant_table *table,
                                       int64_t constant) {
  value _variant;

  if (!ocaml_avutil_find_variant(table, constant, &_variant))
    ocaml_avutil_raise_failure("%s has no constructor for the value %lld",
                               table->name, (long long)constant);

  return _variant;
}

int64_t
ocaml_avutil_constant_of_variant(const ocaml_ffmpeg_variant_table *table,
                                 value _variant) {
  for (size_t i = 0; i < table->length; i++) {
    if (table->entries[i].variant == _variant)
      return table->entries[i].constant;
  }

  ocaml_avutil_raise_failure("invalid value of type %s", table->name);
}

/* The first entry holding the largest flag of [mask] below [bound]. */
static const ocaml_ffmpeg_variant_entry *
largest_flag_below(const ocaml_ffmpeg_variant_table *table, uint64_t mask,
                   uint64_t bound) {
  const ocaml_ffmpeg_variant_entry *largest = NULL;

  for (size_t i = 0; i < table->length; i++) {
    const ocaml_ffmpeg_variant_entry *entry = &table->entries[i];
    uint64_t flag = (uint64_t)entry->constant;

    if (flag == 0 || (mask & flag) != flag || flag >= bound)
      continue;
    if (!largest || flag > (uint64_t)largest->constant)
      largest = entry;
  }

  return largest;
}

value ocaml_avutil_flags_of_mask(const ocaml_ffmpeg_variant_table *table,
                                 int64_t mask) {
  CAMLparam0();
  CAMLlocal2(_flags, _cell);
  const ocaml_ffmpeg_variant_entry *entry =
      largest_flag_below(table, (uint64_t)mask, UINT64_MAX);

  _flags = Val_emptylist;

  while (entry) {
    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, entry->variant);
    Store_field(_cell, 1, _flags);
    _flags = _cell;
    entry =
        largest_flag_below(table, (uint64_t)mask, (uint64_t)entry->constant);
  }

  CAMLreturn(_flags);
}

int64_t ocaml_avutil_mask_of_flags(const ocaml_ffmpeg_variant_table *table,
                                   value _flags) {
  int64_t mask = 0;

  for (; _flags != Val_emptylist; _flags = Field(_flags, 1))
    mask |= ocaml_avutil_constant_of_variant(table, Field(_flags, 0));

  return mask;
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_pixel_format_table(void) {
  return pixel_format_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_sample_format_table(void) {
  return sample_format_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_color_space_table(void) {
  return color_space_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_color_range_table(void) {
  return color_range_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_color_primaries_table(void) {
  return color_primaries_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_color_trc_table(void) {
  return color_trc_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_chroma_location_table(void) {
  return chroma_location_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_hw_device_type_table(void) {
  return hw_device_type_table();
}

const ocaml_ffmpeg_variant_table *ocaml_avutil_media_type_table(void) {
  return media_types_table();
}

enum caml_ba_kind
ocaml_avutil_bigarray_kind_of_sample_format(enum AVSampleFormat sample_format) {
  switch (sample_format) {
  case AV_SAMPLE_FMT_U8:
  case AV_SAMPLE_FMT_U8P:
    return CAML_BA_UINT8;
  case AV_SAMPLE_FMT_S16:
  case AV_SAMPLE_FMT_S16P:
    return CAML_BA_SINT16;
  case AV_SAMPLE_FMT_S32:
  case AV_SAMPLE_FMT_S32P:
    return CAML_BA_INT32;
  case AV_SAMPLE_FMT_S64:
  case AV_SAMPLE_FMT_S64P:
    return CAML_BA_INT64;
  case AV_SAMPLE_FMT_FLT:
  case AV_SAMPLE_FMT_FLTP:
    return CAML_BA_FLOAT32;
  case AV_SAMPLE_FMT_DBL:
  case AV_SAMPLE_FMT_DBLP:
    return CAML_BA_FLOAT64;
  default:
    ocaml_avutil_raise_failure("sample format %d has no bigarray kind",
                               (int)sample_format);
  }
}

AVRational ocaml_avutil_rational_of_value(value _rational) {
  AVRational rational;

  rational.num = ocaml_avutil_int_of_value(Field(_rational, 0), "numerator");
  rational.den = ocaml_avutil_int_of_value(Field(_rational, 1), "denominator");

  return rational;
}

value ocaml_avutil_value_of_rational(AVRational rational) {
  value _rational = caml_alloc_tuple(2);

  Field(_rational, 0) = Val_int(rational.num);
  Field(_rational, 1) = Val_int(rational.den);

  return _rational;
}

int64_t ocaml_avutil_time_format_units(value _time_format) {
  if (_time_format == PVV_Second)
    return 1;
  if (_time_format == PVV_Millisecond)
    return 1000;
  if (_time_format == PVV_Microsecond)
    return 1000000;
  return 1000000000;
}

static value some_int64_unless(int64_t number, int64_t absent) {
  if (number == absent)
    return Val_none;

  return caml_alloc_some(caml_copy_int64(number));
}

static int64_t int64_of_option(value _number, int64_t absent) {
  return Is_some(_number) ? Int64_val(Some_val(_number)) : absent;
}

static value some_string(const char *text) {
  if (!text)
    return Val_none;

  return caml_alloc_some(caml_copy_string(text));
}

static value some_rational_unless_unknown(AVRational rational) {
  if (rational.num == 0)
    return Val_none;

  return caml_alloc_some(ocaml_avutil_value_of_rational(rational));
}

AVDictionary *ocaml_avutil_dictionary_of_pairs(value _pairs) {
  AVDictionary *dictionary = NULL;

  for (; _pairs != Val_emptylist; _pairs = Field(_pairs, 1)) {
    value _pair = Field(_pairs, 0);
    int error = av_dict_set(&dictionary, String_val(Field(_pair, 0)),
                            String_val(Field(_pair, 1)), 0);

    if (error < 0) {
      av_dict_free(&dictionary);
      ocaml_avutil_raise_error(error);
    }
  }

  return dictionary;
}

value ocaml_avutil_pairs_of_dictionary(const AVDictionary *dictionary) {
  CAMLparam0();
  CAMLlocal4(_pairs, _last, _cell, _pair);
  const AVDictionaryEntry *entry = NULL;

  _pairs = Val_emptylist;

  while ((entry = av_dict_iterate(dictionary, entry))) {
    _pair = caml_alloc_tuple(2);
    Store_field(_pair, 0, caml_copy_string(entry->key));
    Store_field(_pair, 1, caml_copy_string(entry->value));

    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, _pair);
    Store_field(_cell, 1, Val_emptylist);

    if (_pairs == Val_emptylist)
      _pairs = _cell;
    else
      Store_field(_last, 1, _cell);
    _last = _cell;
  }

  CAMLreturn(_pairs);
}

/* The text of an Avutil.value; [number] is storage for a rendered number.
   This is the one rendering of option values. */
static const char *render_option_value(value _value, char *number,
                                       size_t number_size) {
  value _tag = Field(_value, 0);
  value _payload = Field(_value, 1);

  if (_tag == PVV_String)
    return String_val(_payload);

  if (_tag == PVV_Int)
    snprintf(number, number_size, "%lld", (long long)Long_val(_payload));
  else if (_tag == PVV_Int64)
    snprintf(number, number_size, "%lld", (long long)Int64_val(_payload));
  else
    snprintf(number, number_size, "%.17g", Double_val(_payload));

  return number;
}

CAMLprim value ocaml_avutil_render_option_value(value _value) {
  CAMLparam1(_value);
  char number[64];

  CAMLreturn(
      caml_copy_string(render_option_value(_value, number, sizeof(number))));
}

AVDictionary *ocaml_avutil_dictionary_of_options(value _bindings) {
  AVDictionary *dictionary = NULL;
  char number[64];

  for (mlsize_t i = 0; i < Wosize_val(_bindings); i++) {
    value _binding = Field(_bindings, i);
    int error = av_dict_set(
        &dictionary, String_val(Field(_binding, 0)),
        render_option_value(Field(_binding, 1), number, sizeof(number)), 0);

    if (error < 0) {
      av_dict_free(&dictionary);
      ocaml_avutil_raise_error(error);
    }
  }

  return dictionary;
}

value ocaml_avutil_unused_options(AVDictionary **dictionary) {
  CAMLparam0();
  CAMLlocal1(_keys);
  const AVDictionaryEntry *entry = NULL;
  mlsize_t index = 0;

  _keys = caml_alloc_tuple(av_dict_count(*dictionary));

  while ((entry = av_dict_iterate(*dictionary, entry)))
    Store_field(_keys, index++, caml_copy_string(entry->key));

  av_dict_free(dictionary);

  CAMLreturn(_keys);
}

static pthread_key_t registered_thread_key;

static void unregister_thread(void *registered) {
  (void)registered;
  caml_c_thread_unregister();
}

/* The key is created when the stubs are loaded, before the OCaml runtime
   creates the key of its thread descriptors: C libraries clear the values
   of an exiting thread in key order, and unregistering reads the runtime's. */
__attribute__((constructor)) static void create_registered_thread_key(void) {
  pthread_key_create(&registered_thread_key, unregister_thread);
}

void ocaml_avutil_register_thread(void) {
  if (caml_c_thread_register())
    pthread_setspecific(registered_thread_key, (void *)1);
}

CAMLprim value ocaml_avutil_version(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_version);
  unsigned version = avutil_version();

  _version = caml_alloc_tuple(3);
  Store_field(_version, 0, Val_int(AV_VERSION_MAJOR(version)));
  Store_field(_version, 1, Val_int(AV_VERSION_MINOR(version)));
  Store_field(_version, 2, Val_int(AV_VERSION_MICRO(version)));

  CAMLreturn(_version);
}

CAMLprim value ocaml_avutil_qp2lambda(value _unit) {
  (void)_unit;
  return Val_int(FF_QP2LAMBDA);
}

CAMLprim value ocaml_avutil_time_base(value _unit) {
  (void)_unit;
  return ocaml_avutil_value_of_rational(AV_TIME_BASE_Q);
}

CAMLprim value ocaml_avutil_rational_of_float(value _number) {
  return ocaml_avutil_value_of_rational(av_d2q(Double_val(_number), INT_MAX));
}

/* The log offset puts the evaluator's messages above every log level. */
CAMLprim value ocaml_avutil_expr_parse_and_eval(value _expression) {
  CAMLparam1(_expression);
  double result;
  int error =
      av_expr_parse_and_eval(&result, String_val(_expression), NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, AV_LOG_MAX_OFFSET, NULL);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(caml_copy_double(result));
}

/* Log capture, spec/avutil.md §7.1.

   log_callback queues one message per log call. ocaml_avutil_log_wait, run
   by the delivery thread of Avutil.Log, hands the queue to OCaml.
   log_mutex protects the queue and is held for a few instructions only,
   never across an FFmpeg call or while waiting for the runtime lock: a
   thread may log whatever it holds. */

typedef struct log_message {
  struct log_message *next;
  char text[];
} log_message;

static pthread_mutex_t log_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t log_queued_condition = PTHREAD_COND_INITIALIZER;
static log_message *log_queue_head = NULL;
static log_message **log_queue_tail = &log_queue_head;
static int log_capturing = 0;
/* Number of messages queued since the program started: the sequence number
   of the next one. */
static intnat log_queued_count = 0;

static _Thread_local int log_print_prefix = 1;

static void log_callback(void *logging_context, int level, const char *format,
                         va_list arguments) {
  char line[LOG_LINE_MAX];
  log_message *message;
  size_t length;

  if (level >= 0)
    level &= 0xff;
  if (level > av_log_get_level())
    return;

  av_log_format_line2(logging_context, level, format, arguments, line,
                      sizeof(line), &log_print_prefix);
  length = strlen(line);

  message = av_malloc(sizeof(*message) + length + 1);
  if (!message)
    return;
  memcpy(message->text, line, length + 1);
  message->next = NULL;

  pthread_mutex_lock(&log_mutex);
  if (log_capturing) {
    *log_queue_tail = message;
    log_queue_tail = &message->next;
    log_queued_count++;
    message = NULL;
    pthread_cond_signal(&log_queued_condition);
  }
  pthread_mutex_unlock(&log_mutex);

  av_free(message);
}

static intnat set_log_capture(int capturing) {
  intnat next_sequence_number;

  pthread_mutex_lock(&log_mutex);
  log_capturing = capturing;
  next_sequence_number = log_queued_count;
  pthread_mutex_unlock(&log_mutex);

  return next_sequence_number;
}

/* Both return the sequence number of the next message to be queued. */
CAMLprim value ocaml_avutil_log_start_capture(value _unit) {
  (void)_unit;
  av_log_set_callback(log_callback);
  return Val_long(set_log_capture(1));
}

CAMLprim value ocaml_avutil_log_stop_capture(value _unit) {
  (void)_unit;
  av_log_set_callback(av_log_default_callback);
  return Val_long(set_log_capture(0));
}

/* Blocks until messages are queued and returns them all, in order. */
CAMLprim value ocaml_avutil_log_wait(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_messages);
  log_message *messages;
  mlsize_t count = 0;

  caml_release_runtime_system();
  pthread_mutex_lock(&log_mutex);
  while (!log_queue_head)
    pthread_cond_wait(&log_queued_condition, &log_mutex);
  messages = log_queue_head;
  log_queue_head = NULL;
  log_queue_tail = &log_queue_head;
  pthread_mutex_unlock(&log_mutex);
  caml_acquire_runtime_system();

  for (log_message *message = messages; message; message = message->next)
    count++;

  _messages = caml_alloc_tuple(count);

  for (mlsize_t i = 0; i < count; i++) {
    log_message *message = messages;

    messages = message->next;
    Store_field(_messages, i, caml_copy_string(message->text));
    av_free(message);
  }

  CAMLreturn(_messages);
}

CAMLprim value ocaml_avutil_set_log_level(value _level) {
  av_log_set_level(
      (int)ocaml_avutil_constant_of_variant(&log_level_table, _level));
  return Val_unit;
}

CAMLprim value ocaml_avutil_log(value _level, value _message) {
  CAMLparam2(_level, _message);

  av_log(NULL, (int)ocaml_avutil_constant_of_variant(&log_level_table, _level),
         "%s\n", String_val(_message));

  CAMLreturn(Val_unit);
}

/* A frame value: the native frame first, where Frame_val reads it, then
   references to the buffers a make-writable step took away from it, which
   the bigarrays of earlier visits still point into. */
typedef struct retired_buffers {
  AVFrame *holder;
  struct retired_buffers *next;
} retired_buffers;

typedef struct {
  AVFrame *frame;
  retired_buffers *retired;
} frame_block;

#define FrameBlock_val(v) ((frame_block *)Data_custom_val(v))

static void finalize_frame(value _frame) {
  frame_block *block = FrameBlock_val(_frame);

  while (block->retired) {
    retired_buffers *retired = block->retired;

    block->retired = retired->next;
    av_frame_free(&retired->holder);
    av_free(retired);
  }
  av_frame_free(&block->frame);
}

/* Gives the frame buffers of its own when it shares them, keeping the ones
   it had referenced. Returns FFmpeg's code. */
static int make_frame_writable(frame_block *block) {
  retired_buffers *retired;
  int error;

  if (av_frame_is_writable(block->frame))
    return 0;

  retired = av_malloc(sizeof(*retired));
  if (!retired)
    return AVERROR(ENOMEM);
  retired->holder = av_frame_clone(block->frame);
  if (!retired->holder) {
    av_free(retired);
    return AVERROR(ENOMEM);
  }

  error = av_frame_make_writable(block->frame);
  if (error < 0) {
    av_frame_free(&retired->holder);
    av_free(retired);
    return error;
  }
  retired->next = block->retired;
  block->retired = retired;

  return 0;
}

static struct custom_operations frame_operations = {
    "ocaml_avutil_frame",       finalize_frame,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static size_t frame_buffers_size(const AVFrame *frame) {
  size_t size = 0;

  for (int i = 0; i < AV_NUM_DATA_POINTERS; i++) {
    if (frame->buf[i])
      size += frame->buf[i]->size;
  }
  for (int i = 0; i < frame->nb_extended_buf; i++)
    size += frame->extended_buf[i]->size;

  return size;
}

value ocaml_avutil_wrap_frame(AVFrame *frame) {
  value _frame;

  if (!frame)
    ocaml_avutil_raise_failure("null frame");

  _frame = caml_alloc_custom_mem(&frame_operations, sizeof(frame_block),
                                 frame_buffers_size(frame));
  FrameBlock_val(_frame)->frame = frame;
  FrameBlock_val(_frame)->retired = NULL;

  return _frame;
}

CAMLprim value ocaml_avutil_frame_pts(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(some_int64_unless(Frame_val(_frame)->pts, AV_NOPTS_VALUE));
}

CAMLprim value ocaml_avutil_frame_set_pts(value _frame, value _pts) {
  AVFrame *frame = Frame_val(_frame);

  frame->pts = int64_of_option(_pts, AV_NOPTS_VALUE);
  frame->best_effort_timestamp = frame->pts;

  return Val_unit;
}

CAMLprim value ocaml_avutil_frame_duration(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(some_int64_unless(Frame_val(_frame)->duration, 0));
}

CAMLprim value ocaml_avutil_frame_set_duration(value _frame, value _duration) {
  Frame_val(_frame)->duration = int64_of_option(_duration, 0);
  return Val_unit;
}

CAMLprim value ocaml_avutil_frame_pkt_dts(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(some_int64_unless(Frame_val(_frame)->pkt_dts, AV_NOPTS_VALUE));
}

CAMLprim value ocaml_avutil_frame_set_pkt_dts(value _frame, value _dts) {
  Frame_val(_frame)->pkt_dts = int64_of_option(_dts, AV_NOPTS_VALUE);
  return Val_unit;
}

CAMLprim value ocaml_avutil_frame_best_effort_timestamp(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(some_int64_unless(Frame_val(_frame)->best_effort_timestamp,
                               AV_NOPTS_VALUE));
}

CAMLprim value ocaml_avutil_frame_metadata(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(ocaml_avutil_pairs_of_dictionary(Frame_val(_frame)->metadata));
}

CAMLprim value ocaml_avutil_frame_set_metadata(value _frame, value _metadata) {
  CAMLparam2(_frame, _metadata);
  AVDictionary *metadata = ocaml_avutil_dictionary_of_pairs(_metadata);
  AVFrame *frame = Frame_val(_frame);

  av_dict_free(&frame->metadata);
  frame->metadata = metadata;

  CAMLreturn(Val_unit);
}

#define FrameSideDataType_val(v)                                               \
  ((enum AVFrameSideDataType)ocaml_avutil_constant_of_variant(                 \
      frame_side_data_type_table(), (v)))

CAMLprim value ocaml_avutil_frame_side_data_name(value _kind) {
  CAMLparam1(_kind);
  const char *name = av_frame_side_data_name(FrameSideDataType_val(_kind));

  CAMLreturn(caml_copy_string(name ? name : ""));
}

CAMLprim value ocaml_avutil_frame_side_data_props(value _kind) {
  CAMLparam1(_kind);
  const AVSideDataDescriptor *descriptor =
      av_frame_side_data_desc(FrameSideDataType_val(_kind));

  CAMLreturn(ocaml_avutil_flags_of_mask(side_data_prop_table(),
                                        descriptor ? descriptor->props : 0));
}

CAMLprim value ocaml_avutil_frame_side_data(value _frame) {
  CAMLparam1(_frame);
  CAMLlocal4(_entries, _cell, _entry, _data);
  const AVFrame *frame = Frame_val(_frame);

  _entries = Val_emptylist;

  for (int i = frame->nb_side_data - 1; i >= 0; i--) {
    const AVFrameSideData *side_data = frame->side_data[i];
    value _kind;

    if (!ocaml_avutil_find_variant(frame_side_data_type_table(),
                                   side_data->type, &_kind))
      continue;

    _data = caml_alloc_initialized_string(side_data->size,
                                          (const char *)side_data->data);
    _entry = caml_alloc_tuple(2);
    Store_field(_entry, 0, _kind);
    Store_field(_entry, 1, _data);
    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, _entry);
    Store_field(_cell, 1, _entries);
    _entries = _cell;
  }

  CAMLreturn(_entries);
}

CAMLprim value ocaml_avutil_frame_find_side_data(value _frame, value _kind) {
  CAMLparam2(_frame, _kind);
  CAMLlocal2(_entry, _data);
  const AVFrameSideData *side_data =
      av_frame_get_side_data(Frame_val(_frame), FrameSideDataType_val(_kind));

  if (!side_data)
    CAMLreturn(Val_none);

  _data = caml_alloc_initialized_string(side_data->size,
                                        (const char *)side_data->data);
  _entry = caml_alloc_tuple(2);
  Store_field(_entry, 0, _kind);
  Store_field(_entry, 1, _data);

  CAMLreturn(caml_alloc_some(_entry));
}

static int add_side_data_entry(AVFrameSideData ***side_data, int *count,
                               value _entry) {
  const ocaml_ffmpeg_variant_table *table = frame_side_data_type_table();
  size_t size = caml_string_length(Field(_entry, 1));
  AVFrameSideData *added;

  for (size_t i = 0; i < table->length; i++) {
    if (table->entries[i].variant != Field(_entry, 0))
      continue;

    added = av_frame_side_data_new(
        side_data, count, (enum AVFrameSideDataType)table->entries[i].constant,
        size, AV_FRAME_SIDE_DATA_FLAG_REPLACE);
    if (!added)
      return AVERROR(ENOMEM);
    memcpy(added->data, String_val(Field(_entry, 1)), size);
    return 0;
  }

  return AVERROR(EINVAL);
}

int ocaml_avutil_add_side_data(AVFrameSideData ***side_data, int *count,
                               value _entries) {
  for (value _cell = _entries; _cell != Val_emptylist;
       _cell = Field(_cell, 1)) {
    int error = add_side_data_entry(side_data, count, Field(_cell, 0));

    if (error < 0)
      return error;
  }

  return 0;
}

CAMLprim value ocaml_avutil_frame_add_side_data(value _frame, value _entry) {
  CAMLparam2(_frame, _entry);
  AVFrame *frame = Frame_val(_frame);
  int error =
      add_side_data_entry(&frame->side_data, &frame->nb_side_data, _entry);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avutil_frame_remove_side_data(value _frame, value _kind) {
  CAMLparam2(_frame, _kind);

  av_frame_remove_side_data(Frame_val(_frame), FrameSideDataType_val(_kind));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avutil_frame_dup(value _frame) {
  CAMLparam1(_frame);
  CAMLlocal1(_copy);
  AVFrame *copy = av_frame_clone(Frame_val(_frame));

  if (!copy)
    caml_raise_out_of_memory();
  /* The wrapper raises before it owns the frame. */
  _copy = ocaml_avutil_wrap_frame(copy);

  CAMLreturn(_copy);
}

#define DISPLAY_MATRIX_LENGTH 9

CAMLprim value ocaml_avutil_display_matrix(value _clockwise_angle, value _hflip,
                                           value _vflip) {
  CAMLparam0();
  CAMLlocal2(_matrix, _element);
  int32_t matrix[DISPLAY_MATRIX_LENGTH];

  av_display_rotation_set(matrix, Double_val(_clockwise_angle));
  av_display_matrix_flip(matrix, Bool_val(_hflip), Bool_val(_vflip));

  _matrix = caml_alloc_tuple(DISPLAY_MATRIX_LENGTH);
  for (int i = 0; i < DISPLAY_MATRIX_LENGTH; i++) {
    _element = caml_copy_int32(matrix[i]);
    Store_field(_matrix, i, _element);
  }

  CAMLreturn(_matrix);
}

CAMLprim value ocaml_avutil_display_rotation(value _matrix) {
  CAMLparam1(_matrix);
  int32_t matrix[DISPLAY_MATRIX_LENGTH];

  for (int i = 0; i < DISPLAY_MATRIX_LENGTH; i++)
    matrix[i] = Int32_val(Field(_matrix, i));

  CAMLreturn(caml_copy_double(av_display_rotation_get(matrix)));
}

static void finalize_channel_layout(value _layout) {
  AVChannelLayout *layout = ChannelLayout_val(_layout);

  if (layout) {
    av_channel_layout_uninit(layout);
    av_free(layout);
  }
}

static struct custom_operations channel_layout_operations = {
    "ocaml_avutil_channel_layout", finalize_channel_layout,
    custom_compare_default,        custom_hash_default,
    custom_serialize_default,      custom_deserialize_default,
    custom_compare_ext_default,    custom_fixed_length_default};

/* Stores in [_layout], a registered local of the caller, a handle owning a
   zeroed layout, and returns that layout for FFmpeg to fill. */
static AVChannelLayout *alloc_channel_layout(value *_layout) {
  AVChannelLayout *layout;

  *_layout = caml_alloc_custom(&channel_layout_operations,
                               sizeof(AVChannelLayout *), 0, 1);
  ChannelLayout_val(*_layout) = NULL;

  layout = av_mallocz(sizeof(*layout));
  if (!layout)
    caml_raise_out_of_memory();
  ChannelLayout_val(*_layout) = layout;

  return layout;
}

value ocaml_avutil_copy_channel_layout(const AVChannelLayout *source) {
  CAMLparam0();
  CAMLlocal1(_layout);
  int error = av_channel_layout_copy(alloc_channel_layout(&_layout), source);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_layout);
}

CAMLprim value ocaml_avutil_standard_channel_layouts(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_layouts);
  const AVChannelLayout *layout;
  void *iterator = NULL;
  mlsize_t count = 0;

  while (av_channel_layout_standard(&iterator))
    count++;

  _layouts = caml_alloc_tuple(count);
  iterator = NULL;

  for (mlsize_t i = 0; i < count; i++) {
    layout = av_channel_layout_standard(&iterator);
    Store_field(_layouts, i, ocaml_avutil_copy_channel_layout(layout));
  }

  CAMLreturn(_layouts);
}

CAMLprim value ocaml_avutil_find_channel_layout(value _name) {
  CAMLparam1(_name);
  CAMLlocal1(_layout);
  AVChannelLayout *layout = alloc_channel_layout(&_layout);

  if (av_channel_layout_from_string(layout, String_val(_name)) < 0)
    caml_raise_not_found();

  CAMLreturn(_layout);
}

CAMLprim value ocaml_avutil_default_channel_layout(value _channels) {
  CAMLparam1(_channels);
  CAMLlocal1(_layout);
  intnat channels = Long_val(_channels);
  AVChannelLayout *layout;

  if (channels < 1 || channels > INT_MAX)
    caml_raise_not_found();

  layout = alloc_channel_layout(&_layout);
  av_channel_layout_default(layout, (int)channels);

  CAMLreturn(_layout);
}

CAMLprim value ocaml_avutil_compare_channel_layouts(value _first,
                                                    value _second) {
  int difference = av_channel_layout_compare(ChannelLayout_val(_first),
                                             ChannelLayout_val(_second));

  if (difference < 0)
    ocaml_avutil_raise_error(difference);

  return Val_bool(difference == 0);
}

CAMLprim value ocaml_avutil_channel_layout_description(value _layout) {
  CAMLparam1(_layout);
  CAMLlocal1(_description);
  const AVChannelLayout *layout = ChannelLayout_val(_layout);
  char short_description[128];
  char *description;
  int size = av_channel_layout_describe(layout, short_description,
                                        sizeof(short_description));

  if (size < 0)
    ocaml_avutil_raise_error(size);
  if ((size_t)size <= sizeof(short_description))
    CAMLreturn(caml_copy_string(short_description));

  description = av_malloc(size);
  if (!description)
    caml_raise_out_of_memory();

  size = av_channel_layout_describe(layout, description, size);
  if (size < 0) {
    av_free(description);
    ocaml_avutil_raise_error(size);
  }

  _description = caml_copy_string(description);
  av_free(description);

  CAMLreturn(_description);
}

CAMLprim value ocaml_avutil_channel_layout_nb_channels(value _layout) {
  return Val_int(ChannelLayout_val(_layout)->nb_channels);
}

CAMLprim value ocaml_avutil_channel_layout_mask(value _layout) {
  CAMLparam1(_layout);
  const AVChannelLayout *layout = ChannelLayout_val(_layout);

  if (layout->order != AV_CHANNEL_ORDER_NATIVE)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_int64((int64_t)layout->u.mask)));
}

CAMLprim value ocaml_avutil_sample_format_name(value _sample_format) {
  CAMLparam1(_sample_format);
  CAMLreturn(
      some_string(av_get_sample_fmt_name(SampleFormat_val(_sample_format))));
}

CAMLprim value ocaml_avutil_find_sample_format(value _name) {
  value _sample_format;
  enum AVSampleFormat sample_format = av_get_sample_fmt(String_val(_name));

  if (sample_format == AV_SAMPLE_FMT_NONE ||
      !ocaml_avutil_find_variant(sample_format_table(), sample_format,
                                 &_sample_format))
    caml_raise_not_found();

  return _sample_format;
}

CAMLprim value ocaml_avutil_sample_format_id(value _sample_format) {
  return Val_int(SampleFormat_val(_sample_format));
}

CAMLprim value ocaml_avutil_find_sample_format_id(value _id) {
  value _sample_format;

  if (!ocaml_avutil_find_variant(sample_format_table(), Long_val(_id),
                                 &_sample_format))
    caml_raise_not_found();

  return _sample_format;
}

/* FFmpeg's from_name functions may match a name by prefix and return
   another value: a name that is exactly the name of a value is looked up
   here first. */
#define COLOR_PROPERTY(property, type, name_of, from_name)                     \
  CAMLprim value ocaml_avutil_##property##_name(value _property) {             \
    CAMLparam1(_property);                                                     \
    const char *name = name_of((type)ocaml_avutil_constant_of_variant(         \
        property##_table(), _property));                                       \
                                                                               \
    CAMLreturn(caml_copy_string(name ? name : ""));                            \
  }                                                                            \
                                                                               \
  CAMLprim value ocaml_avutil_##property##_from_name(value _name) {            \
    const ocaml_ffmpeg_variant_table *table = property##_table();              \
    value _property;                                                           \
    int constant;                                                              \
                                                                               \
    for (size_t i = 0; i < table->length; i++) {                               \
      const char *name = name_of((type)table->entries[i].constant);            \
                                                                               \
      if (name && !strcmp(name, String_val(_name)))                            \
        return caml_alloc_some(table->entries[i].variant);                     \
    }                                                                          \
                                                                               \
    constant = from_name(String_val(_name));                                   \
    if (constant < 0 ||                                                        \
        !ocaml_avutil_find_variant(table, constant, &_property))               \
      return Val_none;                                                         \
                                                                               \
    return caml_alloc_some(_property);                                         \
  }

COLOR_PROPERTY(color_space, enum AVColorSpace, av_color_space_name,
               av_color_space_from_name)
COLOR_PROPERTY(color_range, enum AVColorRange, av_color_range_name,
               av_color_range_from_name)
COLOR_PROPERTY(color_primaries, enum AVColorPrimaries, av_color_primaries_name,
               av_color_primaries_from_name)
COLOR_PROPERTY(color_trc, enum AVColorTransferCharacteristic,
               av_color_transfer_name, av_color_transfer_from_name)
COLOR_PROPERTY(chroma_location, enum AVChromaLocation, av_chroma_location_name,
               av_chroma_location_from_name)

CAMLprim value ocaml_avutil_pixel_format_descriptor(value _pixel_format) {
  CAMLparam1(_pixel_format);
  CAMLlocal4(_descriptor, _components, _component, _cell);
  const AVPixFmtDescriptor *descriptor =
      av_pix_fmt_desc_get(PixelFormat_val(_pixel_format));

  if (!descriptor)
    caml_raise_not_found();

  _components = Val_emptylist;

  for (int i = descriptor->nb_components - 1; i >= 0; i--) {
    const AVComponentDescriptor *component = &descriptor->comp[i];

    _component = caml_alloc_tuple(5);
    Store_field(_component, 0, Val_int(component->plane));
    Store_field(_component, 1, Val_int(component->step));
    Store_field(_component, 2, Val_int(component->offset));
    Store_field(_component, 3, Val_int(component->shift));
    Store_field(_component, 4, Val_int(component->depth));

    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, _component);
    Store_field(_cell, 1, _components);
    _components = _cell;
  }

  _descriptor = caml_alloc_tuple(7);
  Store_field(_descriptor, 0, caml_copy_string(descriptor->name));
  Store_field(_descriptor, 1, Val_int(descriptor->nb_components));
  Store_field(_descriptor, 2, Val_int(descriptor->log2_chroma_w));
  Store_field(_descriptor, 3, Val_int(descriptor->log2_chroma_h));
  Store_field(_descriptor, 4,
              ocaml_avutil_flags_of_mask(pixel_format_flag_table(),
                                         (int64_t)descriptor->flags));
  Store_field(_descriptor, 5, _components);
  Store_field(_descriptor, 6, some_string(descriptor->alias));

  CAMLreturn(_descriptor);
}

CAMLprim value ocaml_avutil_pixel_format_bits(value _descriptor) {
  const AVPixFmtDescriptor *descriptor =
      av_pix_fmt_desc_get(av_get_pix_fmt(String_val(Field(_descriptor, 0))));

  if (!descriptor)
    ocaml_avutil_raise_failure("unknown pixel format descriptor");

  return Val_int(av_get_bits_per_pixel(descriptor));
}

CAMLprim value ocaml_avutil_pixel_format_planes(value _pixel_format) {
  int planes = av_pix_fmt_count_planes(PixelFormat_val(_pixel_format));

  if (planes < 0)
    ocaml_avutil_raise_error(planes);

  return Val_int(planes);
}

CAMLprim value ocaml_avutil_pixel_format_name(value _pixel_format) {
  CAMLparam1(_pixel_format);
  CAMLreturn(some_string(av_get_pix_fmt_name(PixelFormat_val(_pixel_format))));
}

CAMLprim value ocaml_avutil_find_pixel_format(value _name) {
  enum AVPixelFormat pixel_format = av_get_pix_fmt(String_val(_name));

  if (pixel_format == AV_PIX_FMT_NONE)
    ocaml_avutil_raise_failure("unknown pixel format name");

  return Val_PixelFormat(pixel_format);
}

CAMLprim value ocaml_avutil_pixel_format_id(value _pixel_format) {
  return Val_int(PixelFormat_val(_pixel_format));
}

CAMLprim value ocaml_avutil_find_pixel_format_id(value _id) {
  value _pixel_format;

  if (!ocaml_avutil_find_variant(pixel_format_table(), Long_val(_id),
                                 &_pixel_format))
    caml_raise_not_found();

  return _pixel_format;
}

CAMLprim value ocaml_avutil_audio_create_frame(value _sample_format,
                                               value _layout,
                                               value _sample_rate,
                                               value _nb_samples) {
  CAMLparam4(_sample_format, _layout, _sample_rate, _nb_samples);
  enum AVSampleFormat sample_format = SampleFormat_val(_sample_format);
  int sample_rate = ocaml_avutil_int_of_value(_sample_rate, "sample rate");
  int nb_samples = ocaml_avutil_int_of_value(_nb_samples, "sample count");
  AVFrame *frame;
  int error;

  if (nb_samples < 1)
    ocaml_avutil_raise_failure("sample count below 1");

  frame = av_frame_alloc();
  if (!frame)
    caml_raise_out_of_memory();

  frame->format = sample_format;
  frame->sample_rate = sample_rate;
  frame->nb_samples = nb_samples;

  error = av_channel_layout_copy(&frame->ch_layout, ChannelLayout_val(_layout));
  if (error >= 0)
    error = av_frame_get_buffer(frame, 0);
  if (error < 0) {
    av_frame_free(&frame);
    ocaml_avutil_raise_error(error);
  }

  CAMLreturn(ocaml_avutil_wrap_frame(frame));
}

CAMLprim value ocaml_avutil_audio_frame_sample_format(value _frame) {
  return Val_SampleFormat(Frame_val(_frame)->format);
}

CAMLprim value ocaml_avutil_audio_frame_sample_rate(value _frame) {
  return Val_int(Frame_val(_frame)->sample_rate);
}

CAMLprim value ocaml_avutil_audio_frame_channels(value _frame) {
  return Val_int(Frame_val(_frame)->ch_layout.nb_channels);
}

CAMLprim value ocaml_avutil_audio_frame_channel_layout(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(ocaml_avutil_copy_channel_layout(&Frame_val(_frame)->ch_layout));
}

CAMLprim value ocaml_avutil_audio_frame_nb_samples(value _frame) {
  return Val_int(Frame_val(_frame)->nb_samples);
}

CAMLprim value ocaml_avutil_video_create_frame(value _width, value _height,
                                               value _pixel_format) {
  CAMLparam3(_width, _height, _pixel_format);
  enum AVPixelFormat pixel_format = PixelFormat_val(_pixel_format);
  int width = ocaml_avutil_int_of_value(_width, "width");
  int height = ocaml_avutil_int_of_value(_height, "height");
  AVFrame *frame;
  int error;

  if (width < 1 || height < 1)
    ocaml_avutil_raise_failure("width or height below 1");

  frame = av_frame_alloc();
  if (!frame)
    caml_raise_out_of_memory();

  frame->format = pixel_format;
  frame->width = width;
  frame->height = height;

  error = av_frame_get_buffer(frame, VIDEO_FRAME_ALIGN);
  if (error < 0) {
    av_frame_free(&frame);
    ocaml_avutil_raise_error(error);
  }

  CAMLreturn(ocaml_avutil_wrap_frame(frame));
}

/* The plane count of a frame that holds software pixel data. Raises a
   failure for any other frame. */
static int software_plane_count(const AVFrame *frame) {
  const AVPixFmtDescriptor *descriptor = av_pix_fmt_desc_get(frame->format);
  int plane_count = av_pix_fmt_count_planes(frame->format);

  if (!descriptor || plane_count < 0 || !frame->data[0] ||
      (descriptor->flags & AV_PIX_FMT_FLAG_HWACCEL))
    ocaml_avutil_raise_failure("the frame holds no software pixel data");

  return plane_count;
}

CAMLprim value ocaml_avutil_video_frame_linesize(value _frame, value _plane) {
  const AVFrame *frame = Frame_val(_frame);
  intnat plane = Long_val(_plane);

  if (plane < 0 || plane >= software_plane_count(frame))
    ocaml_avutil_raise_failure("the frame has no plane %ld", (long)plane);

  return Val_int(frame->linesize[plane]);
}

static void finalize_buffer(value _buffer) {
  av_buffer_unref(&HwContext_val(_buffer));
}

/* Handles on one AVBufferRef: hardware contexts, and the references that
   keep the buffers of visited video planes alive. */
static struct custom_operations buffer_operations = {
    "ocaml_avutil_buffer",      finalize_buffer,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value alloc_buffer_handle(void) {
  value _buffer =
      caml_alloc_custom(&buffer_operations, sizeof(AVBufferRef *), 0, 1);

  HwContext_val(_buffer) = NULL;

  return _buffer;
}

/* The planes of spec/avutil.md §8.2: the frame value keeps their buffers
   alive, and Avutil.Video.frame_visit ties each bigarray to it. */
CAMLprim value ocaml_avutil_video_frame_planes(value _frame,
                                               value _make_writable) {
  CAMLparam1(_frame);
  CAMLlocal3(_planes, _plane, _data);
  AVFrame *frame = Frame_val(_frame);
  int plane_count = software_plane_count(frame);
  ptrdiff_t linesizes[4];
  size_t sizes[4];
  int error;

  if (Bool_val(_make_writable)) {
    error = make_frame_writable(FrameBlock_val(_frame));
    if (error < 0)
      ocaml_avutil_raise_error(error);
  }

  for (int i = 0; i < 4; i++) {
    if (i < plane_count && frame->linesize[i] < 0)
      ocaml_avutil_raise_failure("plane %d has a negative line size", i);
    linesizes[i] = frame->linesize[i];
  }

  error =
      av_image_fill_plane_sizes(sizes, frame->format, frame->height, linesizes);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  _planes = caml_alloc_tuple(plane_count);

  for (int i = 0; i < plane_count; i++) {
    AVBufferRef *owner = av_frame_get_plane_buffer(frame, i);

    if (!owner || frame->data[i] < owner->data ||
        frame->data[i] + sizes[i] > owner->data + owner->size)
      ocaml_avutil_raise_failure("plane %d lies outside its buffer", i);

    _data =
        caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
                           1, frame->data[i], (intnat)sizes[i]);

    _plane = caml_alloc_tuple(2);
    Store_field(_plane, 0, _data);
    Store_field(_plane, 1, Val_int(frame->linesize[i]));
    Store_field(_planes, i, _plane);
  }

  CAMLreturn(_planes);
}

CAMLprim value ocaml_avutil_video_frame_width(value _frame) {
  return Val_int(Frame_val(_frame)->width);
}

CAMLprim value ocaml_avutil_video_frame_height(value _frame) {
  return Val_int(Frame_val(_frame)->height);
}

CAMLprim value ocaml_avutil_video_frame_pixel_format(value _frame) {
  return Val_PixelFormat(Frame_val(_frame)->format);
}

CAMLprim value ocaml_avutil_video_frame_pixel_aspect(value _frame) {
  CAMLparam1(_frame);
  CAMLreturn(
      some_rational_unless_unknown(Frame_val(_frame)->sample_aspect_ratio));
}

CAMLprim value ocaml_avutil_video_frame_color_space(value _frame) {
  return Val_ColorSpace(Frame_val(_frame)->colorspace);
}

CAMLprim value ocaml_avutil_video_frame_color_range(value _frame) {
  return Val_ColorRange(Frame_val(_frame)->color_range);
}

CAMLprim value ocaml_avutil_video_frame_color_primaries(value _frame) {
  return Val_ColorPrimaries(Frame_val(_frame)->color_primaries);
}

CAMLprim value ocaml_avutil_video_frame_color_trc(value _frame) {
  return Val_ColorTrc(Frame_val(_frame)->color_trc);
}

CAMLprim value ocaml_avutil_video_frame_chroma_location(value _frame) {
  return Val_ChromaLocation(Frame_val(_frame)->chroma_location);
}

static void finalize_subtitle(value _subtitle) {
  AVSubtitle *subtitle = Subtitle_val(_subtitle);

  if (subtitle) {
    avsubtitle_free(subtitle);
    av_free(subtitle);
  }
}

static struct custom_operations subtitle_operations = {
    "ocaml_avutil_subtitle",    finalize_subtitle,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value alloc_subtitle_handle(void) {
  value _subtitle =
      caml_alloc_custom(&subtitle_operations, sizeof(AVSubtitle *), 0, 1);

  Subtitle_val(_subtitle) = NULL;

  return _subtitle;
}

value ocaml_avutil_wrap_subtitle(AVSubtitle *subtitle) {
  value _subtitle;

  if (!subtitle)
    ocaml_avutil_raise_failure("null subtitle");

  _subtitle = alloc_subtitle_handle();
  Subtitle_val(_subtitle) = subtitle;

  return _subtitle;
}

/* The native rectangle records no plane size: spec/avutil.md §4.14. */
static size_t subtitle_plane_size(const AVSubtitleRect *rectangle, int plane) {
  if (plane == 1 && rectangle->type == SUBTITLE_BITMAP)
    return AVPALETTE_SIZE;
  if (rectangle->h > 0 && rectangle->linesize[plane] > 0)
    return (size_t)rectangle->h * (size_t)rectangle->linesize[plane];
  return 0;
}

/* A copy of an OCaml string as subtitle text; an empty one is absent. */
static char *subtitle_text(value _text) {
  char *text;

  if (String_val(_text)[0] == 0)
    return NULL;

  text = av_strdup(String_val(_text));
  if (!text)
    caml_raise_out_of_memory();

  return text;
}

static void fill_subtitle_picture(AVSubtitleRect *rectangle, value _picture) {
  value _planes = Field(_picture, 5);
  value _data = Field(_planes, 0);
  value _linesizes = Field(_planes, 1);

  if (Wosize_val(_data) != SUBTITLE_PLANES ||
      Wosize_val(_linesizes) != SUBTITLE_PLANES)
    ocaml_avutil_raise_failure("a subtitle picture has exactly %d planes",
                               SUBTITLE_PLANES);

  rectangle->x = ocaml_avutil_int_of_value(Field(_picture, 0), "x");
  rectangle->y = ocaml_avutil_int_of_value(Field(_picture, 1), "y");
  rectangle->w = ocaml_avutil_int_of_value(Field(_picture, 2), "w");
  rectangle->h = ocaml_avutil_int_of_value(Field(_picture, 3), "h");
  rectangle->nb_colors =
      ocaml_avutil_int_of_value(Field(_picture, 4), "nb_colors");

  for (int i = 0; i < SUBTITLE_PLANES; i++)
    rectangle->linesize[i] =
        ocaml_avutil_int_of_value(Field(_linesizes, i), "line size");

  for (int i = 0; i < SUBTITLE_PLANES; i++) {
    struct caml_ba_array *plane = Caml_ba_array_val(Field(_data, i));
    size_t length = plane->dim[0];

    if (length == 0 && i == 0)
      ocaml_avutil_raise_failure("the first plane of a picture is empty");
    if (length == 0)
      continue;
    if (length != subtitle_plane_size(rectangle, i))
      ocaml_avutil_raise_failure("plane %d has %zu bytes, %zu expected", i,
                                 length, subtitle_plane_size(rectangle, i));

    rectangle->data[i] = av_memdup(plane->data, length);
    if (!rectangle->data[i])
      caml_raise_out_of_memory();
  }
}

/* avsubtitle_free reads every rectangle below num_rects: the count grows as
   rectangles are added. */
static AVSubtitleRect *add_subtitle_rectangle(AVSubtitle *subtitle) {
  AVSubtitleRect *rectangle = av_mallocz(sizeof(*rectangle));

  if (!rectangle)
    caml_raise_out_of_memory();
  subtitle->rects[subtitle->num_rects++] = rectangle;

  return rectangle;
}

static uint32_t subtitle_display_time(value _time) {
  intnat time = Long_val(_time);

  if (time < 0 || (uint64_t)time > UINT32_MAX)
    ocaml_avutil_raise_failure("display time out of range");

  return (uint32_t)time;
}

CAMLprim value ocaml_avutil_subtitle_create_frame(value _content) {
  CAMLparam1(_content);
  CAMLlocal1(_subtitle);
  AVSubtitle *subtitle;
  intnat format = Long_val(Field(_content, 0));
  unsigned rectangle_count = 0;

  if (format < 0 || format > UINT16_MAX)
    ocaml_avutil_raise_failure("subtitle format out of range");

  _subtitle = alloc_subtitle_handle();
  subtitle = av_mallocz(sizeof(*subtitle));
  if (!subtitle)
    caml_raise_out_of_memory();
  Subtitle_val(_subtitle) = subtitle;

  subtitle->format = (uint16_t)format;
  subtitle->start_display_time = subtitle_display_time(Field(_content, 1));
  subtitle->end_display_time = subtitle_display_time(Field(_content, 2));
  subtitle->pts = int64_of_option(Field(_content, 4), AV_NOPTS_VALUE);

  for (value _rectangles = Field(_content, 3); _rectangles != Val_emptylist;
       _rectangles = Field(_rectangles, 1))
    rectangle_count++;

  if (rectangle_count > 0) {
    subtitle->rects = av_calloc(rectangle_count, sizeof(*subtitle->rects));
    if (!subtitle->rects)
      caml_raise_out_of_memory();
  }

  for (value _rectangles = Field(_content, 3); _rectangles != Val_emptylist;
       _rectangles = Field(_rectangles, 1)) {
    value _rectangle = Field(_rectangles, 0);
    AVSubtitleRect *rectangle = add_subtitle_rectangle(subtitle);

    rectangle->flags = (int)ocaml_avutil_mask_of_flags(subtitle_flag_table(),
                                                       Field(_rectangle, 1));
    rectangle->type = (enum AVSubtitleType)ocaml_avutil_constant_of_variant(
        subtitle_type_table(), Field(_rectangle, 2));
    rectangle->text = subtitle_text(Field(_rectangle, 3));
    rectangle->ass = subtitle_text(Field(_rectangle, 4));

    if (Is_some(Field(_rectangle, 0)))
      fill_subtitle_picture(rectangle, Some_val(Field(_rectangle, 0)));
  }

  CAMLreturn(_subtitle);
}

static value subtitle_picture(const AVSubtitleRect *rectangle) {
  CAMLparam0();
  CAMLlocal5(_picture, _planes, _data, _linesizes, _plane);

  _data = caml_alloc_tuple(SUBTITLE_PLANES);
  _linesizes = caml_alloc_tuple(SUBTITLE_PLANES);

  for (int i = 0; i < SUBTITLE_PLANES; i++) {
    size_t size = rectangle->data[i] ? subtitle_plane_size(rectangle, i) : 0;

    _plane = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL,
                                (intnat)size);
    if (size > 0)
      memcpy(Caml_ba_data_val(_plane), rectangle->data[i], size);

    Store_field(_data, i, _plane);
    Store_field(_linesizes, i, Val_int(rectangle->linesize[i]));
  }

  _planes = caml_alloc_tuple(2);
  Store_field(_planes, 0, _data);
  Store_field(_planes, 1, _linesizes);

  _picture = caml_alloc_tuple(6);
  Store_field(_picture, 0, Val_int(rectangle->x));
  Store_field(_picture, 1, Val_int(rectangle->y));
  Store_field(_picture, 2, Val_int(rectangle->w));
  Store_field(_picture, 3, Val_int(rectangle->h));
  Store_field(_picture, 4, Val_int(rectangle->nb_colors));
  Store_field(_picture, 5, _planes);

  CAMLreturn(_picture);
}

static value subtitle_rectangle(const AVSubtitleRect *rectangle) {
  CAMLparam0();
  CAMLlocal2(_rectangle, _picture);

  _picture = Val_none;
  if (rectangle->data[0])
    _picture = caml_alloc_some(subtitle_picture(rectangle));

  _rectangle = caml_alloc_tuple(5);
  Store_field(_rectangle, 0, _picture);
  Store_field(
      _rectangle, 1,
      ocaml_avutil_flags_of_mask(subtitle_flag_table(), rectangle->flags));
  Store_field(
      _rectangle, 2,
      ocaml_avutil_variant_of_constant(subtitle_type_table(), rectangle->type));
  Store_field(_rectangle, 3,
              caml_copy_string(rectangle->text ? rectangle->text : ""));
  Store_field(_rectangle, 4,
              caml_copy_string(rectangle->ass ? rectangle->ass : ""));

  CAMLreturn(_rectangle);
}

CAMLprim value ocaml_avutil_subtitle_content(value _subtitle) {
  CAMLparam1(_subtitle);
  CAMLlocal3(_content, _rectangles, _cell);
  const AVSubtitle *subtitle = Subtitle_val(_subtitle);

  _rectangles = Val_emptylist;

  for (unsigned i = subtitle->num_rects; i > 0; i--) {
    _cell = caml_alloc_tuple(2);
    Store_field(_cell, 0, subtitle_rectangle(subtitle->rects[i - 1]));
    Store_field(_cell, 1, _rectangles);
    _rectangles = _cell;
  }

  _content = caml_alloc_tuple(5);
  Store_field(_content, 0, Val_int(subtitle->format));
  Store_field(_content, 1, Val_long(subtitle->start_display_time));
  Store_field(_content, 2, Val_long(subtitle->end_display_time));
  Store_field(_content, 3, _rectangles);
  Store_field(_content, 4, some_int64_unless(subtitle->pts, AV_NOPTS_VALUE));

  CAMLreturn(_content);
}

CAMLprim value ocaml_avutil_subtitle_pts(value _subtitle) {
  CAMLparam1(_subtitle);
  CAMLreturn(some_int64_unless(Subtitle_val(_subtitle)->pts, AV_NOPTS_VALUE));
}

#define OptionClass_val(v) (*(const AVClass **)Data_abstract_val(v))

value ocaml_avutil_wrap_option_class(const AVClass *option_class) {
  value _class = caml_alloc(1, Abstract_tag);

  OptionClass_val(_class) = option_class;

  return _class;
}

/* The option of spec/avutil.md §4.15 before Avutil.Options assembles it:
   the union member read for the default follows the option's type. */
static value raw_option(const AVOption *option) {
  CAMLparam0();
  CAMLlocal2(_option, _type);
  int element_type = option->type & ~AV_OPT_TYPE_FLAG_ARRAY;
  int is_array = (option->type & AV_OPT_TYPE_FLAG_ARRAY) != 0;
  int64_t default_integer = 0;
  double default_float = 0;
  const char *default_string = NULL;

  if (!ocaml_avutil_find_variant(&option_type_table, element_type, &_type))
    _type = PVV_Unsupported;

  if (!is_array) {
    switch (element_type) {
    case AV_OPT_TYPE_CONST:
      default_integer = option->default_val.i64;
      default_float = option->default_val.dbl;
      break;
    case AV_OPT_TYPE_DOUBLE:
    case AV_OPT_TYPE_FLOAT:
    case AV_OPT_TYPE_RATIONAL:
      default_float = option->default_val.dbl;
      break;
    case AV_OPT_TYPE_STRING:
    case AV_OPT_TYPE_BINARY:
    case AV_OPT_TYPE_DICT:
    case AV_OPT_TYPE_IMAGE_SIZE:
    case AV_OPT_TYPE_VIDEO_RATE:
    case AV_OPT_TYPE_COLOR:
    case AV_OPT_TYPE_CHLAYOUT:
      default_string = option->default_val.str;
      break;
    default:
      default_integer = option->default_val.i64;
    }
  }

  _option = caml_alloc_tuple(11);
  Store_field(_option, 0, caml_copy_string(option->name ? option->name : ""));
  Store_field(
      _option, 1,
      some_string(option->help && option->help[0] ? option->help : NULL));
  Store_field(_option, 2, some_string(option->unit));
  Store_field(_option, 3, _type);
  Store_field(_option, 4, Val_bool(is_array));
  Store_field(_option, 5,
              ocaml_avutil_flags_of_mask(&option_flag_table, option->flags));
  Store_field(_option, 6, caml_copy_int64(default_integer));
  Store_field(_option, 7, caml_copy_double(default_float));
  Store_field(_option, 8, some_string(default_string));
  Store_field(_option, 9, caml_copy_double(option->min));
  Store_field(_option, 10, caml_copy_double(option->max));

  CAMLreturn(_option);
}

/* av_opt_next reads only the class pointer of its object, so the address of
   a class pointer stands for an object of the class. */
CAMLprim value ocaml_avutil_class_options(value _class) {
  CAMLparam1(_class);
  CAMLlocal1(_options);
  const AVClass *option_class = OptionClass_val(_class);
  const AVOption *option = NULL;
  mlsize_t count = 0;

  if (!option_class)
    CAMLreturn(Atom(0));

  while ((option = av_opt_next(&option_class, option)))
    count++;

  _options = caml_alloc_tuple(count);

  for (mlsize_t i = 0; i < count; i++) {
    option = av_opt_next(&option_class, option);
    Store_field(_options, i, raw_option(option));
  }

  CAMLreturn(_options);
}

CAMLprim value ocaml_avutil_child_classes(value _class) {
  CAMLparam1(_class);
  CAMLlocal1(_children);
  const AVClass *option_class = OptionClass_val(_class);
  const AVClass *child;
  void *iterator = NULL;
  mlsize_t count = 0;

  if (!option_class)
    CAMLreturn(Atom(0));

  while (av_opt_child_class_iterate(option_class, &iterator))
    count++;

  _children = caml_alloc_tuple(count);
  iterator = NULL;

  for (mlsize_t i = 0; i < count; i++) {
    child = av_opt_child_class_iterate(option_class, &iterator);
    Store_field(_children, i, ocaml_avutil_wrap_option_class(child));
  }

  CAMLreturn(_children);
}

#define OptionAccess_val(v)                                                    \
  (*(const ocaml_avutil_option_access **)Data_abstract_val(v))

value ocaml_avutil_option_object(value _owner,
                                 const ocaml_avutil_option_access *access) {
  CAMLparam1(_owner);
  CAMLlocal2(_object, _access);

  _access = caml_alloc(1, Abstract_tag);
  OptionAccess_val(_access) = access;

  _object = caml_alloc_tuple(2);
  Store_field(_object, 0, _owner);
  Store_field(_object, 1, _access);

  CAMLreturn(_object);
}

/* Evaluates [read], an FFmpeg option read on [object] of [name] with
   [flags], between the acquire and the release of the owner. [read] is
   evaluated after the acquire, which may allocate. */
#define READ_OPTION(read)                                                      \
  do {                                                                         \
    const ocaml_avutil_option_access *access =                                 \
        OptionAccess_val(Field(_object, 1));                                   \
    void *object = access->acquire(Field(_object, 0));                         \
    const char *name = String_val(_name);                                      \
    int flags = Bool_val(_search_children) ? AV_OPT_SEARCH_CHILDREN : 0;       \
    int error = (read);                                                        \
                                                                               \
    access->release(Field(_object, 0));                                        \
    if (error < 0)                                                             \
      ocaml_avutil_raise_error(error);                                         \
  } while (0)

CAMLprim value ocaml_avutil_get_option_string(value _search_children,
                                              value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  CAMLlocal1(_text);
  uint8_t *text = NULL;

  READ_OPTION(av_opt_get(object, name, flags, &text));

  _text = caml_copy_string(text ? (char *)text : "");
  av_free(text);

  CAMLreturn(_text);
}

CAMLprim value ocaml_avutil_get_option_int64(value _search_children,
                                             value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  int64_t number;

  READ_OPTION(av_opt_get_int(object, name, flags, &number));

  CAMLreturn(caml_copy_int64(number));
}

CAMLprim value ocaml_avutil_get_option_int(value _search_children, value _name,
                                           value _object) {
  CAMLparam3(_search_children, _name, _object);
  int64_t number;

  READ_OPTION(av_opt_get_int(object, name, flags, &number));

  if (number < Min_long || number > Max_long)
    ocaml_avutil_raise_failure("option value out of the integer range");

  CAMLreturn(Val_long(number));
}

CAMLprim value ocaml_avutil_get_option_float(value _search_children,
                                             value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  double number;

  READ_OPTION(av_opt_get_double(object, name, flags, &number));

  CAMLreturn(caml_copy_double(number));
}

CAMLprim value ocaml_avutil_get_option_rational(value _search_children,
                                                value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  AVRational rational;

  READ_OPTION(av_opt_get_q(object, name, flags, &rational));

  CAMLreturn(ocaml_avutil_value_of_rational(rational));
}

/* av_opt_get_video_rate fails on every video-rate option: the option is
   read as text, which FFmpeg writes as a fraction. */
CAMLprim value ocaml_avutil_get_option_video_rate(value _search_children,
                                                  value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  AVRational rate;
  uint8_t *text = NULL;
  int error;

  READ_OPTION(av_opt_get(object, name, flags, &text));

  error = av_parse_video_rate(&rate, text ? (char *)text : "");
  av_free(text);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(ocaml_avutil_value_of_rational(rate));
}

CAMLprim value ocaml_avutil_get_option_image_size(value _search_children,
                                                  value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  CAMLlocal1(_size);
  int width, height;

  READ_OPTION(av_opt_get_image_size(object, name, flags, &width, &height));

  _size = caml_alloc_tuple(2);
  Store_field(_size, 0, Val_int(width));
  Store_field(_size, 1, Val_int(height));

  CAMLreturn(_size);
}

CAMLprim value ocaml_avutil_get_option_pixel_format(value _search_children,
                                                    value _name,
                                                    value _object) {
  CAMLparam3(_search_children, _name, _object);
  enum AVPixelFormat pixel_format;

  READ_OPTION(av_opt_get_pixel_fmt(object, name, flags, &pixel_format));

  CAMLreturn(Val_PixelFormat(pixel_format));
}

CAMLprim value ocaml_avutil_get_option_sample_format(value _search_children,
                                                     value _name,
                                                     value _object) {
  CAMLparam3(_search_children, _name, _object);
  enum AVSampleFormat sample_format;

  READ_OPTION(av_opt_get_sample_fmt(object, name, flags, &sample_format));

  CAMLreturn(Val_SampleFormat(sample_format));
}

CAMLprim value ocaml_avutil_get_option_channel_layout(value _search_children,
                                                      value _name,
                                                      value _object) {
  CAMLparam3(_search_children, _name, _object);
  CAMLlocal1(_layout);
  AVChannelLayout *layout = alloc_channel_layout(&_layout);

  READ_OPTION(av_opt_get_chlayout(object, name, flags, layout));

  CAMLreturn(_layout);
}

CAMLprim value ocaml_avutil_get_option_dictionary(value _search_children,
                                                  value _name, value _object) {
  CAMLparam3(_search_children, _name, _object);
  CAMLlocal1(_pairs);
  AVDictionary *dictionary = NULL;

  READ_OPTION(av_opt_get_dict_val(object, name, flags, &dictionary));

  _pairs = ocaml_avutil_pairs_of_dictionary(dictionary);
  av_dict_free(&dictionary);

  CAMLreturn(_pairs);
}

CAMLprim value ocaml_avutil_create_device_context(value _device, value _options,
                                                  value _device_type) {
  CAMLparam3(_device, _options, _device_type);
  CAMLlocal1(_context);
  enum AVHWDeviceType device_type = HwDeviceType_val(_device_type);
  AVDictionary *options;
  AVBufferRef *context = NULL;
  char *device = NULL;
  int error;

  _context = alloc_buffer_handle();
  options = ocaml_avutil_dictionary_of_options(_options);

  if (caml_string_length(_device) > 0) {
    device = av_strdup(String_val(_device));
    if (!device) {
      av_dict_free(&options);
      caml_raise_out_of_memory();
    }
  }

  caml_release_runtime_system();
  error = av_hwdevice_ctx_create(&context, device_type, device, options, 0);
  caml_acquire_runtime_system();

  av_dict_free(&options);
  av_free(device);

  if (error < 0)
    ocaml_avutil_raise_error(error);

  HwContext_val(_context) = context;

  CAMLreturn(_context);
}

CAMLprim value ocaml_avutil_create_frame_context(value _width, value _height,
                                                 value _source_pixel_format,
                                                 value _hardware_pixel_format,
                                                 value _device) {
  CAMLparam5(_width, _height, _source_pixel_format, _hardware_pixel_format,
             _device);
  CAMLlocal1(_context);
  int width = ocaml_avutil_int_of_value(_width, "width");
  int height = ocaml_avutil_int_of_value(_height, "height");
  enum AVPixelFormat source_pixel_format =
      PixelFormat_val(_source_pixel_format);
  enum AVPixelFormat hardware_pixel_format =
      PixelFormat_val(_hardware_pixel_format);
  AVHWFramesContext *frames;
  AVBufferRef *context;
  int error;

  _context = alloc_buffer_handle();
  context = av_hwframe_ctx_alloc(HwContext_val(_device));
  if (!context)
    caml_raise_out_of_memory();
  HwContext_val(_context) = context;

  frames = (AVHWFramesContext *)context->data;
  frames->width = width;
  frames->height = height;
  frames->sw_format = source_pixel_format;
  frames->format = hardware_pixel_format;

  caml_release_runtime_system();
  error = av_hwframe_ctx_init(context);
  caml_acquire_runtime_system();

  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_context);
}
