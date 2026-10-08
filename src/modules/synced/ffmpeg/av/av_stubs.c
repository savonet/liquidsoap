/* Stubs of the av binding, spec/avformat.md.

   A container value is a custom block pointing at a native record: the
   format context, the decoder or encoder of each stream, the custom I/O
   context, and the roots of the closures FFmpeg calls. Av allocates the
   record closed, registers ocaml_av_release as its finaliser, then opens
   it: the release that drops the roots and waits for I/O never runs in the
   finaliser of the block, which only frees the record.

   Every FFmpeg call that can reach a callback is made with the runtime
   lock released and the container's guard taken. */

#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "av_stubs.h"

#include <libavutil/avstring.h>
#include <libavutil/mem.h>

#define CUSTOM_IO_BUFFER_SIZE 32768
#define SUBTITLE_PACKET_MAX (1024 * 1024)

/* The states of spec/avformat.md §2.1; an input is open or closed. */
enum { CONTAINER_OPEN, CONTAINER_STARTED, CONTAINER_FAILED, CONTAINER_CLOSED };

/* The closures of a container, and the bytes value handed to the read and
   write closures. */
enum {
  ROOT_INTERRUPT,
  ROOT_READ,
  ROOT_WRITE,
  ROOT_SEEK,
  ROOT_BUFFER,
  ROOT_COUNT
};

/* Av.kind: the media kinds a stream is listed and tagged under. */
enum { KIND_AUDIO, KIND_VIDEO, KIND_SUBTITLE, KIND_DATA, KIND_OTHER };

/* What a container keeps per stream. [codec] is the decoder of an input
   stream read as frames, opened on first use, or the encoder of an output
   stream. */
typedef struct {
  AVCodecContext *codec;
  const AVCodec *preferred_decoder;
  AVDictionary *decoder_options;
  AVPacket *encoded;
  int end_signalled;
  int copy_reserved;
} stream_state;

typedef struct {
  AVFormatContext *context;
  AVIOContext *custom_io;
  ocaml_avutil_guard guard;
  atomic_int state;
  int is_output;
  int interleaved;
  int opened_io;
  stream_state *streams;
  unsigned stream_capacity;
  int pending_stream;
  int ended;
  int rooted;
  value roots[ROOT_COUNT];
} container;

#define Container_val(v) (*(container **)Data_custom_val(v))

static void finalize_container(value _container) {
  av_free(Container_val(_container));
}

static struct custom_operations container_operations = {
    "ocaml_av_container",       finalize_container,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value ocaml_av_alloc_container(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_container);
  container *record;

  _container =
      caml_alloc_custom(&container_operations, sizeof(container *), 0, 1);
  Container_val(_container) = NULL;

  record = av_mallocz(sizeof(*record));
  if (!record)
    caml_raise_out_of_memory();
  atomic_store(&record->state, CONTAINER_CLOSED);
  record->pending_stream = -1;
  Container_val(_container) = record;

  CAMLreturn(_container);
}

static void root_closures(container *record) {
  for (int i = 0; i < ROOT_COUNT; i++) {
    record->roots[i] = Val_unit;
    caml_register_generational_global_root(&record->roots[i]);
  }
  record->rooted = 1;
}

static void set_root(container *record, int root, value _closure) {
  caml_modify_generational_global_root(&record->roots[root], _closure);
}

static void free_custom_io(container *record) {
  if (record->custom_io) {
    av_freep(&record->custom_io->buffer);
    avio_context_free(&record->custom_io);
  }
}

static void free_streams(container *record) {
  for (unsigned i = 0; i < record->stream_capacity; i++) {
    avcodec_free_context(&record->streams[i].codec);
    av_dict_free(&record->streams[i].decoder_options);
    av_packet_free(&record->streams[i].encoded);
  }
  av_freep(&record->streams);
  record->stream_capacity = 0;
}

/* Step 3 of the release: frees every native object, with the lock released
   around what can wait for I/O or call a closure, then drops the roots.
   Returns the code of the last I/O close. Safe in any state. */
static int release_container(container *record) {
  int error = 0;

  if (!record)
    return 0;

  atomic_store(&record->state, CONTAINER_CLOSED);

  caml_release_runtime_system();
  free_streams(record);
  if (record->context && record->is_output) {
    if (record->opened_io)
      error = avio_closep(&record->context->pb);
    avformat_free_context(record->context);
    record->context = NULL;
  } else {
    avformat_close_input(&record->context);
  }
  free_custom_io(record);
  caml_acquire_runtime_system();

  if (record->rooted) {
    for (int i = 0; i < ROOT_COUNT; i++)
      caml_remove_generational_global_root(&record->roots[i]);
    record->rooted = 0;
  }

  return error;
}

/* The finaliser Av registers on every container: release by collection. */
CAMLprim value ocaml_av_release(value _container) {
  CAMLparam1(_container);

  release_container(Container_val(_container));

  CAMLreturn(Val_unit);
}

static void check_state(container *record) {
  int state = atomic_load(&record->state);

  if (state == CONTAINER_CLOSED)
    ocaml_avutil_raise_closed();
  if (state == CONTAINER_FAILED)
    ocaml_avutil_raise_failed();
}

/* The record of a usable container with its guard taken exclusively or
   shared. They raise the state errors, the guard released. */
static container *exclusive(value _container) {
  container *record = Container_val(_container);

  if (!ocaml_avutil_guard_try_exclusive(&record->guard))
    ocaml_avutil_raise_in_use();
  if (atomic_load(&record->state) >= CONTAINER_FAILED) {
    ocaml_avutil_guard_release_exclusive(&record->guard);
    check_state(record);
  }

  return record;
}

static container *shared(value _container) {
  container *record = Container_val(_container);

  if (!ocaml_avutil_guard_try_shared(&record->guard))
    ocaml_avutil_raise_in_use();
  if (atomic_load(&record->state) >= CONTAINER_FAILED) {
    ocaml_avutil_guard_release_shared(&record->guard);
    check_state(record);
  }

  return record;
}

/* Ends an exclusive operation: releases the guard, then raises when [error]
   is a failure. */
static void done(container *record, int error) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (error < 0)
    ocaml_avutil_raise_error(error);
}

CAMLnoret static void fail_exclusive(container *record, const char *message) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  ocaml_avutil_raise_failure("%s", message);
}

CAMLnoret static void fail_shared(container *record, const char *message) {
  ocaml_avutil_guard_release_shared(&record->guard);
  ocaml_avutil_raise_failure("%s", message);
}

AVFormatContext *ocaml_av_format_context(value _container) {
  check_state(Container_val(_container));
  return Container_val(_container)->context;
}

int ocaml_av_try_guard(value _container) {
  return ocaml_avutil_guard_try_exclusive(&Container_val(_container)->guard);
}

void ocaml_av_release_guard(value _container) {
  ocaml_avutil_guard_release_exclusive(&Container_val(_container)->guard);
}

/* The state of stream [index], the table grown to hold it. Returns null
   when memory is short. Needs no lock. */
static stream_state *stream_slot(container *record, unsigned index) {
  if (index >= record->stream_capacity) {
    unsigned capacity = index + 8;
    stream_state *streams =
        av_realloc_array(record->streams, capacity, sizeof(*streams));

    if (!streams)
      return NULL;
    memset(streams + record->stream_capacity, 0,
           (capacity - record->stream_capacity) * sizeof(*streams));
    record->streams = streams;
    record->stream_capacity = capacity;
  }

  return &record->streams[index];
}

static int stream_exists(container *record, intnat index) {
  return index >= 0 && index < (intnat)record->context->nb_streams;
}

static int kind_of_stream(const AVStream *stream) {
  switch (stream->codecpar->codec_type) {
  case AVMEDIA_TYPE_AUDIO:
    return KIND_AUDIO;
  case AVMEDIA_TYPE_VIDEO:
    return KIND_VIDEO;
  case AVMEDIA_TYPE_SUBTITLE:
    return KIND_SUBTITLE;
  case AVMEDIA_TYPE_DATA:
    return KIND_DATA;
  default:
    return KIND_OTHER;
  }
}

/* Callbacks, spec/avformat.md §7.1. FFmpeg calls them without the runtime
   lock, on the thread of the operation or on one of its own. */

static void log_exception(value _exception) {
  CAMLparam1(_exception);
  CAMLlocal1(_text);

  _text = caml_callback_exn(*caml_named_value("ocaml_av_string_of_exception"),
                            _exception);
  if (Is_exception_result(_text))
    _text = Val_unit;
  else
    av_log(NULL, AV_LOG_ERROR, "%s\n", String_val(_text));

  CAMLreturn0;
}

/* Whether a closure raised. The result of a callback is an encoded
   exception then, which the collector must not meet in a root: the
   exception replaces it at once, and is logged. */
static int closure_raised(value *_result) {
  if (!Is_exception_result(*_result))
    return 0;

  *_result = Extract_exception(*_result);
  log_exception(*_result);

  return 1;
}

/* The C result of a closure that returned an integer: a negative one is an
   error code, one above [limit] a failure of the closure. */
static int io_result(value *_result, int limit) {
  intnat result;

  if (closure_raised(_result))
    return AVERROR_EXTERNAL;

  result = Long_val(*_result);
  if (result < INT_MIN || result > limit)
    return AVERROR_EXTERNAL;

  return (int)result;
}

static int locked_read(container *record, uint8_t *buffer, int length) {
  CAMLparam0();
  CAMLlocal1(_result);
  int result;

  _result =
      caml_callback3_exn(record->roots[ROOT_READ], record->roots[ROOT_BUFFER],
                         Val_int(0), Val_int(length));
  result = io_result(&_result, length);
  if (result > 0)
    memcpy(buffer, Bytes_val(record->roots[ROOT_BUFFER]), result);

  CAMLreturnT(int, result == 0 ? AVERROR_EOF : result);
}

static int read_callback(void *opaque, uint8_t *buffer, int size) {
  container *record = opaque;
  int result;

  ocaml_avutil_register_thread();
  caml_acquire_runtime_system();
  result =
      locked_read(record, buffer,
                  size < CUSTOM_IO_BUFFER_SIZE ? size : CUSTOM_IO_BUFFER_SIZE);
  caml_release_runtime_system();

  return result;
}

/* FFmpeg takes a write as all or nothing: the closure is called until it
   consumed everything. */
static int locked_write(container *record, const uint8_t *buffer, int size) {
  CAMLparam0();
  CAMLlocal1(_result);
  int written = 0;

  while (written < size) {
    int length = size - written < CUSTOM_IO_BUFFER_SIZE ? size - written
                                                        : CUSTOM_IO_BUFFER_SIZE;
    int offset = 0;

    memcpy(Bytes_val(record->roots[ROOT_BUFFER]), buffer + written, length);

    while (offset < length) {
      int consumed;

      _result = caml_callback3_exn(record->roots[ROOT_WRITE],
                                   record->roots[ROOT_BUFFER], Val_int(offset),
                                   Val_int(length - offset));
      consumed = io_result(&_result, length - offset);
      if (consumed <= 0)
        CAMLreturnT(int, consumed == 0 ? AVERROR_EXTERNAL : consumed);
      offset += consumed;
    }

    written += length;
  }

  CAMLreturnT(int, size);
}

static int write_callback(void *opaque, const uint8_t *buffer, int size) {
  container *record = opaque;
  int result;

  ocaml_avutil_register_thread();
  caml_acquire_runtime_system();
  result = locked_write(record, buffer, size);
  caml_release_runtime_system();

  return result;
}

static int64_t locked_seek(container *record, int64_t offset, int whence) {
  CAMLparam0();
  CAMLlocal1(_result);

  _result = caml_callback2_exn(record->roots[ROOT_SEEK], Val_long(offset),
                               Val_int(whence));
  if (closure_raised(&_result))
    CAMLreturnT(int64_t, AVERROR_EXTERNAL);

  CAMLreturnT(int64_t, Long_val(_result));
}

/* The whence values are the constructors of Unix.seek_command, in order. */
static int64_t seek_callback(void *opaque, int64_t offset, int whence) {
  container *record = opaque;
  int64_t result;

  whence &= ~AVSEEK_FORCE;
  if (whence == SEEK_SET)
    whence = 0;
  else if (whence == SEEK_CUR)
    whence = 1;
  else if (whence == SEEK_END)
    whence = 2;
  else
    return AVERROR(ENOSYS);

  ocaml_avutil_register_thread();
  caml_acquire_runtime_system();
  result = locked_seek(record, offset, whence);
  caml_release_runtime_system();

  return result;
}

static int locked_interrupt(container *record) {
  CAMLparam0();
  CAMLlocal1(_result);

  _result = caml_callback_exn(record->roots[ROOT_INTERRUPT], Val_unit);
  if (closure_raised(&_result))
    CAMLreturnT(int, 1);

  CAMLreturnT(int, Bool_val(_result));
}

static int interrupt_callback(void *opaque) {
  container *record = opaque;
  int result;

  ocaml_avutil_register_thread();
  caml_acquire_runtime_system();
  result = locked_interrupt(record);
  caml_release_runtime_system();

  return result;
}

static void install_interrupt(container *record, value _interrupt) {
  if (Is_some(_interrupt)) {
    set_root(record, ROOT_INTERRUPT, Some_val(_interrupt));
    record->context->interrupt_callback.callback = interrupt_callback;
    record->context->interrupt_callback.opaque = record;
  }
}

/* Creates the custom I/O context of a container whose read or write closure
   is rooted, with [_seek], a seek closure option. Returns 0 when memory is
   short. */
static int create_custom_io(container *record, int write, value _seek) {
  CAMLparam1(_seek);
  CAMLlocal1(_buffer);
  uint8_t *buffer;

  _buffer = caml_alloc_string(CUSTOM_IO_BUFFER_SIZE);
  set_root(record, ROOT_BUFFER, _buffer);
  if (Is_some(_seek))
    set_root(record, ROOT_SEEK, Some_val(_seek));

  buffer = av_malloc(CUSTOM_IO_BUFFER_SIZE);
  if (!buffer)
    CAMLreturnT(int, 0);

  record->custom_io = avio_alloc_context(buffer, CUSTOM_IO_BUFFER_SIZE, write,
                                         record, write ? NULL : read_callback,
                                         write ? write_callback : NULL,
                                         Is_some(_seek) ? seek_callback : NULL);
  if (!record->custom_io)
    av_free(buffer);

  CAMLreturnT(int, record->custom_io != NULL);
}

/* Formats, spec/avformat.md §4.2. */

value ocaml_av_wrap_input_format(const AVInputFormat *format) {
  value _format;

  if (!format)
    ocaml_avutil_raise_failure("null input format");

  _format = caml_alloc(1, Abstract_tag);
  InputFormat_val(_format) = format;

  return _format;
}

value ocaml_av_wrap_output_format(const AVOutputFormat *format) {
  value _format;

  if (!format)
    ocaml_avutil_raise_failure("null output format");

  _format = caml_alloc(1, Abstract_tag);
  OutputFormat_val(_format) = format;

  return _format;
}

static value copy_text(const char *text) {
  return caml_copy_string(text ? text : "");
}

CAMLprim value ocaml_av_input_format_name(value _format) {
  CAMLparam1(_format);
  CAMLreturn(copy_text(InputFormat_val(_format)->name));
}

CAMLprim value ocaml_av_input_format_long_name(value _format) {
  CAMLparam1(_format);
  CAMLreturn(copy_text(InputFormat_val(_format)->long_name));
}

CAMLprim value ocaml_av_output_format_name(value _format) {
  CAMLparam1(_format);
  CAMLreturn(copy_text(OutputFormat_val(_format)->name));
}

CAMLprim value ocaml_av_output_format_long_name(value _format) {
  CAMLparam1(_format);
  CAMLreturn(copy_text(OutputFormat_val(_format)->long_name));
}

CAMLprim value ocaml_av_find_input_format(value _name) {
  CAMLparam1(_name);
  const AVInputFormat *format = av_find_input_format(String_val(_name));

  if (!format)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_av_wrap_input_format(format)));
}

static const char *text_or_null(value _text) {
  return caml_string_length(_text) > 0 ? String_val(_text) : NULL;
}

CAMLprim value ocaml_av_guess_output_format(value _short_name, value _filename,
                                            value _mime) {
  CAMLparam3(_short_name, _filename, _mime);
  const AVOutputFormat *format = av_guess_format(
      text_or_null(_short_name), text_or_null(_filename), text_or_null(_mime));

  if (!format)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_av_wrap_output_format(format)));
}

CAMLprim value ocaml_av_output_format_audio_codec(value _format) {
  return ocaml_avutil_variant_of_constant(
      ocaml_avcodec_audio_id_table(), OutputFormat_val(_format)->audio_codec);
}

CAMLprim value ocaml_av_output_format_video_codec(value _format) {
  return ocaml_avutil_variant_of_constant(
      ocaml_avcodec_video_id_table(), OutputFormat_val(_format)->video_codec);
}

CAMLprim value ocaml_av_output_format_subtitle_codec(value _format) {
  return ocaml_avutil_variant_of_constant(
      ocaml_avcodec_subtitle_id_table(),
      OutputFormat_val(_format)->subtitle_codec);
}

CAMLprim value ocaml_av_version(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_version);
  unsigned version = avformat_version();

  _version = caml_alloc_tuple(3);
  Store_field(_version, 0, Val_int(AV_VERSION_MAJOR(version)));
  Store_field(_version, 1, Val_int(AV_VERSION_MINOR(version)));
  Store_field(_version, 2, Val_int(AV_VERSION_MICRO(version)));

  CAMLreturn(_version);
}

CAMLprim value ocaml_av_init(value _unit) {
  (void)_unit;
  avformat_network_init();
  return Val_unit;
}

CAMLprim value ocaml_av_container_options(value _unit) {
  (void)_unit;
  return ocaml_avutil_wrap_option_class(avformat_get_class());
}

/* Fails an open: releases everything, then raises [error], or a failure
   with [message] when there is one. */
CAMLnoret static void fail_open(container *record, AVDictionary **options,
                                int error, const char *message) {
  av_dict_free(options);
  release_container(record);
  if (message)
    ocaml_avutil_raise_failure("%s", message);
  ocaml_avutil_raise_error(error);
}

/* [_source] is Av.source: Url { url; interrupt } or Custom { read; seek }.
   Opens the demuxer of a closed container and returns the option keys it
   did not use. The streams are not probed. */
CAMLprim value ocaml_av_open_input(value _container, value _source,
                                   value _format, value _options) {
  CAMLparam4(_container, _source, _format, _options);
  container *record = Container_val(_container);
  const AVInputFormat *format =
      Is_some(_format) ? InputFormat_val(Some_val(_format)) : NULL;
  int custom = Tag_val(_source) == 1;
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  char *url = NULL;
  int error;

  root_closures(record);
  record->is_output = 0;
  record->pending_stream = -1;
  record->ended = 0;

  if (!custom && !format && caml_string_length(Field(_source, 0)) == 0)
    fail_open(record, &options, 0, "neither a URL nor a format");

  record->context = avformat_alloc_context();
  if (!record->context)
    fail_open(record, &options, AVERROR(ENOMEM), NULL);

  if (custom) {
    set_root(record, ROOT_READ, Field(_source, 0));
    if (!create_custom_io(record, 0, Field(_source, 1)))
      fail_open(record, &options, AVERROR(ENOMEM), NULL);
    record->context->pb = record->custom_io;
  } else {
    install_interrupt(record, Field(_source, 1));
    url = av_strdup(String_val(Field(_source, 0)));
    if (!url)
      fail_open(record, &options, AVERROR(ENOMEM), NULL);
  }

  caml_release_runtime_system();
  error = avformat_open_input(&record->context, url, format, &options);
  caml_acquire_runtime_system();
  av_free(url);

  if (error < 0)
    fail_open(record, &options, error, NULL);

  atomic_store(&record->state, CONTAINER_OPEN);

  CAMLreturn(ocaml_avutil_unused_options(&options));
}

CAMLprim value ocaml_av_stream_kinds(value _container) {
  CAMLparam1(_container);
  CAMLlocal1(_kinds);
  container *record = shared(_container);
  unsigned count = record->context->nb_streams;

  ocaml_avutil_guard_release_shared(&record->guard);

  _kinds = caml_alloc_tuple(count);
  record = shared(_container);
  for (unsigned i = 0; i < count && i < record->context->nb_streams; i++)
    Store_field(_kinds, i,
                Val_int(kind_of_stream(record->context->streams[i])));
  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(_kinds);
}

/* The decoder and the decoder options a configure function chose for a
   stream; [_codec] is a codec option. */
CAMLprim value ocaml_av_configure_stream(value _container, value _index,
                                         value _codec, value _options) {
  CAMLparam4(_container, _index, _codec, _options);
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  container *record = Container_val(_container);
  stream_state *stream;

  if (!ocaml_avutil_guard_try_exclusive(&record->guard)) {
    av_dict_free(&options);
    ocaml_avutil_raise_in_use();
  }

  stream = stream_slot(record, Int_val(_index));
  if (!stream) {
    av_dict_free(&options);
    done(record, AVERROR(ENOMEM));
  }

  av_dict_free(&stream->decoder_options);
  stream->decoder_options = options;
  stream->preferred_decoder =
      Is_some(_codec) ? Codec_val(Some_val(_codec)) : NULL;
  done(record, 0);

  CAMLreturn(Val_unit);
}

/* FFmpeg's probing takes one forced decoder per media type: the first
   preferred decoder of each. */
static void force_decoders(container *record) {
  AVFormatContext *context = record->context;

  for (unsigned i = 0; i < context->nb_streams && i < record->stream_capacity;
       i++) {
    const AVCodec *preferred = record->streams[i].preferred_decoder;
    const AVCodec **forced;

    if (!preferred)
      continue;

    switch (context->streams[i]->codecpar->codec_type) {
    case AVMEDIA_TYPE_AUDIO:
      forced = &context->audio_codec;
      break;
    case AVMEDIA_TYPE_VIDEO:
      forced = &context->video_codec;
      break;
    case AVMEDIA_TYPE_SUBTITLE:
      forced = &context->subtitle_codec;
      break;
    default:
      continue;
    }

    if (!*forced)
      *forced = preferred;
    else if (*forced != preferred)
      av_log(context, AV_LOG_WARNING,
             "stream %u prefers the decoder %s, probing uses %s\n", i,
             preferred->name, (*forced)->name);
  }
}

static int probe_streams(container *record) {
  AVFormatContext *context = record->context;
  unsigned count = context->nb_streams;
  AVDictionary **options = NULL;
  int error = 0;

  force_decoders(record);

  if (count > 0) {
    options = av_calloc(count, sizeof(*options));
    if (!options)
      return AVERROR(ENOMEM);
  }
  for (unsigned i = 0; i < count && i < record->stream_capacity && error >= 0;
       i++)
    error = av_dict_copy(&options[i], record->streams[i].decoder_options, 0);

  if (error >= 0)
    error = avformat_find_stream_info(context, options);

  for (unsigned i = 0; i < count; i++)
    av_dict_free(&options[i]);
  av_free(options);

  return error;
}

CAMLprim value ocaml_av_find_stream_info(value _container) {
  CAMLparam1(_container);
  container *record = exclusive(_container);
  int error;

  caml_release_runtime_system();
  error = probe_streams(record);
  caml_acquire_runtime_system();
  done(record, error);

  CAMLreturn(Val_unit);
}

static value rescaled_option(int64_t duration, AVRational time_base,
                             value _time_format) {
  int64_t units = ocaml_avutil_time_format_units(_time_format);
  int64_t rescaled;

  if (duration == AV_NOPTS_VALUE)
    return Val_none;

  rescaled = av_rescale_q(duration, time_base, (AVRational){1, (int)units});
  if (rescaled == 0)
    return Val_none;

  return caml_alloc_some(caml_copy_int64(rescaled));
}

CAMLprim value ocaml_av_input_duration(value _container, value _time_format) {
  CAMLparam2(_container, _time_format);
  container *record = shared(_container);
  int64_t duration = record->context->duration;

  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(rescaled_option(duration, AV_TIME_BASE_Q, _time_format));
}

/* A copy of a dictionary of the container taken under the guard; [index] is
   a stream, or -1 for the container. Raises. */
static AVDictionary *copied_metadata(value _container, intnat index) {
  container *record = shared(_container);
  AVDictionary *copy = NULL;
  int error;

  if (index >= 0 && !stream_exists(record, index))
    fail_shared(record, "the container has no such stream");

  error = av_dict_copy(&copy,
                       index < 0 ? record->context->metadata
                                 : record->context->streams[index]->metadata,
                       0);
  ocaml_avutil_guard_release_shared(&record->guard);
  if (error < 0) {
    av_dict_free(&copy);
    ocaml_avutil_raise_error(error);
  }

  return copy;
}

CAMLprim value ocaml_av_metadata(value _container, value _index) {
  CAMLparam2(_container, _index);
  CAMLlocal1(_pairs);
  AVDictionary *metadata = copied_metadata(_container, Long_val(_index));

  _pairs = ocaml_avutil_pairs_of_dictionary(metadata);
  av_dict_free(&metadata);

  CAMLreturn(_pairs);
}

CAMLprim value ocaml_av_input_format(value _container) {
  CAMLparam1(_container);
  container *record = shared(_container);
  const AVInputFormat *format = record->context->iformat;

  ocaml_avutil_guard_release_shared(&record->guard);
  if (!format)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_av_wrap_input_format(format)));
}

static void *acquire_options(value _container) {
  return shared(_container)->context;
}

static void release_options(value _container) {
  ocaml_avutil_guard_release_shared(&Container_val(_container)->guard);
}

static const ocaml_avutil_option_access container_option_access = {
    acquire_options, release_options};

CAMLprim value ocaml_av_input_object(value _container) {
  CAMLparam1(_container);
  check_state(Container_val(_container));
  CAMLreturn(ocaml_avutil_option_object(_container, &container_option_access));
}

CAMLprim value ocaml_av_find_best_stream(value _container, value _kind) {
  CAMLparam2(_container, _kind);
  static const enum AVMediaType types[] = {
      AVMEDIA_TYPE_AUDIO, AVMEDIA_TYPE_VIDEO, AVMEDIA_TYPE_SUBTITLE};
  container *record = exclusive(_container);
  int index;

  caml_release_runtime_system();
  index = av_find_best_stream(record->context, types[Int_val(_kind)], -1, -1,
                              NULL, 0);
  caml_acquire_runtime_system();
  done(record, index < 0 ? AVERROR_STREAM_NOT_FOUND : 0);

  CAMLreturn(Val_int(index));
}

/* The stream of a stream value with the container's guard taken shared.
   Raises, the guard released, when the container has no such stream. */
static AVStream *shared_stream(value _container, value _index,
                               container **record) {
  *record = shared(_container);
  if (!stream_exists(*record, Long_val(_index)))
    fail_shared(*record, "the container has no such stream");

  return (*record)->context->streams[Long_val(_index)];
}

CAMLprim value ocaml_av_stream_parameters(value _container, value _index) {
  CAMLparam2(_container, _index);
  CAMLlocal1(_parameters);
  container *record;
  AVCodecParameters *copy = avcodec_parameters_alloc();
  int error = AVERROR(ENOMEM);

  if (!copy)
    caml_raise_out_of_memory();

  /* The copy is taken under the guard; the value is built after it. */
  if (!ocaml_avutil_guard_try_shared(&Container_val(_container)->guard)) {
    avcodec_parameters_free(&copy);
    ocaml_avutil_raise_in_use();
  }
  record = Container_val(_container);
  if (atomic_load(&record->state) < CONTAINER_FAILED &&
      stream_exists(record, Long_val(_index)))
    error = avcodec_parameters_copy(
        copy, record->context->streams[Long_val(_index)]->codecpar);
  else
    error = AVERROR_STREAM_NOT_FOUND;
  ocaml_avutil_guard_release_shared(&record->guard);

  if (error < 0) {
    avcodec_parameters_free(&copy);
    check_state(record);
    if (error == AVERROR_STREAM_NOT_FOUND)
      ocaml_avutil_raise_failure("the container has no such stream");
    ocaml_avutil_raise_error(error);
  }

  _parameters = ocaml_avcodec_copy_parameters(copy);
  avcodec_parameters_free(&copy);

  CAMLreturn(_parameters);
}

static value some_rational_unless_unset(AVRational rational) {
  if (rational.num == 0)
    return Val_none;

  return caml_alloc_some(ocaml_avutil_value_of_rational(rational));
}

CAMLprim value ocaml_av_stream_avg_frame_rate(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  AVRational rate = shared_stream(_container, _index, &record)->avg_frame_rate;

  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(some_rational_unless_unset(rate));
}

CAMLprim value ocaml_av_stream_time_base(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  AVRational time_base = shared_stream(_container, _index, &record)->time_base;

  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(ocaml_avutil_value_of_rational(time_base));
}

CAMLprim value ocaml_av_stream_pixel_aspect(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  AVRational ratio =
      shared_stream(_container, _index, &record)->sample_aspect_ratio;

  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(some_rational_unless_unset(ratio));
}

CAMLprim value ocaml_av_stream_duration(value _container, value _index,
                                        value _time_format) {
  CAMLparam3(_container, _index, _time_format);
  container *record;
  AVStream *stream = shared_stream(_container, _index, &record);
  int64_t duration = stream->duration;
  AVRational time_base = stream->time_base;

  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(rescaled_option(duration, time_base, _time_format));
}

CAMLprim value ocaml_av_stream_frame_size(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  intnat index = Long_val(_index);
  int frame_size;

  shared_stream(_container, _index, &record);
  if (!record->is_output || (unsigned)index >= record->stream_capacity ||
      !record->streams[index].codec)
    fail_shared(record, "the stream has no encoder");
  frame_size = record->streams[index].codec->frame_size;
  ocaml_avutil_guard_release_shared(&record->guard);

  CAMLreturn(Val_int(frame_size));
}

/* The stream of a stream value whose fields may be written: the guard is
   taken exclusively and the header of an output is not written. Raises,
   the guard released. */
static AVStream *writable_stream(value _container, value _index,
                                 container **record) {
  *record = exclusive(_container);
  if (atomic_load(&(*record)->state) == CONTAINER_STARTED)
    fail_exclusive(*record, "the header is written");
  if (!stream_exists(*record, Long_val(_index)))
    fail_exclusive(*record, "the container has no such stream");

  return (*record)->context->streams[Long_val(_index)];
}

CAMLprim value ocaml_av_stream_set_avg_frame_rate(value _container,
                                                  value _index, value _rate) {
  CAMLparam3(_container, _index, _rate);
  AVRational rate = {0, 1};
  container *record;

  if (Is_some(_rate))
    rate = ocaml_avutil_rational_of_value(Some_val(_rate));
  writable_stream(_container, _index, &record)->avg_frame_rate = rate;
  done(record, 0);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_av_stream_set_time_base(value _container, value _index,
                                             value _time_base) {
  CAMLparam3(_container, _index, _time_base);
  AVRational time_base = ocaml_avutil_rational_of_value(_time_base);
  container *record;

  writable_stream(_container, _index, &record)->time_base = time_base;
  done(record, 0);

  CAMLreturn(Val_unit);
}

/* [_index] is a stream, or -1 for the container. */
CAMLprim value ocaml_av_set_metadata(value _container, value _index,
                                     value _metadata) {
  CAMLparam3(_container, _index, _metadata);
  AVDictionary *metadata = ocaml_avutil_dictionary_of_pairs(_metadata);
  container *record = Container_val(_container);
  AVDictionary **target = NULL;
  int state;

  if (!ocaml_avutil_guard_try_exclusive(&record->guard)) {
    av_dict_free(&metadata);
    ocaml_avutil_raise_in_use();
  }

  state = atomic_load(&record->state);
  if (state == CONTAINER_OPEN && Long_val(_index) < 0)
    target = &record->context->metadata;
  else if (state == CONTAINER_OPEN && stream_exists(record, Long_val(_index)))
    target = &record->context->streams[Long_val(_index)]->metadata;

  if (!target) {
    av_dict_free(&metadata);
    ocaml_avutil_guard_release_exclusive(&record->guard);
    check_state(record);
    ocaml_avutil_raise_failure(state == CONTAINER_STARTED
                                   ? "the header is written"
                                   : "the container has no such stream");
  }

  av_dict_free(target);
  *target = metadata;
  done(record, 0);

  CAMLreturn(Val_unit);
}

/* Reading, spec/avformat.md §4.3. Av.read_input drives these steps. */

static value indexed(int index, int kind, value _content) {
  CAMLparam1(_content);
  CAMLlocal1(_result);

  _result = caml_alloc_tuple(3);
  Store_field(_result, 0, Val_int(index));
  Store_field(_result, 1, Val_int(kind));
  Store_field(_result, 2, _content);

  CAMLreturn(caml_alloc_some(_result));
}

/* Reads the next packet of a stream of a listed kind into [packet]. Called
   without the lock. */
static int read_listed_packet(container *record, AVPacket *packet) {
  for (;;) {
    int error = av_read_frame(record->context, packet);

    if (error < 0)
      return error;
    if (kind_of_stream(record->context->streams[packet->stream_index]) !=
        KIND_OTHER)
      return 0;
    av_packet_unref(packet);
  }
}

/* The next packet as (index, kind, packet), None at the end of the input. */
CAMLprim value ocaml_av_read_packet(value _container) {
  CAMLparam1(_container);
  container *record = exclusive(_container);
  AVPacket *packet = av_packet_alloc();
  int error = AVERROR(ENOMEM);
  int index = 0, kind = 0;

  if (record->ended)
    error = AVERROR_EOF;
  else if (packet) {
    caml_release_runtime_system();
    error = read_listed_packet(record, packet);
    caml_acquire_runtime_system();
  }

  if (error >= 0) {
    index = packet->stream_index;
    kind = kind_of_stream(record->context->streams[index]);
  } else {
    av_packet_free(&packet);
    record->ended = error == AVERROR_EOF;
  }

  if (error == AVERROR_EOF) {
    done(record, 0);
    CAMLreturn(Val_none);
  }
  done(record, error);

  CAMLreturn(indexed(index, kind, ocaml_avcodec_wrap_packet(packet)));
}

/* The opened decoder of an input stream, opened on first use. Called with
   the lock held; a failure leaves the stream with no decoder. */
static int stream_decoder(container *record, unsigned index,
                          AVCodecContext **decoder) {
  AVStream *stream = record->context->streams[index];
  stream_state *state = stream_slot(record, index);
  AVDictionary *options = NULL;
  const AVCodec *codec;
  AVCodecContext *context;
  int error;

  if (!state)
    return AVERROR(ENOMEM);
  if (state->codec) {
    *decoder = state->codec;
    return 0;
  }

  codec = state->preferred_decoder;
  if (!codec)
    codec = avcodec_find_decoder(stream->codecpar->codec_id);
  if (!codec)
    return AVERROR_DECODER_NOT_FOUND;

  context = avcodec_alloc_context3(codec);
  if (!context)
    return AVERROR(ENOMEM);

  error = avcodec_parameters_to_context(context, stream->codecpar);
  if (error >= 0)
    error = av_dict_copy(&options, state->decoder_options, 0);
  if (error >= 0) {
    context->pkt_timebase = stream->time_base;
    error = ocaml_avcodec_open(context, codec, &options);
  }
  av_dict_free(&options);

  if (error < 0) {
    avcodec_free_context(&context);
    return error;
  }

  state = stream_slot(record, index);
  state->codec = context;
  *decoder = context;

  return 0;
}

/* Gives a packet to the decoder of its stream, whose frames are then the
   pending ones. */
CAMLprim value ocaml_av_decode_packet(value _container, value _index,
                                      value _packet) {
  CAMLparam3(_container, _index, _packet);
  container *record = exclusive(_container);
  const AVPacket *packet = Packet_val(_packet);
  AVCodecContext *decoder;
  int error;

  if (!stream_exists(record, Long_val(_index)))
    fail_exclusive(record, "the container has no such stream");

  error = stream_decoder(record, Int_val(_index), &decoder);
  if (error >= 0) {
    caml_release_runtime_system();
    error = avcodec_send_packet(decoder, packet);
    caml_acquire_runtime_system();
    record->pending_stream = Int_val(_index);
  }
  done(record, error);

  CAMLreturn(Val_unit);
}

/* Receives a frame from the decoder of a stream. Called without the lock;
   returns 0 with a frame, 1 with none, or FFmpeg's failure. */
static int receive_stream_frame(container *record, int index, AVFrame **frame) {
  int error;

  *frame = av_frame_alloc();
  if (!*frame)
    return AVERROR(ENOMEM);

  error = avcodec_receive_frame(record->streams[index].codec, *frame);
  if (error >= 0)
    return 0;

  av_frame_free(frame);

  return error == AVERROR(EAGAIN) || error == AVERROR_EOF ? 1 : error;
}

/* The next frame of the stream whose decoder was last given a packet, as
   (index, kind, frame); None when it has none ready. */
CAMLprim value ocaml_av_receive_pending(value _container) {
  CAMLparam1(_container);
  container *record = exclusive(_container);
  int index = record->pending_stream;
  AVFrame *frame = NULL;
  int result, kind;

  if (index < 0 || (unsigned)index >= record->stream_capacity ||
      !record->streams[index].codec) {
    done(record, 0);
    CAMLreturn(Val_none);
  }

  caml_release_runtime_system();
  result = receive_stream_frame(record, index, &frame);
  caml_acquire_runtime_system();

  if (result != 0)
    record->pending_stream = -1;
  kind = kind_of_stream(record->context->streams[index]);
  done(record, result < 0 ? result : 0);

  if (result != 0)
    CAMLreturn(Val_none);

  CAMLreturn(indexed(index, kind, ocaml_avutil_wrap_frame(frame)));
}

/* The next frame a decoder still holds once the input ended; None when the
   stream has no opened decoder or the decoder is drained. */
CAMLprim value ocaml_av_drain_stream(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record = exclusive(_container);
  intnat index = Long_val(_index);
  AVFrame *frame = NULL;
  int result, kind;

  if (!stream_exists(record, index) ||
      (unsigned)index >= record->stream_capacity ||
      !record->streams[index].codec ||
      record->streams[index].codec->codec_type == AVMEDIA_TYPE_SUBTITLE) {
    done(record, 0);
    CAMLreturn(Val_none);
  }

  caml_release_runtime_system();
  if (!record->streams[index].end_signalled) {
    avcodec_send_packet(record->streams[index].codec, NULL);
    record->streams[index].end_signalled = 1;
  }
  result = receive_stream_frame(record, (int)index, &frame);
  caml_acquire_runtime_system();

  kind = kind_of_stream(record->context->streams[index]);
  done(record, result < 0 ? result : 0);

  if (result != 0)
    CAMLreturn(Val_none);

  CAMLreturn(indexed((int)index, kind, ocaml_avutil_wrap_frame(frame)));
}

/* A decoded subtitle with no timestamp takes its packet's, and one with no
   end display time the packet's duration. */
static void complete_subtitle(AVSubtitle *subtitle, const AVPacket *packet,
                              AVRational time_base) {
  if (subtitle->pts == AV_NOPTS_VALUE && packet->pts != AV_NOPTS_VALUE)
    subtitle->pts = av_rescale_q(packet->pts, time_base, AV_TIME_BASE_Q);
  if (subtitle->end_display_time == 0 && packet->duration > 0)
    subtitle->end_display_time = (uint32_t)av_rescale_q(
        packet->duration, time_base, (AVRational){1, 1000});
}

CAMLprim value ocaml_av_decode_subtitle(value _container, value _index,
                                        value _packet) {
  CAMLparam3(_container, _index, _packet);
  container *record = exclusive(_container);
  AVPacket *packet = Packet_val(_packet);
  AVSubtitle *subtitle = NULL;
  AVCodecContext *decoder;
  int decoded = 0;
  int error;

  if (!stream_exists(record, Long_val(_index)))
    fail_exclusive(record, "the container has no such stream");

  error = stream_decoder(record, Int_val(_index), &decoder);
  if (error >= 0) {
    subtitle = av_mallocz(sizeof(*subtitle));
    error = subtitle ? 0 : AVERROR(ENOMEM);
  }
  if (error >= 0) {
    caml_release_runtime_system();
    error = avcodec_decode_subtitle2(decoder, subtitle, &decoded, packet);
    caml_acquire_runtime_system();
  }

  if (error >= 0 && decoded)
    complete_subtitle(subtitle, packet,
                      record->context->streams[Int_val(_index)]->time_base);
  else
    av_freep(&subtitle);
  done(record, error);

  if (!subtitle)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(ocaml_avutil_wrap_subtitle(subtitle)));
}

static const int seek_flags[] = {AVSEEK_FLAG_BACKWARD, AVSEEK_FLAG_BYTE,
                                 AVSEEK_FLAG_ANY, AVSEEK_FLAG_FRAME};

/* Every frame decoded before a seek is discarded. */
static void reset_decoders(container *record) {
  for (unsigned i = 0; i < record->stream_capacity; i++) {
    if (record->streams[i].codec)
      avcodec_flush_buffers(record->streams[i].codec);
    record->streams[i].end_signalled = 0;
  }
  record->pending_stream = -1;
  record->ended = 0;
}

/* [_times] is (min_ts option, max_ts option, ts); [_stream] is a stream
   index, or -1. */
CAMLprim value ocaml_av_seek(value _container, value _flags, value _stream,
                             value _time_format, value _times) {
  CAMLparam5(_container, _flags, _stream, _time_format, _times);
  int64_t units = ocaml_avutil_time_format_units(_time_format);
  int64_t minimum = INT64_MIN, maximum = INT64_MAX;
  int64_t target = Int64_val(Field(_times, 2));
  int has_minimum = Is_some(Field(_times, 0));
  int has_maximum = Is_some(Field(_times, 1));
  intnat stream = Long_val(_stream);
  AVRational time_base = AV_TIME_BASE_Q;
  container *record;
  int flags = 0;
  int error;

  if (has_minimum)
    minimum = Int64_val(Some_val(Field(_times, 0)));
  if (has_maximum)
    maximum = Int64_val(Some_val(Field(_times, 1)));
  for (value _flag = _flags; _flag != Val_emptylist; _flag = Field(_flag, 1))
    flags |= seek_flags[Int_val(Field(_flag, 0))];

  record = exclusive(_container);
  if (stream >= 0 && !stream_exists(record, stream))
    fail_exclusive(record, "the container has no such stream");
  if (stream >= 0)
    time_base = record->context->streams[stream]->time_base;

  if (!(flags & (AVSEEK_FLAG_BYTE | AVSEEK_FLAG_FRAME))) {
    AVRational unit = {1, (int)units};

    target = av_rescale_q(target, unit, time_base);
    if (has_minimum)
      minimum = av_rescale_q(minimum, unit, time_base);
    if (has_maximum)
      maximum = av_rescale_q(maximum, unit, time_base);
  }

  caml_release_runtime_system();
  error = avformat_seek_file(record->context, (int)stream, minimum, target,
                             maximum, flags);
  if (error >= 0)
    reset_decoders(record);
  caml_acquire_runtime_system();
  done(record, error);

  CAMLreturn(Val_unit);
}

/* [_target] is Av.target: Url { url; interrupt }, Custom { write; seek },
   or No_file. Opens a closed container for writing and returns the option
   keys no consumer used. */
CAMLprim value ocaml_av_open_output(value _container, value _target,
                                    value _format, value _interleaved,
                                    value _options) {
  CAMLparam5(_container, _target, _format, _interleaved, _options);
  container *record = Container_val(_container);
  const AVOutputFormat *format =
      Is_some(_format) ? OutputFormat_val(Some_val(_format)) : NULL;
  int has_url = Is_block(_target) && Tag_val(_target) == 0;
  int custom = Is_block(_target) && Tag_val(_target) == 1;
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  char *url = NULL;
  int error;

  root_closures(record);
  record->is_output = 1;
  record->interleaved = Bool_val(_interleaved);
  record->opened_io = 0;

  if (has_url) {
    url = av_strdup(String_val(Field(_target, 0)));
    if (!url)
      fail_open(record, &options, AVERROR(ENOMEM), NULL);
  }

  error = avformat_alloc_output_context2(&record->context, format, NULL, url);
  if (error < 0 || !record->context) {
    av_free(url);
    fail_open(record, &options, error < 0 ? error : AVERROR_MUXER_NOT_FOUND,
              NULL);
  }

  if (!has_url && !custom && !(record->context->oformat->flags & AVFMT_NOFILE))
    fail_open(record, &options, 0, "the format needs a file");
  if (custom && (record->context->oformat->flags & AVFMT_NOFILE))
    fail_open(record, &options, 0, "the format needs no file");

  if (custom) {
    set_root(record, ROOT_WRITE, Field(_target, 0));
    if (!create_custom_io(record, 1, Field(_target, 1)))
      fail_open(record, &options, AVERROR(ENOMEM), NULL);
    record->context->pb = record->custom_io;
    record->context->flags |= AVFMT_FLAG_CUSTOM_IO;
  }
  if (has_url)
    install_interrupt(record, Field(_target, 1));

  error = av_opt_set_dict2(record->context, &options, AV_OPT_SEARCH_CHILDREN);
  if (error >= 0 && has_url &&
      !(record->context->oformat->flags & AVFMT_NOFILE)) {
    caml_release_runtime_system();
    error = avio_open2(&record->context->pb, url, AVIO_FLAG_WRITE,
                       &record->context->interrupt_callback, &options);
    caml_acquire_runtime_system();
    record->opened_io = error >= 0;
  }
  av_free(url);

  if (error < 0)
    fail_open(record, &options, error, NULL);

  atomic_store(&record->state, CONTAINER_OPEN);

  CAMLreturn(ocaml_avutil_unused_options(&options));
}

CAMLprim value ocaml_av_output_started(value _container) {
  container *record = shared(_container);
  int started = atomic_load(&record->state) == CONTAINER_STARTED;

  ocaml_avutil_guard_release_shared(&record->guard);

  return Val_bool(started);
}

/* An output whose header is not written, its guard taken exclusively: the
   state in which streams are added. Raises, the guard released and
   [options], which may be null, freed. */
static container *configurable_output(value _container,
                                      AVDictionary **options) {
  container *record = Container_val(_container);
  const char *failure = NULL;
  int state;

  if (!ocaml_avutil_guard_try_exclusive(&record->guard)) {
    if (options)
      av_dict_free(options);
    ocaml_avutil_raise_in_use();
  }

  state = atomic_load(&record->state);
  if (!record->is_output)
    failure = "not an output";
  else if (state == CONTAINER_STARTED)
    failure = "the header is written";

  if (failure || state >= CONTAINER_FAILED) {
    if (options)
      av_dict_free(options);
    ocaml_avutil_guard_release_exclusive(&record->guard);
    check_state(record);
    ocaml_avutil_raise_failure("%s", failure);
  }

  return record;
}

/* Adds a stream to the muxer, the table grown first: nothing that can fail
   follows the point where the muxer accepted the stream. */
static AVStream *add_stream(container *record) {
  if (!stream_slot(record, record->context->nb_streams))
    return NULL;

  return avformat_new_stream(record->context, NULL);
}

CAMLprim value ocaml_av_reserve_stream_copy(value _container) {
  CAMLparam1(_container);
  container *record = configurable_output(_container, NULL);
  AVStream *stream = add_stream(record);
  int index = 0;

  if (stream) {
    index = stream->index;
    record->streams[index].copy_reserved = 1;
  }
  done(record, stream ? 0 : AVERROR(ENOMEM));

  CAMLreturn(Val_int(index));
}

/* The codec tag is cleared for the muxer to choose its own. */
CAMLprim value ocaml_av_initialize_stream_copy(value _container, value _index,
                                               value _parameters) {
  CAMLparam3(_container, _index, _parameters);
  container *record = configurable_output(_container, NULL);
  intnat index = Long_val(_index);
  AVStream *stream;
  int error;

  if (!stream_exists(record, index) || !record->streams[index].copy_reserved)
    fail_exclusive(record, "the stream copy is initialised already");

  stream = record->context->streams[index];
  error = avcodec_parameters_copy(stream->codecpar,
                                  CodecParameters_val(_parameters));
  if (error >= 0) {
    stream->codecpar->codec_tag = 0;
    record->streams[index].copy_reserved = 0;
  }
  done(record, error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_av_new_data_stream(value _container, value _time_base,
                                        value _id) {
  CAMLparam3(_container, _time_base, _id);
  AVRational time_base = ocaml_avutil_rational_of_value(_time_base);
  enum AVCodecID id = (enum AVCodecID)ocaml_avutil_constant_of_variant(
      ocaml_avcodec_unknown_id_table(), _id);
  container *record = configurable_output(_container, NULL);
  AVStream *stream = add_stream(record);
  int index = 0;

  if (stream) {
    index = stream->index;
    stream->time_base = time_base;
    stream->codecpar->codec_type = AVMEDIA_TYPE_DATA;
    stream->codecpar->codec_id = id;
  }
  done(record, stream ? 0 : AVERROR(ENOMEM));

  CAMLreturn(Val_int(index));
}

/* Opens [encoder], configured by the caller, adds its stream, ends the
   exclusive operation and returns (index, unused option keys).

   A failure before the muxer accepted the stream leaves the output
   unchanged, one after it leaves it failed. */
static value add_encoding_stream(container *record, AVCodecContext *encoder,
                                 const AVCodec *codec, int configured,
                                 AVDictionary **options,
                                 AVRational frame_rate) {
  CAMLparam0();
  CAMLlocal2(_result, _unused);
  AVStream *stream = NULL;
  int error = configured;

  if (error >= 0 && (record->context->oformat->flags & AVFMT_GLOBALHEADER))
    encoder->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
  if (error >= 0)
    error = ocaml_avcodec_open(encoder, codec, options);
  if (error >= 0) {
    stream = add_stream(record);
    error = stream ? 0 : AVERROR(ENOMEM);
  }

  if (error < 0) {
    avcodec_free_context(&encoder);
    av_dict_free(options);
    done(record, error);
  }

  record->streams[stream->index].codec = encoder;
  stream->time_base = encoder->time_base;
  stream->avg_frame_rate = frame_rate;
  error = avcodec_parameters_from_context(stream->codecpar, encoder);
  if (error < 0) {
    atomic_store(&record->state, CONTAINER_FAILED);
    av_dict_free(options);
    done(record, error);
  }
  done(record, 0);

  _unused = ocaml_avutil_unused_options(options);
  _result = caml_alloc_tuple(2);
  Store_field(_result, 0, Val_int(stream->index));
  Store_field(_result, 1, _unused);

  CAMLreturn(_result);
}

static AVCodecContext *alloc_encoder(container *record, const AVCodec *codec,
                                     AVDictionary **options) {
  AVCodecContext *encoder = avcodec_alloc_context3(codec);

  if (!encoder) {
    av_dict_free(options);
    done(record, AVERROR(ENOMEM));
  }

  return encoder;
}

/* [_settings] is (channel layout, sample rate, sample format, time base). */
CAMLprim value ocaml_av_new_audio_stream(value _container, value _options,
                                         value _settings, value _codec) {
  CAMLparam4(_container, _options, _settings, _codec);
  const AVCodec *codec = Codec_val(_codec);
  int sample_rate =
      ocaml_avutil_int_of_value(Field(_settings, 1), "sample rate");
  enum AVSampleFormat sample_format = SampleFormat_val(Field(_settings, 2));
  AVRational time_base = ocaml_avutil_rational_of_value(Field(_settings, 3));
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  container *record = configurable_output(_container, &options);
  AVCodecContext *encoder = alloc_encoder(record, codec, &options);
  int configured = ocaml_avcodec_set_audio_encoding(
      encoder, ChannelLayout_val(Field(_settings, 0)), sample_rate,
      sample_format, time_base);

  CAMLreturn(add_encoding_stream(record, encoder, codec, configured, &options,
                                 (AVRational){0, 1}));
}

/* [_settings] is (frame rate option, hardware context option, pixel
   format, width, height, time base). */
CAMLprim value ocaml_av_new_video_stream(value _container, value _options,
                                         value _settings, value _codec) {
  CAMLparam4(_container, _options, _settings, _codec);
  const AVCodec *codec = Codec_val(_codec);
  enum AVPixelFormat pixel_format = PixelFormat_val(Field(_settings, 2));
  int width = ocaml_avutil_int_of_value(Field(_settings, 3), "width");
  int height = ocaml_avutil_int_of_value(Field(_settings, 4), "height");
  AVRational time_base = ocaml_avutil_rational_of_value(Field(_settings, 5));
  AVRational frame_rate = {0, 1};
  AVDictionary *options;
  container *record;
  AVCodecContext *encoder;
  int configured;

  if (Is_some(Field(_settings, 0)))
    frame_rate = ocaml_avutil_rational_of_value(Some_val(Field(_settings, 0)));

  options = ocaml_avutil_dictionary_of_options(_options);
  record = configurable_output(_container, &options);
  encoder = alloc_encoder(record, codec, &options);
  configured = ocaml_avcodec_set_video_encoding(encoder, pixel_format, width,
                                                height, time_base, frame_rate,
                                                Field(_settings, 1));

  CAMLreturn(add_encoding_stream(record, encoder, codec, configured, &options,
                                 frame_rate));
}

static int is_text_subtitle_codec(const AVCodec *codec) {
  const AVCodecDescriptor *descriptor = avcodec_descriptor_get(codec->id);

  return descriptor && (descriptor->props & AV_CODEC_PROP_TEXT_SUB);
}

/* [_settings] is (header, time base); an empty header is none. */
CAMLprim value ocaml_av_new_subtitle_stream(value _container, value _options,
                                            value _settings, value _codec) {
  CAMLparam4(_container, _options, _settings, _codec);
  const AVCodec *codec = Codec_val(_codec);
  AVRational time_base = ocaml_avutil_rational_of_value(Field(_settings, 1));
  size_t header_size = caml_string_length(Field(_settings, 0));
  AVDictionary *options = ocaml_avutil_dictionary_of_options(_options);
  container *record = configurable_output(_container, &options);
  AVCodecContext *encoder = alloc_encoder(record, codec, &options);
  int configured = 0;

  encoder->time_base = time_base;
  if (header_size > 0 && is_text_subtitle_codec(codec)) {
    encoder->subtitle_header = av_mallocz(header_size + 1);
    if (encoder->subtitle_header) {
      memcpy(encoder->subtitle_header, String_val(Field(_settings, 0)),
             header_size);
      encoder->subtitle_header_size = (int)header_size;
    } else {
      configured = AVERROR(ENOMEM);
    }
  }

  CAMLreturn(add_encoding_stream(record, encoder, codec, configured, &options,
                                 (AVRational){0, 1}));
}

/* Writes the header of an output that has none. Called without the lock;
   a failure leaves the output open. */
static int ensure_header(container *record) {
  int error;

  if (atomic_load(&record->state) != CONTAINER_OPEN)
    return 0;

  error = avformat_write_header(record->context, NULL);
  if (error >= 0)
    atomic_store(&record->state, CONTAINER_STARTED);

  return error;
}

/* Writes [packet], whose times are in [time_base], to stream [index], and
   frees it. Called without the lock. */
static int write_owned_packet(container *record, int index, AVPacket *packet,
                              AVRational time_base) {
  int error = ensure_header(record);

  if (error >= 0) {
    av_packet_rescale_ts(packet, time_base,
                         record->context->streams[index]->time_base);
    packet->stream_index = index;
    error = record->interleaved
                ? av_interleaved_write_frame(record->context, packet)
                : av_write_frame(record->context, packet);
  }
  av_packet_free(&packet);

  return error;
}

/* An output stream with the guard taken exclusively; [encoding] says
   whether the stream must have an encoder or must have none. Raises, the
   guard released. */
static container *output_stream(value _container, value _index, int encoding) {
  container *record = exclusive(_container);
  intnat index = Long_val(_index);
  int has_encoder;

  if (!record->is_output || !stream_exists(record, index))
    fail_exclusive(record, "the output has no such stream");

  has_encoder = (unsigned)index < record->stream_capacity &&
                record->streams[index].codec != NULL;
  if (has_encoder != encoding)
    fail_exclusive(record, encoding ? "the stream has no encoder"
                                    : "the stream encodes frames");

  return record;
}

CAMLprim value ocaml_av_write_packet(value _container, value _index,
                                     value _time_base, value _packet) {
  CAMLparam4(_container, _index, _time_base, _packet);
  AVRational time_base = ocaml_avutil_rational_of_value(_time_base);
  container *record = output_stream(_container, _index, 0);
  AVPacket *reference = av_packet_alloc();
  int index = Int_val(_index);
  int error = AVERROR(ENOMEM);

  if (reference)
    error = av_packet_ref(reference, Packet_val(_packet));
  if (error < 0) {
    av_packet_free(&reference);
  } else {
    caml_release_runtime_system();
    error = write_owned_packet(record, index, reference, time_base);
    caml_acquire_runtime_system();
  }
  done(record, error);

  CAMLreturn(Val_unit);
}

/* The encoding steps of Av.write_frame. A send answers false when the
   encoder wants its output read first; [_frame] is a frame option. */
CAMLprim value ocaml_av_stream_send_frame(value _container, value _index,
                                          value _frame) {
  CAMLparam3(_container, _index, _frame);
  container *record = output_stream(_container, _index, 1);
  const AVFrame *frame = Is_some(_frame) ? Frame_val(Some_val(_frame)) : NULL;
  AVCodecContext *encoder = record->streams[Int_val(_index)].codec;
  int error;

  caml_release_runtime_system();
  error = ensure_header(record);
  if (error >= 0)
    error = ocaml_avcodec_encoder_send(encoder, frame);
  caml_acquire_runtime_system();

  if (error == AVERROR(EAGAIN)) {
    done(record, 0);
    CAMLreturn(Val_false);
  }
  done(record, error);

  CAMLreturn(Val_true);
}

/* Receives the next packet of the stream's encoder and holds it for
   ocaml_av_write_encoded. Returns 0 with none, 1 with a packet, 2 with a
   key packet. */
CAMLprim value ocaml_av_stream_receive_packet(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record = output_stream(_container, _index, 1);
  stream_state *stream = &record->streams[Int_val(_index)];
  int error = 0;
  int result = 0;

  if (!stream->encoded) {
    stream->encoded = av_packet_alloc();
    if (!stream->encoded)
      done(record, AVERROR(ENOMEM));

    caml_release_runtime_system();
    error = avcodec_receive_packet(stream->codec, stream->encoded);
    caml_acquire_runtime_system();
    if (error < 0)
      av_packet_free(&stream->encoded);
    if (error == AVERROR(EAGAIN) || error == AVERROR_EOF)
      error = 0;
  }

  if (stream->encoded)
    result = (stream->encoded->flags & AV_PKT_FLAG_KEY) ? 2 : 1;
  done(record, error);

  CAMLreturn(Val_int(result));
}

CAMLprim value ocaml_av_write_encoded(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record = output_stream(_container, _index, 1);
  int index = Int_val(_index);
  stream_state *stream = &record->streams[index];
  AVPacket *packet = stream->encoded;
  int error = 0;

  stream->encoded = NULL;
  if (packet) {
    caml_release_runtime_system();
    error = write_owned_packet(record, index, packet, stream->codec->time_base);
    caml_acquire_runtime_system();
  }
  done(record, error);

  CAMLreturn(Val_unit);
}

/* Encodes a subtitle into [packet]. Called without the lock; returns the
   size of the packet, 0 when the subtitle encodes to nothing. */
static int encode_subtitle(AVCodecContext *encoder, AVStream *stream,
                           AVSubtitle *subtitle, AVPacket *packet) {
  AVRational milliseconds = {1, 1000};
  uint8_t *buffer = av_malloc(SUBTITLE_PACKET_MAX);
  int size;

  if (!buffer)
    return AVERROR(ENOMEM);

  size =
      avcodec_encode_subtitle(encoder, buffer, SUBTITLE_PACKET_MAX, subtitle);
  if (size > 0) {
    int error = av_new_packet(packet, size);

    if (error < 0) {
      av_free(buffer);
      return error;
    }
    memcpy(packet->data, buffer, size);
    packet->pts =
        av_rescale_q(subtitle->pts, AV_TIME_BASE_Q, stream->time_base) +
        av_rescale_q(subtitle->start_display_time, milliseconds,
                     stream->time_base);
    packet->dts = packet->pts;
    packet->duration = av_rescale_q((int64_t)subtitle->end_display_time -
                                        subtitle->start_display_time,
                                    milliseconds, stream->time_base);
  }
  av_free(buffer);

  return size;
}

CAMLprim value ocaml_av_write_subtitle(value _container, value _index,
                                       value _subtitle) {
  CAMLparam3(_container, _index, _subtitle);
  container *record = output_stream(_container, _index, 1);
  int index = Int_val(_index);
  AVSubtitle *subtitle = Subtitle_val(_subtitle);
  AVPacket *packet = av_packet_alloc();
  int error = AVERROR(ENOMEM);

  if (packet) {
    AVStream *stream = record->context->streams[index];

    caml_release_runtime_system();
    error = ensure_header(record);
    if (error >= 0)
      error = encode_subtitle(record->streams[index].codec, stream, subtitle,
                              packet);
    if (error > 0)
      error = write_owned_packet(record, index, packet, stream->time_base);
    else
      av_packet_free(&packet);
    caml_acquire_runtime_system();
  }
  done(record, error);

  CAMLreturn(Val_unit);
}

/* The error a flush of the I/O left behind. Called without the lock. */
static int flush_io(container *record) {
  AVIOContext *io = record->context->pb;

  if (!io)
    return 0;

  avio_flush(io);

  return io->error < 0 ? io->error : 0;
}

CAMLprim value ocaml_av_flush(value _container) {
  CAMLparam1(_container);
  container *record = exclusive(_container);
  int error = 0;

  if (record->is_output && atomic_load(&record->state) == CONTAINER_STARTED) {
    caml_release_runtime_system();
    error = record->interleaved
                ? av_interleaved_write_frame(record->context, NULL)
                : av_write_frame(record->context, NULL);
    if (error >= 0)
      error = flush_io(record);
    caml_acquire_runtime_system();
  }
  done(record, error < 0 ? error : 0);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_av_tell(value _container) {
  CAMLparam1(_container);
  container *record = exclusive(_container);
  int64_t position = -1;

  if (record->context->pb) {
    caml_release_runtime_system();
    position = avio_tell(record->context->pb);
    caml_acquire_runtime_system();
  }
  done(record, 0);

  if (position < 0)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(Val_long(position)));
}

/* Flushes the encoder of a stream into the muxer. Called without the
   lock; returns the first failure and attempts every step. */
static int flush_encoder(container *record, int index) {
  AVCodecContext *encoder = record->streams[index].codec;
  AVPacket *held = record->streams[index].encoded;
  int first_error = 0;
  int error;

  record->streams[index].encoded = NULL;
  if (held)
    first_error = write_owned_packet(record, index, held, encoder->time_base);

  error = ocaml_avcodec_encoder_send(encoder, NULL);
  if (error < 0 && error != AVERROR_EOF && first_error >= 0)
    first_error = error;

  for (;;) {
    AVPacket *packet = av_packet_alloc();

    if (!packet)
      return first_error < 0 ? first_error : AVERROR(ENOMEM);

    error = avcodec_receive_packet(encoder, packet);
    if (error < 0) {
      av_packet_free(&packet);
      if (error != AVERROR_EOF && error != AVERROR(EAGAIN) && first_error >= 0)
        first_error = error;
      return first_error;
    }

    error = write_owned_packet(record, index, packet, encoder->time_base);
    if (error < 0 && first_error >= 0)
      first_error = error;
  }
}

/* Step 2 of Av.close on an output. Called without the lock. */
static int finish_output(container *record) {
  int first_error = 0;
  int error;

  for (unsigned i = 0;
       i < record->context->nb_streams && i < record->stream_capacity; i++) {
    AVCodecContext *encoder = record->streams[i].codec;

    if (!encoder || encoder->codec_type == AVMEDIA_TYPE_SUBTITLE)
      continue;
    error = flush_encoder(record, (int)i);
    if (error < 0 && first_error >= 0)
      first_error = error;
  }

  if (atomic_load(&record->state) == CONTAINER_STARTED) {
    error = av_write_trailer(record->context);
    if (error >= 0)
      error = flush_io(record);
    if (error < 0 && first_error >= 0)
      first_error = error;
  }

  return first_error;
}

CAMLprim value ocaml_av_close(value _container) {
  CAMLparam1(_container);
  container *record = Container_val(_container);
  int state = atomic_load(&record->state);
  int first_error = 0;
  int error;

  if (state == CONTAINER_CLOSED)
    CAMLreturn(Val_unit);
  if (!ocaml_avutil_guard_try_exclusive(&record->guard))
    ocaml_avutil_raise_in_use();

  if (record->is_output && state != CONTAINER_FAILED) {
    caml_release_runtime_system();
    first_error = finish_output(record);
    caml_acquire_runtime_system();
  }

  error = release_container(record);
  if (first_error >= 0)
    first_error = error;
  done(record, first_error);

  CAMLreturn(Val_unit);
}

/* The payload of the first NAL unit of [type] in Annex-B data, after its
   [header_size]-byte header; [*size] is what remains of the data. */
static const uint8_t *find_nal(const uint8_t *data, int data_size,
                               int header_size, int type_mask, int type_shift,
                               int type, int *size) {
  for (int i = 0; i + 3 + header_size <= data_size; i++) {
    if (data[i] != 0 || data[i + 1] != 0 || data[i + 2] != 1)
      continue;
    if (((data[i + 3] >> type_shift) & type_mask) == type) {
      *size = data_size - (i + 3 + header_size);
      return data + i + 3 + header_size;
    }
  }

  return NULL;
}

static int starts_with_start_code(const uint8_t *data, int size) {
  return (size >= 4 && !data[0] && !data[1] && !data[2] && data[3] == 1) ||
         (size >= 3 && !data[0] && !data[1] && data[2] == 1);
}

static int h264_attribute(const AVCodecParameters *parameters, char *text,
                          size_t text_size) {
  const uint8_t *data = parameters->extradata;
  int size = parameters->extradata_size;
  int offset;

  if (!data || !starts_with_start_code(data, size))
    return 0;

  offset = data[2] == 1 ? 3 : 4;
  if (size < offset + 4 || (data[offset] & 0x1f) != 7)
    return 0;

  snprintf(text, text_size, "avc1.%02x%02x%02x", data[offset + 1],
           data[offset + 2], data[offset + 3]);

  return 1;
}

/* The general profile and level of an HEVC SPS: its profile is in the
   second payload byte and its level twelve bytes in. */
static int hevc_attribute(const AVCodecParameters *parameters, char *text,
                          size_t text_size) {
  int profile = parameters->profile;
  int level = parameters->level;
  int sps_size = 0;
  const uint8_t *sps = NULL;

  if (parameters->extradata)
    sps = find_nal(parameters->extradata, parameters->extradata_size, 2, 0x3f,
                   1, 33, &sps_size);
  if (sps && sps_size >= 13) {
    profile = sps[1] & 0x1f;
    level = sps[12];
  }

  if (parameters->codec_tag == MKTAG('h', 'v', 'c', '1') &&
      profile != AV_PROFILE_UNKNOWN && level != AV_LEVEL_UNKNOWN) {
    snprintf(text, text_size, "hvc1.%d.4.L%d.B01", profile, level);
    return 1;
  }

  if (!parameters->codec_tag)
    return 0;
  snprintf(text, text_size, "%c%c%c%c", parameters->codec_tag & 0xff,
           (parameters->codec_tag >> 8) & 0xff,
           (parameters->codec_tag >> 16) & 0xff,
           (parameters->codec_tag >> 24) & 0xff);

  return 1;
}

static int codec_attribute(const AVCodecParameters *parameters, char *text,
                           size_t text_size) {
  const char *fixed = NULL;

  switch (parameters->codec_id) {
  case AV_CODEC_ID_H264:
    return h264_attribute(parameters, text, text_size);
  case AV_CODEC_ID_HEVC:
    return hevc_attribute(parameters, text, text_size);
  case AV_CODEC_ID_AAC:
    if (parameters->profile == AV_PROFILE_UNKNOWN)
      return 0;
    snprintf(text, text_size, "mp4a.40.%d", parameters->profile + 1);
    return 1;
  case AV_CODEC_ID_MP2:
    fixed = "mp4a.40.33";
    break;
  case AV_CODEC_ID_MP3:
    fixed = "mp4a.40.34";
    break;
  case AV_CODEC_ID_AC3:
    fixed = "ac-3";
    break;
  case AV_CODEC_ID_EAC3:
    fixed = "ec-3";
    break;
  case AV_CODEC_ID_FLAC:
    fixed = "fLaC";
    break;
  default:
    return 0;
  }

  snprintf(text, text_size, "%s", fixed);

  return 1;
}

CAMLprim value ocaml_av_codec_attr(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  AVStream *stream = shared_stream(_container, _index, &record);
  char text[64];
  int found = codec_attribute(stream->codecpar, text, sizeof(text));

  ocaml_avutil_guard_release_shared(&record->guard);
  if (!found)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_string(text)));
}

CAMLprim value ocaml_av_bitrate(value _container, value _index) {
  CAMLparam2(_container, _index);
  container *record;
  AVStream *stream = shared_stream(_container, _index, &record);
  const AVCodecParameters *parameters = stream->codecpar;
  int64_t bit_rate = parameters->bit_rate;
  const AVPacketSideData *side_data = av_packet_side_data_get(
      parameters->coded_side_data, parameters->nb_coded_side_data,
      AV_PKT_DATA_CPB_PROPERTIES);

  if (bit_rate <= 0 && side_data && side_data->size >= sizeof(AVCPBProperties))
    bit_rate = ((const AVCPBProperties *)side_data->data)->max_bitrate;
  ocaml_avutil_guard_release_shared(&record->guard);

  if (bit_rate <= 0)
    CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(Val_long(bit_rate)));
}
