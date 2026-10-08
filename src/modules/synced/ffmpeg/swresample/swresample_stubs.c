/* Stubs of the swresample binding, spec/swresample.md.

   A converter is a custom block pointing at a native record: the
   libswresample context, its guard, and the format of each side, which
   every input is checked against before anything is read.

   The conversions take frames, or planes held in bigarrays of any element
   type, and write into a frame or into fresh bigarrays allocated for the
   most samples the call can produce and cut to what it produced. */

#include <limits.h>
#include <string.h>

#include "avutil_stubs.h"

#include <libavutil/mem.h>
#include <libswresample/swresample.h>

#include "swresample_options_stubs.h"

#define MAX_CHANNELS 64

typedef struct {
  enum AVSampleFormat sample_format;
  int channels;
  int sample_rate;
} side;

typedef struct {
  struct SwrContext *context;
  ocaml_avutil_guard guard;
  AVChannelLayout output_layout;
  side input;
  side output;
} resampler;

#define Resampler_val(v) (*(resampler **)Data_custom_val(v))

static void finalize_resampler(value _resampler) {
  resampler *record = Resampler_val(_resampler);

  if (record) {
    swr_free(&record->context);
    av_channel_layout_uninit(&record->output_layout);
    av_free(record);
  }
}

static struct custom_operations resampler_operations = {
    "ocaml_swresample_resampler", finalize_resampler,
    custom_compare_default,       custom_hash_default,
    custom_serialize_default,     custom_deserialize_default,
    custom_compare_ext_default,   custom_fixed_length_default};

CAMLprim value ocaml_swresample_version(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_version);
  unsigned version = swresample_version();

  _version = caml_alloc_tuple(3);
  Store_field(_version, 0, Val_int(AV_VERSION_MAJOR(version)));
  Store_field(_version, 1, Val_int(AV_VERSION_MINOR(version)));
  Store_field(_version, 2, Val_int(AV_VERSION_MICRO(version)));

  CAMLreturn(_version);
}

CAMLprim value ocaml_swresample_is_planar(value _sample_format) {
  return Val_bool(av_sample_fmt_is_planar(SampleFormat_val(_sample_format)));
}

/* Swresample.setting: the tag is the resampler option, the argument its
   value. Raises. */
static int apply_setting(struct SwrContext *context, value _setting) {
  static const char *const names[] = {"dither_method", "resampler",
                                      "filter_type"};
  const ocaml_ffmpeg_variant_table *tables[] = {
      swresample_options_dither_type_table(), swresample_options_engine_table(),
      swresample_options_filter_type_table()};
  int option = Tag_val(_setting);

  return av_opt_set_int(
      context, names[option],
      ocaml_avutil_constant_of_variant(tables[option], Field(_setting, 0)), 0);
}

/* [_input] and [_output] are (channel layout, sample format, sample rate). */
CAMLprim value ocaml_swresample_create(value _settings, value _input,
                                       value _output) {
  CAMLparam3(_settings, _input, _output);
  CAMLlocal1(_resampler);
  side input = {SampleFormat_val(Field(_input, 1)), 0,
                ocaml_avutil_int_of_value(Field(_input, 2), "sample rate")};
  side output = {SampleFormat_val(Field(_output, 1)), 0,
                 ocaml_avutil_int_of_value(Field(_output, 2), "sample rate")};
  const AVChannelLayout *input_layout = ChannelLayout_val(Field(_input, 0));
  const AVChannelLayout *output_layout = ChannelLayout_val(Field(_output, 0));
  resampler *record;
  int error;

  input.channels = input_layout->nb_channels;
  output.channels = output_layout->nb_channels;
  if (input.channels > MAX_CHANNELS || output.channels > MAX_CHANNELS)
    ocaml_avutil_raise_failure("too many channels");

  _resampler =
      caml_alloc_custom(&resampler_operations, sizeof(resampler *), 0, 1);
  Resampler_val(_resampler) = NULL;
  record = av_mallocz(sizeof(*record));
  if (!record)
    caml_raise_out_of_memory();
  Resampler_val(_resampler) = record;
  record->input = input;
  record->output = output;

  error = av_channel_layout_copy(&record->output_layout, output_layout);
  if (error >= 0)
    error = swr_alloc_set_opts2(&record->context, output_layout,
                                output.sample_format, output.sample_rate,
                                input_layout, input.sample_format,
                                input.sample_rate, 0, NULL);
  for (value _setting = _settings; error >= 0 && _setting != Val_emptylist;
       _setting = Field(_setting, 1))
    error = apply_setting(record->context, Field(_setting, 0));
  if (error >= 0) {
    caml_release_runtime_system();
    error = swr_init(record->context);
    caml_acquire_runtime_system();
  }
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_resampler);
}

CAMLnoret static void fail(resampler *record, const char *message) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  ocaml_avutil_raise_failure("%s", message);
}

/* The channel planes of [count] samples of an input, whose memory outlives
   the conversion: frames and bigarrays are native memory. */
typedef struct {
  const uint8_t *planes[MAX_CHANNELS];
  int count;
} input_samples;

/* The number of buffers a side holds its channels in, and the bytes one
   sample takes in each. */
static int plane_count(const side *format) {
  return av_sample_fmt_is_planar(format->sample_format) ? format->channels : 1;
}

static size_t plane_sample_size(const side *format) {
  size_t bytes = av_get_bytes_per_sample(format->sample_format);

  return av_sample_fmt_is_planar(format->sample_format)
             ? bytes
             : bytes * format->channels;
}

/* Reads a Swresample.samples option, whose planes are bigarrays each in a
   block of its own, into [input], cut to the range
   [offset, offset + length); a negative [length] is everything after the
   offset. Called with the guard taken; raises with the guard released. */
static void read_input(resampler *record, value _input, intnat offset,
                       intnat length, input_samples *input) {
  int planes = plane_count(&record->input);
  size_t sample_size = plane_sample_size(&record->input);
  const uint8_t *data[MAX_CHANNELS];
  intnat available;
  value _samples;

  memset(input, 0, sizeof(*input));
  if (!Is_some(_input))
    return;
  _samples = Some_val(_input);

  if (Tag_val(_samples) == 1) {
    const AVFrame *frame = Frame_val(Field(_samples, 0));

    if (frame->format != record->input.sample_format ||
        frame->ch_layout.nb_channels != record->input.channels)
      fail(record, "the frame is not of the converter's format and channels");
    available = frame->nb_samples;
    for (int i = 0; i < planes; i++)
      data[i] = frame->extended_data[i];
  } else {
    value _planes = Field(_samples, 0);

    if (Wosize_val(_planes) != (mlsize_t)planes)
      fail(record, "the number of planes is not the one of the format");
    available = 0;
    for (int i = 0; i < planes; i++) {
      struct caml_ba_array *plane =
          Caml_ba_array_val(Field(Field(_planes, i), 0));
      intnat samples = sample_size ? caml_ba_byte_size(plane) / sample_size : 0;

      if (i > 0 && samples != available)
        fail(record, "the planes differ in length");
      available = samples;
      data[i] = plane->data;
    }
  }

  if (length < 0)
    length = available - offset;
  if (offset < 0 || length < 0 || offset > available ||
      length > available - offset || length > INT_MAX)
    fail(record, "the range lies outside the samples");

  input->count = (int)length;
  for (int i = 0; i < planes; i++)
    input->planes[i] = data[i] + offset * sample_size;
}

/* The most samples a conversion of [input] can produce, at least 1. */
static int output_bound(resampler *record, const input_samples *input) {
  int bound = swr_get_out_samples(record->context, input->count);

  return bound < 1 ? 1 : bound;
}

/* Converts, or flushes when the input is empty of planes. Called without
   the lock. */
static int run(resampler *record, uint8_t **output, int bound,
               const input_samples *input, int flush) {
  return swr_convert(record->context, output, bound,
                     flush ? NULL : input->planes, flush ? 0 : input->count);
}

static resampler *exclusive(value _resampler) {
  resampler *record = Resampler_val(_resampler);

  if (!ocaml_avutil_guard_try_exclusive(&record->guard))
    ocaml_avutil_raise_in_use();

  return record;
}

CAMLprim value ocaml_swresample_convert_to_frame(value _resampler, value _input,
                                                 value _offset, value _length) {
  CAMLparam2(_resampler, _input);
  resampler *record = exclusive(_resampler);
  input_samples input;
  AVFrame *frame;
  int bound;
  int produced = AVERROR(ENOMEM);

  read_input(record, _input, Long_val(_offset), Long_val(_length), &input);
  bound = output_bound(record, &input);

  frame = av_frame_alloc();
  if (frame) {
    frame->format = record->output.sample_format;
    frame->sample_rate = record->output.sample_rate;
    frame->nb_samples = bound;
    produced =
        av_channel_layout_copy(&frame->ch_layout, &record->output_layout);
    if (produced >= 0)
      produced = av_frame_get_buffer(frame, 0);
  }
  if (produced >= 0) {
    int flush = !Is_some(_input);

    caml_release_runtime_system();
    produced = run(record, frame->extended_data, bound, &input, flush);
    caml_acquire_runtime_system();
  }

  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (produced < 0) {
    av_frame_free(&frame);
    ocaml_avutil_raise_error(produced);
  }
  frame->nb_samples = produced;

  CAMLreturn(ocaml_avutil_wrap_frame(frame));
}

/* [_raw] asks for planes of bytes; otherwise the elements have the kind of
   the output sample format. */
CAMLprim value ocaml_swresample_convert_to_planes(value _resampler,
                                                  value _input, value _offset,
                                                  value _length, value _raw) {
  CAMLparam3(_resampler, _input, _raw);
  CAMLlocal2(_planes, _plane);
  resampler *record = Resampler_val(_resampler);
  int planes = plane_count(&record->output);
  size_t sample_size = plane_sample_size(&record->output);
  size_t element_size =
      Bool_val(_raw)
          ? 1
          : (size_t)av_get_bytes_per_sample(record->output.sample_format);
  int kind = Bool_val(_raw) ? CAML_BA_UINT8
                            : (int)ocaml_avutil_bigarray_kind_of_sample_format(
                                  record->output.sample_format);
  uint8_t *output[MAX_CHANNELS] = {0};
  input_samples input;
  int bound, produced, flush;

  record = exclusive(_resampler);
  read_input(record, _input, Long_val(_offset), Long_val(_length), &input);
  bound = output_bound(record, &input);
  ocaml_avutil_guard_release_exclusive(&record->guard);

  _planes = caml_alloc_tuple(planes);
  for (int i = 0; i < planes; i++) {
    _plane = caml_ba_alloc_dims(kind | CAML_BA_C_LAYOUT, 1, NULL,
                                (intnat)(bound * sample_size / element_size));
    Store_field(_planes, i, _plane);
  }
  for (int i = 0; i < planes; i++)
    output[i] = Caml_ba_data_val(Field(_planes, i));

  record = exclusive(_resampler);
  flush = !Is_some(_input);
  caml_release_runtime_system();
  produced = run(record, output, bound, &input, flush);
  caml_acquire_runtime_system();
  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (produced < 0)
    ocaml_avutil_raise_error(produced);

  for (int i = 0; i < planes; i++)
    Caml_ba_array_val(Field(_planes, i))->dim[0] =
        produced * sample_size / element_size;

  CAMLreturn(_planes);
}

CAMLprim value ocaml_swresample_data_of_bytes(value _bytes) {
  CAMLparam1(_bytes);
  CAMLlocal1(_data);
  size_t size = caml_string_length(_bytes);

  _data = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL,
                             (intnat)size);
  memcpy(Caml_ba_data_val(_data), Bytes_val(_bytes), size);

  CAMLreturn(_data);
}

CAMLprim value ocaml_swresample_bytes_of_data(value _data) {
  CAMLparam1(_data);
  CAMLreturn(caml_alloc_initialized_string(Caml_ba_array_val(_data)->dim[0],
                                           Caml_ba_data_val(_data)));
}
