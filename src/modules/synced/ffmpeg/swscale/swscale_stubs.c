/* Stubs of the swscale binding, spec/swscale.md.

   A scaler is a custom block pointing at a native record: the libswscale
   context, its guard, and the geometry it was created for, which every
   argument is checked against before libswscale reads or writes anything.

   Conversions go through sws_scale_frame, which is what runs on several
   threads. Planes held in bigarrays are presented to it as frames whose
   buffers are not owned. */

#include <limits.h>
#include <string.h>

#include "avutil_stubs.h"

#include <libavutil/imgutils.h>
#include <libavutil/mem.h>
#include <libavutil/pixdesc.h>
#include <libswscale/swscale.h>

#define PLANE_PADDING 16
#define SCALE_FRAME_ALIGN 32
#define MAX_BUFFERS 4

typedef struct {
  int width;
  int height;
  enum AVPixelFormat pixel_format;
} geometry;

typedef struct {
  struct SwsContext *context;
  ocaml_avutil_guard guard;
  geometry source;
  geometry destination;
} scaler;

#define Scaler_val(v) (*(scaler **)Data_custom_val(v))

static void finalize_scaler(value _scaler) {
  scaler *record = Scaler_val(_scaler);

  if (record) {
    sws_freeContext(record->context);
    av_free(record);
  }
}

static struct custom_operations scaler_operations = {
    "ocaml_swscale_scaler",     finalize_scaler,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value ocaml_swscale_version(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_version);
  unsigned version = swscale_version();

  _version = caml_alloc_tuple(3);
  Store_field(_version, 0, Val_int(AV_VERSION_MAJOR(version)));
  Store_field(_version, 1, Val_int(AV_VERSION_MINOR(version)));
  Store_field(_version, 2, Val_int(AV_VERSION_MICRO(version)));

  CAMLreturn(_version);
}

/* Swscale.flag, in the order of its constructors. */
static const int scaler_flags[] = {SWS_FAST_BILINEAR, SWS_BILINEAR, SWS_BICUBIC,
                                   SWS_PRINT_INFO};

/* [_geometry] is (width, height, pixel format). Raises. */
static geometry geometry_of_value(value _geometry) {
  geometry result;

  result.width = ocaml_avutil_int_of_value(Field(_geometry, 0), "width");
  result.height = ocaml_avutil_int_of_value(Field(_geometry, 1), "height");
  result.pixel_format = PixelFormat_val(Field(_geometry, 2));
  if (result.width < 1 || result.height < 1)
    ocaml_avutil_raise_failure("width or height below 1");

  return result;
}

static int set_geometry(struct SwsContext *context, const geometry *source,
                        const geometry *destination, int flags, int threads) {
  const struct {
    const char *name;
    int64_t setting;
  } settings[] = {
      {"srcw", source->width},
      {"srch", source->height},
      {"src_format", source->pixel_format},
      {"dstw", destination->width},
      {"dsth", destination->height},
      {"dst_format", destination->pixel_format},
      {"sws_flags", flags},
      {"threads", threads},
  };

  for (size_t i = 0; i < sizeof(settings) / sizeof(settings[0]); i++) {
    int error =
        av_opt_set_int(context, settings[i].name, settings[i].setting, 0);

    if (error < 0)
      return error;
  }

  return 0;
}

CAMLprim value ocaml_swscale_create(value _threads, value _flags, value _source,
                                    value _destination) {
  CAMLparam4(_threads, _flags, _source, _destination);
  CAMLlocal1(_scaler);
  geometry source = geometry_of_value(_source);
  geometry destination = geometry_of_value(_destination);
  int threads = ocaml_avutil_int_of_value(_threads, "threads");
  scaler *record;
  int flags = 0;
  int error;

  for (value _flag = _flags; _flag != Val_emptylist; _flag = Field(_flag, 1))
    flags |= scaler_flags[Int_val(Field(_flag, 0))];

  _scaler = caml_alloc_custom(&scaler_operations, sizeof(scaler *), 0, 1);
  Scaler_val(_scaler) = NULL;
  record = av_mallocz(sizeof(*record));
  if (!record)
    caml_raise_out_of_memory();
  Scaler_val(_scaler) = record;

  record->source = source;
  record->destination = destination;
  record->context = sws_alloc_context();
  if (!record->context)
    caml_raise_out_of_memory();

  error = set_geometry(record->context, &source, &destination, flags, threads);
  if (error >= 0) {
    caml_release_runtime_system();
    error = sws_init_context(record->context, NULL, NULL);
    caml_acquire_runtime_system();
  }
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(_scaler);
}

static scaler *exclusive(value _scaler) {
  scaler *record = Scaler_val(_scaler);

  if (!ocaml_avutil_guard_try_exclusive(&record->guard))
    ocaml_avutil_raise_in_use();

  return record;
}

CAMLnoret static void fail(scaler *record, const char *message) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  ocaml_avutil_raise_failure("%s", message);
}

/* The buffers an image of a format needs: its planes, and the palette of a
   paletted format. An image may hold more, which are ignored. */
static int buffer_count(enum AVPixelFormat pixel_format) {
  const AVPixFmtDescriptor *descriptor = av_pix_fmt_desc_get(pixel_format);
  int planes = av_pix_fmt_count_planes(pixel_format);

  if (!descriptor || planes < 0)
    return -1;

  return planes + ((descriptor->flags & AV_PIX_FMT_FLAG_PAL) ? 1 : 0);
}

typedef struct {
  uint8_t *data[MAX_BUFFERS];
  int linesize[MAX_BUFFERS];
  size_t size[MAX_BUFFERS];
  int count;
} image;

/* Reads a (data * int) array into [result] and checks it against an image
   of [rows] rows of [shape]: the buffers the format needs, line sizes wide
   enough, buffers long enough. Returns a message on a mismatch, null
   otherwise. Raises nothing. */
static const char *read_planes(value _planes, const geometry *shape, int rows,
                               image *result) {
  int minimum_linesizes[MAX_BUFFERS];
  ptrdiff_t linesizes[MAX_BUFFERS] = {0};
  size_t sizes[MAX_BUFFERS];
  int count = buffer_count(shape->pixel_format);

  memset(result, 0, sizeof(*result));
  if (count < 0 || Wosize_val(_planes) < (mlsize_t)count)
    return "the image holds fewer buffers than its pixel format needs";
  if (av_image_fill_linesizes(minimum_linesizes, shape->pixel_format,
                              shape->width) < 0)
    return "the pixel format has no line size";

  result->count = count;
  for (int i = 0; i < count; i++) {
    value _plane = Field(_planes, i);
    intnat linesize = Long_val(Field(_plane, 1));

    if (linesize < minimum_linesizes[i] || linesize > INT_MAX)
      return "a line size is too small for the width";
    result->data[i] = Caml_ba_data_val(Field(_plane, 0));
    result->size[i] = Caml_ba_array_val(Field(_plane, 0))->dim[0];
    result->linesize[i] = (int)linesize;
    linesizes[i] = linesize;
  }

  if (av_image_fill_plane_sizes(sizes, shape->pixel_format, rows, linesizes) <
      0)
    return "the pixel format has no plane size";
  for (int i = 0; i < count; i++) {
    if (result->size[i] < sizes[i])
      return "a plane is shorter than its line size and the height require";
  }

  return NULL;
}

CAMLprim value ocaml_swscale_scale(value _scaler, value _source, value _row,
                                   value _rows, value _destination,
                                   value _offset) {
  CAMLparam5(_scaler, _source, _row, _rows, _destination);
  CAMLxparam1(_offset);
  scaler *record = exclusive(_scaler);
  const AVPixFmtDescriptor *descriptor =
      av_pix_fmt_desc_get(record->destination.pixel_format);
  intnat row = Long_val(_row), rows = Long_val(_rows),
         offset = Long_val(_offset);
  const uint8_t *source_data[MAX_BUFFERS];
  uint8_t *destination_data[MAX_BUFFERS];
  geometry padded = record->destination;
  image source, destination;
  const char *mismatch;
  int error;

  if (row < 0 || rows < 0 || offset < 0 || row + rows > record->source.height ||
      offset > INT_MAX - record->destination.height)
    fail(record, "the slice lies outside the image");
  if (!descriptor || offset % (1 << descriptor->log2_chroma_h) != 0)
    fail(record, "the row offset splits a chroma row");

  mismatch = read_planes(_source, &record->source, (int)rows, &source);
  if (!mismatch) {
    padded.height += (int)offset;
    mismatch = read_planes(_destination, &padded, padded.height, &destination);
  }
  if (mismatch)
    fail(record, mismatch);

  for (int i = 0; i < MAX_BUFFERS; i++) {
    int chroma = i == 1 || i == 2;
    intnat plane_offset = chroma ? offset >> descriptor->log2_chroma_h : offset;

    source_data[i] = source.data[i];
    destination_data[i] = destination.data[i];
    if (destination.data[i] &&
        !(descriptor->flags & AV_PIX_FMT_FLAG_PAL && i == 1))
      destination_data[i] += plane_offset * destination.linesize[i];
  }

  caml_release_runtime_system();
  error = sws_scale(record->context, source_data, source.linesize, (int)row,
                    (int)rows, destination_data, destination.linesize);
  caml_acquire_runtime_system();

  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (error < 0)
    ocaml_avutil_raise_error(error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_swscale_scale_bytecode(value *arguments, int count) {
  (void)count;
  return ocaml_swscale_scale(arguments[0], arguments[1], arguments[2],
                             arguments[3], arguments[4], arguments[5]);
}

static void keep_buffer(void *opaque, uint8_t *data) {
  (void)opaque;
  (void)data;
}

/* A frame over buffers that belong to someone else. Returns null when
   memory is short. Needs no lock. */
static AVFrame *frame_over(const image *planes, const geometry *shape) {
  AVFrame *frame = av_frame_alloc();

  if (!frame)
    return NULL;

  frame->width = shape->width;
  frame->height = shape->height;
  frame->format = shape->pixel_format;

  for (int i = 0; i < planes->count; i++) {
    frame->data[i] = planes->data[i];
    frame->linesize[i] = planes->linesize[i];
    frame->buf[i] = av_buffer_create(planes->data[i], planes->size[i],
                                     keep_buffer, NULL, 0);
    if (!frame->buf[i]) {
      av_frame_free(&frame);
      return NULL;
    }
  }

  return frame;
}

/* The source frame of a conversion for a Swscale.image: the frame itself,
   or a frame over the planes, in which case [*owned] is set. Called with
   the guard taken; raises with the guard released. */
static AVFrame *source_frame(scaler *record, value _image, int *owned) {
  image planes;
  const char *mismatch;
  AVFrame *frame;

  *owned = Tag_val(_image) == 0;
  if (!*owned) {
    frame = Frame_val(Field(_image, 0));
    if (frame->width != record->source.width ||
        frame->height != record->source.height ||
        frame->format != record->source.pixel_format)
      fail(record, "the frame is not of the scaler's size and pixel format");
    return frame;
  }

  mismatch = read_planes(Field(_image, 0), &record->source,
                         record->source.height, &planes);
  if (mismatch)
    fail(record, mismatch);

  frame = frame_over(&planes, &record->source);
  if (!frame) {
    ocaml_avutil_guard_release_exclusive(&record->guard);
    caml_raise_out_of_memory();
  }

  return frame;
}

/* Runs the conversion and frees what it owns; ends the exclusive
   operation. Raises on failure, [destination] freed. */
static void convert(scaler *record, AVFrame *source, int source_owned,
                    AVFrame *destination) {
  int error = AVERROR(ENOMEM);

  if (destination) {
    caml_release_runtime_system();
    error = sws_scale_frame(record->context, destination, source);
    caml_acquire_runtime_system();
  }

  if (source_owned)
    av_frame_free(&source);
  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (error < 0) {
    av_frame_free(&destination);
    ocaml_avutil_raise_error(error);
  }
}

CAMLprim value ocaml_swscale_convert_to_frame(value _scaler, value _image) {
  CAMLparam2(_scaler, _image);
  scaler *record = exclusive(_scaler);
  int source_owned;
  AVFrame *source = source_frame(record, _image, &source_owned);
  AVFrame *destination = av_frame_alloc();

  if (destination) {
    destination->width = record->destination.width;
    destination->height = record->destination.height;
    destination->format = record->destination.pixel_format;
    if (av_frame_get_buffer(destination, SCALE_FRAME_ALIGN) < 0)
      av_frame_free(&destination);
  }
  convert(record, source, source_owned, destination);

  CAMLreturn(ocaml_avutil_wrap_frame(destination));
}

/* A runtime-owned plane of [size] visible bytes, with the padding
   libswscale may touch past a plane. */
static value alloc_plane(size_t size) {
  value _plane = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL,
                                    (intnat)(size + PLANE_PADDING));

  Caml_ba_array_val(_plane)->dim[0] = size;

  return _plane;
}

CAMLprim value ocaml_swscale_convert_to_planes(value _scaler, value _image) {
  CAMLparam2(_scaler, _image);
  CAMLlocal3(_planes, _plane, _data);
  scaler *record = Scaler_val(_scaler);
  const geometry *shape = &record->destination;
  int count = av_pix_fmt_count_planes(shape->pixel_format);
  int linesizes[MAX_BUFFERS];
  ptrdiff_t wide_linesizes[MAX_BUFFERS];
  size_t sizes[MAX_BUFFERS];
  image planes = {0};
  AVFrame *source, *destination;
  int source_owned;

  if (count < 0 ||
      av_image_fill_linesizes(linesizes, shape->pixel_format, shape->width) < 0)
    ocaml_avutil_raise_failure("the output pixel format has no planes");
  for (int i = 0; i < MAX_BUFFERS; i++)
    wide_linesizes[i] = linesizes[i];
  if (av_image_fill_plane_sizes(sizes, shape->pixel_format, shape->height,
                                wide_linesizes) < 0)
    ocaml_avutil_raise_failure("the output pixel format has no plane size");

  _planes = caml_alloc_tuple(count);
  for (int i = 0; i < count; i++) {
    _data = alloc_plane(sizes[i]);
    _plane = caml_alloc_tuple(2);
    Store_field(_plane, 0, _data);
    Store_field(_plane, 1, Val_int(linesizes[i]));
    Store_field(_planes, i, _plane);
  }

  planes.count = count;
  for (int i = 0; i < count; i++) {
    planes.data[i] = Caml_ba_data_val(Field(Field(_planes, i), 0));
    planes.linesize[i] = linesizes[i];
    planes.size[i] = sizes[i] + PLANE_PADDING;
  }

  record = exclusive(_scaler);
  source = source_frame(record, _image, &source_owned);
  destination = frame_over(&planes, shape);
  convert(record, source, source_owned, destination);
  av_frame_free(&destination);

  CAMLreturn(_planes);
}

CAMLprim value ocaml_swscale_plane_of_string(value _content) {
  CAMLparam1(_content);
  CAMLlocal1(_plane);
  size_t size = caml_string_length(_content);

  _plane = alloc_plane(size);
  memcpy(Caml_ba_data_val(_plane), String_val(_content), size);

  CAMLreturn(_plane);
}

CAMLprim value ocaml_swscale_string_of_plane(value _plane) {
  CAMLparam1(_plane);
  CAMLreturn(caml_alloc_initialized_string(Caml_ba_array_val(_plane)->dim[0],
                                           Caml_ba_data_val(_plane)));
}
