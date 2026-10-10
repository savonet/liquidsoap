/* Services of the avutil binding for the stubs of dependent libraries.

   Every function needs the OCaml runtime lock unless its comment says
   otherwise. "Raises" means the function may leave through an OCaml
   exception: the caller frees what it owns before the call. */

#ifndef OCAML_FFMPEG_AVUTIL_STUBS_H
#define OCAML_FFMPEG_AVUTIL_STUBS_H

#define CAML_NAME_SPACE 1

#include <stdatomic.h>
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <libavutil/avutil.h>
#include <libavutil/channel_layout.h>
#include <libavutil/dict.h>
#include <libavutil/frame.h>
#include <libavutil/hwcontext.h>
#include <libavutil/opt.h>
#include <libavutil/pixfmt.h>
#include <libavutil/rational.h>
#include <libavutil/samplefmt.h>

#include "polymorphic_variant_values_stubs.h"

#ifdef OCAML_FFMPEG_GC_STRESS
/* Collects and compacts: every allocation, callback and lock release of the
   stubs goes through it in the collection-at-every-allocation build. */
void ocaml_avutil_gc_stress(void);

#define OCAML_FFMPEG_STRESSED(call) (ocaml_avutil_gc_stress(), call)
#define caml_alloc(...) OCAML_FFMPEG_STRESSED(caml_alloc(__VA_ARGS__))
#define caml_alloc_tuple(...)                                                  \
  OCAML_FFMPEG_STRESSED(caml_alloc_tuple(__VA_ARGS__))
#define caml_alloc_small(...)                                                  \
  OCAML_FFMPEG_STRESSED(caml_alloc_small(__VA_ARGS__))
#define caml_alloc_some(...) OCAML_FFMPEG_STRESSED(caml_alloc_some(__VA_ARGS__))
#define caml_alloc_string(...)                                                 \
  OCAML_FFMPEG_STRESSED(caml_alloc_string(__VA_ARGS__))
#define caml_alloc_initialized_string(...)                                     \
  OCAML_FFMPEG_STRESSED(caml_alloc_initialized_string(__VA_ARGS__))
#define caml_alloc_float_array(...)                                            \
  OCAML_FFMPEG_STRESSED(caml_alloc_float_array(__VA_ARGS__))
#define caml_alloc_custom(...)                                                 \
  OCAML_FFMPEG_STRESSED(caml_alloc_custom(__VA_ARGS__))
#define caml_alloc_custom_mem(...)                                             \
  OCAML_FFMPEG_STRESSED(caml_alloc_custom_mem(__VA_ARGS__))
#define caml_copy_string(...)                                                  \
  OCAML_FFMPEG_STRESSED(caml_copy_string(__VA_ARGS__))
#define caml_copy_int32(...) OCAML_FFMPEG_STRESSED(caml_copy_int32(__VA_ARGS__))
#define caml_copy_int64(...) OCAML_FFMPEG_STRESSED(caml_copy_int64(__VA_ARGS__))
#define caml_copy_double(...)                                                  \
  OCAML_FFMPEG_STRESSED(caml_copy_double(__VA_ARGS__))
#define caml_ba_alloc(...) OCAML_FFMPEG_STRESSED(caml_ba_alloc(__VA_ARGS__))
#define caml_ba_alloc_dims(...)                                                \
  OCAML_FFMPEG_STRESSED(caml_ba_alloc_dims(__VA_ARGS__))
#define caml_callback_exn(...)                                                 \
  OCAML_FFMPEG_STRESSED(caml_callback_exn(__VA_ARGS__))
#define caml_callback2_exn(...)                                                \
  OCAML_FFMPEG_STRESSED(caml_callback2_exn(__VA_ARGS__))
#define caml_callback3_exn(...)                                                \
  OCAML_FFMPEG_STRESSED(caml_callback3_exn(__VA_ARGS__))
#define caml_enter_blocking_section()                                          \
  OCAML_FFMPEG_STRESSED(caml_enter_blocking_section())
#endif

/* Raises Avutil.Error with the constructor of a negative FFmpeg code. */
CAMLnoret void ocaml_avutil_raise_error(int error_code);

/* Raises Avutil.Error (`Failure message). The message is formatted into
   storage of the call. */
CAMLnoret void ocaml_avutil_raise_failure(const char *format, ...);

/* The state errors of spec/binding-contract.md §5.3. */
CAMLnoret void ocaml_avutil_raise_closed(void);
CAMLnoret void ocaml_avutil_raise_failed(void);
CAMLnoret void ocaml_avutil_raise_in_use(void);

/* The use guard of a stateful handle, spec/binding-contract.md §6.2: 0 when
   free, -1 when taken exclusively, the number of holders when shared. The
   functions below need no lock, raise nothing and never wait; a try returns
   0 when the guard is taken in a conflicting way. */
typedef atomic_int ocaml_avutil_guard;

static inline int ocaml_avutil_guard_try_exclusive(ocaml_avutil_guard *guard) {
  int free_guard = 0;

  return atomic_compare_exchange_strong(guard, &free_guard, -1);
}

static inline void
ocaml_avutil_guard_release_exclusive(ocaml_avutil_guard *guard) {
  atomic_store(guard, 0);
}

static inline int ocaml_avutil_guard_try_shared(ocaml_avutil_guard *guard) {
  int holders = atomic_load(guard);

  while (holders >= 0) {
    if (atomic_compare_exchange_weak(guard, &holders, holders + 1))
      return 1;
  }

  return 0;
}

static inline void
ocaml_avutil_guard_release_shared(ocaml_avutil_guard *guard) {
  atomic_fetch_sub(guard, 1);
}

/* The C int of an OCaml integer. Raises a failure naming [name] when the
   integer does not fit. */
int ocaml_avutil_int_of_value(value _number, const char *name);

/* Stores the first constructor of the table whose C constant is [constant].
   Returns 0 when the table has none. Allocates nothing, raises nothing. */
int ocaml_avutil_find_variant(const ocaml_ffmpeg_variant_table *table,
                              int64_t constant, value *_variant);

/* As ocaml_avutil_find_variant; raises a failure naming the table and the
   value when the table has no entry. */
value ocaml_avutil_variant_of_constant(const ocaml_ffmpeg_variant_table *table,
                                       int64_t constant);

/* Raises a failure when [_variant] is no constructor of the table. */
int64_t
ocaml_avutil_constant_of_variant(const ocaml_ffmpeg_variant_table *table,
                                 value _variant);

/* The flags of the table set in [mask], as a list in ascending bit order. */
value ocaml_avutil_flags_of_mask(const ocaml_ffmpeg_variant_table *table,
                                 int64_t mask);

/* The bitwise OR of a list of flags. Raises as
   ocaml_avutil_constant_of_variant. */
int64_t ocaml_avutil_mask_of_flags(const ocaml_ffmpeg_variant_table *table,
                                   value _flags);

/* The generated tables. These functions need no lock and raise nothing. */
const ocaml_ffmpeg_variant_table *ocaml_avutil_pixel_format_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_sample_format_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_color_space_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_color_range_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_color_primaries_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_color_trc_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_chroma_location_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_hw_device_type_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avutil_media_type_table(void);

#define OCAML_AVUTIL_ENUM_VAL(name, type, _variant)                            \
  ((type)ocaml_avutil_constant_of_variant(ocaml_avutil_##name##_table(),       \
                                          (_variant)))
#define OCAML_AVUTIL_VAL_ENUM(name, constant)                                  \
  ocaml_avutil_variant_of_constant(ocaml_avutil_##name##_table(),              \
                                   (int64_t)(constant))

#define PixelFormat_val(v)                                                     \
  OCAML_AVUTIL_ENUM_VAL(pixel_format, enum AVPixelFormat, v)
#define Val_PixelFormat(c) OCAML_AVUTIL_VAL_ENUM(pixel_format, c)
#define SampleFormat_val(v)                                                    \
  OCAML_AVUTIL_ENUM_VAL(sample_format, enum AVSampleFormat, v)
#define Val_SampleFormat(c) OCAML_AVUTIL_VAL_ENUM(sample_format, c)
#define ColorSpace_val(v)                                                      \
  OCAML_AVUTIL_ENUM_VAL(color_space, enum AVColorSpace, v)
#define Val_ColorSpace(c) OCAML_AVUTIL_VAL_ENUM(color_space, c)
#define ColorRange_val(v)                                                      \
  OCAML_AVUTIL_ENUM_VAL(color_range, enum AVColorRange, v)
#define Val_ColorRange(c) OCAML_AVUTIL_VAL_ENUM(color_range, c)
#define ColorPrimaries_val(v)                                                  \
  OCAML_AVUTIL_ENUM_VAL(color_primaries, enum AVColorPrimaries, v)
#define Val_ColorPrimaries(c) OCAML_AVUTIL_VAL_ENUM(color_primaries, c)
#define ColorTrc_val(v)                                                        \
  OCAML_AVUTIL_ENUM_VAL(color_trc, enum AVColorTransferCharacteristic, v)
#define Val_ColorTrc(c) OCAML_AVUTIL_VAL_ENUM(color_trc, c)
#define ChromaLocation_val(v)                                                  \
  OCAML_AVUTIL_ENUM_VAL(chroma_location, enum AVChromaLocation, v)
#define Val_ChromaLocation(c) OCAML_AVUTIL_VAL_ENUM(chroma_location, c)
#define HwDeviceType_val(v)                                                    \
  OCAML_AVUTIL_ENUM_VAL(hw_device_type, enum AVHWDeviceType, v)
#define Val_MediaType(c) OCAML_AVUTIL_VAL_ENUM(media_type, c)

/* The bigarray element kind of a sample format. Raises a failure for a
   format that has none. */
enum caml_ba_kind
ocaml_avutil_bigarray_kind_of_sample_format(enum AVSampleFormat sample_format);

/* Raises a failure when a member does not fit a C int. */
AVRational ocaml_avutil_rational_of_value(value _rational);

value ocaml_avutil_value_of_rational(AVRational rational);

/* Units per second of a Time_format.t. Raises nothing. */
int64_t ocaml_avutil_time_format_units(value _time_format);

/* The native layout a Channel_layout.t owns. Its address is stable. */
#define ChannelLayout_val(v) (*(AVChannelLayout **)Data_custom_val(v))

/* A Channel_layout.t owning a copy of [source], which must be native
   memory. Raises. */
value ocaml_avutil_copy_channel_layout(const AVChannelLayout *source);

#define Frame_val(v) (*(AVFrame **)Data_custom_val(v))

/* A frame value that takes ownership of [frame]. Raises a failure on a null
   pointer. */
value ocaml_avutil_wrap_frame(AVFrame *frame);

/* Adds the entries of a Frame_side_data.raw list to a native side-data
   array, each replacing the entry of its kind unless the kind allows several.
   Allocates nothing on the OCaml heap, raises nothing, returns FFmpeg's code.
 */
int ocaml_avutil_add_side_data(AVFrameSideData ***side_data, int *count,
                               value _entries);

struct AVSubtitle;

/* The AVSubtitle of a subtitle value; libavcodec declares the structure. */
#define Subtitle_val(v) (*(struct AVSubtitle **)Data_custom_val(v))

/* A subtitle value that takes ownership of [subtitle], allocated with
   av_malloc, and of everything it holds. Raises a failure on a null
   pointer. */
value ocaml_avutil_wrap_subtitle(struct AVSubtitle *subtitle);

/* The buffer reference of a hardware device or frame context value. */
#define HwContext_val(v) (*(AVBufferRef **)Data_custom_val(v))

/* An Options.t for a class; [option_class] may be null. */
value ocaml_avutil_wrap_option_class(const AVClass *option_class);

/* How an Options.obj reaches the native object inside its owner: [acquire]
   returns it with the owner's guard taken shared and raises the state
   errors, [release] undoes it, raises nothing and allocates nothing. */
typedef struct {
  void *(*acquire)(value _owner);
  void (*release)(value _owner);
} ocaml_avutil_option_access;

/* An Options.obj that keeps [_owner] alive. [access] must be static. */
value ocaml_avutil_option_object(value _owner,
                                 const ocaml_avutil_option_access *access);

/* spec/avutil.md §9.1 step 2: a dictionary holding the bindings of a
   (string * value) array, a later binding of a key replacing an earlier
   one. Allocates nothing on the OCaml heap; on failure frees the dictionary
   and raises. */
AVDictionary *ocaml_avutil_dictionary_of_options(value _bindings);

/* spec/avutil.md §9.1 step 4: the keys left in the dictionary as a string
   array. Frees the dictionary and nulls the pointer. */
value ocaml_avutil_unused_options(AVDictionary **dictionary);

/* A dictionary holding a (string * string) list, a later pair replacing an
   earlier one of the same key; null for the empty list. On failure frees
   the dictionary and raises. */
AVDictionary *ocaml_avutil_dictionary_of_pairs(value _pairs);

/* The entries of a dictionary, which may be null, as a (string * string)
   list in the dictionary's order. */
value ocaml_avutil_pairs_of_dictionary(const AVDictionary *dictionary);

/* Registers the calling thread with the OCaml runtime when it is not
   registered yet; a thread it registered is unregistered when it exits.
   Called without the lock; the lock is not held on return. */
void ocaml_avutil_register_thread(void);

#endif
