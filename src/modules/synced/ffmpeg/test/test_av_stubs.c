/* Doubles for the av conformance tests. */

#include "av_stubs.h"

#include <pthread.h>

typedef struct {
  AVFormatContext *context;
  int result;
} callback_call;

static void *call_read(void *argument) {
  callback_call *call = argument;
  uint8_t buffer[16];

  call->result = call->context->pb->read_packet(call->context->pb->opaque,
                                                buffer, sizeof(buffer));

  return NULL;
}

static void *call_interrupt(void *argument) {
  callback_call *call = argument;

  call->result = call->context->interrupt_callback.callback(
      call->context->interrupt_callback.opaque);

  return NULL;
}

/* Calls the read callback, or the interrupt callback, of a container from a
   thread created here, and returns what the callback answered. */
CAMLprim value test_call_from_thread(value _container, value _interrupt) {
  CAMLparam2(_container, _interrupt);
  callback_call call = {ocaml_av_format_context(_container), 0};
  void *(*function)(void *) = Bool_val(_interrupt) ? call_interrupt : call_read;
  pthread_t thread;

  caml_release_runtime_system();
  pthread_create(&thread, NULL, function, &call);
  pthread_join(thread, NULL);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(call.result));
}

#include <libavutil/imgutils.h>

/* The plane sizes FFmpeg computes for an image with the smallest line
   sizes its format allows. */
CAMLprim value test_image_plane_sizes(value _pixel_format, value _width,
                                      value _height) {
  CAMLparam3(_pixel_format, _width, _height);
  CAMLlocal1(_sizes);
  enum AVPixelFormat pixel_format = PixelFormat_val(_pixel_format);
  int planes = av_pix_fmt_count_planes(pixel_format);
  int linesizes[4];
  ptrdiff_t wide_linesizes[4];
  size_t sizes[4];

  av_image_fill_linesizes(linesizes, pixel_format, Int_val(_width));
  for (int i = 0; i < 4; i++)
    wide_linesizes[i] = linesizes[i];
  av_image_fill_plane_sizes(sizes, pixel_format, Int_val(_height),
                            wide_linesizes);

  _sizes = caml_alloc_tuple(planes);
  for (int i = 0; i < planes; i++)
    Store_field(_sizes, i, Val_long(sizes[i]));

  CAMLreturn(_sizes);
}
