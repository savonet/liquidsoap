#define CAML_NAME_SPACE 1

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <libavdevice/avdevice.h>

#include "av_stubs.h"
#include "avutil_stubs.h"

CAMLprim value ocaml_avdevice_init(value unit) {
  CAMLparam0();
  avdevice_register_all();
  CAMLreturn(Val_unit);
}

static value get_input_devices(avioformat_const AVInputFormat *(
    *input_device_next)(avioformat_const AVInputFormat *)) {
  CAMLparam0();
  CAMLlocal2(v, ans);
  avioformat_const AVInputFormat *fmt = NULL;
  int len = 0;

  while ((fmt = input_device_next(fmt)))
    len++;

  ans = caml_alloc_tuple(len);

  int i = 0;
  fmt = NULL;

  while ((fmt = input_device_next(fmt))) {

    value_of_inputFormat(&v, fmt);
    Store_field(ans, i, v);
    i++;
  }

  CAMLreturn(ans);
}

CAMLprim value ocaml_avdevice_get_audio_input_formats(value unit) {
  CAMLparam0();
  CAMLreturn(get_input_devices(av_input_audio_device_next));
}

CAMLprim value ocaml_avdevice_get_video_input_formats(value unit) {
  CAMLparam0();
  CAMLreturn(get_input_devices(av_input_video_device_next));
}

static value get_output_devices(avioformat_const AVOutputFormat *(
    *output_device_next)(avioformat_const AVOutputFormat *)) {
  CAMLparam0();
  CAMLlocal2(ans, tmp);
  avioformat_const AVOutputFormat *fmt = NULL;
  int len = 0;

  while ((fmt = output_device_next(fmt)))
    len++;

  ans = caml_alloc_tuple(len);

  int i = 0;
  fmt = NULL;

  while ((fmt = output_device_next(fmt))) {
    value_of_outputFormat(&tmp, fmt);
    Store_field(ans, i, tmp);
    i++;
  }

  CAMLreturn(ans);
}

CAMLprim value ocaml_avdevice_get_audio_output_formats(value unit) {
  CAMLparam0();
  CAMLreturn(get_output_devices(av_output_audio_device_next));
}

CAMLprim value ocaml_avdevice_get_video_output_formats(value unit) {
  CAMLparam0();
  CAMLreturn(get_output_devices(av_output_video_device_next));
}

static const enum AVAppToDevMessageType APP_TO_DEV_MESSAGE_TYPES[] = {
    AV_APP_TO_DEV_NONE,        AV_APP_TO_DEV_PAUSE,
    AV_APP_TO_DEV_PLAY,        AV_APP_TO_DEV_TOGGLE_PAUSE,
    AV_APP_TO_DEV_MUTE,        AV_APP_TO_DEV_UNMUTE,
    AV_APP_TO_DEV_TOGGLE_MUTE, AV_APP_TO_DEV_GET_VOLUME,
    AV_APP_TO_DEV_GET_MUTE};

static const enum AVAppToDevMessageType APP_TO_DEV_MESSAGE_WITH_DATA_TYPES[] = {
    AV_APP_TO_DEV_WINDOW_SIZE, AV_APP_TO_DEV_WINDOW_REPAINT,
    AV_APP_TO_DEV_SET_VOLUME};

CAMLprim value ocaml_avdevice_app_to_dev_control_message(value _message,
                                                         value _av) {
  CAMLparam2(_message, _av);
  enum AVAppToDevMessageType message_type;
  void *data = NULL;
  size_t data_size = 0;
  double dbl;
  AVDeviceRect rect;

  if (Is_block(_message)) {
    message_type = APP_TO_DEV_MESSAGE_WITH_DATA_TYPES[Tag_val(_message)];

    if (message_type == AV_APP_TO_DEV_SET_VOLUME) {
      dbl = Double_val(Field(_message, 0));
      data = &dbl;
      data_size = sizeof(dbl);
    } else {
      rect.x = Int_val(Field(_message, 0));
      rect.y = Int_val(Field(_message, 1));
      rect.width = Int_val(Field(_message, 2));
      rect.height = Int_val(Field(_message, 3));

      if (message_type == AV_APP_TO_DEV_WINDOW_SIZE || rect.width > 0) {
        data = &rect;
        data_size = sizeof(rect);
      }
    }
  } else {
    message_type = APP_TO_DEV_MESSAGE_TYPES[Int_val(_message)];
  }

  caml_release_runtime_system();
  AVFormatContext *format_context = ocaml_av_get_format_context(&_av);
  int ret = avdevice_app_to_dev_control_message(format_context, message_type,
                                                data, data_size);
  caml_acquire_runtime_system();

  if (ret < 0)
    ocaml_avutil_raise_error(ret);

  CAMLreturn(Val_unit);
}

#define NONE_TAG 0
#define CREATE_WINDOW_BUFFER_TAG 0
#define PREPARE_WINDOW_BUFFER_TAG 1
#define DISPLAY_WINDOW_BUFFER_TAG 2
#define DESTROY_WINDOW_BUFFER_TAG 3
#define BUFFER_OVERFLOW_TAG 4
#define BUFFER_UNDERFLOW_TAG 5
#define BUFFER_READABLE_TAG 1
#define BUFFER_WRITABLE_TAG 2
#define MUTE_STATE_CHANGED_TAG 3
#define VOLUME_LEVEL_CHANGED_TAG 4

static int ocaml_control_message_callback(struct AVFormatContext *ctx, int type,
                                          void *data, size_t data_size) {
  CAMLparam0();
  CAMLlocal3(msg, opt, res);
  enum AVDevToAppMessageType message_type = (enum AVDevToAppMessageType)type;
  int ret = 0;

  if (message_type == AV_DEV_TO_APP_NONE) {
    msg = Val_int(NONE_TAG);
  } else if (message_type == AV_DEV_TO_APP_CREATE_WINDOW_BUFFER) {
    if (data) {
      AVDeviceRect *rect = (AVDeviceRect *)data;
      opt = caml_alloc_tuple(4);
      Store_field(opt, 0, Val_int(rect->x));
      Store_field(opt, 1, Val_int(rect->y));
      Store_field(opt, 2, Val_int(rect->width));
      Store_field(opt, 3, Val_int(rect->height));
    } else {
      opt = Val_int(0);
    }

    msg = caml_alloc(1, CREATE_WINDOW_BUFFER_TAG);
    Store_field(msg, 0, opt);
  } else if (message_type == AV_DEV_TO_APP_PREPARE_WINDOW_BUFFER) {
    msg = Val_int(PREPARE_WINDOW_BUFFER_TAG);
  } else if (message_type == AV_DEV_TO_APP_DISPLAY_WINDOW_BUFFER) {
    msg = Val_int(DISPLAY_WINDOW_BUFFER_TAG);
  } else if (message_type == AV_DEV_TO_APP_DESTROY_WINDOW_BUFFER) {
    msg = Val_int(DESTROY_WINDOW_BUFFER_TAG);
  } else if (message_type == AV_DEV_TO_APP_BUFFER_OVERFLOW) {
    msg = Val_int(BUFFER_OVERFLOW_TAG);
  } else if (message_type == AV_DEV_TO_APP_BUFFER_UNDERFLOW) {
    msg = Val_int(BUFFER_UNDERFLOW_TAG);
  } else if (message_type == AV_DEV_TO_APP_BUFFER_READABLE ||
             message_type == AV_DEV_TO_APP_BUFFER_WRITABLE) {

    if (data) {
      opt = caml_alloc_tuple(1);
      Store_field(opt, 0, caml_copy_int64(*((int64_t *)data)));
    } else {
      opt = Val_int(0);
    }
    msg = caml_alloc(1, message_type == AV_DEV_TO_APP_BUFFER_READABLE
                            ? BUFFER_READABLE_TAG
                            : BUFFER_WRITABLE_TAG);
    Store_field(msg, 0, opt);
  } else if (message_type == AV_DEV_TO_APP_MUTE_STATE_CHANGED) {
    msg = caml_alloc(1, MUTE_STATE_CHANGED_TAG);
    Store_field(msg, 0, (*((int *)data)) ? Val_true : Val_false);
  } else if (message_type == AV_DEV_TO_APP_VOLUME_LEVEL_CHANGED) {
    msg = caml_alloc(1, VOLUME_LEVEL_CHANGED_TAG);
    Store_field(msg, 0, caml_copy_double(*((double *)data)));
  }
  res = caml_callback_exn(*ocaml_av_get_control_message_callback(ctx), msg);

  if (Is_exception_result(res)) {
    res = Extract_exception(res);
    ret = AVERROR_UNKNOWN;
  }
  CAMLreturn(ret);
}

static int c_control_message_callback(struct AVFormatContext *ctx, int type,
                                      void *data, size_t data_size) {
  ocaml_ffmpeg_register_thread();

  caml_acquire_runtime_system();
  int ret = ocaml_control_message_callback(ctx, type, data, data_size);
  caml_release_runtime_system();

  return ret;
}

CAMLprim value ocaml_avdevice_set_control_message_callback(
    value _control_message_callback, value _av) {
  CAMLparam2(_control_message_callback, _av);

  caml_release_runtime_system();
  ocaml_av_set_control_message_callback(&_av, c_control_message_callback,
                                        &_control_message_callback);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}
