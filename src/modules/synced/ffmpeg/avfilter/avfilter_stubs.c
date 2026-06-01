#include <string.h>

#define CAML_NAME_SPACE 1

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include "avutil_stubs.h"
#include "polymorphic_variant_values_stubs.h"
#include <libavfilter/avfilter.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/mem.h>

#define AvFilterContext_val(v) (*(AVFilterContext **)Data_abstract_val(v))

static inline value value_of_avfiltercontext(value ret,
                                             AVFilterContext *avfiltercontext) {
  ret = caml_alloc(1, Abstract_tag);
  AvFilterContext_val(ret) = avfiltercontext;
  return ret;
}

CAMLprim value ocaml_avfilter_register_all(value unit) {
  CAMLparam0();
#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(7, 14, 100)
  avfilter_register_all();
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_alloc_pads(const AVFilterPad *pads, int pad_count,
                                         const char *name) {
  CAMLparam0();
  CAMLlocal2(pad, _pads);

  int i, pad_type;

  _pads = caml_alloc_tuple(pad_count);

  for (i = 0; i < pad_count; i++) {
    pad = caml_alloc_tuple(6);
    Store_field(pad, 0, caml_copy_string(avfilter_pad_get_name(pads, i)));
    Store_field(pad, 1, caml_copy_string(name));

    switch (avfilter_pad_get_type(pads, i)) {
    case AVMEDIA_TYPE_VIDEO:
      pad_type = PVV_Video;
      break;
    case AVMEDIA_TYPE_AUDIO:
      pad_type = PVV_Audio;
      break;
    case AVMEDIA_TYPE_DATA:
      pad_type = PVV_Data;
      break;
    case AVMEDIA_TYPE_SUBTITLE:
      pad_type = PVV_Subtitle;
      break;
    case AVMEDIA_TYPE_ATTACHMENT:
      pad_type = PVV_Attachment;
      break;
    default:
      pad_type = PVV_Unknown;
    }

    Store_field(pad, 2, pad_type);
    Store_field(pad, 3, Val_int(i));
    Store_field(pad, 4, Val_none);
    Store_field(pad, 5, Val_none);

    Store_field(_pads, i, pad);
  }

  CAMLreturn(_pads);
}

CAMLprim value ocaml_avfilter_get_all_filters(value unit) {
  CAMLparam0();
  CAMLlocal5(pad, pads, cur, ret, tmp);
  int c = 0;
  const AVFilter *f = NULL;

#if LIBAVFILTER_VERSION_INT >= AV_VERSION_INT(7, 14, 100)
  void *opaque = 0;
#endif

#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(7, 14, 100)
  while ((f = avfilter_next(f)))
    c++;
#else
  while ((f = av_filter_iterate(&opaque)))
    c++;
#endif

  ret = caml_alloc_tuple(c);

  c = 0;
  f = NULL;

#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(7, 14, 100)
  while ((f = avfilter_next(f))) {
#else
  opaque = 0;
  while ((f = av_filter_iterate(&opaque))) {
#endif
    cur = caml_alloc_tuple(6);
    Store_field(cur, 0, caml_copy_string(f->name));
    Store_field(cur, 1, caml_copy_string(f->description));
    Store_field(cur, 2,
                ocaml_avfilter_alloc_pads(
#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(8, 3, 100)
                    f->inputs, avfilter_pad_count(f->inputs), f->name
#else
                    f->inputs, avfilter_filter_pad_count(f, 0), f->name
#endif
                    ));
    Store_field(cur, 3,
                ocaml_avfilter_alloc_pads(
#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(8, 3, 100)
                    f->outputs, avfilter_pad_count(f->outputs), f->name
#else
                    f->outputs, avfilter_filter_pad_count(f, 1), f->name
#endif
                    ));
    Store_field(cur, 4, value_of_avclass(&tmp, f->priv_class));
    Store_field(cur, 5, Val_int(f->flags));

    Store_field(ret, c, cur);
    c++;
  }

  CAMLreturn(ret);
}

#define Filter_graph_val(v) (*(AVFilterGraph **)Data_custom_val(v))

static void finalize_filter_graph(value v) {
  AVFilterGraph *graph = Filter_graph_val(v);
  avfilter_graph_free(&graph);
}

static struct custom_operations filter_graph_ops = {
    "ocaml_avfilter_filter_graph", finalize_filter_graph,
    custom_compare_default,        custom_hash_default,
    custom_serialize_default,      custom_deserialize_default};

CAMLprim value ocaml_avfilter_init(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  AVFilterGraph *graph = avfilter_graph_alloc();

  if (!graph)
    caml_raise_out_of_memory();

  ret = caml_alloc_custom(&filter_graph_ops, sizeof(AVFilterGraph *), 1, 0);

  Filter_graph_val(ret) = graph;

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_create_filter(value _args, value _instance_name,
                                            value _name, value _graph) {
  CAMLparam4(_instance_name, _args, _name, _graph);
  CAMLlocal2(ret, tmp);

  char *name = NULL;
  char *args = NULL;
  AVFilterGraph *graph = Filter_graph_val(_graph);
  const AVFilter *filter = avfilter_get_by_name(String_val(_name));
  AVFilterContext *context;
  int err;

  if (!filter)
    caml_raise_not_found();

  name = av_strndup(String_val(_instance_name),
                    caml_string_length(_instance_name));
  if (!name)
    caml_raise_out_of_memory();

  if (_args != Val_none) {
    args = av_strndup(String_val(Some_val(_args)),
                      caml_string_length(Some_val(_args)));

    if (!args) {
      if (name)
        av_free(name);
      caml_raise_out_of_memory();
    }
  }

  caml_release_runtime_system();
  err = avfilter_graph_create_filter(&context, filter, name, args, NULL, graph);
  caml_acquire_runtime_system();

  if (name)
    av_free(name);
  if (args)
    av_free(args);

  if (err < 0)
    ocaml_avutil_raise_error(err);

  ret = caml_alloc_tuple(3);
  Store_field(ret, 0, value_of_avfiltercontext(tmp, context));
  Store_field(ret, 1,
              ocaml_avfilter_alloc_pads(context->input_pads, context->nb_inputs,
                                        filter->name));
  Store_field(ret, 2,
              ocaml_avfilter_alloc_pads(context->output_pads,
                                        context->nb_outputs, filter->name));

  CAMLreturn(ret);
}

static void append_avfilter_in_out(AVFilterInOut **filter, char *name,
                                   AVFilterContext *filter_ctx, int pad_idx) {
  AVFilterInOut *cur = *filter;

  if (cur) {
    while (cur->next)
      cur = cur->next;
    cur->next = avfilter_inout_alloc();
    if (cur)
      cur = cur->next;
  } else {
    *filter = avfilter_inout_alloc();
    cur = *filter;
  }

  if (!cur) {
    avfilter_inout_free(filter);
    caml_raise_out_of_memory();
  }

  cur->name = name;
  cur->filter_ctx = filter_ctx;
  cur->pad_idx = pad_idx;
  cur->next = NULL;
};

CAMLprim value ocaml_avfilter_process_commands(value _flags, value _cmd,
                                               value _arg, value _filter) {
  CAMLparam4(_flags, _cmd, _arg, _filter);
  char buf[4096] = {0};
  char *cmd;
  char *arg;
  int err;
  AVFilterContext *filter_ctx = AvFilterContext_val(_filter);

  cmd = av_malloc(caml_string_length(_cmd) + 1);
  if (!cmd)
    caml_raise_out_of_memory();

  arg = av_malloc(caml_string_length(_arg) + 1);
  if (!arg) {
    av_free(cmd);
    caml_raise_out_of_memory();
  }

  memcpy(cmd, String_val(_cmd), caml_string_length(_cmd) + 1);
  memcpy(arg, String_val(_arg), caml_string_length(_arg) + 1);

  caml_release_runtime_system();
  err = avfilter_process_command(filter_ctx, cmd, arg, buf, sizeof(buf),
                                 Int_val(_flags));
  caml_acquire_runtime_system();

  av_free(cmd);
  av_free(arg);

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(caml_copy_string(buf));
}

CAMLprim value ocaml_avfilter_parse(value _inputs, value _outputs,
                                    value _filters, value _graph) {
  CAMLparam4(_inputs, _outputs, _filters, _graph);
  CAMLlocal1(_pad);

  int c, err, idx;
  AVFilterInOut *inputs = NULL;
  AVFilterInOut *outputs = NULL;
  AVFilterGraph *graph = Filter_graph_val(_graph);
  AVFilterContext *filter_ctx;
  char *filters, *name;

  for (c = 0; c < Wosize_val(_inputs); c++) {
    _pad = Field(_inputs, c);
    name = av_strdup(String_val(Field(_pad, 0)));
    filter_ctx = AvFilterContext_val(Field(_pad, 1));
    idx = Int_val(Field(_pad, 2));

    append_avfilter_in_out(&inputs, name, filter_ctx, idx);
  }

  for (c = 0; c < Wosize_val(_outputs); c++) {
    _pad = Field(_outputs, c);
    name = av_strdup(String_val(Field(_pad, 0)));
    filter_ctx = AvFilterContext_val(Field(_pad, 1));
    idx = Int_val(Field(_pad, 2));

    append_avfilter_in_out(&outputs, name, filter_ctx, idx);
  }

  filters = av_strndup(String_val(_filters), caml_string_length(_filters));

  if (!filters) {
    if (inputs)
      avfilter_inout_free(&inputs);
    if (outputs)
      avfilter_inout_free(&outputs);
    caml_raise_out_of_memory();
  }

  caml_release_runtime_system();
  err = avfilter_graph_parse_ptr(graph, filters, &inputs, &outputs, NULL);
  caml_acquire_runtime_system();

  av_free(filters);

  if (inputs)
    avfilter_inout_free(&inputs);

  if (outputs)
    avfilter_inout_free(&outputs);

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_append_context(value _filter, value _ctx) {
  CAMLparam2(_filter, _ctx);
  CAMLlocal1(ret);
  int n = Wosize_val(_filter);
  int i;

  ret = caml_alloc_tuple(n + 1);

  for (i = 0; i < n; i++) {
    Store_field(ret, i, Field(_filter, i));
  }

  Store_field(ret, n, _ctx);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_get_content(value _filter) {
  CAMLparam1(_filter);
  int n = Wosize_val(_filter);
  CAMLreturn(Field(_filter, n - 1));
}

CAMLprim value ocaml_avfilter_link(value _src, value _srcpad, value _dst,
                                   value _dstpad) {
  CAMLparam4(_src, _srcpad, _dst, _dstpad);
  AVFilterContext *src = AvFilterContext_val(_src);
  AVFilterContext *dst = AvFilterContext_val(_dst);

  caml_release_runtime_system();
  int err = avfilter_link(src, Int_val(_srcpad), dst, Int_val(_dstpad));
  caml_acquire_runtime_system();

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_buffersink_get_time_base(value _src) {
  CAMLparam1(_src);
  CAMLlocal1(ret);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  AVRational time_base = av_buffersink_get_time_base(filter_ctx);

  value_of_rational(&time_base, &ret);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_buffersink_get_frame_rate(value _src) {
  CAMLparam1(_src);
  CAMLlocal1(ret);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  AVRational frame_rate = av_buffersink_get_frame_rate(filter_ctx);

  value_of_rational(&frame_rate, &ret);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_buffersink_get_sample_format(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int sample_format = av_buffersink_get_format(filter_ctx);

  CAMLreturn(Val_SampleFormat((enum AVSampleFormat)sample_format));
}

CAMLprim value ocaml_avfilter_buffersink_get_w(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int w = av_buffersink_get_w(filter_ctx);

  CAMLreturn(Val_int(w));
}

CAMLprim value ocaml_avfilter_buffersink_get_h(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int h = av_buffersink_get_h(filter_ctx);

  CAMLreturn(Val_int(h));
}

CAMLprim value ocaml_avfilter_buffersink_get_pixel_format(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int pixel_format = av_buffersink_get_format(filter_ctx);

  CAMLreturn(Val_PixelFormat((enum AVPixelFormat)pixel_format));
}

CAMLprim value ocaml_avfilter_buffersink_get_pixel_aspect(value _src) {
  CAMLparam1(_src);
  CAMLlocal2(ans, ret);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  AVRational pixel_aspect = av_buffersink_get_sample_aspect_ratio(filter_ctx);

  if (pixel_aspect.num == 0)
    CAMLreturn(Val_none);

  value_of_rational(&pixel_aspect, &ans);

  ret = caml_alloc_tuple(1);
  Store_field(ret, 0, ans);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_buffersink_get_channels(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int channels = av_buffersink_get_channels(filter_ctx);

  CAMLreturn(Val_int(channels));
}

CAMLprim value ocaml_avfilter_buffersink_get_channel_layout(value _src) {
  CAMLparam1(_src);
  CAMLlocal1(ret);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);
  AVChannelLayout channel_layout;
  int err;

  err = av_buffersink_get_ch_layout(filter_ctx, &channel_layout);

  if (err < 0)
    ocaml_avutil_raise_error(err);

  value_of_channel_layout(&ret, &channel_layout);

  CAMLreturn(ret);
}

CAMLprim value ocaml_avfilter_buffersink_get_sample_rate(value _src) {
  CAMLparam1(_src);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  int sample_rate = av_buffersink_get_sample_rate(filter_ctx);

  CAMLreturn(Val_int(sample_rate));
}

CAMLprim value ocaml_avfilter_buffersink_set_frame_size(value _src,
                                                        value _size) {
  CAMLparam2(_src, _size);
  AVFilterContext *filter_ctx = AvFilterContext_val(_src);

  av_buffersink_set_frame_size(filter_ctx, Int_val(_size));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_config(value _graph) {
  CAMLparam1(_graph);

  caml_release_runtime_system();
  int err = avfilter_graph_config(Filter_graph_val(_graph), NULL);
  caml_acquire_runtime_system();

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_write_frame(value _config, value _filter,
                                          value _frame) {
  CAMLparam3(_config, _filter, _frame);
  AVFilterContext *filter_ctx = AvFilterContext_val(_filter);

  caml_release_runtime_system();
  int err = av_buffersrc_write_frame(filter_ctx, Frame_val(_frame));
  caml_acquire_runtime_system();

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_write_eof_frame(value _config, value _filter) {
  CAMLparam2(_config, _filter);
  AVFilterContext *filter_ctx = AvFilterContext_val(_filter);

  caml_release_runtime_system();
  int err = av_buffersrc_write_frame(filter_ctx, NULL);
  caml_acquire_runtime_system();

  if (err < 0)
    ocaml_avutil_raise_error(err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_get_frame(value _config, value _filter) {
  CAMLparam2(_config, _filter);
  CAMLlocal1(frame_value);
  AVFilterContext *filter_ctx = AvFilterContext_val(_filter);

  AVFrame *frame = av_frame_alloc();

  if (!frame) {
    caml_raise_out_of_memory();
  }

  caml_release_runtime_system();
  int err = av_buffersink_get_frame(filter_ctx, frame);
  caml_acquire_runtime_system();

  if (err < 0) {
    av_frame_free(&frame);
    ocaml_avutil_raise_error(err);
  }

  value_of_frame(&frame_value, frame);

  CAMLreturn(frame_value);
}

CAMLprim value ocaml_avfilter_int_of_flag(value _flag) {
  CAMLparam1(_flag);

  switch (_flag) {
  case PVV_Dynamic_inputs:
    CAMLreturn(Val_int(AVFILTER_FLAG_DYNAMIC_INPUTS));
  case PVV_Dynamic_outputs:
    CAMLreturn(Val_int(AVFILTER_FLAG_DYNAMIC_OUTPUTS));
  case PVV_Slice_threads:
    CAMLreturn(Val_int(AVFILTER_FLAG_SLICE_THREADS));
  case PVV_Support_timeline_generic:
    CAMLreturn(Val_int(AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC));
  case PVV_Support_timeline_internal:
    CAMLreturn(Val_int(AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL));
  default:
    caml_failwith("Invalid flag type!");
  }
}

CAMLprim value ocaml_avfilter_get_array_separator(value _filter_name,
                                                  value _option_name) {
  CAMLparam2(_filter_name, _option_name);
  const char *filter_name = String_val(_filter_name);
  const char *option_name = String_val(_option_name);

  const AVFilter *filter = avfilter_get_by_name(filter_name);

  if (!filter || !filter->priv_class)
    caml_failwith("Invalid filter!");

  // av_opt_find expects a fake object which is a double pointer to AVClass
  const AVClass *class_ptr = filter->priv_class;
  const struct AVOption *option =
      av_opt_find(&class_ptr, option_name, NULL, 0, 0);

#ifdef HAVE_AV_OPT_TYPE_FLAG_ARRAY
  if (!option || !(option->type & AV_OPT_TYPE_FLAG_ARRAY))
    caml_failwith("Invalid filter option!");

  if (option->default_val.arr && option->default_val.arr->sep)
    CAMLreturn(Val_int(option->default_val.arr->sep));
#else
  (void)option;
#endif
  caml_failwith("Invalid filter!");
}
