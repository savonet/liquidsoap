/* Stubs of the avfilter binding, spec/avfilter.md.

   A graph value is a custom block pointing at a native record: the filter
   graph, its guard and its state. A filter instance is a pointer into the
   graph, valid while the graph is not failed: every operation that takes
   one checks the state first, under the guard.

   Avfilter builds its registry, its pads and its endpoints from the plain
   records these stubs return. */

#include <string.h>

#include "avutil_stubs.h"

#include <libavfilter/avfilter.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavutil/mem.h>

#define COMMAND_RESPONSE_MAX 4096
#define TABLE_LENGTH(table) (sizeof(table) / sizeof((table)[0]))

/* The states of spec/avfilter.md §2.1. */
enum { GRAPH_CONFIGURING, GRAPH_RUNNING, GRAPH_FAILED };

/* Avfilter.role: the four endpoint filters, in this order. */
static const char *const endpoint_names[] = {"abuffer", "buffer", "abuffersink",
                                             "buffersink"};

typedef struct {
  AVFilterGraph *graph;
  ocaml_avutil_guard guard;
  atomic_int state;
} filter_graph;

#define Graph_val(v) (*(filter_graph **)Data_custom_val(v))
#define Instance_val(v) (*(AVFilterContext **)Data_abstract_val(v))

static void finalize_graph(value _graph) {
  filter_graph *record = Graph_val(_graph);

  if (record) {
    avfilter_graph_free(&record->graph);
    av_free(record);
  }
}

static struct custom_operations graph_operations = {
    "ocaml_avfilter_graph",     finalize_graph,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static const ocaml_ffmpeg_variant_entry filter_flag_entries[] = {
    {PVV_Dynamic_inputs, AVFILTER_FLAG_DYNAMIC_INPUTS},
    {PVV_Dynamic_outputs, AVFILTER_FLAG_DYNAMIC_OUTPUTS},
    {PVV_Slice_threads, AVFILTER_FLAG_SLICE_THREADS},
    {PVV_Support_timeline_generic, AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC},
    {PVV_Support_timeline_internal, AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL},
};
static const ocaml_ffmpeg_variant_table filter_flag_table = {
    filter_flag_entries, TABLE_LENGTH(filter_flag_entries), "Avfilter.flag"};

static const ocaml_ffmpeg_variant_entry command_flag_entries[] = {
    {PVV_Fast, AVFILTER_CMD_FLAG_FAST},
};
static const ocaml_ffmpeg_variant_table command_flag_table = {
    command_flag_entries, TABLE_LENGTH(command_flag_entries),
    "Avfilter.command_flag"};

/* The pads of one direction as (name, kind) pairs, kind being 0 for audio,
   1 for video and 2 for anything else. */
static value raw_pads(const AVFilterPad *pads, unsigned count) {
  CAMLparam0();
  CAMLlocal2(_pads, _pad);

  _pads = caml_alloc_tuple(count);
  for (unsigned i = 0; i < count; i++) {
    const char *name = avfilter_pad_get_name(pads, i);
    enum AVMediaType type = avfilter_pad_get_type(pads, i);

    _pad = caml_alloc_tuple(2);
    Store_field(_pad, 0, caml_copy_string(name ? name : ""));
    Store_field(_pad, 1,
                Val_int(type == AVMEDIA_TYPE_AUDIO   ? 0
                        : type == AVMEDIA_TYPE_VIDEO ? 1
                                                     : 2));
    Store_field(_pads, i, _pad);
  }

  CAMLreturn(_pads);
}

/* Every registered filter as (name, description, option class, flags,
   input pads, output pads). */
CAMLprim value ocaml_avfilter_registry(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal2(_filters, _filter);
  const AVFilter *filter;
  void *iterator = NULL;
  mlsize_t count = 0;

  while (av_filter_iterate(&iterator))
    count++;

  _filters = caml_alloc_tuple(count);
  iterator = NULL;

  for (mlsize_t i = 0; i < count; i++) {
    filter = av_filter_iterate(&iterator);
    _filter = caml_alloc_tuple(6);
    Store_field(_filter, 0, caml_copy_string(filter->name ? filter->name : ""));
    Store_field(
        _filter, 1,
        caml_copy_string(filter->description ? filter->description : ""));
    Store_field(_filter, 2, ocaml_avutil_wrap_option_class(filter->priv_class));
    Store_field(_filter, 3,
                ocaml_avutil_flags_of_mask(&filter_flag_table, filter->flags));
    Store_field(_filter, 4,
                raw_pads(filter->inputs, avfilter_filter_pad_count(filter, 0)));
    Store_field(
        _filter, 5,
        raw_pads(filter->outputs, avfilter_filter_pad_count(filter, 1)));
    Store_field(_filters, i, _filter);
  }

  CAMLreturn(_filters);
}

CAMLprim value ocaml_avfilter_init(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_graph);
  filter_graph *record;

  _graph = caml_alloc_custom(&graph_operations, sizeof(filter_graph *), 0, 1);
  Graph_val(_graph) = NULL;
  record = av_mallocz(sizeof(*record));
  if (!record)
    caml_raise_out_of_memory();
  Graph_val(_graph) = record;

  record->graph = avfilter_graph_alloc();
  if (!record->graph)
    caml_raise_out_of_memory();

  CAMLreturn(_graph);
}

/* The record of a graph that is not failed, its guard taken exclusively or
   shared. They raise the in-use and the failed errors, the guard released. */
static filter_graph *exclusive(value _graph) {
  filter_graph *record = Graph_val(_graph);

  if (!ocaml_avutil_guard_try_exclusive(&record->guard))
    ocaml_avutil_raise_in_use();
  if (atomic_load(&record->state) == GRAPH_FAILED) {
    ocaml_avutil_guard_release_exclusive(&record->guard);
    ocaml_avutil_raise_failed();
  }

  return record;
}

static filter_graph *shared(value _graph) {
  filter_graph *record = Graph_val(_graph);

  if (!ocaml_avutil_guard_try_shared(&record->guard))
    ocaml_avutil_raise_in_use();
  if (atomic_load(&record->state) == GRAPH_FAILED) {
    ocaml_avutil_guard_release_shared(&record->guard);
    ocaml_avutil_raise_failed();
  }

  return record;
}

static void done(filter_graph *record, int error) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  if (error < 0)
    ocaml_avutil_raise_error(error);
}

CAMLnoret static void fail(filter_graph *record, const char *message) {
  ocaml_avutil_guard_release_exclusive(&record->guard);
  ocaml_avutil_raise_failure("%s", message);
}

/* A graph that is still configured, its guard taken exclusively. */
static filter_graph *configuring(value _graph) {
  filter_graph *record = exclusive(_graph);

  if (atomic_load(&record->state) == GRAPH_RUNNING)
    fail(record, "the graph is launched");

  return record;
}

static value wrap_instance(AVFilterContext *instance) {
  value _instance = caml_alloc(1, Abstract_tag);

  Instance_val(_instance) = instance;

  return _instance;
}

/* Returns (instance, input pads, output pads). An empty [_arguments] is no
   argument. */
CAMLprim value ocaml_avfilter_attach(value _graph, value _filter_name,
                                     value _instance_name, value _arguments) {
  CAMLparam4(_graph, _filter_name, _instance_name, _arguments);
  CAMLlocal1(_result);
  const AVFilter *filter = avfilter_get_by_name(String_val(_filter_name));
  char *instance_name = av_strdup(String_val(_instance_name));
  char *arguments = caml_string_length(_arguments) > 0
                        ? av_strdup(String_val(_arguments))
                        : NULL;
  AVFilterContext *instance = NULL;
  filter_graph *record;
  int error = 0;

  if (!instance_name || (caml_string_length(_arguments) > 0 && !arguments)) {
    av_free(instance_name);
    av_free(arguments);
    caml_raise_out_of_memory();
  }

  if (!ocaml_avutil_guard_try_exclusive(&Graph_val(_graph)->guard)) {
    av_free(instance_name);
    av_free(arguments);
    ocaml_avutil_raise_in_use();
  }
  record = Graph_val(_graph);

  if (atomic_load(&record->state) != GRAPH_CONFIGURING) {
    int failed = atomic_load(&record->state) == GRAPH_FAILED;

    av_free(instance_name);
    av_free(arguments);
    ocaml_avutil_guard_release_exclusive(&record->guard);
    if (failed)
      ocaml_avutil_raise_failed();
    ocaml_avutil_raise_failure("the graph is launched");
  }

  if (avfilter_graph_get_filter(record->graph, instance_name)) {
    av_free(instance_name);
    av_free(arguments);
    ocaml_avutil_guard_release_exclusive(&record->guard);
    caml_raise_constant(*caml_named_value("ocaml_avfilter_exists"));
  }

  if (!filter) {
    error = AVERROR_FILTER_NOT_FOUND;
  } else {
    caml_release_runtime_system();
    error = avfilter_graph_create_filter(&instance, filter, instance_name,
                                         arguments, NULL, record->graph);
    caml_acquire_runtime_system();
  }
  av_free(instance_name);
  av_free(arguments);
  done(record, error);

  _result = caml_alloc_tuple(3);
  Store_field(_result, 0, wrap_instance(instance));
  Store_field(_result, 1, raw_pads(instance->input_pads, instance->nb_inputs));
  Store_field(_result, 2,
              raw_pads(instance->output_pads, instance->nb_outputs));

  CAMLreturn(_result);
}

CAMLprim value ocaml_avfilter_link(value _graph, value _source,
                                   value _source_pad, value _destination,
                                   value _destination_pad) {
  CAMLparam5(_graph, _source, _source_pad, _destination, _destination_pad);
  filter_graph *record = configuring(_graph);
  int error =
      avfilter_link(Instance_val(_source), Int_val(_source_pad),
                    Instance_val(_destination), Int_val(_destination_pad));

  done(record, error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_process_command(value _graph, value _instance,
                                              value _command, value _argument,
                                              value _flags) {
  CAMLparam5(_graph, _instance, _command, _argument, _flags);
  CAMLlocal1(_response);
  int flags = (int)ocaml_avutil_mask_of_flags(&command_flag_table, _flags);
  filter_graph *record = exclusive(_graph);
  char *command = av_strdup(String_val(_command));
  char *argument = av_strdup(String_val(_argument));
  char *response = av_mallocz(COMMAND_RESPONSE_MAX);
  int error = AVERROR(ENOMEM);

  if (command && argument && response) {
    caml_release_runtime_system();
    error = avfilter_process_command(Instance_val(_instance), command, argument,
                                     response, COMMAND_RESPONSE_MAX, flags);
    caml_acquire_runtime_system();
  }
  av_free(command);
  av_free(argument);
  ocaml_avutil_guard_release_exclusive(&record->guard);

  if (error < 0) {
    av_free(response);
    ocaml_avutil_raise_error(error);
  }
  _response = caml_copy_string(response);
  av_free(response);

  CAMLreturn(_response);
}

/* A list of open ends for FFmpeg's parser from an array of (label,
   instance, pad index). Returns 0 when memory is short, the list freed. */
static int endpoint_list(value _nodes, AVFilterInOut **list) {
  AVFilterInOut **next = list;

  *list = NULL;
  for (mlsize_t i = 0; i < Wosize_val(_nodes); i++) {
    value _node = Field(_nodes, i);
    AVFilterInOut *node = avfilter_inout_alloc();

    if (node)
      node->name = av_strdup(String_val(Field(_node, 0)));
    if (!node || !node->name) {
      avfilter_inout_free(&node);
      avfilter_inout_free(list);
      return 0;
    }
    node->filter_ctx = Instance_val(Field(_node, 1));
    node->pad_idx = Int_val(Field(_node, 2));
    *next = node;
    next = &node->next;
  }

  return 1;
}

/* FFmpeg frees every filter of a graph whose parsing failed: the graph is
   failed from then on. */
CAMLprim value ocaml_avfilter_parse(value _graph, value _inputs, value _outputs,
                                    value _description) {
  CAMLparam4(_graph, _inputs, _outputs, _description);
  filter_graph *record = configuring(_graph);
  char *description = av_strdup(String_val(_description));
  AVFilterInOut *inputs = NULL, *outputs = NULL;
  int error = AVERROR(ENOMEM);

  if (description && endpoint_list(_inputs, &inputs) &&
      endpoint_list(_outputs, &outputs)) {
    caml_release_runtime_system();
    error = avfilter_graph_parse_ptr(record->graph, description, &inputs,
                                     &outputs, NULL);
    caml_acquire_runtime_system();
    if (error < 0)
      atomic_store(&record->state, GRAPH_FAILED);
  }

  avfilter_inout_free(&inputs);
  avfilter_inout_free(&outputs);
  av_free(description);
  done(record, error);

  CAMLreturn(Val_unit);
}

static int endpoint_role(const AVFilterContext *instance) {
  for (int role = 0; role < 4; role++) {
    if (!strcmp(instance->filter->name, endpoint_names[role]))
      return role;
  }

  return -1;
}

/* Configures the graph and returns its endpoints as (instance name, role,
   instance), in the order the instances were created. */
CAMLprim value ocaml_avfilter_launch(value _graph) {
  CAMLparam1(_graph);
  CAMLlocal2(_endpoints, _endpoint);
  filter_graph *record = configuring(_graph);
  mlsize_t count = 0, index = 0;
  int error;

  caml_release_runtime_system();
  error = avfilter_graph_config(record->graph, NULL);
  caml_acquire_runtime_system();
  atomic_store(&record->state, error < 0 ? GRAPH_FAILED : GRAPH_RUNNING);
  done(record, error);

  for (unsigned i = 0; i < record->graph->nb_filters; i++)
    count += endpoint_role(record->graph->filters[i]) >= 0;

  _endpoints = caml_alloc_tuple(count);
  for (unsigned i = 0; i < record->graph->nb_filters; i++) {
    AVFilterContext *instance = record->graph->filters[i];
    int role = endpoint_role(instance);

    if (role < 0)
      continue;
    _endpoint = caml_alloc_tuple(3);
    Store_field(_endpoint, 0,
                caml_copy_string(instance->name ? instance->name : ""));
    Store_field(_endpoint, 1, Val_int(role));
    Store_field(_endpoint, 2, wrap_instance(instance));
    Store_field(_endpoints, index++, _endpoint);
  }

  CAMLreturn(_endpoints);
}

/* [_frame] is a frame option: None marks the end of the stream. The source
   takes a reference of its own. */
CAMLprim value ocaml_avfilter_push(value _graph, value _source, value _frame) {
  CAMLparam3(_graph, _source, _frame);
  filter_graph *record = exclusive(_graph);
  AVFrame *frame = Is_some(_frame) ? Frame_val(Some_val(_frame)) : NULL;
  AVFilterContext *source = Instance_val(_source);
  int error;

  caml_release_runtime_system();
  error =
      av_buffersrc_add_frame_flags(source, frame, AV_BUFFERSRC_FLAG_KEEP_REF);
  caml_acquire_runtime_system();
  done(record, error);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_avfilter_pull(value _graph, value _sink) {
  CAMLparam2(_graph, _sink);
  filter_graph *record = exclusive(_graph);
  AVFilterContext *sink = Instance_val(_sink);
  AVFrame *frame = av_frame_alloc();
  int error = AVERROR(ENOMEM);

  if (frame) {
    caml_release_runtime_system();
    error = av_buffersink_get_frame(sink, frame);
    caml_acquire_runtime_system();
  }
  if (error < 0)
    av_frame_free(&frame);
  done(record, error);

  CAMLreturn(ocaml_avutil_wrap_frame(frame));
}

CAMLprim value ocaml_avfilter_set_frame_size(value _graph, value _sink,
                                             value _size) {
  CAMLparam3(_graph, _sink, _size);
  int size = ocaml_avutil_int_of_value(_size, "frame size");
  filter_graph *record;

  if (size < 1)
    ocaml_avutil_raise_failure("frame size below 1");

  record = exclusive(_graph);
  av_buffersink_set_frame_size(Instance_val(_sink), size);
  done(record, 0);

  CAMLreturn(Val_unit);
}

#define SINK_INTEGER(name, read)                                               \
  CAMLprim value ocaml_avfilter_sink_##name(value _graph, value _sink) {       \
    filter_graph *record = shared(_graph);                                     \
    int result = read(Instance_val(_sink));                                    \
                                                                               \
    ocaml_avutil_guard_release_shared(&record->guard);                         \
                                                                               \
    return Val_int(result);                                                    \
  }

#define SINK_RATIONAL(name, read)                                              \
  CAMLprim value ocaml_avfilter_sink_##name(value _graph, value _sink) {       \
    filter_graph *record = shared(_graph);                                     \
    AVRational result = read(Instance_val(_sink));                             \
                                                                               \
    ocaml_avutil_guard_release_shared(&record->guard);                         \
                                                                               \
    return ocaml_avutil_value_of_rational(result);                             \
  }

SINK_INTEGER(width, av_buffersink_get_w)
SINK_INTEGER(height, av_buffersink_get_h)
SINK_INTEGER(channels, av_buffersink_get_channels)
SINK_INTEGER(sample_rate, av_buffersink_get_sample_rate)
SINK_INTEGER(format, av_buffersink_get_format)
SINK_RATIONAL(time_base, av_buffersink_get_time_base)
SINK_RATIONAL(frame_rate, av_buffersink_get_frame_rate)
SINK_RATIONAL(sample_aspect_ratio, av_buffersink_get_sample_aspect_ratio)

CAMLprim value ocaml_avfilter_pixel_format_of_id(value _id) {
  return Val_PixelFormat(Int_val(_id));
}

CAMLprim value ocaml_avfilter_sample_format_of_id(value _id) {
  return Val_SampleFormat(Int_val(_id));
}

CAMLprim value ocaml_avfilter_sink_channel_layout(value _graph, value _sink) {
  CAMLparam2(_graph, _sink);
  CAMLlocal1(_layout);
  AVChannelLayout layout = {0};
  filter_graph *record = shared(_graph);
  int error = av_buffersink_get_ch_layout(Instance_val(_sink), &layout);

  ocaml_avutil_guard_release_shared(&record->guard);
  if (error < 0) {
    av_channel_layout_uninit(&layout);
    ocaml_avutil_raise_error(error);
  }

  /* ponytail: a custom layout leaks its map if this copy runs out of
     memory; copy under a handle allocated first if that ever matters. */
  _layout = ocaml_avutil_copy_channel_layout(&layout);
  av_channel_layout_uninit(&layout);

  CAMLreturn(_layout);
}

/* The separator of an array option of a filter, 0 when the option declares
   none. Raises a failure when there is no such array option. */
CAMLprim value ocaml_avfilter_array_separator(value _filter_name,
                                              value _option_name) {
  const AVFilter *filter = avfilter_get_by_name(String_val(_filter_name));
  const AVClass *option_class = filter ? filter->priv_class : NULL;
  const AVOption *option = NULL;

  if (option_class)
    option = av_opt_find(&option_class, String_val(_option_name), NULL, 0,
                         AV_OPT_SEARCH_FAKE_OBJ);
  if (!option || !(option->type & AV_OPT_TYPE_FLAG_ARRAY))
    ocaml_avutil_raise_failure("no such array option");

  return Val_int(option->default_val.arr ? option->default_val.arr->sep : 0);
}
