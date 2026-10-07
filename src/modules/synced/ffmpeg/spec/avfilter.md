# avfilter — as-built specification (Part A)

Mechanism notes are in [language-notes/avfilter.md](language-notes/avfilter.md).
Observations and judgement are in [findings/avfilter.md](findings/avfilter.md).

## 1. Scope

Binds `libavfilter`: the filter registry, filter graphs, buffer sources
(`libavfilter/buffersrc.h`) and buffer sinks (`libavfilter/buffersink.h`).
It also uses `av_opt_find` from `libavutil` for one function.

Sibling dependency: the `avutil` binding library only. From it the library
uses: the `Avutil.Error` exception and its raising helper (FFmpeg error code
to `Avutil.error`), the rational conversion, the channel-layout wrapper
(which deep-copies the layout into a garbage-collected value), the frame
wrapper (garbage-collected value that owns an `AVFrame`), the
`Avutil.Options.t` wrapper around a `const AVClass *`, the sample-format and
pixel-format conversions, and the polymorphic-variant hash table generated
for `avutil`.

No minimum version is stated in the library. The code uses
`AVChannelLayout` and `av_buffersink_get_ch_layout` unconditionally, which
sets the practical floor. Two older API generations are still selected by
version tests (section 10). Whether the library is built at all is decided
by the detection step.

The library installs no C header and exports nothing to sibling stubs.

Module initialisation calls `avfilter_register_all()` on libavfilter older
than 7.14.100 and does nothing on later versions. It then enumerates all
filters (section 4.3); if any of `abuffer`, `buffer`, `abuffersink`,
`buffersink` is absent, module initialisation raises
`Failure "ffmpeg API error: missing buffer or sink!"`.

## 2. Objects

### 2.1 Filter graph (`config`)

- C object: one `AVFilterGraph`, from `avfilter_graph_alloc()`.
- OCaml side: `config` is a record that holds the graph handle plus mutable
  bookkeeping:
  - `names`: instance names attached so far through `attach`;
  - four association lists `(instance name, filter context)`: audio inputs
    (`abuffer` instances), video inputs (`buffer`), audio outputs
    (`abuffersink`), video outputs (`buffersink`). New entries are prepended.
- Creation: `init ()`. Raises `Out_of_memory` when the allocation fails.
- Ownership: the graph handle owns the `AVFilterGraph`. The graph owns every
  `AVFilterContext` created in it.
- Release: garbage collection only. The finaliser calls
  `avfilter_graph_free`, which frees all filter contexts of the graph. There
  is no explicit close.
- State: the graph has two phases that the types do not separate.
  1. _Configuration_: `attach`, `link`, `parse` add filters and links.
     Each call acts on the C graph immediately; nothing is recorded for
     later replay except the bookkeeping lists above.
  2. _Running_: after `launch` (which calls `avfilter_graph_config`), frames
     are pushed and pulled through the closures it returns.
     The same `config` value stays valid for all operations in both phases;
     no call is refused on the OCaml side because of the phase.

### 2.2 Filter context (internal; public as `'a context`)

- C object: an `AVFilterContext *` borrowed from the graph.
- OCaml side: an opaque handle that holds the raw pointer. It has no
  finaliser and holds no reference to the graph.
- Public appearances:
  - `'a context` (the type of `output.context`) _is_ this handle; the
    parameter is phantom (`` `Audio `` or `` `Video ``);
  - every attached pad carries `Some handle` and `Some graph handle`;
  - an ``[`Attached] filter`` carries the handle in a hidden trailing field
    that the record type does not declare (section 2.4).
- Lifetime: the pointer is valid while the graph is alive and no `parse`
  on it has failed (a failed `avfilter_graph_parse_ptr` frees every filter
  context of the graph). What keeps the graph alive:
  - the `config` record;
  - any attached pad (it stores the graph handle);
  - any input function and any output `handler` returned by `launch` (each
    closure captures the graph handle and passes it to the stub as an
    otherwise unused first argument, so the graph stays reachable for the
    duration of the call);
  - a `Utils.audio_converter` (it stores such closures).
    A bare `'a context` value, and the hidden field of an attached filter,
    do not keep the graph alive by themselves.

### 2.3 Pad (`('a, 'b, 'c) pad`)

An immutable OCaml record, abstract in the interface, with six fields:

| Field          | Content                                                               |
| -------------- | --------------------------------------------------------------------- |
| pad name       | `avfilter_pad_get_name(pads, i)`, copied                              |
| filter name    | the _filter's_ name (`AVFilter.name`), copied — not the instance name |
| media type     | polymorphic variant from `avfilter_pad_get_type` (section 3)          |
| index          | position `i` in the C pad array it was read from                      |
| filter context | `None` for an unattached pad, `Some handle` once attached             |
| graph          | `None` for an unattached pad, `Some graph handle` once attached       |

Type parameters: `'a` attached/unattached, `'b` media type
(``[`Audio]`` / ``[`Video]``), `'c` direction (``[`Input]`` /
``[`Output]``). All three are phantom except that `'b` is also the type of
the media-type field.

Pads are plain data; they own nothing. An attached pad keeps its graph
alive.

### 2.4 Filter (`'a filter`)

A public record: `name`, `description`, `options`, `flags`, `io`.

- `name`, `description`: copies of `AVFilter.name` and
  `AVFilter.description`.
- `options`: an `Avutil.Options.t` that wraps `AVFilter.priv_class` (the
  pointer is stored as is, including when it is `NULL`). It points into
  FFmpeg's static data.
- `flags`: decoded from `AVFilter.flags` (section 3).
- `io`: `{ inputs; outputs }`, each `{ audio; video }`, each a pad list
  sorted by increasing pad index.

``[`Unattached] filter`` values come from the registry and describe the
filter's static pads. ``[`Attached] filter`` values come from `attach`:
same `name`, `description`, `options`, `flags` as the unattached filter,
`io` rebuilt from the instance's actual pads, and one extra hidden trailing
field that holds the filter context. `process_command` reads the _last_
field of the value it is given to find the context.

### 2.5 Graph endpoints (`t`, `'a input`, `'a output`)

`launch` returns `t = { inputs; outputs }`, each split `{ audio; video }`,
each an association list keyed by instance name.

- `'a input` is a function ``[`Frame of 'a frame | `Flush] -> unit`` bound
  to one buffer source.
- `'a output` is `{ context; handler }`: the sink's filter context and a
  function `unit -> 'a frame` bound to that sink.

### 2.6 `Utils.audio_converter`

A record of the sink time base (read once), the input function and the
output handler of a private graph. It keeps that graph alive.

## 3. Enumerations and constants

### 3.1 Pad media type (C to OCaml only)

| `avfilter_pad_get_type`   | Variant stored in the pad |
| ------------------------- | ------------------------- |
| `AVMEDIA_TYPE_VIDEO`      | `` `Video ``              |
| `AVMEDIA_TYPE_AUDIO`      | `` `Audio ``              |
| `AVMEDIA_TYPE_DATA`       | `` `Data ``               |
| `AVMEDIA_TYPE_SUBTITLE`   | `` `Subtitle ``           |
| `AVMEDIA_TYPE_ATTACHMENT` | `` `Attachment ``         |
| anything else             | `` `Unknown ``            |

The variant hashes come from the table generated for `avutil`. When pads are
split into `{ audio; video }`, a pad whose type is `` `Audio `` goes to
`audio`; every other pad goes to `video` and its media-type field is
overwritten with `` `Video ``.

### 3.2 Filter flags (OCaml to C, used to decode)

| `flag`                           | C constant                                |
| -------------------------------- | ----------------------------------------- |
| `` `Dynamic_inputs ``            | `AVFILTER_FLAG_DYNAMIC_INPUTS`            |
| `` `Dynamic_outputs ``           | `AVFILTER_FLAG_DYNAMIC_OUTPUTS`           |
| `` `Slice_threads ``             | `AVFILTER_FLAG_SLICE_THREADS`             |
| `` `Support_timeline_generic ``  | `AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC`  |
| `` `Support_timeline_internal `` | `AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL` |

Hand-written. A filter's `flags` list contains, in the table's order, each
flag whose constant has a non-zero bitwise AND with `AVFilter.flags`. C
flags outside the table are dropped. The conversion raises
`Failure "Invalid flag type!"` on any other variant (not reachable through
the typed API).

### 3.3 Command flags

| `command_flag` | Value                                                                  |
| -------------- | ---------------------------------------------------------------------- |
| `` `Fast ``    | `2` (`AVFILTER_CMD_FLAG_FAST`), written as a literal on the OCaml side |

Flags in a list are OR-ed; the empty list is `0`.

### 3.4 Formats

`pixel_format` and `sample_format` convert with `avutil`'s generated
pixel-format and sample-format tables (C to OCaml); behaviour on a missing
mapping is that of those conversions.

## 4. Operations

### 4.1 Types

```ocaml
type config
type ground_arg =
  [ `String of string | `Int of int | `Int64 of int64
  | `Float of float | `Rational of rational ]
type valued_arg = [ ground_arg | `Array of ground_arg list ]
type args = [ `Flag of string | `Pair of string * valued_arg ]
type ('a, 'b) av = { audio : 'a; video : 'b }
type ('a, 'b) io = { inputs : 'a; outputs : 'b }
type ('a, 'b, 'c) pad
type ('a, 'b) pads =
  (('a, [ `Audio ], 'b) pad list, ('a, [ `Video ], 'b) pad list) av
type flag =
  [ `Dynamic_inputs | `Dynamic_outputs | `Slice_threads
  | `Support_timeline_generic | `Support_timeline_internal ]
type 'a filter = {
  name : string;
  description : string;
  options : Avutil.Options.t;
  flags : flag list;
  io : (('a, [ `Input ]) pads, ('a, [ `Output ]) pads) io;
}
type 'a input = [ `Frame of 'a frame | `Flush ] -> unit
type 'a context
type 'a output = { context : 'a context; handler : unit -> 'a frame }
type 'a entries = (string * 'a) list
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) av
type outputs = ([ `Audio ] output entries, [ `Video ] output entries) av
type t = (inputs, outputs) io
```

`frame` and `rational` are `Avutil`'s.

### 4.2 Output context accessors

All take the filter context of a buffer sink (the only way to obtain an
`'a context` is `output.context` from `launch`, so the graph is configured).
None releases the runtime lock. None takes the graph; the caller must keep
the graph alive (section 2.2).

| Signature                                                              | C call                                                       | Result                                                                                 |
| ---------------------------------------------------------------------- | ------------------------------------------------------------ | -------------------------------------------------------------------------------------- |
| `val time_base : _ context -> Avutil.rational`                         | `av_buffersink_get_time_base`                                | `{num; den}`                                                                           |
| ``val frame_rate : [ `Video ] context -> Avutil.rational``             | `av_buffersink_get_frame_rate`                               | `{num; den}`                                                                           |
| ``val width : [ `Video ] context -> int``                              | `av_buffersink_get_w`                                        | int                                                                                    |
| ``val height : [ `Video ] context -> int``                             | `av_buffersink_get_h`                                        | int                                                                                    |
| ``val pixel_aspect : [ `Video ] context -> Avutil.rational option``    | `av_buffersink_get_sample_aspect_ratio`                      | `None` when the numerator is `0`, else `Some {num; den}`                               |
| ``val pixel_format : [ `Video ] context -> Avutil.Pixel_format.t``     | `av_buffersink_get_format`                                   | cast to `enum AVPixelFormat`, converted                                                |
| ``val channels : [ `Audio ] context -> int``                           | `av_buffersink_get_channels`                                 | int                                                                                    |
| ``val channel_layout : [ `Audio ] context -> Avutil.Channel_layout.t`` | `av_buffersink_get_ch_layout` into a local `AVChannelLayout` | a fresh layout value (deep copy of the local); a negative return raises `Avutil.Error` |
| ``val sample_rate : [ `Audio ] context -> int``                        | `av_buffersink_get_sample_rate`                              | int                                                                                    |
| ``val sample_format : [ `Audio ] context -> Avutil.Sample_format.t``   | `av_buffersink_get_format`                                   | cast to `enum AVSampleFormat`, converted                                               |
| ``val set_frame_size : [ `Audio ] context -> int -> unit``             | `av_buffersink_set_frame_size(ctx, n)`                       | unit                                                                                   |

### 4.3 Array separator

```ocaml
val get_array_separator : filter_name:string -> option_name:string -> char
```

1. `avfilter_get_by_name(filter_name)`. If the filter is not found or its
   `priv_class` is `NULL`: `Failure "Invalid filter!"`.
2. `av_opt_find(&priv_class, option_name, NULL, 0, 0)` — the object passed
   is a pointer to the class pointer.
3. Where array-typed options exist (libavutil >= 59.1.100):
   - option not found, or its type lacks `AV_OPT_TYPE_FLAG_ARRAY`:
     `Failure "Invalid filter option!"`;
   - if `default_val.arr` is non-`NULL` and its `sep` is non-zero: return
     `sep` as a `char`;
   - otherwise: `Failure "Invalid filter!"`.
4. On older libavutil: always `Failure "Invalid filter!"` (after step 1).

### 4.4 Registry

```ocaml
exception Exists
val filters : [ `Unattached ] filter list
val find : string -> [ `Unattached ] filter
val find_opt : string -> [ `Unattached ] filter option
val abuffer : [ `Unattached ] filter
val buffer : [ `Unattached ] filter
val abuffersink : [ `Unattached ] filter
val buffersink : [ `Unattached ] filter
```

Computed once at module initialisation:

1. Iterate all registered filters twice (first to count, then to fill):
   `av_filter_iterate` (or `avfilter_next` on old versions, section 10).
2. For each filter read `name`, `description`, the input pad array and the
   output pad array (section 2.3; each pad's filter-name field is the
   filter's name, context and graph fields are `None`), `priv_class`, and
   `flags` as an int. Pad counts come from `avfilter_filter_pad_count(f, 0)`
   and `(f, 1)` (or `avfilter_pad_count` on old versions).
3. On the OCaml side build the `filter` record: split pads (section 11.1),
   decode flags (section 3.2).
4. The filters named exactly `abuffer`, `buffer`, `abuffersink`,
   `buffersink` become the four dedicated values and are _removed_ from the
   list.
5. `filters` is the remaining list sorted by `name` (polymorphic `compare`).

`find name` returns the first element of `filters` with that name and raises
`Not_found` otherwise. `find_opt` returns `None` instead. Neither finds the
four buffer/sink filters.

### 4.5 Pad accessors

```ocaml
val pad_name : _ pad -> string
val filter_name : _ pad -> string
```

Return the pad-name and filter-name fields (section 2.3).

### 4.6 Graph creation

```ocaml
val init : unit -> config
```

`avfilter_graph_alloc()`; `Out_of_memory` on `NULL`. Returns a `config`
with empty bookkeeping. No graph option is set (thread count, scale options
and the like keep FFmpeg defaults).

### 4.7 Attach

```ocaml
val attach :
  ?args:args list -> name:string -> [ `Unattached ] filter -> config ->
  [ `Attached ] filter
```

1. If `name` is already in the graph's `names` list: raise `Exists`.
2. Build the argument string from `args` (section 11.2), or pass none when
   `args` is absent. Building it can raise `Failure` (array separator).
3. `avfilter_get_by_name(filter.name)`; `Not_found` when `NULL`.
4. Copy `name` and the argument string to C strings (`Out_of_memory` on
   allocation failure).
5. With the runtime lock released:
   `avfilter_graph_create_filter(&ctx, filter, name, args_or_NULL, NULL,
graph)`. The copies are freed afterwards on both paths.
6. A negative result raises `Avutil.Error`. Nothing is recorded on the OCaml
   side in that case (`names` and the endpoint lists are unchanged).
7. Read the instance's pads from `ctx->input_pads` / `ctx->nb_inputs` and
   `ctx->output_pads` / `ctx->nb_outputs`. These reflect pads the filter
   created at initialisation, so their number can differ from the
   unattached filter's.
8. Split them (section 11.1) and set, in every pad, the context field to
   `Some ctx` and the graph field to `Some graph`.
9. Prepend `name` to `names`.
10. If `filter.name` is `abuffer`, `buffer`, `abuffersink` or `buffersink`,
    prepend `(name, ctx)` to the matching endpoint list (section 2.1).
11. Return the unattached filter record with `io` replaced, extended with
    the hidden context field.

Filters created by `parse` are not seen by steps 1, 9 and 10.

### 4.8 Link

```ocaml
val link :
  ([ `Attached ], 'a, [ `Output ]) pad -> ([ `Attached ], 'a, [ `Input ]) pad ->
  unit
```

Applied immediately: with the runtime lock released,
`avfilter_link(src_ctx, src.index, dst_ctx, dst.index)`. Negative result
raises `Avutil.Error`. If either pad has no context:
`Failure "ffmpeg API error: filter is not attached!"` (not reachable through
the typed API). The types force both pads to have the same media type; they
do not force the same graph.

### 4.9 Commands

```ocaml
type command_flag = [ `Fast ]
val process_command :
  ?flags:command_flag list -> cmd:string -> ?arg:string ->
  [ `Attached ] filter -> string
```

Defaults: `flags = []` (0), `arg = ""`.

1. Read the filter context from the last field of the filter value.
2. Copy `cmd` and `arg` to C buffers (`Out_of_memory` on failure).
3. With the runtime lock released:
   `avfilter_process_command(ctx, cmd, arg, res, 4096, flags)` where `res` is
   a zero-filled 4096-byte buffer.
4. Free the copies. A negative result raises `Avutil.Error`.
5. Return `res` up to its first NUL byte as a fresh string.

The command goes to that one filter instance; `avfilter_graph_send_command`
is not used.

### 4.10 Parse

```ocaml
type ('a, 'b, 'c) parse_node = {
  node_name : string;
  node_args : args list option;
  node_pad : ('a, 'b, 'c) pad;
}
type ('a, 'b) parse_av =
  ( ('a, [ `Audio ], 'b) parse_node list,
    ('a, [ `Video ], 'b) parse_node list ) av
type 'a parse_io = (('a, [ `Input ]) parse_av, ('a, [ `Output ]) parse_av) io
val parse : [ `Attached ] parse_io -> string -> config -> unit
```

`parse { inputs; outputs } description graph`:

1. Turn each node into `(node_name, pad context, pad index)`. A pad with no
   context raises `Failure "parse: unattached pad"`. `node_args` is not
   read.
2. `inputs` becomes one array: audio nodes then video nodes, each in list
   order. Same for `outputs`.
3. For each array build an `AVFilterInOut` linked list in array order:
   `name` is a C copy of `node_name`, `filter_ctx` and `pad_idx` from the
   node, `next` chained. An allocation failure of a list element frees that
   list and raises `Out_of_memory`.
4. Copy `description` to a C string (`Out_of_memory` on failure, both lists
   freed).
5. With the runtime lock released:
   `avfilter_graph_parse_ptr(graph, description, &inputs_list,
&outputs_list, NULL)`.
6. Free the description copy and whatever remains of both lists with
   `avfilter_inout_free`.
7. A negative result raises `Avutil.Error`.

Meaning, as with the C API: the `inputs` nodes are _input pads_ of already
attached filters (typically a sink's input) that the description's
correspondingly labelled open outputs get linked to; the `outputs` nodes are
_output pads_ of already attached filters (typically a buffer source's
output) that feed the description's labelled open inputs. `node_name` is the
label used in the description.

Filters that the description creates get no OCaml handle, are not added to
`names`, and are not added to the endpoint lists even when they are buffer
sources or sinks. Open pads left after parsing are not reported.

### 4.11 Launch

```ocaml
val launch : config -> t
```

1. With the runtime lock released: `avfilter_graph_config(graph, NULL)`.
   Negative result raises `Avutil.Error`; nothing is returned.
2. Build the result from the endpoint lists as they are at that moment:
   - `inputs.audio` / `inputs.video`: for each `(name, ctx)`, `(name, f)`
     where `f` is the write function below;
   - `outputs.audio` / `outputs.video`: for each `(name, ctx)`,
     `(name, { context = ctx; handler })` where `handler` is the read
     function below.
     List order is the reverse of attach order (most recently attached
     first).

Write function, `` `Frame frame ``: with the runtime lock released,
`av_buffersrc_write_frame(ctx, frame)`. The source takes its own reference;
the caller's frame value stays valid and unchanged. Negative result raises
`Avutil.Error`.

Write function, `` `Flush ``: with the runtime lock released,
`av_buffersrc_write_frame(ctx, NULL)`, which marks end of stream on that
source. Negative result raises `Avutil.Error`.

Read function (`handler ()`):

1. `av_frame_alloc()`; `Out_of_memory` on `NULL`.
2. With the runtime lock released: `av_buffersink_get_frame(ctx, frame)`.
3. Negative result: free the frame, raise `Avutil.Error` — `` `Eagain ``
   when no frame is available yet, `` `Eof `` once the sink has been flushed
   and drained.
4. Otherwise wrap the frame in a garbage-collected `Avutil.frame` that owns
   it and return it.

Nothing on the OCaml side prevents calling `launch` more than once on the
same `config`, or `attach`/`link`/`parse` after it; each call is forwarded
to FFmpeg as described.

### 4.12 `Utils`

```ocaml
type audio_converter
type audio_params = {
  sample_rate : int;
  channel_layout : Avutil.Channel_layout.t;
  sample_format : Avutil.Sample_format.t;
}
val init_audio_converter :
  ?out_params:audio_params -> ?out_frame_size:int ->
  in_time_base:Avutil.rational -> in_params:audio_params -> unit ->
  audio_converter
val time_base : audio_converter -> Avutil.rational
val convert_audio :
  audio_converter -> (Avutil.audio Avutil.frame -> unit) ->
  [ `Frame of Avutil.audio Avutil.frame | `Flush ] -> unit
```

`init_audio_converter`:

1. `init ()` a private graph.
2. `attach` `abuffer` under instance name `"abuffer"` with, in this list
   order (see section 11.2 for the resulting string order):
   `sample_rate=<in rate>`, `time_base=<num>/<den>`,
   `channel_layout=<Avutil.Channel_layout.get_description in layout>`,
   `sample_fmt=<Avutil.Sample_format.get_id in format>` (an integer).
3. If `out_params` is given: `find "aresample"`, `attach` it under instance
   name `"aresample"` with `in_sample_rate`, `in_chlayout` (description
   string), `in_sample_fmt` (integer id), `out_sample_rate`, `out_chlayout`,
   `out_sample_fmt`; `link` the source's first audio output to its first
   audio input; continue from its first audio output.
4. `attach` `abuffersink` under instance name `"sink"` with no arguments;
   `link` the current output to its first audio input.
5. `launch`. Take the first audio input function and the first audio
   output.
6. If `out_frame_size` is given: `set_frame_size sink_context n`.
7. Read `time_base sink_context` once and store it.

Any exception from these steps propagates (`Not_found` if `aresample` is
absent, `Avutil.Error`, `Failure`). A missing first pad is an assertion
failure.

`time_base c` returns the stored time base.

`convert_audio c cb input`:

1. Call the input function with `input` (push a frame or flush).
2. Loop: call the output handler; pass each frame to `cb`; repeat.
3. ``Avutil.Error `Eagain`` ends the loop normally.
4. ``Avutil.Error `Eof`` is absorbed when `input` is `` `Flush `` and
   propagates when it is a frame.
5. Any other exception, including one raised by `cb`, propagates. An
   ``Avutil.Error `Eagain`` raised by `cb` itself ends the loop the same way
   as step 3.

## 5. Errors

| Exception                                                       | Raised by                                                                                                                                                                                                                          |
| --------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Avfilter.Exists`                                               | `attach`, duplicate instance name among names attached through `attach`                                                                                                                                                            |
| `Not_found`                                                     | `find`; `attach` when `avfilter_get_by_name` fails; `Utils.init_audio_converter` when `aresample` is absent                                                                                                                        |
| `Avutil.Error e`                                                | negative FFmpeg result in `attach`, `link`, `parse`, `launch`, `process_command`, `channel_layout`, the write functions and the read handler. `e` is `avutil`'s mapping of the code (`` `Eagain ``, `` `Eof ``, …, `` `Other n ``) |
| `Failure "Invalid filter!"`, `Failure "Invalid filter option!"` | `get_array_separator`; `attach` with an `` `Array `` argument                                                                                                                                                                      |
| `Failure "ffmpeg API error: missing buffer or sink!"`           | module initialisation                                                                                                                                                                                                              |
| `Failure "ffmpeg API error: filter is not attached!"`           | `link` on a pad without context                                                                                                                                                                                                    |
| `Failure "parse: unattached pad"`                               | `parse` on a pad without context                                                                                                                                                                                                   |
| `Failure "Invalid flag type!"`                                  | flag conversion on an unknown variant                                                                                                                                                                                              |
| `Out_of_memory`                                                 | `init`, `attach`, `parse`, `process_command`, read handler, on C allocation failure                                                                                                                                                |
| `Assert_failure`                                                | `Utils.init_audio_converter` when an expected pad or endpoint is missing                                                                                                                                                           |

The binding itself closes or invalidates nothing on error. FFmpeg does: a
failed `avfilter_graph_parse_ptr` frees every filter context in the graph,
including those created by earlier `attach` calls, and the binding keeps
its handles to them and their `names` entries.

## 6. Blocking and concurrency

The runtime lock is released around: `avfilter_graph_create_filter`,
`avfilter_link`, `avfilter_graph_parse_ptr`, `avfilter_graph_config`,
`avfilter_process_command`, `av_buffersrc_write_frame` (frame and flush),
`av_buffersink_get_frame`. Other OCaml threads run meanwhile.

The lock is held for: registry enumeration, `init`, all sink accessors,
`set_frame_size`, `get_array_separator`.

The library takes no lock of its own. Two threads that use the same graph
at the same time reach FFmpeg concurrently. The OCaml bookkeeping in
`config` is unsynchronised mutable state.

Global state: the registry values, computed once at module initialisation.
No thread registration is done by this library.

## 7. Callbacks

Nothing. The library installs no C-to-OCaml callback. (`convert_audio` calls
its `cb` from OCaml.)

## 8. Data transfer

| Path                                                                                  | Copy or share                                                                                                                                                 |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Filter and pad names, descriptions                                                    | copied into OCaml strings                                                                                                                                     |
| `options`                                                                             | shares FFmpeg's static `AVClass` pointer                                                                                                                      |
| Instance name, argument string, graph description, command and argument, parse labels | copied into C strings for the call; freed after (parse labels are owned and freed by the in/out lists)                                                        |
| Frame pushed to a source                                                              | not copied by the binding; FFmpeg adds its own reference to the frame's buffers; the OCaml frame stays owned by its value                                     |
| Frame pulled from a sink                                                              | a new `AVFrame` allocated by the binding, filled by FFmpeg (reference move from the sink), owned by the returned OCaml frame value and freed by its finaliser |
| Command response                                                                      | 4096-byte C buffer, copied up to the first NUL                                                                                                                |
| Channel layout                                                                        | copied into a new layout value                                                                                                                                |
| Rationals, ints                                                                       | by value                                                                                                                                                      |

## 9. Options

- `filter.options` exposes the filter's private `AVClass` for inspection
  through `Avutil.Options`.
- Filter options are set only at `attach`, through the argument string
  passed to `avfilter_graph_create_filter` (section 11.2). There is no
  option dictionary and no unused-option reporting; FFmpeg rejects unknown
  options with an error from `attach`.
- Options are not settable through AVOption on an attached filter by this
  library; `process_command` is the only runtime control.
- No graph-level option is exposed.

## 10. Version-dependent behaviour

| Condition                               | Below                                                                                                       | At or above                                                                                                         |
| --------------------------------------- | ----------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| libavfilter 7.14.100                    | module initialisation calls `avfilter_register_all()`; enumeration uses `avfilter_next`                     | no registration call; enumeration uses `av_filter_iterate`                                                          |
| libavfilter 8.3.100                     | static pad counts from `avfilter_pad_count(pads)`                                                           | from `avfilter_filter_pad_count(filter, is_output)`                                                                 |
| libavutil 59.1.100 (array option types) | `get_array_separator` always raises `Failure "Invalid filter!"` for an existing filter with a private class | looks the option up and returns the separator it declares; raises `Failure "Invalid filter!"` when it declares none |

## 11. Logic on the OCaml side

### 11.1 Pad splitting

Input: an array of pads as read from C. Output: `{ audio; video }`.

1. A pad whose media type equals `` `Audio `` goes to `audio`; any other
   pad goes to `video` with its media type set to `` `Video ``.
2. Each list is sorted by increasing pad index.

The pad index is the index in the filter's whole pad array, not the
position within its media type.

### 11.2 Argument string

`args` is rendered to the string given to `avfilter_graph_create_filter`:

1. Each element is rendered:
   - `` `Flag s `` → `s`;
   - `` `Pair (k, v) `` with a ground value → `k=<v>`;
   - `` `Pair (k, `Array vs) `` → `k=<v1><sep><v2>…` where `sep` is
     `get_array_separator ~filter_name:<filter's name> ~option_name:k`.
2. Ground values render as: `` `String s `` → `s` verbatim (no quoting or
   escaping); `` `Int i `` → decimal; `` `Int64 i `` → decimal;
   `` `Float f `` → OCaml's `string_of_float` (for example `1.` for 1.0);
   `` `Rational {num; den} `` → `num/den`.
3. The rendered elements are joined with `:` in the **reverse** of the list
   order: the last element of `args` comes first in the string.
4. `args = Some []` yields the empty string; absent `args` yields no string
   (`NULL`).

### 11.3 Flag decoding

Section 3.2: each of the five flags is tested against the C flag word with
one conversion call per flag per filter, at module initialisation.

### 11.4 Endpoint tracking

Section 4.7 step 10 and section 4.11 step 2: endpoints are recognised by
the _filter name_ at `attach` time and listed by `launch`.

### 11.5 Command flags, parse marshalling, converter

Sections 3.3, 4.10 steps 1–2, and 4.12.
