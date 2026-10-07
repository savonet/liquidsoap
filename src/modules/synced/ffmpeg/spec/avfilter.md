# avfilter

Filter graphs. It follows [binding-contract.md](binding-contract.md); section
numbers match.

## 1. Scope

`Avfilter` binds libavfilter: the filter registry, filter graphs, buffer
sources and buffer sinks.

It depends on `avutil` only: the error exception, rationals, channel layouts,
frames, option classes, and the sample-format and pixel-format conversions.
It installs no C header and provides nothing to other libraries.

**Module initialisation** enumerates every filter FFmpeg registers (§4.4). It
fails only when one of `abuffer`, `buffer`, `abuffersink`, `buffersink` is
not registered (I2). Absent descriptions and pad names do not make it fail
(A3).

## 2. Objects

### 2.1 Filter graph — `config`

- **Native object**: one filter graph, with every filter instance created in
  it.
- **Creation**: `init ()`.
- **Ownership**: the value owns the graph; the graph owns its filter
  instances.
- **Kept alive by**: every dependent handle of §2.2 (L3).
- **Release**: by collection only.
- **Guard**: §6.2.
- **States**:

  | Operation                                             | Configuring                   | Running                 | Failed       |
  | ----------------------------------------------------- | ----------------------------- | ----------------------- | ------------ |
  | `attach`, `link`, `parse`                             | performed                     | failure: graph launched | failed error |
  | `launch`                                              | configures → Running          | failure: graph launched | failed error |
  | `process_command`                                     | performed                     | performed               | failed error |
  | sink accessors, `set_frame_size`, pushing and pulling | not reachable (no handle yet) | performed               | failed error |

  A graph becomes **Failed** when `parse` or `launch` fails. FFmpeg frees
  every filter of a graph whose parsing failed, those created before the call
  included, and leaves a graph whose configuration failed in an unspecified
  state. No handle of a failed graph reaches a filter instance again. An
  implementation MAY instead guarantee that a failed `parse` leaves the graph
  exactly as it was, and then keeps it Configuring.

### 2.2 Dependent handles

Each designates a filter instance of a graph, keeps that graph alive, shares
its guard, and raises the failed error once the graph is failed:

| Handle                                    | Designates                             |
| ----------------------------------------- | -------------------------------------- |
| an attached pad                           | one pad of a filter instance           |
| an ``[`Attached] filter``                 | the filter instance its pads belong to |
| `'a context` (the `context` of an output) | a buffer sink                          |
| an `'a input` function                    | a buffer source                        |
| an `'a output` handler                    | a buffer sink                          |
| `Utils.audio_converter`                   | the source and sink of a private graph |

### 2.3 Pad — `('a, 'b, 'c) pad`

Abstract. A pad records the pad's name, the name of its filter (the filter,
not the instance), its media type, and its index in the filter's whole pad
array. An attached pad is a dependent handle; an unattached pad is plain data.

Type parameters: `'a` attached or unattached, `'b` media type (``[`Audio]``
or ``[`Video]``), `'c` direction (``[`Input]`` or ``[`Output]``).

### 2.4 Filter — `'a filter`

A public record: `name`, `description`, `options`, `flags`, `io`.

- ``[`Unattached] filter`` values come from the registry and describe the
  filter's static pads.
- ``[`Attached] filter`` values come from `attach`: the same `name`,
  `description`, `options` and `flags`, with `io` holding the instance's
  actual pads.

The record is public, so a caller can build or alter one (B3). An operation
that takes an ``[`Attached] filter`` finds the filter instance through the
record's pads, which are abstract and cannot be forged. A record with no
attached pad, or whose pads belong to several instances, raises a failure.

### 2.5 Graph endpoints — `t`, `'a input`, `'a output`

`launch` returns `t = { inputs; outputs }`, each `{ audio; video }`, each an
association list from instance name to endpoint.

## 3. Enumerations and constants

**Pad media type.** A pad of audio type goes to the `audio` list, a pad of
video type to the `video` list. A pad of any other media type is in neither.
The index of a pad is its position among all the pads of the filter in that
direction, so leaving one out renumbers nothing.

**Filter flags** (C to OCaml, per E6):

| `flag`                           | C constant                                |
| -------------------------------- | ----------------------------------------- |
| `` `Dynamic_inputs ``            | `AVFILTER_FLAG_DYNAMIC_INPUTS`            |
| `` `Dynamic_outputs ``           | `AVFILTER_FLAG_DYNAMIC_OUTPUTS`           |
| `` `Slice_threads ``             | `AVFILTER_FLAG_SLICE_THREADS`             |
| `` `Support_timeline_generic ``  | `AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC`  |
| `` `Support_timeline_internal `` | `AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL` |

**Command flags** (OCaml to C): `` `Fast `` is `AVFILTER_CMD_FLAG_FAST`.

**Parameter:**

| Name                   | Recommended | Meaning                                                                                         |
| ---------------------- | ----------- | ----------------------------------------------------------------------------------------------- |
| `COMMAND_RESPONSE_MAX` | 4096 bytes  | Size of the buffer a filter writes its answer to a command into. FFmpeg truncates to that size. |

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

`frame` and `rational` are `Avutil`'s. Pad lists are in ascending pad index.

### 4.2 Sink accessors

All take the `context` of an output of a launched graph.

| Signature                                                              | Result                                                                                                                    |
| ---------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `val time_base : _ context -> Avutil.rational`                         | the sink's time base                                                                                                      |
| ``val frame_rate : [ `Video ] context -> Avutil.rational``             | the sink's frame rate                                                                                                     |
| ``val width : [ `Video ] context -> int``                              | the width of its frames                                                                                                   |
| ``val height : [ `Video ] context -> int``                             | the height of its frames                                                                                                  |
| ``val pixel_aspect : [ `Video ] context -> Avutil.rational option``    | the sample aspect ratio; `None` when unknown                                                                              |
| ``val pixel_format : [ `Video ] context -> Avutil.Pixel_format.t``     | the pixel format (E3)                                                                                                     |
| ``val channels : [ `Audio ] context -> int``                           | the channel count                                                                                                         |
| ``val channel_layout : [ `Audio ] context -> Avutil.Channel_layout.t`` | an independent copy of the layout                                                                                         |
| ``val sample_rate : [ `Audio ] context -> int``                        | the sample rate                                                                                                           |
| ``val sample_format : [ `Audio ] context -> Avutil.Sample_format.t``   | the sample format (E3)                                                                                                    |
| ``val set_frame_size : [ `Audio ] context -> int -> unit``             | makes the sink deliver frames of exactly that many samples per channel, except the last. A size below 1 raises a failure. |

### 4.3 Array separator

```ocaml
val get_array_separator : filter_name:string -> option_name:string -> char
```

The character that separates the elements of an array-typed option of a
filter.

- The separator the option declares, and `,` when it declares none.
- A failure when FFmpeg knows no filter of that name, when the filter has no
  option of that name, and when the option is not array-typed.

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

Computed once at module initialisation, from every filter FFmpeg registers.

- Each filter gives a record: its name, its description (A3), its private
  option class (no class when it has no option), its flags (§3), and its
  static input and output pads split by media type (§3).
- `abuffer`, `buffer`, `abuffersink`, `buffersink` are the four filters of
  those names: the audio and video buffer sources and sinks.
- `filters` holds every other filter, sorted by name in ascending byte order.
  The four endpoint filters are not in it.
- `find name` returns the element of `filters` of that name and raises
  `Not_found` when there is none. `find_opt` returns `None` instead. Neither
  finds the four endpoint filters.

### 4.5 Pad accessors

```ocaml
val pad_name : _ pad -> string
val filter_name : _ pad -> string
```

The pad's name (A3) and the name of its filter.

### 4.6 Graph creation

```ocaml
val init : unit -> config
```

A new, empty graph in the Configuring state. Every graph option keeps
FFmpeg's default.

### 4.7 Attach

```ocaml
val attach :
  ?args:args list -> name:string -> [ `Unattached ] filter -> config ->
  [ `Attached ] filter
```

Creates an instance of the filter in the graph, under the instance name
`name`, initialised with the arguments `args` (§11.2).

- `Exists` is raised when the graph already has an instance of that name,
  whether `attach` or `parse` created it.
- The result's pads are the instance's actual pads, attached. Their number
  can differ from the unattached filter's: some filters create pads when they
  are initialised.
- A filter FFmpeg does not know raises ``Error `Filter_not_found``. Any other
  FFmpeg failure, a rejected argument among them, raises the mapped error.
- On failure the graph is unchanged.

### 4.8 Link

```ocaml
val link :
  ([ `Attached ], 'a, [ `Output ]) pad -> ([ `Attached ], 'a, [ `Input ]) pad ->
  unit
```

Connects an output pad to an input pad (`avfilter_link`). The types require
the same media type. Two pads of different graphs raise a failure. An FFmpeg
failure, such as a pad already linked, raises the mapped error.

### 4.9 Commands

```ocaml
type command_flag = [ `Fast ]
val process_command :
  ?flags:command_flag list -> cmd:string -> ?arg:string ->
  [ `Attached ] filter -> string
```

Sends a command to one filter instance (`avfilter_process_command`) and
returns its answer, truncated to `COMMAND_RESPONSE_MAX`. `flags` defaults to
none and `arg` to the empty string. A failure raises the mapped error.

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

`parse { inputs; outputs } description graph` adds the filters and links of a
textual graph description to the graph, and connects the description's open
ends to pads of filters already attached.

- A node names a label of the description (`node_name`) and an attached pad
  (`node_pad`). `node_args` has no meaning: it is ignored.
- As in FFmpeg's API, the naming is from the point of view of the
  description. The `inputs` nodes are **input pads** of attached filters,
  typically a sink's input, that the description's open outputs of that label
  are linked to. The `outputs` nodes are **output pads** of attached filters,
  typically a source's output, that feed the description's open inputs of
  that label.
- Within `inputs` and within `outputs`, audio nodes come before video nodes,
  each in list order.
- A pad of another graph raises a failure before anything is parsed.
- The filters the description creates belong to the graph. Their instance
  names count for `Exists`. Buffer sources and sinks among them are endpoints
  of the graph (§4.11). No ``[`Attached] filter`` value is returned for them.
- On failure the mapped error is raised and the graph is failed (§2.1).

### 4.11 Launch

```ocaml
val launch : config -> t
```

Configures the graph (`avfilter_graph_config`); it becomes Running. On
failure the mapped error is raised and the graph is failed.

The result lists **every** buffer source and buffer sink of the graph,
whichever of `attach` and `parse` created it, by instance name, in the order
the instances were created in the graph:

- `inputs.audio`, `inputs.video`: one input function per audio, respectively
  video, buffer source;
- `outputs.audio`, `outputs.video`: one `{ context; handler }` per buffer
  sink.

**Input function**, `` `Frame frame ``: gives the frame to the source, which
takes a reference of its own (A4). A failure raises the mapped error.

**Input function**, `` `Flush ``: marks the end of the stream on that source.
A source that was flushed accepts no further frame: FFmpeg answers
``Error `Eof``. There is no way back; a caller that needs the graph again
builds a new one.

**Handler** `handler ()`: the next frame the sink has ready, as a fresh frame
value. It raises ``Error `Eagain`` when none is ready yet and ``Error `Eof``
once the sink is drained after a flush; both are part of normal operation. A
receive that returns no frame releases what it allocated.

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

A converter that re-frames audio to a fixed frame size and, optionally,
converts it to another rate, layout and format. §11.3 gives its construction.

- `init_audio_converter`: without `out_params` the output has the input's
  parameters. With `out_frame_size`, every frame delivered has exactly that
  many samples per channel, except the last one after a flush.
- `time_base c`: the time base of the frames the converter delivers.
- `convert_audio c cb input`: pushes a frame, or the end of the stream, and
  calls `cb` on every frame that becomes available, in order.
  - Pushing a frame when no output is ready yet is not an error.
  - After `` `Flush `` every remaining frame is delivered.
  - An exception raised by `cb` propagates, whatever it is (§7.2).

## 5. Errors

| Raised                                    | By                                                                                                                             |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `Avfilter.Exists`                         | `attach`, when the graph has an instance of that name                                                                          |
| `Not_found`                               | `find`                                                                                                                         |
| `Error e`, `e` mapped from an FFmpeg code | `attach`, `link`, `parse`, `launch`, `process_command`, `channel_layout`, pushing, pulling                                     |
| ``Error `Eagain``, ``Error `Eof``         | an output handler; ``Error `Eof`` also when pushing to a flushed source                                                        |
| ``Error `Filter_not_found``               | `attach`; `Utils.init_audio_converter` when FFmpeg lacks the resampling filter                                                 |
| the state errors of the contract's §5.3   | per §2.1                                                                                                                       |
| ``Error (`Failure msg)``                  | "graph launched"; pads of different graphs; a filter record with no usable pad (§2.4); `get_array_separator`; `set_frame_size` |
| `Out_of_memory`                           | any failed allocation                                                                                                          |

## 6. Blocking and concurrency

### 6.1 The runtime lock

Contract M1 covers `attach`, `parse`, `launch`, `process_command`, pushing to
a source and pulling from a sink.

### 6.2 Guards

A graph has one guard, shared by every dependent handle of §2.2 (M9).

| Exclusive                                                                                  | Shared         |
| ------------------------------------------------------------------------------------------ | -------------- |
| `attach`, `link`, `parse`, `launch`, `process_command`, `set_frame_size`, pushing, pulling | sink accessors |

Pushing to one source of a graph while another thread pulls from a sink of
the same graph is a conflict: one of the two raises the in-use error.

### 6.3 Global state

The registry values, computed at module initialisation and immutable. The
library registers no thread.

## 7. Callbacks

### 7.1 Functions FFmpeg calls

None.

### 7.2 The function passed to `Utils.convert_audio`

Called by the binding between native steps (contract §7.2). The converter is
not in use while it runs. When it raises, the frames not yet delivered stay in
the converter's graph; the next `convert_audio` delivers them first.

## 8. Data transfer

| Path                                                                                  | Copy or share                                                        |
| ------------------------------------------------------------------------------------- | -------------------------------------------------------------------- |
| filter and pad names, descriptions                                                    | copied                                                               |
| `options`                                                                             | a borrowed option class                                              |
| instance name, argument string, graph description, command and argument, parse labels | copied for the call                                                  |
| frame pushed to a source                                                              | FFmpeg takes a reference of its own; the caller's frame is unchanged |
| frame pulled from a sink                                                              | a new frame owned by the returned value; no copy of the data         |
| command answer                                                                        | copied                                                               |
| channel layout                                                                        | copied                                                               |

## 9. Options

- `filter.options` exposes the filter's private option class for inspection
  through `Avutil.Options`.
- Filter options are set at `attach` only, through the argument string
  (§11.2). There is no option table and no report of unused options: FFmpeg
  rejects an unknown option and `attach` fails.
- `process_command` is the only control of a running filter.
- No graph-level option is exposed.

## 10. Version-dependent behaviour

None.

## 11. Composite operations

### 11.1 Endpoints

A filter instance is a graph endpoint when its filter is one of the four
buffer filters. `launch` finds endpoints by walking the graph, so it sees the
ones a parsed description created.

### 11.2 Argument string

`args` is rendered to the option string FFmpeg's filters parse:

1. Each element is rendered, in list order:
   - `` `Flag s `` gives `s`;
   - `` `Pair (k, v) `` with a ground value gives `k=<v>`;
   - `` `Pair (k, `Array vs) `` gives `k=<v1><sep><v2>…`, where `sep` is
     `get_array_separator` for the filter and `k`.
2. Ground values render as: `` `String s `` verbatim; `` `Int `` and
   `` `Int64 `` in decimal; `` `Float `` as a decimal text FFmpeg parses back
   to the same value; `` `Rational {num; den} `` as `num/den`.
3. The rendered elements are joined with `:`, **in list order**.
4. An absent or empty `args` gives no argument.

Nothing is quoted or escaped: a caller whose string value contains `:`, `=`,
`'`, `\` or the array separator writes it in FFmpeg's option-string syntax.

FFmpeg binds a value with no key (`` `Flag ``) to the filter's options in
declaration order and rejects one that follows a `key=value` pair.

### 11.3 Audio converter

`init_audio_converter` builds a private graph:

1. An audio buffer source with the input's sample rate, time base, channel
   layout and sample format.
2. With `out_params`: FFmpeg's resampling filter (`aresample`) set from the
   input and output parameters, linked after the source.
3. An audio buffer sink, linked last.
4. The graph is launched. With `out_frame_size`, the sink's frame size is set.
5. The sink's time base is read once.

Any failure raises the error of the step that failed.

`convert_audio c cb input`:

1. Deliver to `cb` every frame the sink has ready.
2. Push `input`.
3. Deliver to `cb` every frame the sink has ready, until the sink has none
   (after a frame) or is drained (after `` `Flush ``).

Only the sink's own "none ready" and "drained" answers end a delivery loop;
the same errors raised by `cb` propagate.
