# FFmpeg filter graphs

This file owns how libavfilter filters reach scripts: the `ffmpeg.filter.*`
builtins generated from FFmpeg's filter registry, `ffmpeg.filter.create` and
the life of a graph (description, build, launch, end, rebuild), graph inputs
and outputs as sources, the clocks a graph runs in, frame flow, timestamps and
metadata through a graph, `ffmpeg.filter.parse`, the private helper graphs
(Display, Fps, AFormat, Fit) other parts of the layer use, and the script
definitions built on them. It relies on the avfilter bindings
(`src/modules/synced/ffmpeg/spec/avfilter.md`), option introspection
(`avutil.md`, Option introspection), the clock rules (`src/core/clock/spec/`),
raw content ([content.md](content.md)) and the encoder's timestamp rules
([timestamps.md](timestamps.md)). Bitstream filters are in
[bitstream-filters.md](bitstream-filters.md).

## 1. Vocabulary

| Term           | Meaning                                                                                                                             |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| graph value    | the script value of type `ffmpeg.filter.graph` handed to the function given to `ffmpeg.filter.create`                               |
| pad value      | a script value of type `ffmpeg.filter.audio` or `ffmpeg.filter.video`: a way to obtain one attached output pad of the current graph |
| probe config   | an avfilter graph used only while the script describes the graph, to learn pad counts and validate arguments; never launched        |
| generation     | one avfilter graph built from the description, launched, run until its end of stream, then dropped                                  |
| graph input    | the operator made by `ffmpeg.filter.audio.input`/`video.input`: an output, in the liquidsoap sense, feeding one buffer source       |
| sink           | the liquidsoap end of one buffer sink, made by `ffmpeg.filter.audio.output`/`video.output`                                          |
| graph source   | the single liquidsoap source that carries every output of one graph, one field per output                                           |
| main time base | `1/main_rate`, the time base of liquidsoap's main ticks ([common.md §4](common.md#4-liquidsoap-time-bases))                         |

## 2. Settings

| Key                                 | Type  | Default | Effect                                                                         |
| ----------------------------------- | ----- | ------- | ------------------------------------------------------------------------------ |
| `settings.ffmpeg.filter_max_buffer` | float | `10.`   | seconds of data one output of a graph may hold before the graph raises (§12.3) |

Also read here, described in [common.md §1](common.md#1-settings): `settings.ffmpeg.autorotate`
(§16.1), `settings.ffmpeg.format_change.*` (§11.4, §16) and
`settings.ffmpeg.scaling_algorithm` (§16.4).

Logs of this area go to the label `ffmpeg.filter`.

## 3. Script values

Three abstract types, all printed by their name and none serialisable:

| Type                  | Printed as            | `json.stringify`                                                                    |
| --------------------- | --------------------- | ----------------------------------------------------------------------------------- |
| `ffmpeg.filter.graph` | `ffmpeg.filter.graph` | raises error kind `json`, "Ffmpeg filter graph cannot be represented as json"       |
| `ffmpeg.filter.audio` | `ffmpeg.filter.audio` | raises error kind `json`, "Ffmpeg filter audio input cannot be represented as json" |
| `ffmpeg.filter.video` | `ffmpeg.filter.video` | raises error kind `json`, "Ffmpeg filter video input cannot be represented as json" |

Comparison of two such values is structural over the underlying OCaml value.
Two distinct pad values raise a runtime error of kind `invalid`, message
`compare: functional value` (from the implementation: `print(a == b)` on two
`ffmpeg.filter.sine` outputs).

A pad value holds a function, not a pad: reading it returns the output pad of
the instance attached in the **current generation**, attaching that instance
first when it has not been attached in this generation (§11.2). A graph value
is usable only during the call of `ffmpeg.filter.create` that made it (§7.3).

## 4. Filter builtins

### 4.1 Which filters

At start-up (timed as "FFmpeg filters registration"), every filter of the
bindings' registry gets two builtins. The registry excludes `abuffer`,
`buffer`, `abuffersink` and `buffersink` (`avfilter.md`, Registry); these
four have no builtin. On the build used here there are 1065 names under
`ffmpeg.filter.` (from the implementation, `--list-functions`, bitstream
filters and the hand-written builtins included).

### 4.2 Names and documentation

For a filter `<name>` with description `<d>`:

| Builtin                       | Category                 | Flags   | Description                                                                                                                                       |
| ----------------------------- | ------------------------ | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.filter.<name>`        | `Source / FFmpeg filter` | `extra` | `Ffmpeg filter: <d>` followed by the dynamic-pad sentences below                                                                                  |
| `ffmpeg.filter.<name>.create` | `Source / FFmpeg filter` | `extra` | the same text, then `. Use this operator to initiate the filter independently of its inputs, to be able to send commands to the filter instance.` |

Dynamic-pad sentences, appended after a space, in this order when they apply:

- dynamic inputs: `This filter has dynamic inputs: last two arguments are lists of audio and video inputs. Total number of inputs is determined at runtime.`
- dynamic outputs: `This filter has dynamic outputs: returned value is a tuple of audio and video outputs. Total number of outputs is determined at runtime.`

### 4.3 Signature

`ffmpeg.filter.<name>` takes, in order:

1. One optional labelled argument per option (§5), each of type `T?`,
   default `null`.
2. The graph, unlabelled, of type `ffmpeg.filter.graph`.
3. The inputs, unlabelled (§6.1).

It returns the outputs (§6.2).

`ffmpeg.filter.<name>.create` takes items 1 and 2 only and returns `unit`
with three methods (§6.3).

Example (from the implementation, `liquidsoap -h`):

```
ffmpeg.filter.aecho :
  (?decays : string?, ?delays : string?, ?out_gain : float?, ?in_gain : float?,
   ffmpeg.filter.graph, ffmpeg.filter.audio) -> ffmpeg.filter.audio

ffmpeg.filter.aecho.create :
  (?decays : string?, ?delays : string?, ?out_gain : float?, ?in_gain : float?,
   ffmpeg.filter.graph) -> unit.{output : ffmpeg.filter.audio,
   process_command : (?fast : bool, string, string) -> string,
   set_input : (ffmpeg.filter.audio) -> unit}

ffmpeg.filter.amerge :
  (?layout_mode : int?, ?inputs : int?, ffmpeg.filter.graph,
   [ffmpeg.filter.audio], [ffmpeg.filter.video]) -> ffmpeg.filter.audio

ffmpeg.filter.asplit :
  (?outputs : int?, ffmpeg.filter.graph, ffmpeg.filter.audio) ->
  [ffmpeg.filter.audio] * [ffmpeg.filter.video]
```

### 4.4 Applying a filter

`ffmpeg.filter.<name>(args, graph, inputs)` is `create` followed by
`set_input(inputs)`, returning `output`:

1. Labelled arguments are kept. Of the unlabelled ones, the last `n` are the
   inputs, where `n` is the number of input parameters of the signature
   (§6.1); the others come first.
2. `create` runs with the labelled arguments and the leading unlabelled ones
   (the graph).
3. `set_input` is applied to the inputs, at the call's position.
4. The value of the `output` method is returned.

`create` (applying the filter):

1. Checks the graph value is in scope (§7.3).
2. Picks an instance name `<name>_<k>`: `k` counts the instances of that
   filter name made so far in the process, starting at 0 (`aecho_0`,
   `aecho_1`, …). The counter is process-wide and never reset.
3. Renders the arguments (§5.4).
4. Attaches one instance to the probe config under that name, with those
   arguments. This settles the pad counts of filters whose pads appear only
   once attached, and reports a rejected argument at the script position. A
   failure of `attach` propagates (`avfilter.md`, Attach).
5. Records a cell (§11.2) that attaches the same filter, name and arguments
   into the current generation's config when read.
6. Returns the value with methods.

## 5. Arguments from AVOptions

### 5.1 Which options

The options are `opts` of the filter's option class, its child classes
included (`avutil.md`, Option introspection), in that order. Each gives one
argument named after the option. Options of the same name in a class and a
child class give two arguments of the same name (from the implementation:
`ffmpeg.filter.scale` lists `param0` and `param1` twice).

The arguments appear in the reverse of FFmpeg's order (from the
implementation: `aecho` declares `in_gain, out_gain, delays, decays`; the
builtin lists `decays, delays, out_gain, in_gain`).

### 5.2 Types

| AVOption constructor                                            | Script type | Value to option                                 | Option value to text                    |
| --------------------------------------------------------------- | ----------- | ----------------------------------------------- | --------------------------------------- |
| `Int`                                                           | `int`       | the integer                                     | decimal                                 |
| `Flags`, `Int64`, `UInt64`, `Duration`                          | `int`       | the integer as 64-bit                           | decimal                                 |
| `Float`, `Double`                                               | `float`     | the float                                       | OCaml `string_of_float` (`220.`, `0.5`) |
| `Rational`                                                      | `string`    | `"num/den"`, split on `/`, each part an integer | `num/den`                               |
| `Bool`                                                          | `bool`      | the boolean                                     | `true` / `false`                        |
| `String`, `Binary`, `Dict`, `Image_size`, `Video_rate`, `Color` | `string`    | the string                                      | verbatim                                |
| `Pixel_fmt`                                                     | `string`    | pixel format of that name                       | its name, `none` when it has none       |
| `Sample_fmt`                                                    | `string`    | sample format of that name                      | its name, `none` when it has none       |
| `Channel_layout`                                                | `string`    | channel layout found from the string            | its description                         |
| `Array` of any of the above                                     | `[T]`       | each element as above                           | each element as above                   |

Every argument has type `T?` and default `null`. `null` means the option is
not passed: FFmpeg's default applies.

A `Duration` value is an integer rendered in decimal; FFmpeg parses option
text for a duration as a time in seconds, while the defaults shown in the
documentation are FFmpeg's stored value in microseconds ([findings](findings/filters.md)).

### 5.3 Documentation of an argument

Built from the option's help, default and named values. `to_string` is the
"option value to text" column; a named value prints as `<value> (<name>)`.

| help | default | named values | Text                                                                                                                  |
| ---- | ------- | ------------ | --------------------------------------------------------------------------------------------------------------------- |
| yes  | none    | none         | `<help>`                                                                                                              |
| yes  | any     | any          | `<help>. (<parts>)`, parts among `possible values: <v1>, <v2>…` and `default: <d>`, joined by `, `, the default first |
| no   | none    | some         | `Possible values: <v1>, …`                                                                                            |
| no   | some    | none         | `Default: <d>`                                                                                                        |
| no   | some    | some         | `Default: <d>, possible values: <v1>, …`                                                                              |
| no   | none    | none         | no text                                                                                                               |

An array option shows no default and no named values.

Example (from the implementation): `layout_mode : int? (default: null) method
used to determine the output channel layout. (default: 0, possible values: 0
(legacy), 1 (reset), 2 (normal))`.

### 5.4 Validation and rendering

Run when the filter is applied, for each argument that is not `null`, in
FFmpeg's declaration order:

1. Convert the script value ("value to option"). Any failure raises error 7
   (invalid value) on the argument, message `Invalid value`.
2. When the option has a minimum and the value is below it: invalid value,
   `<name> must be more than <min>`.
3. When the option has a maximum and the value is above it: invalid value,
   `<name> must be less than <max>` (from the implementation:
   `aecho(in_gain=2.)` gives `in_gain must be less than 1.`).
4. When the option has named values and the value is not one of them:
   invalid value, `<name> should be one of: <v1>, <v2>, …`. A `Flags` option
   lists each flag as a named value, so a combination of flags is rejected
   (from the implementation: `scale(sws_flags=6)` gives `sws_flags should be
one of: 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 4096, 262144, 8192,
16384, 524288, 8388608, 1048576, 2048`).
5. Render to an argument pair `name=value`:
   - when the option's default renders as the largest 64-bit integer and the
     value renders as the largest script integer (`2^62 - 1`), the pair
     carries the largest 64-bit integer; likewise for the smallest. This lets
     a script ask for FFmpeg's `INT64_MAX`/`INT64_MIN` sentinels, which a
     script integer cannot hold;
   - otherwise the pair carries the value's text ("option value to text"),
     as a string;
   - an array carries the list of its elements' texts, which the bindings
     join with the filter's array separator (`avfilter.md`, Argument string).

Array arguments skip steps 2–4.

The pairs are passed in the order the builtin lists its arguments, which is
the reverse of FFmpeg's declaration order. Two options of the same name both
read the one script argument of that name, so both are passed. From the
implementation, with `settings.ffmpeg.log.verbosity` at `debug`,
`aecho(in_gain=0.5, delays="500")` logs:

```
[aecho_0 @ …] Setting 'delays' to value '500'
[aecho_0 @ …] Setting 'in_gain' to value '0.5'
```

and `sine(frequency=220.)` logs `Setting 'frequency' to value '220.'`.

## 6. Inputs, outputs and methods of a filter builtin

### 6.1 Inputs

Pad counts come from the instance attached to the probe config (§4.4), but
the **parameters** of the signature come from the unattached filter:

- filter flagged dynamic inputs: two parameters, `[ffmpeg.filter.audio]` then
  `[ffmpeg.filter.video]`;
- otherwise one parameter per static input pad, audio pads first, then video
  pads, of type `ffmpeg.filter.audio` / `ffmpeg.filter.video`.

### 6.2 Outputs

- filter flagged dynamic outputs: a pair `([ffmpeg.filter.audio],
[ffmpeg.filter.video])`, one element per output pad of the attached
  instance, audio then video;
- otherwise one pad value when the filter has exactly one output pad, a tuple
  of pad values (audio first) when it has several, the empty tuple when it
  has none.

The output pad values read the cell of the instance (§3), so they designate
pad `i` of whichever generation is current.

### 6.3 Methods of `create`

| Method            | Type                                       | Behaviour                                                                                                                                                                                                                                                                                 |
| ----------------- | ------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `output`          | as §6.2                                    | the instance's outputs                                                                                                                                                                                                                                                                    |
| `set_input`       | the input parameters of §6.1 → `unit`      | records the links (below). A second call raises error kind `ffmpeg.filter`, `Filter input already set!`, at the position of its first argument                                                                                                                                            |
| `process_command` | `(?fast : bool, string, string) -> string` | sends `command`, `argument` to the instance of the current generation, with the `fast` flag when `fast` is `true` (default `false`), and returns FFmpeg's answer. Outside a generation it returns `graph not started!`. An FFmpeg failure becomes a runtime error of kind `ffmpeg.filter` |

`set_input` records one closure in the graph's build list (§11.3). Run at each
build, the closure links, for each input pad `i` of the attached instance
(audio pads, then video pads):

1. The source pad: with dynamic inputs, element `i` of the audio list (for
   audio pads) or of the video list (for video pads); otherwise the
   unlabelled argument at position `i`, counted over audio pads then video
   pads. A list shorter than the pad count raises error 7, `Invalid number of
input for filter <name>`; extra list elements are ignored.
2. Reading the source pad value and the instance's input pad `i` (both
   attach into the current config when needed).
3. `link source_pad input_pad`. A failure raises error kind `ffmpeg.filter`,
   `Error while connecting filter elements <input filter> to <output
filter>: <exception>`, at the source pad value's position.

Since the closure runs at build time, these errors are raised while
streaming, during a build (§11.3).

## 7. `ffmpeg.filter.create`

### 7.1 Signature

```
ffmpeg.filter.create : (((ffmpeg.filter.graph) -> 'a)) -> 'a
```

(from the implementation). Category `Source / FFmpeg filter`.

### 7.2 Steps

1. Create the probe config and the graph record: no current config,
   generation 0, not failed, an owner `{kind = "ffmpeg filter graph"; id =
"ffmpeg.filter.<n>"}` where `n` counts graphs created in the process from
   0, empty build list, empty input and output registries.
2. Apply the function to the graph value. Everything the script builds with
   the graph happens here: filter builtins, inputs (§8), outputs (§9),
   `parse` (§15).
3. Create the output clock, id `ffmpeg.filter`, default sync, and the input
   clock, id `ffmpeg.filter.input`, sync `passive`, parent the output clock,
   owner the graph's owner (§10).
4. Unify the clock of each graph input with the input clock, and the clock
   of the graph source, when there is one, with the output clock.
5. When there is a graph source: on its wake-up, register the input clock
   as a sub-clock of the output clock; on its sleep, deregister it.
6. Append the launch closure (§11.3, step 4) to the build list, so it runs
   after every attach and link of the description.
7. Drop the probe config: from now on the graph value is out of scope.
8. Return the function's result.

Nothing is attached to a real generation and nothing runs in FFmpeg until
streaming time, except the probe attaches of §4.4 (and see §15.3).

### 7.3 Scope

Every builtin taking a graph value (filters, inputs, outputs, `parse`)
first checks that the probe config is still there. Afterwards it raises error
7 on the graph value: `Graph variables cannot be used outside of
ffmpeg.filter.create!` (from the implementation, with the graph value kept
in a reference and used after `create` returned).

## 8. Graph inputs

### 8.1 Builtins

```
ffmpeg.filter.audio.input :
  (?id : string?, ?pass_metadata : bool, ffmpeg.filter.graph,
   ffmpeg.audio.raw('a)) -> ffmpeg.filter.audio
ffmpeg.filter.video.input :
  (?id : string?, ?pass_metadata : bool, ffmpeg.filter.graph,
   ffmpeg.video.raw('a)) -> ffmpeg.filter.video
```

(from the implementation). `id` defaults to `ffmpeg.filter.audio.input` /
`ffmpeg.filter.video.input`; `pass_metadata` (default `true`) passes
liquidsoap metadata into the graph (§14). The last argument is a track; its
format is unified with raw FFmpeg content with default raw parameters, so its
parameters unify with the upstream source's ([content.md](content.md)).

### 8.2 What it creates

1. A buffer-source name `abuffer_<k>` / `buffer_<k>` (counter as in §4.4,
   keyed by `abuffer` / `buffer`).
2. A graph input operator for the track's source and field: an output, in
   the liquidsoap sense, with kind `ffmpeg.filter.input`, fallible, not
   registered on telnet, auto-started, on a clock of its own (sync
   `passive`, owner the graph's owner, id the buffer-source name) that step 4
   of §7.2 unifies with the input clock. Its `start`, `stop` and `reset` do
   nothing. It can produce a frame only between its wake-up and its sleep,
   and when its source is ready. Its self-sync is its source's.
3. Registers in the graph, in order: the input in the list of graph inputs;
   its flush (§13.1, `flush_input`); its end (`end_input`); a readiness test
   "the format of this generation is known"; a reset that forgets the format
   and resets the input (§11.5).
4. A cell attaching the buffer source into the current config with the
   arguments of §8.3, built from the format settled for this generation.
5. Registers the input's frame setter under the buffer-source name, for the
   launch closure.
6. Returns a pad value reading the buffer source's only output pad.

### 8.3 Buffer-source arguments

Audio (`abuffer`), in this order:

| Argument         | Value                                                                                                                  |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `sample_rate`    | the frame's sample rate                                                                                                |
| `time_base`      | the main time base                                                                                                     |
| `channel_layout` | the layout's mask as a 64-bit integer; a layout with no mask uses the mask of the default layout for its channel count |
| `sample_fmt`     | the sample format's numeric identifier                                                                                 |

Video (`buffer`): `video_buffer_args` of the frame format with the main time
base (`avfilter.md`, `Utils`).

## 9. Graph outputs and the graph source

### 9.1 Builtins

```
ffmpeg.filter.audio.output :
  (?id : string?, ?pass_metadata : bool, ffmpeg.filter.graph,
   ffmpeg.filter.audio) -> ffmpeg.audio.raw('a)
ffmpeg.filter.video.output :
  (?id : string?, ?pass_metadata : bool, ffmpeg.filter.graph,
   ffmpeg.filter.video) -> ffmpeg.video.raw('a)
```

(from the implementation). They are track operators: they return a track of
the graph source. `id` sets the id **of the graph source**, so the last
output given an `id` names the whole graph. `pass_metadata` (default `true`)
passes frame metadata out of the graph (§14). The audio one is in category
`Track / FFmpeg filter`, the video one in `Track / Video processing`.

### 9.2 What an output creates

1. The graph source, on the first output of the graph (§9.3). A graph with
   no output has no graph source.
2. A field of the graph source: the `n`-th audio output of the graph (from 0) gets field `audio_n` (`audio`, `audio_2`, `audio_3`, …), likewise
   `video`, `video_2`, … for video. Only that field is constrained, so the
   other outputs share the source's row.
3. A sink for that field (§13.2), registered with the graph source.
4. A buffer-sink name `abuffersink_<k>` / `buffersink_<k>` and a cell
   attaching an `abuffersink` / `buffersink` with no argument.
5. Registers the sink's output setter under that name, a reset of the sink,
   and a build closure that links the given pad value to the sink's only
   input pad.
6. Returns the field and the graph source.

### 9.3 The graph source

One source per graph, named `ffmpeg.filter`, fallible, carrying every output
as a field of one buffer. All outputs advance together: one buffer, one set
of track marks, one readiness.

| Aspect           | Behaviour                                                                                                                          |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| readiness        | ready when the buffer holds at least one frame's worth (`Frame.size`) or the graph is ready (§12.1)                                |
| frame            | fills the buffer when it holds less than a frame (§12.2), then slices one frame's worth off it; a shorter slice is a partial frame |
| `remaining`      | the buffer's remaining                                                                                                             |
| `seek`           | seeks nothing, returns 0                                                                                                           |
| `abort_track`    | clears the buffer and adds a track mark                                                                                            |
| self-sync        | computed over the graph inputs (`src/core/clock/spec/pacing.md`, Finding the sync source)                                          |
| effective source | itself                                                                                                                             |

## 10. Clocks

Clock rules are in `src/core/clock/spec/`; this section states only the
arrangement.

| Clock        | Id                    | Sync      | Parent       | Owner                                       | Members                                                   |
| ------------ | --------------------- | --------- | ------------ | ------------------------------------------- | --------------------------------------------------------- |
| output clock | `ffmpeg.filter`       | default   | none         | none                                        | the graph source, unified with whatever consumes it       |
| input clock  | `ffmpeg.filter.input` | `passive` | output clock | `ffmpeg filter graph` / `ffmpeg.filter.<n>` | every graph input, and through them the sources they read |

- The input clock is a sub-clock of the output clock while the graph source
  is awake (`clock.md`, Sub-clocks).
- Only the graph ticks the input clock: while the graph is not built (§12.1)
  and to pull (§12.2). The first readiness test of an unbuilt graph starts
  the input clock when it is not started.
- A track used as a graph input is read in the input clock, so the other
  tracks of that source cannot be used outside the graph. The script-level
  helpers pass every track through the graph for that reason (§17).
- The graph source's self-sync is computed over the graph inputs; sync
  sources upstream of the inputs therefore pace the output clock through it.

Example (from the implementation, a graph made of `sine` and `aecho` only):

```
[clock.ffmpeg.filter:3] Starting top-level clock, sync: auto, sources: ffmpeg.filter (passive), source (passive), output.dummy (output), animated by a task (rests)
[clock.ffmpeg.filter.input:3] Starting nested clock, controlled by ffmpeg filter graph ffmpeg.filter.0, sync: passive, sources:
```

## 11. Generations

### 11.1 Why

FFmpeg filter graphs need the exact format of every buffer source before
they are configured, which is known only from the first frame. A buffer source
that has seen end of stream accepts nothing more and a graph cannot be
reopened (`avfilter.md`, Launch). A graph is therefore built per
**generation**: built once every input knows its format, run, ended when an
input runs dry or changes format, and built again from the same description
when data comes back.

### 11.2 Cells

Every filter instance, buffer source and buffer sink of the description is a
cell: `{generation; value; make}`. Reading a cell returns its value when it
was made in the current generation, and otherwise runs `make` (which attaches
into the current config), stores the result with the current generation and
returns it. Reading a cell when there is no current config fails with
`ffmpeg filter graph read outside of a generation!`. The wiring of the
description is plain closures over cells: linking reads both ends, which
attaches them, so the order of the build list does not matter for attaching.

### 11.3 Build

`init_graph`: when every graph input's readiness test (§8.2, step 3) holds,
build. With no graph input, the test holds trivially.

Build:

1. Increment the generation.
2. Create a fresh avfilter config as the current config.
3. Run the build list in order: the links of each `set_input` (§6.3), the
   link of each output (§9.2), the closure of each `parse` (§15), and last
   the launch closure.
4. Launch closure: log at info `Initializing graph (generation <g>)`, launch
   the config, then hand each buffer source's input function to the graph
   input registered under its instance name, and each buffer sink's output to
   the sink registered under its instance name.
5. When any step raises, mark the graph failed and re-raise.

A graph input given its input function first pushes, in order, the frames it
held while there was no graph (§13.1), then clears them. A sink given its
output computes its conformer (§13.2).

### 11.4 What ends a generation

- **Input runs dry**: the pull loop finds the graph not ready (§12.2).
- **Format change**: a frame being pushed into a graph input has a format
  different from the one its buffer source was built with. Audio compares
  sample format, sample rate and channel layout; video compares the frame
  format, ignoring colour properties when
  `settings.ffmpeg.format_change.color` is `false` and pixel aspect when
  `settings.ffmpeg.format_change.pixel_aspect` is `false`. Only a frame
  being pushed counts: a frame that has merely arrived is behind the ones the
  input still holds back (§13.1).

On a format change, the input:

1. Logs at severe: `The format of <input id> changed in the middle of a
stream, from <old> to <new>: rebuilding the filter graph. Its filters
start over.`, the formats rendered as `<fmt>, <rate> Hz, <layout>` for
   audio and `<w>x<h> <pixfmt>, pixel aspect <n/d|unknown>, colour space <…>,
range <…>, primaries <…>, transfer <…>, chroma location <…>` for video.
2. Drops the frames it held for a graph that was never built.
3. Restarts the graph: ends the input of every graph input (`end_input`,
   §13.1), drains every sink into the graph source's buffer, resets (§11.5).
4. Settles its own format to the new one and tries `init_graph`.

The frame then continues to be pushed: into the new generation when the
build happened, held otherwise.

### 11.5 Reset

1. Drop the current config.
2. Run every registered reset: each graph input forgets its format, forgets
   its input function and its "ended" flag; each sink forgets its output and
   its end-of-stream flag and takes a new stream index.

The duration converter of a graph input is kept across a reset: its
timestamps carry on, which keeps what is pushed into the next generation
monotonic.

### 11.6 Failed graph

A graph marked failed is never ready again (§12.1). The exception of the
failed build propagates from wherever the build ran: a readiness test, a pull
of the input clock, or a format change.

## 12. Pull loop

### 12.1 Graph readiness

`is_ready`:

1. When there is no current config:
   - with at least one graph input: start the input clock when it is not
     started, then tick it once. Ticking makes the inputs pull their sources,
     which can settle formats and build the graph (§13.1);
   - with no graph input: `init_graph` (which builds). This happens at
     streaming time, where output content types are known.
2. Ready when the graph is not failed and every graph input is ready (§8.2,
   step 2).

### 12.2 Filling the buffer

Run when the graph source must produce a frame and its buffer holds less than
`Frame.size`:

1. **Wait for launch.** When some sink has no output yet: if the graph is
   ready, tick the input clock once; if after that every sink has its output,
   go on, otherwise stop filling. If the graph is not ready, stop filling.
2. **Loop.**
   1. Drain every sink into the buffer (§13.2), then check the buffer size
      (§12.3).
   2. If the buffer holds at least `Frame.size`, or every sink has seen end of
      stream, stop.
   3. If the graph is ready, tick the input clock once and loop.
   4. Otherwise end the generation: flush every graph input (§13.1,
      `flush_input`), drain every sink, and when every sink has now seen end
      of stream, reset (§11.5). Stop.

Then the graph source slices a frame's worth (or less) off its buffer.

A generation whose sinks reach end of stream while the inputs stay ready is
not reset by this loop: the loop stops at step 2.2 ([findings](findings/filters.md)).

### 12.3 Divergence check

After each drain, when the buffered length of the buffer (the longest field)
exceeds `settings.ffmpeg.filter_max_buffer` seconds, raise a runtime error of
kind `ffmpeg.filter`, at the graph source's position:

```
Filter graph <id> has buffered more than <max, %.02f>s of data for one of its outputs. This happens when the graph's outputs are consumed at diverging rates, which is not supported. Raise `settings.ffmpeg.filter_max_buffer` if the graph legitimately produces that much on a single pass.
```

One output can run ahead of another while a filter fills its lookahead; the
loop waits on the slowest, so outputs that never converge make the fastest
grow until this check fires.

## 13. Frame flow and timestamps

### 13.1 Input side

On each tick of the input clock, each graph input that is ready takes a frame
of its source and, when the frame has data, sends it:

1. Queue the frame's metadata (§14.1).
2. Take the raw FFmpeg content of the input's field and its chunks: every
   chunk with data, in order, each with its stream index, time base and
   frames ([content.md](content.md)). A frame straddling a track boundary
   carries two chunks.
3. When there is a first frame, call the format check with "arrived" (§11.4):
   it settles the format of this generation when none is known, and tries to
   build.
4. Each frame of each chunk goes through the input's duration converter
   (below); every frame the converter releases is **pushed**:
   1. pending metadata, when any, is set as the frame's metadata, and the
      pending set is emptied;
   2. format check with "pushed" (§11.4);
   3. given to the buffer source when the input has its input function, held
      in order otherwise.

**Duration converter** (shared with the output side). One converter per
`(stream index, time base)`: the duration helper of
[common.md §6](common.md#6-duration-helper) in PTS mode.

- Each frame's PTS gets the converter's offset added. On the input side it is
  also converted from the chunk's time base to the main time base, which is
  the buffer source's time base (§8.3).
- When a chunk arrives with a stream index or time base different from the
  converter's, a new converter is made, lined up with the previous one: the
  previous last timestamp and the previous frame's duration are converted
  into the new time base, `position = last + duration`, `offset = position −
PTS of the new frame` (0 when it has none). The same stream index with a
  different time base also logs at important `Unexpected time_base change!`.
  With no previous converter the offset is 0.

The converter is never reset by a new generation (§11.5).

`flush_input` (the source ran dry, or the input goes to sleep): unless already
ended, push every frame the duration converter holds, then `end_input`.

`end_input`: unless already ended, mark ended and send end of stream to the
buffer source. Ending twice is an FFmpeg error; the flag prevents it. Without
an input function it does nothing.

On sleep, a graph input runs `flush_input`.

### 13.2 Output side

Each sink has a stream index, fresh per generation. Its conformer, computed
when it receives its output, comes from the content format of its field in
the graph source:

- audio: the field's format is merged with the sink's channel layout, sample
  rate and sample format; frames pass unchanged;
- video: the raw video conformer of [content.md §9](content.md#9-raw-video-conformance), which compares
  each frame with the declared format, merges compatible parameters and fits
  incompatible frames with the Fit helper (§16.4).

Draining a sink, until FFmpeg answers "none ready" or "drained":

1. Take the next frame from the buffer sink.
2. Pass it through the conformer.
3. Pass the result through the sink's duration converter, keyed by the
   sink's stream index and the sink's time base, with PTS **not** converted
   (offset applied only).
4. For each released batch of length `L` (main ticks): for each frame, at
   position `p` = current length of the field in the buffer (each released
   frame is at offset 0 of its batch):
   - when its metadata has the key `liquidsoap_track_mark`, add a track mark
     at `p`;
   - when `pass_metadata` is set and the metadata, without that key, is not
     empty, add it at `p` with the timestamps of §14.2.
     Then put one chunk `{length = L; stream index; time base = sink time base;
frames}` with the parameters of the batch's first frame into the field.
5. On "drained": emit the tail (below) and mark the sink at end of stream.

**Tail.** The duration converter holds the last frames back, so at end of
stream the frames it still holds (everything a lookahead filter such as
`loudnorm` was sitting on) are emitted as a batch whose length is the sum of
their durations converted to main ticks (frames without duration count 0).

A new stream index per generation makes the next generation's first chunk
line up after the previous generation's last one (§13.1, converter rule),
because a rebuilt graph starts its timestamps over.

What the encoder does with these chunks is in [timestamps.md](timestamps.md).

## 14. Metadata and track marks

### 14.1 Into the graph

FFmpeg frame metadata is the only channel a graph offers. For each liquidsoap
frame a graph input sends:

1. With `pass_metadata`, the **first** metadata of the frame, as a list of
   pairs; otherwise none.
2. When the frame has a track mark, the pair `("liquidsoap_track_mark",
"1")` in front. Track marks travel regardless of `pass_metadata`.
3. These pairs are merged into the pending set; a key already pending takes
   the new value.

The pending set is attached to the next frame pushed (§13.1, step 4.1),
replacing that frame's own metadata, then emptied.

### 14.2 Out of the graph

A frame leaving a sink whose metadata holds `liquidsoap_track_mark` adds a
track mark to the graph source's buffer (§13.2). With `pass_metadata`, the
remaining keys become liquidsoap metadata, plus:

| Key                               | Value                                                      |
| --------------------------------- | ---------------------------------------------------------- |
| `lavfi.liq.pts`                   | the frame's PTS (offset applied), sink time base → seconds |
| `lavfi.liq.duration`              | the frame's duration, in seconds                           |
| `lavfi.liq.best_effort_timestamp` | the frame's best-effort timestamp, in seconds              |

Each is present when the frame has that value, rendered with OCaml
`string_of_float`. A frame whose metadata is empty after removing the track
mark key adds no metadata, timestamps included.

Example (from the implementation: `sine()` → `ffmpeg.raw.encode.audio` →
`ebur128(metadata=true)`, first two metadata seen downstream):

```
[("lavfi.liq.best_effort_timestamp", "0."), ("lavfi.liq.duration", "0.1"), ("lavfi.liq.pts", "0."), ("lavfi.r128.I", "-70.000"), ("lavfi.r128.LRA", "0.000"), ("lavfi.r128.LRA.high", "0.000"), ("lavfi.r128.LRA.low", "0.000"), ("lavfi.r128.M", "-120.691"), ("lavfi.r128.S", "-120.691")]
[("lavfi.liq.best_effort_timestamp", "0.1"), ("lavfi.liq.duration", "0.1"), ("lavfi.liq.pts", "0.1"), ("lavfi.r128.I", "-70.000"), …]
```

A track boundary on any output is a track boundary of the whole graph
source.

## 15. `ffmpeg.filter.parse`

### 15.1 Signature

```
ffmpeg.filter.parse :
  (ffmpeg.filter.graph, description : string,
   ?inputs : {audio : [string * ffmpeg.filter.audio],
              video : [string * ffmpeg.filter.video]},
   ?outputs : {audio : [string], video : [string]}) ->
  {audio : [string * ffmpeg.filter.audio],
   video : [string * ffmpeg.filter.video]}
```

(from the implementation). Category `Source / FFmpeg filter`, flag `extra`.
`inputs` and `outputs` default to `{audio = [], video = []}`. `description`
uses FFmpeg's filtergraph syntax.

- `inputs`: `(label, pad value)` pairs; each pad value feeds the
  description's open input of that label.
- `outputs`: labels of the description's open outputs to hand back.
- The result: for each output label, in order, `(label, pad value)`.

### 15.2 Steps

At the call (after the scope check, §7.3), one closure is added to the build
list. At each build it:

1. For each audio output label `l`, attaches an `aformat` with no argument,
   named `parse_aformat_out_<l>_<k>`; for each video output label, a
   `format` with no argument, named `parse_format_out_<l>_<k>` (counter as
   §4.4, so names change per generation). These pass-through filters give
   the returned pad values a fixed instance to read.
2. Reads every input pad value (attaching its instance). A value that is not
   an output pad fails with `ffmpeg.filter.parse: input pads must be output
pads from previous filters`.
3. Calls the bindings' `parse` (`avfilter.md`, Parse) with: as the
   description's outputs, the input pads of the pass-through filters, under
   their labels; as the description's inputs, the given pads, under their
   labels.
4. Stores the pass-through filters for this generation.

Each returned pad value reads, at build time, the output pad of the
pass-through filter of its label in the current generation.

A filter of the description is not a cell: it is created anew by each
generation's parse, so it has no `process_command`.

### 15.3 Immediate build

After adding its closure, `parse` calls `init_graph` (§11.3). With a graph
input already created, its format is unknown and nothing happens. With no
graph input created before the call, the graph is built at that point,
during the script's description, before the outputs and the launch closure
are registered. That generation is never launched and the graph produces
nothing (from the implementation: a graph made of
`parse(description="sine=frequency=220:duration=5[a_out]")` and one output,
decoded and written for 2 s, writes an empty file; the graph source reports
ready throughout).

## 16. Helper graphs

Private, launched graphs other parts of the layer build for one stream. None
is visible to scripts. Each is a source, zero or more filters and a sink;
`convert` pushes a frame and calls back with every frame ready, `eof` pushes
end of stream and calls back with the rest.

### 16.1 Display

Shows decoded video upright.

The display stage (creation, `convert`, `eof`, `expected_size`,
`pixel_aspect`, the undecided handler) is described in
[side-data.md §3](side-data.md#3-autorotate-on-decoding). Users: every decoder path that turns
decoded frames into content (the decoders' raw, internal, image and builtin
decode paths; `expected_size` in the file decoder).

### 16.2 Fps

Retimes video to a constant frame rate.

`init ?start_pts ~width ~height ~pixel_format ~time_base ?pixel_aspect
?source_fps ?color_range ~target_fps`:

- `source_fps = target_fps`: pass-through, time base `time_base`.
- Otherwise a graph:
  1. `buffer` named `buffer`, arguments in this order: `frame_rate =
source_fps/1` (when given), `range = <colour range name>` (when given),
     `video_size = <w>x<h>`, `pix_fmt = <id>`, `time_base`, `pixel_aspect`
     (when given). No colour space is passed.
  2. `setpts` named `setpts`: `expr = "<start_pts>+PTS-STARTPTS"` with
     `start_pts`, `expr = "PTS-max(STARTPTS, 0)"` without. The decoder passes
     no `start_pts`: negative start timestamps (to be skipped) are kept and a
     positive start (a partial copy dump) is realigned to 0. The encoder
     passes one and realigns every timestamp to it.
  3. `fps` named `fps`: `fps = target_fps/1`, plus `start_time = 0` (first)
     when there is no `start_pts`.
  4. `buffersink`; launched; the time base is the sink's.
     A missing `setpts` or `fps` filter fails with `Could not find setpts ffmpeg
filter!` / `Could not find fps ffmpeg filter!`.

`of_frame_format ~format ~time_base ~target_fps`: `init` from a frame format
(width, height, pixel format, pixel aspect, colour range unless unspecified),
without `source_fps`, so always a graph.

`convert`: pass-through calls back with the frame; a graph pushes it and
calls back until "none ready" ("drained" propagates). `eof`: pass-through
does nothing; a graph pushes end of stream and calls back until "none ready"
or "drained".

Users: the encoders' video paths ([encoder-internal.md §4.6](encoder-internal.md#46-frame-rate-conversion-both-video-modes),
[inline-codecs.md §7](inline-codecs.md#7-video-encoding)) and the decoders' common video path
([decoder-internal.md §2.2](decoder-internal.md#22-internal-video-converter)).

### 16.3 AFormat

Converts audio sample format, layout and rate.

`init ?dst_sample_format ?dst_channel_layout ?dst_sample_rate
~src_sample_format ~src_channel_layout ~src_sample_rate ~src_time_base`:
each destination defaults to its source. When all three are equal (layouts
compared by the bindings), pass-through with `src_time_base`. Otherwise:

1. `abuffer` named `abuffer`: `sample_fmt = <name>`, `channel_layout = <mask>`
   (default layout's mask for the channel count when the layout has none),
   `sample_rate`, `time_base = src_time_base`.
2. `aformat` named `aformat`: `sample_fmts`, `channel_layouts`,
   `sample_rates` from the destination, same renderings. Missing filter:
   `Could not find aformat ffmpeg filter!`.
3. `abuffersink`; launched; the time base is the sink's.

`convert` and `eof` as Fps.

Users: the raw decoder and raw audio content conversion
([decoder.md](decoder.md), [content.md](content.md)).

### 16.4 Fit

Fits video into a picture of another size and pixel format, keeping
proportions and padding the rest. Timestamps are kept.

Target: `{width; height; pixel_format; pixel_aspect option}`.

`fitted_size ?pixel_aspect ~width ~height source` (destination pixel aspect
`pixel_aspect`, square by default; source pixel aspect, square when
unknown):

```
num = source.width  × source_aspect.num × pixel_aspect.den
den = source.height × source_aspect.den × pixel_aspect.num
if num × height ≤ width × den then (max 1 (height × num / den), height)
else                              (width, max 1 (width × den / num))
```

in integer arithmetic, division truncating.

Filters, for a source format and a target, with `known_aspect` the target's
pixel aspect or else the source's:

| Filter   | Arguments                                                                                          |
| -------- | -------------------------------------------------------------------------------------------------- |
| `scale`  | `w`, `h` = `fitted_size ?pixel_aspect:known_aspect`, `flags` = `settings.ffmpeg.scaling_algorithm` |
| `pad`    | `w` = target width, `h` = target height, `x = "(ow-iw)/2"`, `y = "(oh-ih)/2"`                      |
| `setsar` | `sar` = `known_aspect`, `0/1` when unknown                                                         |
| `format` | `pix_fmts` = target pixel format name                                                              |

The chain is built with the bindings' `video_chain` at the main time base.
A Fit holds at most one chain; `convert` reuses it when the target is equal
and the frame's format is the same (with the ignores of §2), and builds a new
one otherwise. `convert` pushes the frame and calls back until "none ready"
or "drained". There is no `eof`: these filters hold no frames.

`scale` does not carry the chroma location over, so a consumer built for the
frames before the fit sees one more format change.

Users: the raw video conformer ([content.md §9](content.md#9-raw-video-conformance)), which graph sinks
use (§13.2); the raw video encoder ([encoder-internal.md §4.5](encoder-internal.md#45-video-raw));
`fitted_size` in the decoders' common path.

## 17. Script definitions

### 17.1 `ffmpeg.filter.audio_video.output`

Defined when `ffmpeg.filter.video.output` exists.

```
ffmpeg.filter.audio_video.output :
  (?id : string?, ffmpeg.filter.graph,
   'b.{tracks : () -> 'a.{audio : ffmpeg.filter.audio}},
   'd.{tracks : () -> 'c.{video : ffmpeg.filter.video}}) ->
  source(audio=ffmpeg.audio.raw('e), video=ffmpeg.video.raw('f))
```

(from the implementation). Applies `ffmpeg.filter.audio.output` to the
`audio` of the first argument's tracks and `ffmpeg.filter.video.output` to the
`video` of the second's, and returns `source(id=id, {audio, video})`.

### 17.2 `video.add_text.ffmpeg` family

Defined when `ffmpeg.filter.drawtext` exists.

`video.add_text.ffmpeg.raw.filter(~color=0xffffff, ~cycle=true, ~font=null,
~metadata=null, ~size=18, ~speed=70, ~x=getter(10), ~y=getter(10), ~graph,
d=getter(""), v)`, with `v` a video pad value:

1. `color` becomes `"0x"` followed by 6 hexadecimal digits.
2. When `speed ≠ 0`, `x` becomes a getter that, on each read, moves left by
   `speed × elapsed seconds since the previous read` (truncated), restarts
   from the getter `x` when it changed, and wraps to
   `settings.frame.video.width() − x` when negative.
3. A `drawtext.create` with `fontfile = font`, `fontsize = "<size>"`, `x`,
   `y` the getters' current values as strings, `fontcolor = color`, `text` the
   current text.
4. With `cycle`, a second `drawtext.create` with the same arguments, whose
   command `x` is `min(<x>-w,<x>-text_w)`.
5. Text sent in commands is escaped: each of `'`, `%`, `\`, `:`, `{`, `}`
   is preceded by `\`.
6. `on_change()`: reads `x`, `y` and `d`; when the triple changed since the
   previous call, sends to each drawtext `process_command("reinit",
"x=<x>:y=<y>:text=<escaped text>")`.
7. `on_metadata(m)`: when `metadata` is set and `m[metadata]`, escaped, is
   not empty, logs at level 3 under `ffmpeg.filter.drawtext` `Setting new
text <text> from metadata <key>` and sends `reinit` with `text=<text>` to
   each drawtext.
8. Chains the drawtexts with `set_input` from `v` and returns the last output
   with methods `on_change` and `on_metadata`.

`video.add_text.ffmpeg.raw(…, ~graph, d, s)`: registers synchronous
`on_frame` and `on_metadata` callbacks on `s`, makes a video graph input of
`s`'s video track, applies `raw.filter`, points the callbacks at its
`on_change` / `on_metadata` (so commands are sent once per frame of `s`), and
returns `ffmpeg.filter.null` of the result.

`video.add_text.ffmpeg(~id=null, …, ~duration=null, d, s)`:

1. `s` is encoded to raw FFmpeg audio and video (`ffmpeg.raw.encode.audio_video`).
2. In a graph: the video through `raw`; the audio through an `acopy`; both
   outputs; returns `source(id=id, {audio, video, metadata, track_marks})`
   taking metadata and track marks from the audio output.
3. Decodes back (`ffmpeg.raw.decode.audio_video`).
4. With `duration`, `max_duration(duration, s)`.

The default `id` is `video.add_text.ffmpeg`. The audio goes through the graph
so that both tracks of `s` live in the input clock (§10).

### 17.3 Autocue

`autocue.internal.ebur128` (when `ffmpeg.filter.ebur128` exists) runs, on a
request processed with `request.process`, the graph `audio.input` →
`ebur128(metadata=true)` (first audio output) → `asetpts(expr="N/SR/TB")` →
`audio.output(id="filter_output")`, collecting every metadata of the result.
`asetpts` rewrites PTS from the sample count. Reason (code comment): "ebur
filter seems to generate invalid PTS". Without `ebur128` it logs at level 2 under
`autocue.internal` `ffmpeg.filter.ebur128 is not available, autocue
disabled!` and returns `[]`.

## 18. Failure summary

| Situation                                        | When                       | Result                                                                              |
| ------------------------------------------------ | -------------------------- | ----------------------------------------------------------------------------------- |
| graph value used after `create`                  | script time                | error 7 `Graph variables cannot be used outside of ffmpeg.filter.create!`           |
| option value cannot be converted                 | script time                | error 7 `Invalid value`                                                             |
| option below min / above max / not a named value | script time                | error 7, §5.4                                                                       |
| FFmpeg rejects the arguments                     | script time (probe attach) | the bindings' error, as a runtime error                                             |
| `set_input` twice                                | script time                | error kind `ffmpeg.filter`, `Filter input already set!`                             |
| dynamic input list too short                     | build                      | error 7 `Invalid number of input for filter <name>`                                 |
| link fails                                       | build                      | error kind `ffmpeg.filter`, `Error while connecting filter elements …`              |
| `parse` input value not an output pad            | build                      | failure `ffmpeg.filter.parse: input pads must be output pads from previous filters` |
| parse or launch fails in FFmpeg                  | build                      | the bindings' error; graph failed, never ready again                                |
| `process_command` outside a generation           | any                        | returns `graph not started!`                                                        |
| `process_command` fails                          | any                        | runtime error kind `ffmpeg.filter`                                                  |
| non-monotonic PTS into or out of the graph       | streaming                  | log important, content-invalid exception                                            |
| format change on an input                        | streaming                  | log severe, graph rebuilt, filter state lost                                        |
| outputs diverge beyond `filter_max_buffer`       | streaming                  | runtime error kind `ffmpeg.filter` (§12.3)                                          |
| `json` of a graph or pad value                   | any                        | error kind `json`                                                                   |
| comparing two pad values                         | any                        | runtime error kind `invalid`, `compare: functional value`                           |
