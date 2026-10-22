# FFmpeg filter graphs

This file owns how libavfilter filters reach scripts: the `ffmpeg.filter.*`
builtins generated from FFmpeg's filter registry, `ffmpeg.filter.create` and
the life of a graph (description, generations, build, end, drain), graph
inputs and outputs as sources, the clocks a graph runs in, frame flow,
timestamps and metadata through a graph, `ffmpeg.filter.parse`, the private
helper graphs (Fps, AFormat, Fit) other parts of the layer use, and the
script definitions built on them. It relies on the avfilter bindings
([avfilter.md](../../../../modules/synced/ffmpeg/spec/avfilter.md)), option
introspection (`avutil.md` §4.15), the clock rules
([clock.md](../../../clock/spec/clock.md)), draining
([draining.md](draining.md)), units and the duration helper
([common.md](common.md)), raw content ([content.md](content.md)) and source
identity ([content.md §5](content.md#5-stream_idx-and-time_base)). Bitstream
filters are in [bitstream-filters.md](bitstream-filters.md).

## 1. Vocabulary

| Term           | Meaning                                                                                                                                    |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| graph value    | the script value of type `ffmpeg.filter.graph` handed to the function given to `ffmpeg.filter.create`                                      |
| pad value      | a script value of type `ffmpeg.filter.audio` or `ffmpeg.filter.video`: designates one output pad of one element of the description         |
| description    | everything the script states with a graph value: filter instances, graph inputs and outputs, links, parsed descriptions                    |
| probe config   | an avfilter graph into which the description is replayed while the script states it, to learn pad counts and report errors; never launched |
| generation     | one avfilter graph built from the description, launched, run, drained, then released                                                       |
| graph input    | the operator made by `ffmpeg.filter.audio.input`/`video.input`: an output, in the liquidsoap sense, feeding one buffer source              |
| sink           | the liquidsoap end of one buffer sink, made by `ffmpeg.filter.audio.output`/`video.output`                                                 |
| graph source   | the single liquidsoap source that carries every output of one graph, one field per output                                                  |
| main time base | `1/main_rate` ([common.md §4](common.md#4-liquidsoap-time-bases))                                                                          |
| timeline       | the positions, in main ticks, of what a graph input has pushed or a graph output has emitted since the graph source last woke up           |

## 2. Settings

| Key                                 | Type  | Default | Effect                                                     |
| ----------------------------------- | ----- | ------- | ---------------------------------------------------------- |
| `settings.ffmpeg.filter_max_buffer` | float | `10.`   | seconds of media one graph may hold at each point of §12.3 |

`filter_max_buffer` protects memory and the duration of one clock tick. It
bounds, in seconds of media: what one output of the graph source buffers,
what a graph input holds before the first build, what one fill pushes into a
generation, and what one drain reads from a sink (§12.3).

Also read here, owned by [common.md §1](common.md#1-settings):
`settings.ffmpeg.format_change.*` (§11.4, §16) and
`settings.ffmpeg.scaling_algorithm` (§16.4).

Logs of this area go to the label `ffmpeg.filter`.

## 3. Script values

Three abstract types, all printed by their name and none serialisable:

| Type                  | Printed as            | `json.stringify`                                                                    |
| --------------------- | --------------------- | ----------------------------------------------------------------------------------- |
| `ffmpeg.filter.graph` | `ffmpeg.filter.graph` | raises error kind `json`, "Ffmpeg filter graph cannot be represented as json"       |
| `ffmpeg.filter.audio` | `ffmpeg.filter.audio` | raises error kind `json`, "Ffmpeg filter audio input cannot be represented as json" |
| `ffmpeg.filter.video` | `ffmpeg.filter.video` | raises error kind `json`, "Ffmpeg filter video input cannot be represented as json" |

The messages are examples; their wording is not binding.

Comparison of two such values is by identity. Each value receives a creation
number, unique in the process, when it is made. Two values are equal when
they are the same value; ordering compares creation numbers. Comparison never
raises.

A pad value designates an element of the description, not a pad of one
generation: reading it during a build returns the output pad of that element
in the config being built (§11.2). A graph value is usable only during the
call of `ffmpeg.filter.create` that made it (§7.3).

Conformance:

- `a == b` on two distinct outputs of `ffmpeg.filter.sine` is `false`;
  `a == a` is `true`.

## 4. Filter builtins

### 4.1 Which filters

At start-up, every filter of the bindings' registry gets two builtins. The
registry excludes `abuffer`, `buffer`, `abuffersink` and `buffersink`
([avfilter.md §4.4](../../../../modules/synced/ffmpeg/spec/avfilter.md#44-registry));
these four have no builtin. Graph inputs and outputs (§8, §9) are the only
way to create buffer sources and sinks.

### 4.2 Names and documentation

For a filter `<name>` with description `<d>` (trailing period removed):

| Builtin                       | Category                 | Flags   | Description                                                                                                                                     |
| ----------------------------- | ------------------------ | ------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `ffmpeg.filter.<name>`        | `Source / FFmpeg filter` | `extra` | `Ffmpeg filter: <d>.` followed by the dynamic-pad sentences below                                                                               |
| `ffmpeg.filter.<name>.create` | `Source / FFmpeg filter` | `extra` | the same text, then `Use this operator to initiate the filter independently of its inputs, to be able to send commands to the filter instance.` |

The names are frozen. The descriptions are documentation: each sentence ends
with exactly one period.

Dynamic-pad sentences, appended after a space, in this order when they apply:

- dynamic inputs: `This filter has dynamic inputs: last two arguments are lists of audio and video inputs. Total number of inputs is determined at runtime.`
- dynamic outputs: `This filter has dynamic outputs: returned value is a tuple of audio and video outputs. Total number of outputs is determined at runtime.`

### 4.3 Signature

`ffmpeg.filter.<name>` takes, in order:

1. One optional labelled argument per option name (§5), each of type `T?`,
   default `null`.
2. The graph, unlabelled, of type `ffmpeg.filter.graph`.
3. The inputs, unlabelled (§6.1).

It returns the outputs (§6.2).

`ffmpeg.filter.<name>.create` takes items 1 and 2 only and returns `unit`
with three methods (§6.3).

Example (`liquidsoap -h`; the order of the labelled arguments is not
binding):

```
ffmpeg.filter.aecho :
  (?in_gain : float?, ?out_gain : float?, ?delays : string?, ?decays : string?,
   ffmpeg.filter.graph, ffmpeg.filter.audio) -> ffmpeg.filter.audio

ffmpeg.filter.aecho.create :
  (?in_gain : float?, ?out_gain : float?, ?delays : string?, ?decays : string?,
   ffmpeg.filter.graph) -> unit.{output : ffmpeg.filter.audio,
   process_command : (?fast : bool, string, string) -> string,
   set_input : (ffmpeg.filter.audio) -> unit}

ffmpeg.filter.amerge :
  (?inputs : int?, ?layout_mode : int?, ffmpeg.filter.graph,
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

`create`:

1. Checks the graph value is in scope (§7.3).
2. Validates and renders the arguments (§5.4).
3. Adds a filter instance to the description, with those arguments, and
   attaches it to the probe config under an instance name unique within the
   config. This settles the pad counts of filters whose pads appear only
   once attached. A failed attach (FFmpeg rejects an argument, the filter is
   unknown) raises a runtime error of kind `ffmpeg.filter` at the script
   position, carrying FFmpeg's message.
4. Returns the value with methods.

## 5. Arguments from AVOptions

### 5.1 Which options

The options are those the bindings list for the filter's option class, child
classes included (`avutil.md` §4.15). Each distinct option name gives one
argument of that name. When a class and a child class declare an option of
the same name, the first one listed (the filter's own class comes first)
gives the argument its type and documentation, and the argument's pair is
passed once (§5.4). FFmpeg then sets that name on the filter, searching
child classes as it does for any filter argument.

Arguments SHOULD be listed in FFmpeg's declaration order.

### 5.2 Types

| AVOption constructor                                            | Script type | Value to option                                 | Option value to text                      |
| --------------------------------------------------------------- | ----------- | ----------------------------------------------- | ----------------------------------------- |
| `Int`                                                           | `int`       | the integer                                     | decimal                                   |
| `Flags`, `Int64`, `UInt64`                                      | `int`       | the integer as 64-bit                           | decimal                                   |
| `Duration`                                                      | `int`       | a number of seconds                             | decimal seconds                           |
| `Float`, `Double`                                               | `float`     | the float                                       | a decimal text FFmpeg parses back exactly |
| `Rational`                                                      | `string`    | `"num/den"`, split on `/`, each part an integer | `num/den`                                 |
| `Bool`                                                          | `bool`      | the boolean                                     | `true` / `false`                          |
| `String`, `Binary`, `Dict`, `Image_size`, `Video_rate`, `Color` | `string`    | the string                                      | verbatim                                  |
| `Pixel_fmt`                                                     | `string`    | pixel format of that name                       | its name, `none` when it has none         |
| `Sample_fmt`                                                    | `string`    | sample format of that name                      | its name, `none` when it has none         |
| `Channel_layout`                                                | `string`    | channel layout found from the string            | its description                           |
| `Array` of any of the above                                     | `[T]`       | each element as above                           | each element as above                     |

Every argument has type `T?` and default `null`. `null` means the option is
not passed: FFmpeg's default applies.

**Durations.** FFmpeg stores a duration option in microseconds and parses
an unsuffixed number in its option text as seconds. The script value is a
number of seconds, rendered as such. The default, minimum and maximum the
bindings report are in microseconds: the documentation (§5.3) and the range
check (§5.4) convert them to seconds. A sub-second duration is written in
the textual form of `ffmpeg.filter.parse` (§15), for example `0.5`.

### 5.3 Documentation of an argument

Built from the option's help, default and named values, all rendered as
script values (§5.2 "option value to text"; a duration in seconds). A named
value prints as `<value> (<name>)`.

| help | default | named values | Text                                                                                                                  |
| ---- | ------- | ------------ | --------------------------------------------------------------------------------------------------------------------- |
| yes  | none    | none         | `<help>`                                                                                                              |
| yes  | any     | any          | `<help>. (<parts>)`, parts among `default: <d>` and `possible values: <v1>, <v2>…`, joined by `, `, the default first |
| no   | none    | some         | `Possible values: <v1>, …`                                                                                            |
| no   | some    | none         | `Default: <d>`                                                                                                        |
| no   | some    | some         | `Default: <d>, possible values: <v1>, …`                                                                              |
| no   | none    | none         | no text                                                                                                               |

An array option shows no default and no named values. For a `Flags` option
the named values are the flags, and the text says that they combine.

Example: `layout_mode : int? (default: null) method used to determine the
output channel layout. (default: 0, possible values: 0 (legacy), 1 (reset),
2 (normal))`.

### 5.4 Validation and rendering

Run when the filter is applied, for each argument that is not `null`:

1. Convert the script value ("value to option"). A failure raises error 7
   (invalid value) on the argument.
2. When the option has a minimum and the value is below it, or a maximum and
   the value is above it: invalid value, naming the option and the bound.
3. **Flags**: every bit set in the value MUST belong to at least one named
   flag; otherwise invalid value, naming the option and its flags. Any
   combination of named flags is accepted.
4. **Other options with named values**: when the option declares a range,
   every value in the range is accepted; the named values are special
   values within it, not a restriction. When the option declares no range,
   the value MUST be one of the named values.
5. Render to an argument pair `name=value`:
   - when the option's default is the largest 64-bit integer and the value
     is the largest script integer, the pair carries the largest 64-bit
     integer; likewise for the smallest. A script integer cannot hold
     FFmpeg's `INT64_MAX`/`INT64_MIN` sentinels, and this lets a script ask
     for them;
   - otherwise the pair carries the value's text (§5.2);
   - an array carries the list of its elements' texts, which the bindings
     join with the filter's array separator
     ([avfilter.md §11.2](../../../../modules/synced/ffmpeg/spec/avfilter.md#112-argument-string)).

Array elements are converted and range-checked one by one; step 4 does not
apply to them.

An argument whose name is declared twice (§5.1) gives one pair. Pair order
carries no meaning: every pair is `key=value`.

Examples of messages (wording not binding): `aecho(in_gain=2.)` raises
`in_gain must be less than 1.`.

Conformance:

- `scale(sws_flags=6)` (`fast_bilinear` and `bilinear`) is accepted; a
  value with a bit that is no flag is rejected at the script position.
- An `Int` option with named value `-1 (auto)` and range `-1..N` accepts
  `5`.
- `sine(duration=1)` produces one second of audio; its documented default
  reads `0` seconds.
- `ffmpeg.filter.scale` lists each option name once.

## 6. Inputs, outputs and methods of a filter builtin

### 6.1 Inputs

The **parameters** of the signature come from the unattached filter:

- filter flagged dynamic inputs: two parameters, `[ffmpeg.filter.audio]` then
  `[ffmpeg.filter.video]`;
- otherwise one parameter per static input pad, audio pads first, then video
  pads, of type `ffmpeg.filter.audio` / `ffmpeg.filter.video`.

### 6.2 Outputs

From the instance attached to the probe config:

- filter flagged dynamic outputs: a pair `([ffmpeg.filter.audio], [ffmpeg.filter.video])`,
  one element per output pad of the attached instance, audio then video;
- otherwise one pad value when the filter has exactly one output pad, a tuple
  of pad values (audio first) when it has several, the empty tuple when it
  has none.

Output pad `i` of an instance designates pad `i` of that instance in
whichever config is being built.

### 6.3 Methods of `create`

| Method            | Type                                       | Behaviour                                                                     |
| ----------------- | ------------------------------------------ | ----------------------------------------------------------------------------- |
| `output`          | as §6.2                                    | the instance's outputs                                                        |
| `set_input`       | the input parameters of §6.1 → `unit`      | adds the instance's input links to the description (below)                    |
| `process_command` | `(?fast : bool, string, string) -> string` | sends `command`, `argument` to the instance of the running generation (below) |

`set_input` adds one link per input pad `i` of the attached instance (audio
pads, then video pads), from:

- with dynamic inputs, element `i` of the audio list (for audio pads) or of
  the video list (for video pads);
- otherwise the unlabelled argument at position `i`, counted over audio pads
  then video pads.

Each link is made in the probe config at the call. At the call:

- a list whose length differs from the instance's pad count of that media
  raises error 7 on the list, naming the filter, the expected and the given
  count;
- a second `set_input` on one instance raises error kind `ffmpeg.filter`
  (`Filter input already set!`, example wording) at its first argument;
- a link FFmpeg refuses raises error kind `ffmpeg.filter` at the position of
  the source pad value, naming the ends in the direction of the data:
  `<source instance>:<pad>` to `<destination instance>:<pad>`, with FFmpeg's
  message.

`process_command` sends the command to the instance in the running
generation, with FFmpeg's fast flag when `fast` is `true` (default `false`),
and returns FFmpeg's answer. Each bindings call on the generation (a push,
a pull, a command) is serialised with the others, so a command waits for
the call in progress rather than failing on the bindings' in-use guard. The
serialisation covers one call at a time and is never held across an input
tick: a command issued from inside a fill (a frame callback, §17.2) runs
between two pushes. When no generation runs it returns
`graph not started!` and sends nothing. When FFmpeg refuses the command,
`process_command` logs a warning and returns FFmpeg's error message as its
answer; it never raises. Reason: commands are typically sent from a
source's frame callback (§17.2), and an error raised there would raise out
of a streaming source ([faults.md §1](faults.md#1-why)).

A command changes the running instance only. A new generation starts every
instance from its creation arguments; the warning of §11.4 says so. A script
that needs a setting to survive a rebuild sends it again, for instance on
each frame (§17.2 does).

## 7. `ffmpeg.filter.create`

### 7.1 Signature

```
ffmpeg.filter.create : (((ffmpeg.filter.graph) -> 'a)) -> 'a
```

Category `Source / FFmpeg filter`.

### 7.2 Steps

1. Create the probe config and an empty description.
2. Apply the function to the graph value. Everything the script states with
   the graph happens here, and each statement is replayed into the probe
   config at its script position: filter attaches (§4.4), links (§6.3),
   graph inputs (§8), graph outputs (§9), parsed descriptions (§15). In the
   probe, a buffer source is attached with placeholder parameters of its
   media (any valid format): the probe is never launched, so the
   placeholder is never negotiated.
3. Check the description is complete: every input pad and every output pad
   of every instance in the probe config is linked. An unlinked pad raises
   error kind `ffmpeg.filter` at the position of the `create` call, naming
   the instance and the pad.
4. Create the output clock and the input clock (§10). Unify the clock of
   each graph input with the input clock, and the clock of the graph source,
   when there is one, with the output clock.
5. Release the probe config: from now on the graph value is out of scope.
6. Return the function's result.

The first generation is built during streaming (§12.2), never at script
time.

Reason for the replay: every error that depends only on the description (an
argument, a pad count, a link, a description's syntax, a dangling pad) is
then reported at the script position, before anything streams. Only errors
that depend on the media formats remain for build time.

### 7.3 Scope

Every builtin taking a graph value (filters, inputs, outputs, `parse`) first
checks that the graph value is in scope. Otherwise it raises error 7 on the
graph value: `Graph variables cannot be used outside of ffmpeg.filter.create!`
(example wording).

### 7.4 Conformance

- A `.create` whose output is never consumed fails at script time, naming
  the instance and pad.
- A dynamic-input filter given more or fewer inputs than its pad count fails
  at script time.
- An invalid `parse` description fails at script time.
- `parse(description="sine=frequency=220:duration=5[a_out]")` with one
  output, written for 2 s, writes 2 s of audio.

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

`id` defaults to `ffmpeg.filter.audio.input` / `ffmpeg.filter.video.input`;
`pass_metadata` (default `true`) passes liquidsoap metadata into the graph
(§14). The last argument is a track; its format is unified with raw FFmpeg
content with default raw parameters, so its parameters unify with the
upstream source's ([content.md §3](content.md#3-parameters)).

### 8.2 What it creates

1. A graph input operator for the track's source and field: an output, in
   the liquidsoap sense, fallible, not registered on telnet, started with
   the graph, in the input clock (§10). It takes a frame only between its
   wake-up and its sleep, and when its source is ready. Its self-sync is its
   source's.
2. A buffer source in the description, whose arguments (§8.3) come from the
   input's format for the generation being built.
3. Returns a pad value designating the buffer source's only output pad.

### 8.3 Buffer-source arguments

Every buffer source of the layer, here and in the helper graphs (§16), is
configured by one rule from a frame format and a time base, so that two
places never describe one format differently:

- audio (`abuffer`): sample rate, time base, channel layout (its mask; a
  layout with no mask uses the default layout's mask for its channel count),
  sample format;
- video (`buffer`): the bindings' buffer arguments for the frame format
  ([avfilter.md §4.12](../../../../modules/synced/ffmpeg/spec/avfilter.md#412-utils)),
  colour properties included, and the time base.

For a graph input the time base is the main time base.

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

They are track operators: they return a track of the graph source. `id`
names the graph source; when several outputs of one graph give an `id`, the
last one evaluated names it. `pass_metadata` (default `true`) passes frame
metadata out of the graph (§14). Both are in category
`Track / FFmpeg filter`.

### 9.2 What an output creates

1. The graph source, on the first output of the graph (§9.3). A graph with
   no output has no graph source and never runs.
2. A field of the graph source: the `n`-th audio output of the graph (from 0) gets field `audio` for `n = 0` and `audio_<n+1>` otherwise (`audio`,
   `audio_2`, `audio_3`, …), likewise `video`, `video_2`, … for video.
3. A sink for that field (§13.2).
4. A buffer sink in the description, linked from the given pad value.
5. Returns the field of the graph source.

### 9.3 The graph source

One source per graph, fallible, carrying every output as a field of one
buffer. All outputs advance together: one buffer, one set of track marks,
one readiness.

| Aspect        | Behaviour                                                                                                |
| ------------- | -------------------------------------------------------------------------------------------------------- |
| readiness     | §12.1                                                                                                    |
| frame         | fills the buffer (§12.2), then takes up to one frame's worth from it                                     |
| `remaining`   | the buffer's remaining                                                                                   |
| `seek`        | seeks nothing, returns 0                                                                                 |
| `abort_track` | clears the buffer and adds a track mark                                                                  |
| self-sync     | computed over the graph inputs ([pacing.md §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)) |
| wake-up       | registers the input clock as a sub-clock of the output clock; timelines start at 0                       |
| sleep         | releases the running generation without draining (§11.4), then deregisters the input clock               |

## 10. Clocks

Clock rules are in [clock.md](../../../clock/spec/clock.md); this section
states the arrangement.

| Clock        | Sync      | Parent       | Owner     | Members                                                   |
| ------------ | --------- | ------------ | --------- | --------------------------------------------------------- |
| output clock | default   | none         | none      | the graph source, unified with whatever consumes it       |
| input clock  | `passive` | output clock | the graph | every graph input, and through them the sources they read |

- The input clock is a sub-clock of the output clock while the graph source
  is awake ([clock.md §10](../../../clock/spec/clock.md#10-sub-clocks)).
- The graph source is the input clock's only reader. It ticks the input
  clock only while filling its buffer (§12.2), inside a tick of the output
  clock. A readiness test never ticks.
- A track used as a graph input is read in the input clock, so the other
  tracks of that source are in the input clock too and cannot be used
  outside the graph. The script-level helpers pass every track through the
  graph for that reason (§17).
- The graph source's self-sync is computed over the graph inputs; a sync
  source upstream of the inputs therefore paces the output clock through it.

Example log (wording not binding), a graph made of `sine` and `aecho` only:

```
[clock.ffmpeg.filter:3] Starting top-level clock, sync: auto, sources: ffmpeg.filter (passive), source (passive), output.dummy (output), animated by a task (rests)
[clock.ffmpeg.filter.input:3] Starting nested clock, controlled by ffmpeg filter graph ffmpeg.filter.0, sync: passive, sources:
```

## 11. Generations

### 11.1 Why

FFmpeg filter graphs need the exact format of every buffer source before
they are configured, and that format is known only from the first frame. A
buffer source that has seen end of stream accepts nothing more, and a graph
cannot be reopened
([avfilter.md §4.11](../../../../modules/synced/ffmpeg/spec/avfilter.md#411-launch)).
A graph is therefore built per **generation**: built once every input knows
its format, run, ended (§11.4), drained (§11.7), and built again from the
same description when data comes back.

### 11.2 Replaying the description

A generation is built by replaying the description into a fresh avfilter
config: each filter instance, buffer source and buffer sink is attached once
in that config, with its creation arguments (buffer sources with the
arguments of §8.3 for this generation's formats), every link is made, and
every parsed description is parsed (§15). A pad value read during the replay
resolves to the pad of its element in the config being built. Reading a pad
value outside a build or a script-time replay is a programming error.

### 11.3 Build

A build happens only inside a fill (§12.2), when there is no generation and
every graph input knows its format for the next generation (§13.1). With no
graph input, that condition holds.

1. Replay the description into a fresh config (§11.2).
2. Launch the config. Log at info `Initializing graph (generation <g>)`
   (example wording), `g` counting generations of the graph from 1.
3. Hand each buffer source to its graph input and each buffer sink to its
   sink. The generation takes a new source id, shared by every output of
   the generation
   ([content.md §5](content.md#5-stream_idx-and-time_base)).
4. Each graph input pushes, in order, the frames it held for this
   generation (§13.1).

When any step raises, the build has failed (§11.6).

### 11.4 What ends a generation

| Event               | Detected                                                                       | Then                                                                                    |
| ------------------- | ------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------- |
| input runs dry      | a fill needs data and some graph input's source is not ready (§12.2)           | drain (§11.7); next build when data comes back                                          |
| format change       | a frame about to be pushed into a graph input differs from that input's format | drain; next build at once with the new format                                           |
| every sink at end   | every sink has answered end of stream (`atrim`, a generator with a duration)   | drain; see below                                                                        |
| graph source sleeps | the graph source goes to sleep                                                 | released without draining: nothing reads its output any more, and its buffer is dropped |
| error               | a push, pull or build raised                                                   | released without draining ([draining.md §5](draining.md#5-discarded-on-purpose)); §11.6 |

**Format change.** A video frame differs from the input's format when the
comparison of [common.md §9](common.md#9-video-format-comparison) says so; an
audio frame when its sample format, sample rate or channel layout differs.
Only a frame being pushed counts: a frame held for a generation not yet
built is pushed later and checked then. The input logs at warning level
(level 3): `The format of <input id> changed in the middle of a stream, from
<old> to <new>: rebuilding the filter graph. Its filters start over.`
(example wording), the formats described as in
[common.md §9](common.md#9-video-format-comparison). The generation is
drained, the input takes the frame's format, the next generation is built in
the same fill (the other inputs keep their formats), and the frame is pushed
into it. No track mark is added: a format change is a mid-stream change.

**Every sink at end.** The graph source ends its track and is
**exhausted**: it is not ready and builds nothing. With at least one graph
input, the exhausted state clears when a readiness test (§12.1) finds some
graph input's source not ready, that is when the inputs run dry; the next
generation is then built when they are ready again. With or without inputs,
it also clears when the graph source goes to sleep and wakes up again.
Reason: the filters ended the stream on purpose (`atrim`, a generator with a
duration); rebuilding at once would restart them on the same input, or
repeat a generator endlessly, which the script did not ask for.

A generation boundary adds no track mark; track marks travel through the
graph as data (§14).

### 11.5 Reset

After a generation ends, its config is released: graph inputs forget their
buffer source and their end-of-stream flag, sinks forget their buffer sink.
Each graph input and each sink keeps its timeline. What happens to the
inputs' formats depends on why the generation ended:

- **format change**: every other input keeps its format (the format of its
  last pushed frame), so the next generation is built at once (§11.4);
- **input runs dry, every sink at end, sleep**: every input forgets its
  format. The next generation is built when every input has delivered a
  frame again, so each buffer source is configured from the data that
  follows, not from the data before the pause.

### 11.6 Errors while streaming

Faults follow [faults.md](faults.md). Every error the script can cause by
its description is a configuration error, reported at script time (§5.4,
§6.3, §7.2, §15.3). While the graph streams, nothing raises out of the graph
source or a graph input.

**The graph source is fallible** (§9.3) and never invents content. A source
fault of the graph source
([faults.md §3](faults.md#3-source-faults-reach-the-script)):

1. ends the graph source's track at the fault: the running generation is
   drained (§11.7) into the buffer, which delivers what it holds. When the
   generation itself failed (an FFmpeg error in it, a failed build), it is
   a stage in error and is released without draining
   ([draining.md §5](draining.md#5-discarded-on-purpose));
2. puts the graph in the **failed** state, logged at error level with the
   graph and the cause: the graph source is not ready (§12.1), so a
   `fallback` or `switch` above it takes another source;
3. clears the failed state after `fault_retry_delay`, or when the graph
   source goes to sleep and wakes up again. The graph is then ready again
   when its inputs are, and the next fill builds a new generation.

`fault_retry_delay` is defined in
[faults.md §3](faults.md#3-source-faults-reach-the-script).

Faults of the graph, by class:

| Fault                                                              | Class                                                         | Outcome                                                                                                                                                               |
| ------------------------------------------------------------------ | ------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| FFmpeg error pushing into or reading from a running generation     | item fault ([faults.md §4](faults.md#4-item-faults-escalate)) | the frame is dropped with a warning; the generation is released without draining; the next fill rebuilds                                                              |
| a build or launch fails (a format the filters refuse)              | source fault                                                  | the steps above                                                                                                                                                       |
| a rebuilt generation fails again before any sink produced a frame  | source fault                                                  | the steps above. Reason: a fault that returns at every rebuild would otherwise rebuild the graph on every frame, so the item fault escalates after one failed rebuild |
| a raw frame at a graph input that breaks the content rules (§13.1) | item fault                                                    | dropped with a warning; `max_consecutive_faults` consecutive drops at one input escalate to a source fault                                                            |
| a limit of §12.3                                                   | per §12.3                                                     | per §12.3                                                                                                                                                             |
| a decreasing video PTS out of a filter (§13.3)                     | item fault, repaired                                          | the frame is kept and the timeline repaired, with a warning; nothing is lost, so it never escalates                                                                   |
| a format change at an input (§11.4)                                | not a fault                                                   | the generation is rebuilt, with a warning                                                                                                                             |
| `process_command` refused by FFmpeg (§6.3)                         | item fault of the command                                     | the answer carries FFmpeg's message, with a warning; nothing raises                                                                                                   |

`max_consecutive_faults` and `fault_log_interval` are defined in
[faults.md §4](faults.md#4-item-faults-escalate): repeated warnings of one
kind are counted and logged with their count once per `fault_log_interval`.

**Operator documentation.** The graph source is fallible, so a script reads
it through `fallback` or `mksafe` when its consumer needs an infallible
source.

### 11.7 Draining a generation

A generation is a stage in the sense of [draining.md](draining.md). Draining
it:

1. Each graph input sends end of stream to its buffer source, once.
2. Each sink reads its buffer sink until FFmpeg answers "drained", emitting
   every frame (§13.2), within the drain limit of §12.3.
3. Each sink drains what it holds itself: the audio resampler, the video
   duration helper (§13.2).
4. The config is reset (§11.5).

Everything drained enters the graph source's buffer before the first output
of the next generation. Drained output is ordinary output: it carries its
metadata and track marks and counts in every length.

Conformance:

- `loudnorm` behind a `buffer` whose input stops for a few seconds and comes
  back: no error, the output resumes, and output duration matches input
  duration to within one frame.
- A graph whose input changes video size mid-stream: every input frame
  reaches the output, the warning is logged once, and no track mark is
  added.
- A graph of only `sine(duration=1)` and one output delivers one second of
  audio, then is not ready.

## 12. Pull loop

### 12.1 Graph readiness

The graph source is ready when its buffer holds data, or when the graph is
not failed, not exhausted (§11.4), and every graph input's source is ready
(trivially with no graph input). Readiness ticks nothing and builds
nothing; it MAY be asked from any thread.

### 12.2 Filling the buffer

Run when the graph source must produce a frame and some field of its buffer
holds less than one frame (`Frame.size`). A fill runs inside a tick of the
output clock.

1. **Read to need.** When a generation runs, read each sink while its field
   holds less than one frame and FFmpeg answers with frames (§13.2).
2. When every field holds at least one frame, stop.
3. When a generation runs and every sink is at end: end the generation and
   become exhausted (§11.4). Stop.
4. When there is no generation and no graph input: build (§11.3) and go to 1.
5. When some graph input's source is not ready: when a generation runs, end
   it (input runs dry, §11.4). Stop.
6. Tick the input clock once. Each graph input that is ready takes a frame
   of its source and pushes or holds it (§13.1); a build may happen there
   (§11.3), or a format change (§11.4).
7. **Read out.** When a generation runs, read every sink reached by a graph
   input until FFmpeg answers "none ready" or end of stream, within the
   limits of §12.3. Other sinks are read to need. Go to 2.

Then the graph source takes up to one frame's worth from its buffer. A
shorter result is a partial frame, as for any source.

Reason for the two reading modes: reading only to need keeps a sink fed by a
generator with no end (`sine` with no duration) from being read forever.
Reading out after a tick moves what the tick released from FFmpeg's sink
queues into the graph source's buffer, where its length is measured, so
outputs consumed at diverging rates are detected (§12.3) instead of growing
inside FFmpeg.

A sink is **reached by a graph input** when the description has a path of
links from some graph input to it. A parsed description (§15) counts as
linking each of its given inputs to each of its returned outputs, since its
inner wiring is not known. The set is computed once, from the description,
when `ffmpeg.filter.create` returns. A sink fed only by generators is read
to need: what a tick released is finite only for sinks reached by an
input.

### 12.3 Resource limits

Each limit is `settings.ffmpeg.filter_max_buffer` seconds of media. A limit
never stops the output and never raises: it logs a warning naming the graph,
the output or input, the limit and what was dropped, then acts as the "When
reached" column says. Repeats are counted and logged at a bounded rate
([faults.md §4](faults.md#4-item-faults-escalate)). The two waits on input
data (held input, fill input) are input-side waits
([faults.md §5](faults.md#5-waiting)): at their bound they become a source
fault of the graph source (§11.6). The generation is drained first (§11.7);
the held frames, which no generation has taken yet, are dropped. The two
others are item faults.

| Limit         | Measured                                                         | Protects                                    | Typical cause                                                               | When reached                                                                                                                                          |
| ------------- | ---------------------------------------------------------------- | ------------------------------------------- | --------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| output buffer | the longest field of the graph source's buffer, after a read out | memory                                      | outputs consumed at diverging rates; a generator feeding one output only    | item fault: the oldest data of that field beyond the limit is dropped; drops in `max_consecutive_faults` consecutive fills escalate to a source fault |
| held input    | frames one graph input holds for a generation not yet built      | memory                                      | one input never delivers its first frame while the others do                | source fault (§11.6); the generation is drained, the held frames dropped                                                                              |
| fill input    | media one graph input pushes within one fill                     | memory inside FFmpeg, and the tick duration | a filter that needs more lookahead than the limit before producing anything | source fault (§11.6), after the generation is drained (§11.7)                                                                                         |
| drain read    | media read from one sink during a drain (§11.7)                  | the tick duration                           | a sink fed by a generator with no end                                       | item fault: reading that sink stops; the rest is released with the generation                                                                         |

Example message (wording not binding):
`Filter graph <id> has buffered more than <max>s of data for one of its outputs and dropped <n>s. This happens when the graph's outputs are consumed at diverging rates. Raise settings.ffmpeg.filter_max_buffer if the graph legitimately produces that much on a single pass.`

Conformance:

- `sine` (no duration) → `aecho` → output → `output.dummy` runs for 10 s
  with bounded memory, and every tick of the output clock completes.
- A graph whose two outputs produce at a ratio of 2:1 keeps streaming:
  after about `filter_max_buffer` seconds it warns and drops the excess of
  the faster output, and its memory stays bounded.
- A graph whose build fails for a format ends its track, logs an error, and
  the output keeps streaming from a `fallback` above it; after
  `fault_retry_delay` the graph is tried again.
- A graph with two inputs, one of which never delivers a frame, ends its
  track after `filter_max_buffer` seconds and is not ready.

## 13. Frame flow and timestamps

### 13.1 Input side

On each tick of the input clock, a ready graph input takes a frame of its
source. For the raw content of its field, run by run and item by item, each
FFmpeg frame is placed on the input's **timeline**: its position is the
input's timeline position at the start of the liquidsoap frame plus the
frame's absolute position within it, run start plus item position
([content.md §4.2](content.md#42-run-layer)). A gap between runs holds no
frame; the timeline still covers it, so the next frame's PTS lies after the
gap. Then each FFmpeg frame:

1. receives the metadata of §14.1;
2. gets as PTS its position on the timeline, in the main time base, and its
   duration, when it has one, rescaled to the main time base
   ([common.md §5](common.md#5-time-base-conversion));
3. is pushed into the buffer source when a generation runs, after the
   format check of §11.4; otherwise it is held in order for the next
   generation (bounded by §12.3). A frame that breaks the raw content rules
   ([content.md §4.2](content.md#42-run-layer): a missing parameter) is
   dropped with a warning naming the input: an item fault (§11.6). The first frame an input holds or pushes
   while it has no format sets its format.

The timeline advances by the liquidsoap frame's length. It continues across
track boundaries, source switches, time-base changes and generations, and
starts at 0 when the graph source wakes up. Timestamps pushed into a graph
are therefore strictly increasing per input across every source switch and
every generation, with no gap and no overlap.

Reason: positions are already absolute, monotonic and in one time base.
Deriving PTS from them needs no duration helper on the input side, no
per-source offset, no line-up rule on a time-base change, and holds nothing
back. The incoming PTS is not read.

### 13.2 Output side

Each sink has a timeline that continues across generations and starts at 0
when the graph source wakes up. Every chunk a sink emits carries the
generation's source id (§11.3) and the main time base, and each of its
frames carries as PTS its position on the sink's timeline. Graph outputs
therefore start one new source, shared by all of them, at every generation
([content.md §5](content.md#5-stream_idx-and-time_base)) while their
timestamps stay strictly increasing.

Reading a sink, each frame:

1. passes through the sink's conformer:
   - audio: the raw audio conformer of
     [content.md §14](content.md#14-raw-audio-conformance): the first
     frame sets the field's format, and a later frame in another format is
     resampled to it;
   - video: the raw video conformer of
     [content.md §9](content.md#9-raw-video-conformance);
2. is placed:
   - **audio**: lengths come from sample counts at the field's rate. Within
     a generation, a frame starts at `s₀ + rescale(n, 1/rate, main)`, where
     `s₀` is the sink's timeline position when the generation started and
     `n` the samples emitted before it in this generation; its length is
     the next start minus its own. Each frame is emitted at once, as one
     chunk;
   - **video**: lengths come from the frame's PTS through the duration
     helper of [common.md §6](common.md#6-duration-helper), in PTS mode,
     `src` the buffer sink's time base, `convert_ts` off, `start` the sink's
     timeline position when the helper is created. A decreasing PTS is
     handled by §13.3 before the helper sees it. A change of the sink's time
     base flushes the helper and creates a new one with `start` set to the
     old one's `end_position`
     ([common.md §6.4](common.md#64-last_ts-t));
3. gets its PTS replaced by its position (main time base), after the
   filter's own PTS, duration and best-effort timestamp were read for §14.2;
4. has its track mark and metadata placed at its position (§14.2).

At the end of a generation, an audio sink drains its conformer's resampler,
when it has one, and a video sink flushes its helper
([common.md §6.3](common.md#63-flush-t)), which returns nothing when it holds
nothing; each emits what it gets as ordinary output.

Reason for sample counts: an audio frame's length is exactly its sample
count, whatever timestamps a filter writes; raw audio encoders lay audio end
to end on the same basis ([timestamps.md §3](timestamps.md#3-audio-internal-and-raw)).
Audio PTS from a filter are read only for §14.2.

### 13.3 Decreasing video timestamps from a filter

A filter's output is FFmpeg's to produce. A video sink that reads a frame
whose PTS is below the previous frame's:

1. logs a warning naming the graph output, the previous and the new PTS
   (an item fault, repaired, [faults.md §4](faults.md#4-item-faults-escalate)),
   repeats counted and logged at a bounded rate;
2. flushes its helper, so the previous frames keep their length (frame
   duration or previous step, [common.md §6.3](common.md#63-flush-t));
3. creates a new helper with `start` the old one's `end_position`, and
   pushes the frame into it.

The frame and those after it continue the sink's timeline. Equal PTS are not
an anomaly: the helper groups them at one position.

Reason: a frame is never dropped and the output never stops for a filter's
timestamp error, and the repair is visible in the log. The frames come from
a filter, not from the user's media, so the rule that invalid media stops
its input does not apply. FFmpeg's own command-line tool treats
non-monotonic timestamps reaching its muxer the same way: it warns and
adjusts.

Conformance:

- A video graph whose filter emits a PTS lower than the previous one keeps
  producing; output positions strictly increase; one warning names the
  output.
- `ebur128(metadata=true)` without `asetpts` produces audio whose length
  equals the input's, with no error.
- A raw stream switching between a 1/44100 and a 1/48000 source through a
  graph: timestamps never decrease and no message is logged.

## 14. Metadata and track marks

### 14.1 Into the graph

FFmpeg frame metadata is the only channel a graph offers. For each
liquidsoap frame a graph input takes:

1. With `pass_metadata`, every metadata of the frame, each at its position.
2. Every track mark of the frame, as the pair
   `("liquidsoap_track_mark", "<n>")`, where `n` counts the input's track
   marks from 1. Track marks travel regardless of `pass_metadata`.

Each entry attaches to the first FFmpeg frame of the field at or after its
position; an entry with no such frame in this liquidsoap frame attaches to
the next FFmpeg frame the input takes. Entries attaching to one frame merge,
the later position winning on a shared key, and merge into the frame's own
metadata, the entries winning.

### 14.2 Out of the graph

For each frame read from a sink, at its position (§13.2):

- when its metadata holds `liquidsoap_track_mark` with a number the sink
  has not seen in this generation, a track mark is added. A filter that
  copies frame properties onto several output frames repeats the key; the
  number makes the mark count once;
- with `pass_metadata`, when its metadata, without that key, is not empty,
  it becomes liquidsoap metadata, plus:

| Key                               | Value                                                  |
| --------------------------------- | ------------------------------------------------------ |
| `lavfi.liq.pts`                   | the frame's PTS as the filter gave it, in seconds      |
| `lavfi.liq.duration`              | the frame's duration as the filter gave it, in seconds |
| `lavfi.liq.best_effort_timestamp` | the frame's best-effort timestamp, in seconds          |

Each is present when the frame has that value. A frame without metadata of
its own adds no metadata. Reason: a timestamp entry on every frame would
turn every video frame into a metadata event downstream; the timestamps
annotate metadata the filters produced.

A filter that does not carry frame properties from input to output loses the
metadata and track marks of those frames. That is the filter's behaviour,
and the layer cannot detect it.

A track boundary on any output is a track boundary of the whole graph
source.

Example (`sine()` → `ffmpeg.raw.encode.audio` → `ebur128(metadata=true)`,
first metadata seen downstream):

```
[("lavfi.liq.best_effort_timestamp", "0."), ("lavfi.liq.duration", "0.1"), ("lavfi.liq.pts", "0."), ("lavfi.r128.I", "-70.000"), ("lavfi.r128.LRA", "0.000"), ("lavfi.r128.LRA.high", "0.000"), ("lavfi.r128.LRA.low", "0.000"), ("lavfi.r128.M", "-120.691"), ("lavfi.r128.S", "-120.691")]
```

Conformance:

- Two metadata in one liquidsoap frame both reach the output, in order, at
  their positions to within one FFmpeg frame.
- Through `asetnsamples` (which splits frames), one input track mark gives
  one output track mark.

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

Category `Source / FFmpeg filter`, flag `extra`. Its documentation says that
it adds the filters of a textual description to the graph. `inputs` and
`outputs` default to `{audio = [], video = []}`. `description` uses FFmpeg's
filtergraph syntax.

- `inputs`: `(label, pad value)` pairs; each pad value feeds the
  description's open input of that label.
- `outputs`: labels of the description's open outputs to hand back.
- The result: for each output label, in order, `(label, pad value)`.

### 15.2 Steps

The call adds a parsed description to the description. Replaying it (at the
call into the probe config, and at each build):

1. For each audio output label `l`, attach a pass-through `aformat` with no
   argument; for each video output label, a pass-through `format`. These
   give the returned pad values a fixed element to designate.
2. Call the bindings' parse
   ([avfilter.md §4.10](../../../../modules/synced/ffmpeg/spec/avfilter.md#410-parse))
   with: the input pads of the pass-through filters, under their labels, as
   the targets of the description's open outputs; the given pads, under
   their labels, as the feeds of the description's open inputs.

Each returned pad value designates the output pad of the pass-through filter
of its label.

### 15.3 Validation at script time

The replay into the probe config at the call reports, at the call's
position, as error kind `ffmpeg.filter`:

- a syntax error, an unknown filter or a rejected argument, with FFmpeg's
  message;
- a filter of the description that is a buffer source or buffer sink
  (`abuffer`, `buffer`, `abuffersink`, `buffersink`), naming it: graph
  inputs and outputs are the graph's only endpoints (§4.1);
- an input value that does not designate an output pad.

Filters created by a description have no script handle and no
`process_command`. A script that needs to command a filter creates it with
`ffmpeg.filter.<name>.create`.

## 16. Helper graphs

Private, launched graphs other parts of the layer build for one stream. None
is visible to scripts. Each is a buffer source (§8.3), zero or more filters
and a buffer sink. `convert` pushes a frame and calls back with every frame
the sink then has ready; `eof` pushes end of stream and calls back with the
rest. Both stop on FFmpeg's "none ready" and "drained" answers alike;
neither raises for them. A helper is single-use after `eof`.

A helper never raises while streaming. An FFmpeg error during `convert`
drops that frame, logs a warning, and rebuilds the helper on the next frame:
an item fault ([faults.md §4](faults.md#4-item-faults-escalate)) of the
helper's owner, counted against the owner's bound. A filter the helper
needs and the installed FFmpeg lacks (`setpts`, `fps`, `aformat`, `scale`,
`pad`, `setsar`, `format`) is a configuration error: the layer checks for
them at start-up and fails the script when one is missing and the script
uses an FFmpeg video or audio conversion.

A helper that holds frames (Fps, AFormat) is a stage
([draining.md](draining.md)): its owner calls `eof` before releasing or
replacing it, and passes the result downstream before the replacement's
first output. Fit holds nothing and needs no drain.

### 16.1 Display

Owned by [side-data.md §3](side-data.md#3-autorotate-on-decoding).

### 16.2 Fps

Retimes video to a constant frame rate.

Inputs: the frame format, the input time base, an optional source frame
rate, the target frame rate, and one start rule:

| Start rule     | User                                                                                                    | Filters                                                                    |
| -------------- | ------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| none           | decoders ([decoder.md §16.1](decoder.md#161-internal-video-converter))                                  | `setpts` with `expr = "PTS-max(STARTPTS, 0)"`; `fps` with `start_time = 0` |
| start PTS `S`  | inline encoders ([inline-codecs.md §7](inline-codecs.md#7-video-encoding))                              | `setpts` with `expr = "S+PTS-STARTPTS"`; `fps`                             |
| start time `T` | the encoder ([encoder-internal.md §4.6](encoder-internal.md#46-frame-rate-conversion-both-video-modes)) | no `setpts`; `fps` with `start_time = T` (seconds)                         |

`fps` always gets `fps = <target>/1`.

- Source frame rate equal to the target: pass-through, time base unchanged.
- Otherwise a graph: buffer source (§8.3), the filters of the rule, a buffer
  sink. The output time base is the sink's.

Reason for the rules:

- **None.** A negative start (frames to skip before 0) is kept, and
  `start_time = 0` drops what precedes 0; a positive start, as in a partial
  dump of a copy, is realigned to 0.
- **Start PTS.** The inline encoder numbers its frames with its own image
  counter; `S` places the first one where the transform's timeline stands.
- **Start time.** The encoder's frames already enter at their output
  position ([timestamps.md §4.1](timestamps.md#41-frame-timestamps-entering-the-converter)),
  which needs no re-anchoring. `T` is the time of the next grid slot to
  write, so a rebuilt stage continues the grid
  ([timestamps.md §4.3](timestamps.md#43-building-and-re-anchoring-a-converter)).

The stage keeps no state across rebuilds: an owner that rebuilds it passes
the rule's value that continues its timeline.

**Gaps.** `fps` repeats the previous frame to fill a gap in its input
timestamps. A frame repeated across a gap in the content would be invented
content ([faults.md §3](faults.md#3-source-faults-reach-the-script)). An
owner whose input has a gap in positions drains the stage at the gap (§16
intro) and builds a new one after it, starting at the grid slot that follows
the gap. Frame repetition within continuous input is the frame-rate
conversion itself and is intended.

A missing `setpts` or `fps` filter is a configuration error (§16 intro).

### 16.3 AFormat

Converts audio sample format, layout and rate.

Inputs: source sample format, layout, rate and time base; optional
destination sample format, layout and rate, each defaulting to the source's.
When all three are equal (layouts compared by the bindings), pass-through
with the source time base. Otherwise buffer source (§8.3), `aformat` with
the destination `sample_fmts`, `channel_layouts` and `sample_rates`, buffer
sink. The output time base is the sink's. A missing `aformat` filter is a
configuration error (§16 intro).

Users: the raw decoder and raw audio content conversion
([decoder-raw.md](decoder-raw.md), [content.md §3.2](content.md#32-ffmpegaudioraw)).

### 16.4 Fit

Fits video into a picture of another size and pixel format, keeping
proportions and padding the rest. Timestamps are kept.

Target: width, height, pixel format, optional pixel aspect.

`fitted_size` (destination pixel aspect `pixel_aspect`, square by default;
source pixel aspect, square when unknown):

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

The chain runs at the main time base. A Fit keeps its chain while the target
and the frame's format (compared as in
[common.md §9](common.md#9-video-format-comparison)) stay the same, and
builds a new one otherwise.

Output frames keep the source frame's colour space, range, primaries,
transfer characteristic and chroma location: where a filter of the chain
drops one (`scale` drops the chroma location), Fit sets it back on the
output frame. A consumer built for the fitted format then sees no second
format change.

Users: the raw video conformer
([content.md §9](content.md#9-raw-video-conformance)), which sinks use
(§13.2); the raw video encoder
([encoder-internal.md §4.5](encoder-internal.md#45-video-raw)); `fitted_size`
in the decoders' common path.

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

Applies `ffmpeg.filter.audio.output` to the `audio` of the first argument's
tracks and `ffmpeg.filter.video.output` to the `video` of the second's, and
returns `source(id=id, {audio, video})`. Its documentation describes `~id`
as the id of the returned source.

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
   previous call, sends to each drawtext `process_command("reinit", "x=<x>:y=<y>:text=<escaped text>")`.
7. `on_metadata(m)`: when `metadata` is set and `m[metadata]`, escaped, is
   not empty, logs at level 3 under `ffmpeg.filter.drawtext`
   `Setting new text <text> from metadata <key>` (example wording) and sends
   `reinit` with `text=<text>` to each drawtext.
8. Chains the drawtexts with `set_input` from `v` and returns the last output
   with methods `on_change` and `on_metadata`.

`video.add_text.ffmpeg.raw(…, ~graph, d, s)`: registers synchronous
`on_frame` and `on_metadata` callbacks on `s`, makes a video graph input of
`s`'s video track, applies `raw.filter`, points the callbacks at its
`on_change` / `on_metadata`, so commands are sent once per frame of `s`, and
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
`asetpts` rewrites the filter's PTS from the sample count, so that
`lavfi.liq.pts` (§14.2), which reports the filter's own PTS, is a position:
`ebur128`'s output PTS are not. Without `ebur128` it logs at level 2 under
`autocue.internal` `ffmpeg.filter.ebur128 is not available, autocue
disabled!` (example wording) and returns `[]`.

## 18. Failure summary

Classes as in [faults.md §2](faults.md#2-classes).

| Situation                                                   | When        | Class                     | Result                                                                        |
| ----------------------------------------------------------- | ----------- | ------------------------- | ----------------------------------------------------------------------------- |
| graph value used after `create`                             | script time | configuration error       | error 7 (§7.3)                                                                |
| option value cannot be converted, out of range, bad flags   | script time | configuration error       | error 7 (§5.4)                                                                |
| FFmpeg rejects the arguments or does not know the filter    | script time | configuration error       | error kind `ffmpeg.filter`, FFmpeg's message (§4.4)                           |
| `set_input` twice; input list of the wrong length           | script time | configuration error       | §6.3                                                                          |
| link refused                                                | script time | configuration error       | error kind `ffmpeg.filter`, source to destination (§6.3)                      |
| unlinked pad                                                | script time | configuration error       | error kind `ffmpeg.filter`, naming instance and pad (§7.2)                    |
| `parse`: syntax, unknown filter, endpoint filter, bad input | script time | configuration error       | error kind `ffmpeg.filter` (§15.3)                                            |
| a helper filter missing from FFmpeg                         | start-up    | configuration error       | the script fails (§16)                                                        |
| build or launch fails (format-dependent)                    | streaming   | source fault              | end of track, not ready, retried after `fault_retry_delay` (§11.6)            |
| FFmpeg error pushing or reading                             | streaming   | item fault                | frame dropped, generation rebuilt; a second failure is a source fault (§11.6) |
| invalid raw frame at a graph input                          | streaming   | item fault                | dropped with a warning; `max_consecutive_faults` escalates (§11.6)            |
| `process_command` outside a generation                      | any         | —                         | returns `graph not started!`                                                  |
| `process_command` refused by FFmpeg                         | any         | item fault                | warning; FFmpeg's message returned (§6.3)                                     |
| decreasing video PTS out of a filter                        | streaming   | item fault, repaired      | warning, timeline repaired (§13.3)                                            |
| format change on an input                                   | streaming   | not a fault               | warning, generation drained and rebuilt (§11.4)                               |
| output-buffer or drain-read limit                           | streaming   | item fault                | warning, excess dropped (§12.3)                                               |
| held-input or fill-input limit                              | streaming   | source fault (input wait) | §11.6 (§12.3)                                                                 |
| `json` of a graph or pad value                              | any         | script error              | error kind `json`                                                             |
