# FFmpeg content

This file owns how FFmpeg data lives inside Liquidsoap frames: the three
FFmpeg content kinds, their parameters and how types name, print, parse,
merge and compare them (§2, §3); the two chunk layers (§4); source identity
with `stream_idx` and `time_base` (§5); every data operation (§6); raw
conformance (§9, §14); how the kinds exist in a process with no libav (§10);
and the checksum (§11). Two of its rules are cross-cutting and other files
link them: the copy parameters with their merge and compatibility (§3, the
type-level half of user decision D2), and source identity (§5, user
decision D3).

It relies on the generic content contract of Liquidsoap (stated in §4 and §6
where the FFmpeg kinds use it), on the FFmpeg bindings for packets, frames
and codec parameters
([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)), and on
the libavfilter helpers of [filters.md §16](filters.md#16-helper-graphs).
Encoder-side timestamp rules live in [timestamps.md](timestamps.md). How
decoders fill content lives in [decoder.md](decoder.md). What an encoder does
at a source switch, its runtime parameter check included, lives in
[encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state).

## 1. Kinds

| Kind name (scripts, types) | Content name (registry) | Payload                                  | Parameters                                        |
| -------------------------- | ----------------------- | ---------------------------------------- | ------------------------------------------------- |
| `ffmpeg.copy`              | `ffmpeg.copy`           | encoded packet: audio, video or subtitle | codec parameters, unknown until a source (§3.1)   |
| `ffmpeg.audio.raw`         | `ffmpeg.raw.audio`      | decoded audio frame                      | channel layout, sample format, sample rate (§3.2) |
| `ffmpeg.video.raw`         | `ffmpeg.raw.video`      | decoded video frame                      | width, height, pixel format, pixel aspect (§3.3)  |

No other FFmpeg content kind exists. The kind name is what a script writes
and what a type prints. The content name is the key under which the content
registers. The content name appears in no script-visible place except as the
name of a method on a format value (dots replaced by `_`, §2.5).

A copy packet is an audio, a video or a subtitle packet. One `ffmpeg.copy`
track holds packets of one medium. Its parameters (§3.1) name that medium
once a source is known.

Raw content is decoded frames that flow from decoder to encoder (or through
FFmpeg filters) without conversion to Liquidsoap's internal PCM or video
formats. Copy content is demuxed packets that are remuxed without decoding.

## 2. Names in types

### 2.1 Annotation syntax

A track annotation is `field=kind` or `field=kind(label=value, …)`:

```liquidsoap
def f(s) =
  (s:source(audio=ffmpeg.audio.raw(sample_rate=48000,channel_layout=stereo,sample_format=fltp)))
end
```

Resolution of an annotation `kind(params)`:

1. The kind name is looked up among all registered kinds. An unknown name is
   a parse error (error 3, for instance
   `Unknown type constructor: <kind>(<params>).`).
2. With no parameters the track type is the kind with a fresh format
   variable: `ffmpeg.copy('a)`.
3. Each `label=value` is parsed by the kind's parameter parser (§3) into a
   format holding only that parameter. An unknown label, any label on
   `ffmpeg.copy`, and a value the parser rejects are each a parse error
   (error 3).
4. The formats of all parameters are merged left to right (§3). A merge
   failure (e.g. `sample_rate=48000,sample_rate=44100`) is a parse error
   (error 3).

Examples:

| Annotation                                                                             | Result                                                                         |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| `audio=ffmpeg.copy`                                                                    | `ffmpeg.copy('a)`                                                              |
| `audio=ffmpeg.copy(codec="aac")`                                                       | error 3                                                                        |
| `audio=ffmpeg.raw.audio`                                                               | error 3 (a content name, not a kind name)                                      |
| `audio=ffmpeg.audio.raw(foo=1)`                                                        | error 3                                                                        |
| `audio=ffmpeg.audio.raw(channel_layout=bogus)`                                         | error 3                                                                        |
| `audio=ffmpeg.audio.raw(sample_rate=abc)`                                              | error 3                                                                        |
| `video=ffmpeg.video.raw(width=640,height=360,pixel_format=yuv420p,pixel_aspect="1/1")` | `ffmpeg.video.raw(width=640,height=360,pixel_format=yuv420p,pixel_aspect=1/1)` |
| `video=ffmpeg.video.raw(pixel_aspect=bogus)`                                           | error 3                                                                        |

Every parse error of an annotation is a configuration error
([faults.md §2](faults.md#2-classes)): the script fails before it streams.

Conformance:

- Every row of the table above gives the stated result, in the libav
  implementation and in the stand-in (§10).
- No annotation raises anything other than a typing or parse error.

### 2.2 Printing

A format prints as `kind` when its parameter string is empty, and as
`kind(params)` otherwise. A kind whose format is still a variable prints as
`kind('a)`. Parameter strings are defined per kind in §3. Each kind prints
its parameters in the order of its parameter table in §3. What a raw kind
prints, its parser reads back to an equal format.

### 2.3 Bare type names

Each kind name is also a plain type name, so `ffmpeg.copy`,
`ffmpeg.audio.raw` and `ffmpeg.video.raw` are valid type annotations outside
`source(…)`. Each stands for the kind with a fresh format variable:

```
def fff(x) = (x:ffmpeg.audio.raw) end
fff   : (ffmpeg.audio.raw('a)) -> ffmpeg.audio.raw('a)
```

### 2.4 Subtyping and errors

Unifying two formats of the same kind merges them (§3). A failed merge is a
type error. Two different kinds never unify. Examples of the error shape
(wording is not binding):

```
Error 5: this value has type
  source(audio=ffmpeg.audio.raw(sample_rate=44100), _)
but it should be a subtype of
  source(audio=ffmpeg.audio.raw(sample_rate=48000), _)
```

```
Error 5: this value has type
  source(audio=ffmpeg.copy('A), _)
but it should be a subtype of
  source(audio=ffmpeg.audio.raw(sample_rate=44100), _)
```

### 2.5 Format values in the language

Each kind registers a content-language entry: the format's value in scripts
is a string, the printed parameter string of §3. Its method name is the
content name with `.` replaced by `_` (`ffmpeg_copy`, `ffmpeg_raw_audio`,
`ffmpeg_raw_video`).

### 2.6 Dumps

A typing dump is the environment written for a process that typechecks
without the streaming core (the playground, the language server). A dump
carries a format as `<kind name>:<serialized params>`.

- Raw kinds: the serialized parameters are the printed parameter string of
  §3.2 or §3.3. Reading a dump parses that string with the kind's parser,
  libav or stand-in (§10). So `ffmpeg.audio.raw(sample_rate=48000)` survives
  a dump and a reload.
- `ffmpeg.copy`: the serialized parameters are the empty string. Copy
  parameters exist only once a source has been opened (§3.1), which never
  happens in the process that writes a dump, and no parser builds them from
  text.
- A reader MUST reject a payload its kind's parser rejects. The rejection
  is a configuration error of the process reading the dump.

The script cache stores the typed term itself, parameters included. A cached
script MUST typecheck exactly as the uncached one.

Conformance: dump then reload `ffmpeg.audio.raw(sample_rate=48000)` and
`ffmpeg.video.raw(width=640)`: both print unchanged.

## 3. Parameters

Every kind supplies, for its parameter type: a default, a printer, a
single-label parser, `compatible` and `merge`. A format holds the
parameters. A merge makes two formats share one value from then on: both
read the merged parameters, and any later merge through either is seen by
both. A format is therefore shared by every operator whose types were
unified with it, for the lifetime of the script.

Generic merge of two formats `p` and `p'` of the same kind:

1. Compute `m = merge(params p, params p')`. A failure is reported as an
   incompatible-format error that prints both formats and names the
   property that differs.
2. Both formats now read `m`.

Generic compatibility of `p` with `p'`: false when the kinds differ, else
`compatible(params p, params p')`. Compatibility changes nothing.

Every kind obeys one invariant: `compatible(a, b)` holds exactly when
`merge(a, b)` succeeds. A merge fills every unset property from the side
that has it and drops nothing.

Per-property merge, used by every kind for every property it compares:

| Left  | Right           | Result                       |
| ----- | --------------- | ---------------------------- |
| unset | unset           | unset                        |
| unset | `v`             | `v`                          |
| `v`   | unset           | `v`                          |
| `v`   | `v'`, equal     | `v`                          |
| `v`   | `v'`, different | failure, naming the property |

Equality is structural unless the property says otherwise.

### 3.1 `ffmpeg.copy`

Copy parameters describe a coded stream. A muxer writes a stream header from
them once, when the container starts, and cannot change it afterwards. Every
packet later written to that stream must be valid under that header. The
parameters therefore MUST carry every property a muxer's stream header
depends on, so that the merge rejects a source whose packets the header
cannot describe (user decision D2).

Parameters are unknown (the default) or known. Known parameters hold the
bindings' codec parameters of one stream, read from the container or from
the encoder that made the packets, plus the stream's average frame rate for
video and the stream's time base for subtitles.

**Compared properties.** Two known parameter sets are compatible when their
media are equal and every property below is compatible. Strict compatibility
compares the full stream header, side data and colour included (user
decision). "Unspecified" is an ordinary value, equal only to itself, except
for the properties marked "when set": demuxers routinely fail to probe those,
so a side that leaves one unset is compatible with any value.

| Medium | Property                                                                      | Comparison                                                            |
| ------ | ----------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| all    | codec id                                                                      | equal                                                                 |
| all    | bitstream form (below)                                                        | equal                                                                 |
| all    | extradata                                                                     | byte-equal; absent and empty are equal                                |
| all    | header side data (below)                                                      | the same kinds, each with a byte-equal payload; order not significant |
| all    | profile, level                                                                | equal, when set (often unprobed)                                      |
| audio  | sample rate                                                                   | equal                                                                 |
| audio  | channel layout                                                                | the bindings' layout comparison                                       |
| audio  | block align, bits per coded sample                                            | equal; 0 (unused by the codec) equal only to 0                        |
| video  | width, height                                                                 | equal                                                                 |
| video  | sample aspect ratio                                                           | equal after reduction, `0/1` (undefined) equal to `1/1`               |
| video  | pixel format                                                                  | equal, when set (often unprobed)                                      |
| video  | field order, colour range, primaries, transfer, colour space, chroma location | equal; unspecified equal only to unspecified                          |

**Bitstream form.** Some codecs carry their configuration either out of
band (in the extradata, with length-prefixed packets) or in band (inside the
packets). The two forms need different stream headers and different
conversion filters in the muxer. The form is derived from the codec id and
the extradata:

| Codecs           | Form                                                                                                                        |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------- |
| H.264, HEVC, VVC | `annexb` when the extradata is empty or starts with a start code (`00 00 01` or `00 00 00 01`); `length_prefixed` otherwise |
| AAC              | `raw` when the extradata is non-empty (an AudioSpecificConfig); `in_band` (ADTS or LATM headers) otherwise                  |
| any other        | `none`                                                                                                                      |

The extradata comparison already covers the form. The form is compared
first so that the error names the cause a user can act on, such as a
bitstream filter to add.

**Header side data.** The coded side data kinds that muxers record in a
stream header: display matrix, stereo 3D, spherical mapping, mastering
display metadata, content light level, Dolby Vision configuration, ICC
profile. Other coded side data (CPB properties, for instance) is carried and
written, not compared.

**Not compared, with reasons.**

- Average frame rate: demuxers estimate it, and two files of one frame rate
  routinely report different averages. Packets carry their own timestamps,
  so no header value of it makes the output undecodable. Some muxers do
  write it (Matroska `DefaultDuration`, the FLV `framerate` metadata): they
  write the first source's value, and a later source with another rate is
  muxed under that header. This is an accepted limit. The frame rate is
  merged: an unset value is filled.
- Subtitle time base, and the sample format of coded audio: they describe
  the demuxer's timestamps and the decoder's output, not the coded stream.
- Codec tag: the muxer chooses it for its own container.

**Relaxed check.** With
`settings.ffmpeg.content.copy.relaxed_compatibility_check` on (§13), two
known parameter sets are compatible when their media and codec ids are
equal, for all three media. Libav (the muxer) is then the judge of every
other difference. One predicate serves the type-level merge and the
encoder's runtime check, so the setting relaxes both at once.

`compatible(p, p')`:

1. Either side unknown → true.
2. Different media → false.
3. Relaxed on → codec ids equal.
4. Otherwise → every compared property is compatible.

`merge(p, p')`:

1. Either side unknown → the other side.
2. Not `compatible(p, p')` → failure naming the first incompatible property
   in table order, and printing both sides.
3. Otherwise → the left side, with every property the left side leaves unset
   (frame rate and the "when set" properties) taken from the right side.
   Under the relaxed check, a property set on both sides keeps the left
   side's value: the left side is the established format, whose header is
   the one written.

Parser: rejects every label. Copy parameters come from a container or an
encoder; there is nothing to build from a string.

Printer: comma-separated `key=value`, no spaces, in this order:

| Media    | Keys                                                                                                                                                                                                 |
| -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| unknown  | (empty string)                                                                                                                                                                                       |
| audio    | `codec` (quoted), `channel_layout` (quoted description), `sample_format` (name, or `none`), `sample_rate`                                                                                            |
| video    | `codec` (quoted), `width`, `height`, `aspect_ratio` (`num/den` of the sample aspect ratio), then `framerate` (`num/den`) only if known, then `pixel_format` only if set (name, or `none` if unnamed) |
| subtitle | `codec` (quoted), `time_base` (`num/den`)                                                                                                                                                            |

The printed string identifies a stream for a reader. It does not show every
compared property; an incompatibility error names the property that differs
(merge step 2).

Example, an AAC + H.264 MP4 decoded as copy:

```
{audio=ffmpeg.copy(codec="aac",channel_layout="stereo",sample_format=fltp,sample_rate=44100),video=ffmpeg.copy(codec="h264",width=320,height=240,aspect_ratio=1/1,framerate=25/1,pixel_format=yuv420p)}
```

#### 3.1.1 Where copy parameters are merged

A copy format starts unknown at type level: scripts cannot write copy
parameters. The first source opened for a track sets them, and every later
source is merged into the same format. An incompatible source is therefore
rejected when it is opened, before any of its packets is produced. The rule
holds only if every path that brings packets into a track goes through a
merge. Each path, and the rule that closes it:

| Path                                                                                            | Rule                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| ----------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A decoder, a reconnecting input, a seek                                                         | The producer merges each stream's parameters into the consuming source's own format before it produces content ([stream-description.md §6](stream-description.md#6-get_type-ctype-container)). It MUST pass that format itself, never a fresh or copied one. A failed merge is a source fault of that source only ([faults.md §3](faults.md#3-source-faults-reach-the-script)): the file is not decoded, the track ends, a playlist moves on; a reconnecting input that keeps failing is not ready. The output keeps streaming. |
| A producer that makes its own packets (inline encoder, bitstream filter)                        | The producer merges its output parameters into its output format before it emits the first packet under each source id (§5). A failed merge is a source fault of that producer ([faults.md §3](faults.md#3-source-faults-reach-the-script)): it emits nothing for that source, logs both parameter sets at error level, ends its track, and while the fault persists is not ready (fallible) or raises a hard error (infallible). It never emits filler. With a fallible producer the output keeps streaming.                   |
| A producer that edits parameters (side-data operator)                                           | The same rule, applied to each new edit result ([side-data.md §5.1](side-data.md#51-per-frame)).                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Content arriving while the format is unknown                                                    | Cannot happen: content with packets carries known parameters (§4.2), and its producer merged them first.                                                                                                                                                                                                                                                                                                                                                                                                                        |
| A merge that keeps one side and drops the other                                                 | Closed by the merge of this section: properties are compared, unset ones are filled, nothing is dropped.                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Frame rate                                                                                      | Not compared, by design (above).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Relaxed check on                                                                                | By design: libav judges.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Two formats that are never unified (an operator whose output format is separate from its input) | Each producer merges into the format it emits under; the encoder's runtime check compares each new source with the stream header.                                                                                                                                                                                                                                                                                                                                                                                               |

The encoder keeps a runtime check as the backstop: at each source switch it
compares the new source's parameters (the run's source parameters, §4.2)
with the parameters its stream header was written from, using `compatible`
of this section. A failure there is an output fault
([faults.md §2](faults.md#2-classes), user decision D2): the output restarts
when the container defines a clean restart, else stops under its restart
policy, and reports through its callbacks
([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)).

Conformance:

- A playlist of an MP4-sourced H.264 file then an MPEG-TS-sourced one,
  decoded as copy: the second file is rejected when it is opened, with an
  error that names the bitstream form and prints both parameter sets. No
  packet of it reaches an encoder.
- Two H.264 files whose extradata differ: rejected the same way.
- Two H.264 files that differ only in their average frame rate, or two AAC
  files that differ only in the reported sample format: accepted.
- With the relaxed setting on, the first two cases are accepted at type
  level and at the encoder. A switch between AAC and MP3 is still rejected.

### 3.2 `ffmpeg.audio.raw`

Parameters, in table order: `channel_layout`, `sample_format`,
`sample_rate`. Each is optional. Default: all unset.

| Label            | Value                                         | Parse failure                  |
| ---------------- | --------------------------------------------- | ------------------------------ |
| `channel_layout` | layout name, looked up by the bindings        | the name is unknown            |
| `sample_format`  | sample format name, looked up by the bindings | the name is unknown            |
| `sample_rate`    | positive decimal integer                      | not a positive decimal integer |
| other            | —                                             | always                         |

Printer: `key=value` for each set parameter, joined by `,`, in table order.
`channel_layout` prints the layout description, `sample_format` the format
name.

`merge` and `compatible`: per-property (§3), `channel_layout` with the
bindings' layout comparison. Compatibility means equality wherever both
sides are set. It does not mean "convertible": conversion is a producer's
duty (§14), not a property of the type.

A decoded frame's parameters set all three properties.

### 3.3 `ffmpeg.video.raw`

Parameters, in table order: `width`, `height`, `pixel_format`,
`pixel_aspect`. Each is optional. Default: all unset.

| Label             | Value                                    | Parse failure                  |
| ----------------- | ---------------------------------------- | ------------------------------ |
| `width`, `height` | positive decimal integer                 | not a positive decimal integer |
| `pixel_format`    | pixel format name (bindings)             | the name is unknown            |
| `pixel_aspect`    | `num/den`, two positive decimal integers | any other text                 |
| other             | —                                        | always                         |

Printer: `key=value` for each set parameter in table order, `pixel_aspect`
as `num/den`. With nothing set the format prints as the bare kind
`ffmpeg.video.raw`.

`merge` and `compatible`: per-property on all four (§3), `pixel_aspect`
compared after reduction.

A decoded frame's parameters set width, height and pixel format, and pixel
aspect when the frame has one.

## 4. Two chunk layers

FFmpeg content is chunked twice.

### 4.1 View layer

The view layer is generic, shared by every content type. A content value is
`{ params; views; total_length }` where each view is
`{ data; offset; length }`: a window of `length` ticks starting at `offset`
into an underlying data value. Slicing and appending frames create and
combine views without touching the data.

### 4.2 Run layer

The data value a view points to is

```
data = { params : P; length : ticks; runs : (start, run) list }
run  = { length : ticks;
         stream_idx : source id;          (§5)
         time_base : rational;            (§7)
         source_params : copy parameters; (ffmpeg.copy only)
         items : (position, item) list }  (positions relative to the run start)
```

- `length` is the data value's length in ticks: the length the value was
  made with (§6.1), or grown to by a blit (§6.6). It is never derived from
  the runs.
- Runs are sorted by `start`, do not overlap, and lie within `[0, length)`.
  An interval no run covers is a **gap**: no source is present there.
- A run is a stretch of time from one source. Its items are sorted by
  position, each with `0 <= position < run.length`. A run with no items and
  a positive length means "this source is present here and has nothing to
  say". Sparse streams (subtitles) and dense streams slower than a frame
  (25 fps video in a 20 ms frame) are represented this way.
- The absolute position of an item in the data value is `start + position`.
  Consumers MUST read positions this way.
- `params` are the format's parameters at the time the value was made.
- `source_params` are the known parameters of the source the run's packets
  come from. Every copy run that holds a packet MUST carry known parameters.
  Copy content with packets and unknown parameters does not exist. A
  consumer that meets such a run drops it as an item fault (§4.3); it never
  substitutes defaults.
- Raw items are frames, and each frame carries its own parameters. Raw runs
  have no `source_params`. Raw content always has full parameters: every
  raw audio frame sets channel layout, sample format and sample rate, and
  every raw video frame sets width, height and pixel format (pixel aspect
  MAY be absent). A producer MUST NOT emit a frame missing one of them; a
  consumer that meets one drops that frame as an item fault naming the
  missing parameter (§4.3). A consumer of raw content reads a frame's
  properties from the frame, never from a format's possibly unset
  parameters.

Two runs of one data value MAY carry different `source_params`: a frame that
straddles a source switch holds a run of each source, and each run keeps its
own parameters.

**Empty-run guarantee.** A copy producer emits, for each field and each
frame it produces, a run carrying its source id, empty when it has no packet
for that stretch. A consumer therefore sees a source's presence and its
switches even on a sparse stream, and never mistakes a quiet source for a
gap: a gap means that no source is present. [decoder-copy.md](decoder-copy.md)
and [encoder-copy.md](encoder-copy.md) rely on this.

### 4.3 Invalid content

Content is **invalid** when it breaks a rule of this file or of the
producers' timestamp rules: a timestamp lower than the previous one within
one source, a group that never advances
([common.md §6](common.md#6-duration-helper)), a copy run with packets and
unknown parameters, a raw frame missing a parameter (§4.2), overlapping
runs, a frame a conformer cannot convert (§9, §14). Every fault is placed in
a class of [faults.md §2](faults.md#2-classes). Content keeps flowing:
invalid content never stops an output, and nothing raises out of a source
while it streams except the hard error of an infallible source that cannot
produce content ([faults.md §3](faults.md#3-source-faults-reach-the-script)).

| Fault                                                                                      | Class (faults.md)                                                         | Containment                                                                                                                                                     |
| ------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| one item: a bad timestamp, a frame missing a parameter, a frame a conformer cannot convert | item fault                                                                | the item is dropped with a warning naming the producer, the rule and the values; counted (below)                                                                |
| one run: a copy run with packets and unknown parameters, a run overlapping another         | item fault                                                                | the run is dropped with a warning; counted as one item                                                                                                          |
| consecutive item faults of one source past the bound                                       | source fault ([faults.md §3](faults.md#3-source-faults-reach-the-script)) | the producing source ends its track; while the fault persists it is not ready (fallible), or raises a hard error (infallible); no filler stands in for the data |
| media that cannot be decoded further, a group that never advances                          | source fault                                                              | as above, decided by [common.md §6](common.md#6-duration-helper) and [decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side)                              |

**Bound** ([faults.md §4](faults.md#4-item-faults-escalate)). Every item
fault in this section counts toward `max_consecutive_faults`, owned by
faults.md §4, per source id, reset by every item delivered. This file adds
no bound of its own.

A consumer that is an encoder (an output) has no source to end. It follows
the output-side rule of faults.md §4: items are dropped and logged, and a
data fault never stops the output. Producers check first, so this case is an
internal invariant breach, not a media fault.

The first drop of a kind is a warning; repeats are counted and logged at
most once per `fault_log_interval`
([faults.md §4](faults.md#4-item-faults-escalate)). This section owns the
rule that a data fault stays inside its source.

Conformance:

- A file with one packet whose timestamp goes backwards, copied to a
  running output, loses that packet and nothing else; the output keeps
  streaming and logs one warning.
- A producer fed only frames missing a parameter ends its track after
  `max_consecutive_faults` frames and a `fallback` above it switches away.

## 5. `stream_idx` and `time_base`

`stream_idx` is the **source id**: a 64-bit identifier of one producer
instance. Equal ids MUST mean the same source, and different ids different
sources. Every consumer that reacts to a change of source (encoders, inline
decoders, bitstream filters, filter inputs) keys on the source id.

Allocation:

- Ids come from one process-wide counter. Allocation is an atomic
  fetch-and-increment, so producers on several threads or domains never
  receive the same id.
- The first id handed out is 1. `0` means "no source", and no producer emits
  it.
- Ids are never reused within a process. A 64-bit counter does not wrap
  within the life of a process.
- An id costs its 64 bits and nothing more. The content layer keeps no table
  keyed by ids or by sources. A producer holds its current id in its own
  state, which ends with the producer. Consumers that keep state per id
  bound its lifetime as
  [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state)
  specifies (user decision D6).

Every event that starts a new timeline or a new parameter set takes a new id
(user decision D3). One producer instance shares its id across all the
fields it produces, so tracks from one source switch together downstream
(user decision D4).

| Producer                              | A new id at                                                                                                                                                                                                                                             | Shared by                                                                                           |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| file or stream decoder                | each stream mapping: at open, at every seek ([decoder.md §12](decoder.md#12-seeking)), and at every timestamp discontinuity ([decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)); a reconnect opens a new decoder ([io.md](io.md)) | every field of that mapping                                                                         |
| inline encoder                        | each source-id record ([inline-codecs.md §5](inline-codecs.md#5-stream-index)): a record is opened by the first transform built on an input source with no open record, and closed by end of input, a seek or a track abort                             | the sibling transforms that hold the record; the record is reference-counted by its live transforms |
| FFmpeg filter graph                   | each graph generation ([filters.md §11](filters.md#11-generations))                                                                                                                                                                                     | every output of that generation                                                                     |
| bitstream filter, side-data operators | never: they keep the incoming run's id                                                                                                                                                                                                                  | —                                                                                                   |

Reason for the inline row: sibling transforms on one input source share an
id so that a downstream copy encoder treats their tracks as one source. A
transform built after its record closed restarts its timestamps, so it gets
a new record and a new id. The record lives in the transforms that hold it
and ends with the last of them, so it costs no permanent allocation (D6).

Within one source id, the runs of one field have one time base and
compatible `source_params` (§3.1). A producer whose output time base changes
takes a new id.

`time_base` is the time base of the timestamps carried inside the run's
items (§7). The content layer never converts with it; it only carries it.

The content layer uses the source id in collapse (§6.5): runs of one id that
continue one another merge. What encoders do on a new id is in
[timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what) and
[encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state).

Conformance:

- Several decoders opened concurrently on several domains, thousands of
  times: no two receive the same id.
- A seek, a reconnect, a new graph generation and a new inline record each
  produce content under an id never seen before in the process.
- The audio and video of one decoder, of one graph generation, and of two
  sibling inline transforms holding one record carry one id.
- Memory held for ids stays flat over a playlist of thousands of sources.

## 6. Operations

Every operation is lossless: no item and no tick of length is lost or moved
except where the operation's definition says so.

### 6.1 make

Run layer: `make ?length params` returns `{ params; length; runs = [] }`,
with `length` 0 when omitted. The whole value is one gap.

View layer: `make ~length params` wraps run data of that same length in one
view `{ data; offset = 0; length }`. View length and data length agree.

A run is built with an explicit source id and time base and, for copy, known
source parameters when it holds packets. Its items are sorted by position.

### 6.2 lift_data

Wraps run-layer data in one view: `offset` defaults to 0, `length` defaults
to the data length minus `offset`. The view-layer params are the data's
params. Every producer hands a chunk of content to a generator this way: it
builds data with one run at start 0, of the run's length, and lifts it.

### 6.3 length

Run layer: the data value's `length`. View layer: `total_length`.

### 6.4 sub of a run

`sub_run run offset len`: a run of length `len`, with the same source id,
time base and source parameters, holding the items with
`offset <= pos < offset + len`, each at `pos - offset`, each passed through
the kind's item copy (§6.7).

### 6.5 collapse

Collapse merges runs that continue one another. Two consecutive runs of a
data value merge when all of these hold:

- the first ends exactly where the second starts (no gap between them);
- they have the same source id;
- they have the same time base;
- for copy, their source parameters are the same value or equal.

The merged run starts at the first's start, has the sum of the lengths, and
holds the first's items followed by the second's items shifted by the
first's length. Every other pair stays apart. Collapse never changes a
timestamp, a time base or an absolute item position.

### 6.6 blit

`blit src src_pos dst dst_pos len` copies the region `[src_pos, src_pos + len)`
of `src` into `dst` at `dst_pos`, in place on `dst`. It requires
`src_pos + len <= src.length`.

1. Each `dst` run that overlaps `[dst_pos, dst_pos + len)` is cut: its part
   before `dst_pos` and its part from `dst_pos + len` on are kept, each as a
   sub-run (§6.4) at its own absolute start. Only the part inside the region
   is replaced.
2. Each `src` run that overlaps the source region contributes the sub-run of
   its overlapping part, placed at
   `dst_pos + (its absolute start in src − src_pos)`.
3. Gaps of the source region stay gaps in `dst`.
4. `dst.length` becomes `max(dst.length, dst_pos + len)`.
5. The runs of `dst` are sorted by start and collapsed (§6.5).
6. `dst.params` are kept when known, and taken from `src` when unknown.

Examples (items shown by absolute position):

| Case                                                                                                                     | Result                                                                                         |
| ------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------- |
| `src` one run `id=2, length=100, [15;35;55;75]`, `dst` one run `id=1, length=100, [5;25;50;85]`; `blit src 20 dst 20 40` | `dst` length 100: `id=1 [0,20)` with 5, `id=2 [20,60)` with 35 and 55, `id=1 [60,100)` with 85 |
| empty `dst`; blit 30 ticks of an empty value at 0, then a run `id=3, length=50, [0;20]` at 30                            | `dst` length 80: gap `[0,30)`, then `id=3 [30,80)` with 30 and 50                              |
| runs `id=3, length 40` and `id=4, length 40` blitted at 0 and 40                                                         | two runs kept apart, total 80                                                                  |
| runs `id=3, length 40, [0]` and `id=3, length 40, [10]`, same time base, blitted at 0 and 40                             | one run `id=3, length 80` with 0 and 50                                                        |

### 6.7 Item copy

| Kind                 | Item copy                                                               |
| -------------------- | ----------------------------------------------------------------------- |
| `ffmpeg.copy`        | a new packet reference to the same buffer (bindings packet duplication) |
| raw audio, raw video | identity: the frame value is shared, not copied                         |

Shared frames are read-only. An operator that edits a frame works on a
duplicate ([side-data.md §6.1](side-data.md#61-per-frame)).

### 6.8 copy

Run layer: a new data value with the same params and length, and every run
rebuilt with its items passed through the item copy.

View layer: consolidation with copy (§6.9).

### 6.9 Consolidation (view layer)

`consolidate ~copy content` turns any list of views into at most one view
starting at offset 0:

1. `total_length = 0` → no views.
2. One view at offset 0 whose length is its data's length, and `copy` false
   → unchanged.
3. Otherwise: `buf = make ~length:total_length params`; for each view in
   order, with `pos` starting at 0,
   `blit view.data view.offset buf pos view.length`, then
   `pos += view.length`. The result is one view
   `{ offset = 0; length = total_length; data = buf }`. With `copy` false
   the content is updated in place; with `copy` true a new content is
   returned.

A view whose window reaches past the end of its data contributes its data up
to the data's end, and a gap for the rest. The view length is never
shortened.

`get_data` consolidates without copy and returns the single data value, or
an empty data value of the content's length when there is no view.

Consequences:

- After consolidation the data length equals the view length, and every item
  sits at the absolute position it had in the frame.
- Runs of one source that continue one another across views merge.
- Runs of different sources stay apart: a frame that straddles a source
  switch yields two runs, each with its own source parameters.

Consumers iterate the runs of `get_data` and read each item at
`start + position` ([encoder-internal.md §3](encoder-internal.md#3-input),
[encoder-copy.md §3](encoder-copy.md#3-input),
[filters.md §13](filters.md#13-frame-flow-and-timestamps)).

Conformance:

- Blit of any region into any destination: every item of `dst` outside the
  region and every item of `src` inside it are present afterwards, at their
  absolute positions, and `dst.length` is as step 4 states.
- A frame created with a length, filled first with a producer chunk shorter
  than that length, then with a second chunk after it: after `get_data`, the
  second chunk's items sit where the frame placed them, after the gap.
- Every row of the blit table gives the stated result.

### 6.10 sub, truncate, append, is_empty (view layer)

These never touch FFmpeg data.

- `sub content ofs len`: invalid when `ofs + len > total_length`; otherwise
  keeps the views that intersect `[ofs, ofs + len)`, each narrowed.
  `total_length = len`.
- `truncate content len`: drops the first `len` ticks of views.
- `append c c'`: concatenates the view lists; the total length is the sum.
- `is_empty`: `total_length = 0`.

There is no fill operation.

## 7. Timestamps carried in content

Items keep the timestamps their producer gave them, expressed in the run's
`time_base`:

- copy packets keep their demuxed `pts`, `dts` and `duration` untouched, in
  the stream time base of the container they came from;
- raw frames from decoders keep their `pts` in the stream time base;
- frames from FFmpeg filter outputs carry their position as PTS, in the main
  time base, and their runs carry the main time base
  ([filters.md §13.2](filters.md#132-output-side)).

Positions are in main ticks and are what the frame timeline uses; timestamps
inside items are what encoders use. The content layer never rewrites either:
blit and collapse move runs as wholes and never change a timestamp or a time
base.

Producers derive run lengths and positions from absolute timestamps
(converted, then differenced), never by accumulating per-step rounded
lengths ([common.md §6](common.md#6-duration-helper),
[decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side)). How encoders
turn carried timestamps into output timestamps is in
[timestamps.md](timestamps.md).

## 8. Keyframes

Copy content tracks no keyframe state. A packet's key flag is the packet's
own (bindings). The start of a source is visible as a new source id (§5).
Waiting for a keyframe at the start of a source is encoder behaviour
([encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet),
[§12.3](encoder-copy.md#123-readiness-and-group-start)).

## 9. Raw video conformance

Every producer of raw video (raw decoder, FFmpeg filter outputs, the raw
inline encoder `track.ffmpeg.raw.encode.video`) delivers
frames through a conformer bound to the track's format. The format is open
until the first frame, and the first frame sets it. A later frame in another
format is fitted to it. A format change of decoded content is not a track
boundary: no track mark is inserted and the track does not end. This holds
for raw audio (§14), raw video, and decoding to internal content alike. A
raw producer's parameters are not known at typing, raw inline encoders
included: the first frame sets them. The frame properties that count as a format change are defined once, in
[common.md §9](common.md#9-video-format-comparison).

For each frame, with `declared` the format's current parameters and `params`
the frame's (§3.3):

1. `compatible(declared, params)` → merge the frame's parameters into the
   format (filling unset parameters) and pass the frame on.
2. Otherwise build a target: width, height, pixel format and pixel aspect,
   each from `declared` when set, else from the frame. Merge the target into
   the format, convert the frame through the fit filter (scale into the
   target while keeping proportions; [filters.md §16.4](filters.md#164-fit)),
   and pass on its output. A frame the fit filter rejects is an item fault
   (§4.3).

Merges here change the shared format (§3), so the source's content type
reflects the first frame from then on. The first conversion on a track is
logged at info level with both formats.

Conformance: a stream whose resolution changes mid-file, decoded as raw
video, keeps one format (the first frame's) and one track; every later frame
has that size, with its proportions kept.

## 10. Without libav

The kinds exist in every process, libav or not. They are split into:

- a kind-and-name layer with no libav dependency: the names (§1), the type
  registration (§2.3), printing, the parser hook, the merge and
  compatibility hooks, and the dump form (§2.6). It takes a parameter
  implementation as input;
- a parameter implementation, of which exactly one is present in a process:
  - **libav**: the parameters of §3, read off codecs and frames, and the
    data operations of §6;
  - **stand-in**: parameters are the `(label, value)` pairs a script wrote,
    with no data operations. Only a process that cannot stream links it
    (the playground, the language server's analysis).

The stand-in MUST accept exactly the annotations the libav implementation
accepts, so that a script that typechecks in the playground typechecks in
Liquidsoap:

| Operation           | Behaviour                                                                                                                                                                                                                            |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| default             | no pairs                                                                                                                                                                                                                             |
| parse `label value` | the labels of §3.2 and §3.3 for the raw kinds, none for `ffmpeg.copy`; integers and `num/den` checked as in §3; names checked against FFmpeg's public lists of layout, sample format and pixel format names, built into the stand-in |
| print               | as §3.2 and §3.3, in table order                                                                                                                                                                                                     |
| compatible, merge   | per-property (§3) on normalised values: integers as integers, `num/den` reduced                                                                                                                                                      |

The stand-in compares channel layouts by name. Two names that FFmpeg treats
as one layout MAY therefore compare different in the stand-in; the
playground then reports an error that Liquidsoap would not.

Exactly one implementation registers per process. A second registration
under another name is a configuration error at start-up
([faults.md §2](faults.md#2-classes)), naming both. Reason: both
register the same kinds, and a second registration would otherwise silently
win.

The `%ffmpeg` encoder's frame type refers to these kinds by name, so it
typechecks with either implementation
([encoder-format.md §10](encoder-format.md#10-content-type-implied-by-a-format)).
Outside `%ffmpeg`, encoder types reject `pcm_s16` and `pcm_f32` audio
parameters with an error saying that these are only supported by the
`%ffmpeg` encoder.

## 11. Checksum

Two levels, both MD5 rendered as 32 lowercase hex digits. The checksum
digests position, length, source and time base on purpose: the same items at
other positions, over other lengths or under another source are different
content.

Run layer, per data value:

1. For each run: `"<start>+<length>:<stream_idx>:<num>/<den>:"` followed by
   its items, each `"<pos>=<item checksum>"`, joined by `|`.
2. The data length, then the runs, joined by `||`; MD5 of the result.

Item checksums:

| Kind      | Item checksum                                                                                                 |
| --------- | ------------------------------------------------------------------------------------------------------------- |
| copy      | MD5 of the packet's bytes                                                                                     |
| raw audio | convert the frame with swresample to interleaved `s16`, same channel layout and sample rate; MD5 of the bytes |
| raw video | convert the frame with swscale to `yuv420p`, same size; concatenate the plane data; MD5                       |

View layer: for each view
`"<offset>:<length>:<run-layer checksum of its whole data>"`, joined by `|`,
MD5. The frame checksum (used by the
frame-checksum debugging callbacks of sources) consolidates each field with
copy first, so an FFmpeg field is digested as one view at offset 0.

Conformance: two data values that differ only in a run length, in an empty
run, or in a gap have different checksums.

## 12. Inline transform plumbing

The shared producer of inline codecs and bitstream filters (metadata and
track-mark relay, transform lifetime) is owned by
[inline-codecs.md, The inline producer](inline-codecs.md#3-the-inline-producer).

## 13. Settings

| Key                                                        | Type | Default | Effect                                                                                                                                         |
| ---------------------------------------------------------- | ---- | ------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `settings.ffmpeg.content.copy.relaxed_compatibility_check` | bool | `false` | §3.1: copy parameters are compatible when media and codec ids are equal; relaxes the type-level merge and the encoder's runtime check together |

`settings.ffmpeg.content` is an empty section holding the above. The relaxed
setting is read at each comparison, so a change applies from the next merge
and the next source switch.

The relaxed setting's documentation MUST say that it is an advanced setting,
that it lets sources with different stream headers into one copied stream,
and that the muxer then decides whether the output stays valid.

## 14. Raw audio conformance

Every producer of raw audio (raw decoder, FFmpeg filter outputs, the raw
inline encoder `track.ffmpeg.raw.encode.audio`) delivers
frames through a conformer bound to the track's format. The format is open
until the first frame, and the first frame sets it, as for video (§9).

For each frame, with `declared` the format's parameters and `params` the
frame's (§3.2):

1. `compatible(declared, params)` → merge the frame's parameters into the
   format and pass the frame on.
2. Otherwise convert the frame to `declared` (channel layout, sample format
   and sample rate, each from the frame where `declared` leaves it unset)
   with a resampler, and pass on its output. A frame the resampler rejects
   is an item fault (§4.3). The resampler's output covers
   the input's duration. The resampler drains when it is replaced or the
   stream ends ([draining.md](draining.md)).

No track mark is inserted and the track does not end. The first conversion
on a track is logged at info level with both formats.

Conformance: a raw audio stream whose sample rate changes mid-file keeps the
first frame's format and one track, and the number of output samples matches
the input's duration at that rate, up to the resampler's delay.
