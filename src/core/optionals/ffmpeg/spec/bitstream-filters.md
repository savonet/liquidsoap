# FFmpeg bitstream filters

This file owns the `ffmpeg.filter.bitstream.*` operators: one per FFmpeg
bitstream filter and media kind, applied to encoded (`ffmpeg.copy`) content.
It relies on the bitstream filter bindings
([avcodec.md §4.8](../../../../modules/synced/ffmpeg/spec/avcodec.md#48-bitstreamfilter)),
copy content and its parameters ([content.md §3.1](content.md#31-ffmpegcopy),
[§4.2](content.md#42-run-layer), [§5](content.md#5-stream_idx-and-time_base)),
the argument mapping of [filters.md §5](filters.md#5-arguments-from-avoptions),
the duration helper ([common.md §6](common.md#6-duration-helper)), draining
([draining.md](draining.md)) and the inline producer
([inline-codecs.md §3](inline-codecs.md#3-the-inline-producer)).

## 1. Which operators

At start-up, each bitstream filter of the bindings' list gives operators
according to the codecs it declares:

| Declared codecs                                 | Modes        | Operators                                                                            |
| ----------------------------------------------- | ------------ | ------------------------------------------------------------------------------------ |
| none, or at least one audio and one video codec | audio, video | `ffmpeg.filter.bitstream.<name>.audio`, `ffmpeg.filter.bitstream.<name>.video`       |
| audio codecs only                               | audio        | `ffmpeg.filter.bitstream.<name>`                                                     |
| video codecs only                               | video        | `ffmpeg.filter.bitstream.<name>`                                                     |
| neither audio nor video codecs                  | none         | no operator; logs at info `No valid mode found for filter <name>!` (example wording) |

A codec counts as audio or video when it is in the bindings' audio or video
codec identifier lists. Filters for subtitle or data codecs only (for
instance `pgs_frame_merge`, `eia608_to_smpte436m`) get no operator, because
the layer has no copy track type to apply them to. Examples of operators:
`ffmpeg.filter.bitstream.h264_mp4toannexb`,
`ffmpeg.filter.bitstream.aac_adtstoasc`,
`ffmpeg.filter.bitstream.null.audio`, `ffmpeg.filter.bitstream.null.video`.

Category `Source / FFmpeg filter`, flag `extra`, description `FFmpeg <path>
bitstream filter. See ffmpeg documentation for more details.`, where
`<path>` is `<name>` for a single mode and `<name>.audio` / `<name>.video`
otherwise. Operator names and argument names are frozen.

## 2. Signature

Arguments: the generic `id` of operators, one optional argument per option
name of the filter (types, documentation and validation as
[filters.md §5](filters.md#5-arguments-from-avoptions)), and the source,
unlabelled. The source's type has the filtered field (`audio` or `video`)
of copy content and any other fields; the result has the same fields, the
others unified with the source's. The printed form follows
[side-data.md §5](side-data.md#5-trackffmpegside_data): the filtered field's
format variable differs between input and output.

The filtered field of the result has a copy format of its own, separate from
the input's: a bitstream filter changes codec parameters (for instance
`h264_mp4toannexb` turns AVCC extradata into Annex B), so the output's
parameters are the filter's, never the input's. The operator accepts any
parameters on its input: no merge applies there. Only the output format is
merged, as §5 states, with the compatibility split of
[content.md §3.1](content.md#31-ffmpegcopy).

Examples (`liquidsoap -h`):

```
ffmpeg.filter.bitstream.h264_mp4toannexb :
  (?id : string?, source(video=ffmpeg.copy('a), 'b)) ->
  source(video=ffmpeg.copy('c), 'b)

ffmpeg.filter.bitstream.noise.audio :
  (?id : string?, ?amount : string?, ?drop : string?, ?dropamount : int?,
   source(audio=ffmpeg.copy('a), 'b)) -> source(audio=ffmpeg.copy('c), 'b)
```

Composition: the operator inherits composition from its effective source.

## 3. Options

The rendered argument pairs
([filters.md §5.4](filters.md#54-validation-and-rendering)) become the
filter's option table, passed to the bindings' filter initialisation. An
array value is rendered as one string, its elements' texts joined with the
option's array separator, `,` when the option declares none.

Option names and values are validated at script evaluation (configuration
errors, [faults.md §2](faults.md#2-classes)). An option the filter leaves
unused after its first initialisation
([avutil.md §9.1](../../../../modules/synced/ffmpeg/spec/avutil.md#91-the-option-table-protocol))
is a configuration error too: the operator raises a hard error naming the
option, and the script fails. Every argument comes from the filter's own
option class, so this happens only when FFmpeg declines an option it
declares; it is detected at the first initialisation because the bindings
initialise a filter only with codec parameters.

## 4. Structure

The operator is an inline producer
([inline-codecs.md §3](inline-codecs.md#3-the-inline-producer)): it reads its
source in a child clock, relays metadata and track marks as that section
states, and converts each input frame through a transform, the **handler**
of §5. Other fields than the filtered one go into the buffer verbatim.
Self-sync, readiness, seek, remaining and abort follow the producer.

A bitstream filter keeps the incoming source id
([content.md §5](content.md#5-stream_idx-and-time_base)) and holds no
source-id record. The handler's lifetime follows
[inline-codecs.md §3.2](inline-codecs.md#32-transform-lifetime) without the
record row:

| Event                                                            | Handler                               |
| ---------------------------------------------------------------- | ------------------------------------- |
| end of input (the producer is read while its input is not ready) | drained (§6)                          |
| a run with another source id                                     | drained (§6), then replaced (§5)      |
| the producer seeks its input, `abort_track`, sleep               | discarded                             |
| a call into the filter fails                                     | discarded; the packet is dropped (§7) |

A track boundary does not end the handler.

## 5. Packets through the filter

The handler is the state for one source id: the bitstream filter instance,
its output parameters, its input and output time bases, and a duration
helper.

For each run of the content, at its absolute start
([content.md §4.2](content.md#42-run-layer)), and each packet of the run, in
order:

1. When there is no handler, or the run's source id differs from the
   handler's, drain the current handler (§6) and build a new one:
   1. initialise the bitstream filter with the options (§3), applied
      afresh at every build (a handler never inherits option state from a
      previous one), the run's
      source parameters and the run's time base as the filter's input time
      base; the bindings return the filter's output parameters and output
      time base (libav's `time_base_out`, often equal to the input's);
   2. merge the output parameters into the output field's format before
      emitting anything under this id
      ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)).
      A failed merge is an error of the operator: it emits nothing for that
      source and reports both parameter sets;
   3. create a duration helper in DTS mode, `src` the filter's output time
      base,
      `convert_ts` off, `offset` 0, `start` the output field's position in
      the producer's buffer.
2. A run of the handler's source id with another time base breaks
   [content.md §5](content.md#5-stream_idx-and-time_base). The operator
   logs a warning naming both time bases, drains the handler (§6) and builds
   a new one for the new time base, as for a new source id. When the new
   handler's output time base differs from the previous one's, the output
   runs change time base within one id, which is the same breach on the
   output side; downstream consumers handle it by the same rule.
3. A packet whose DTS is below the last DTS pushed into the handler's
   helper is dropped with a warning naming both values, and is not sent to
   the filter. Equal DTS are accepted. A packet without DTS is sent to the
   filter and passed on without DTS; the helper groups it with its
   neighbours ([common.md §6.2](common.md#62-push-t-item)). Filling a missing
   DTS is the encoder's
   ([timestamps.md §6.4](timestamps.md#64-missing-values)).
4. Send the packet to the filter.
5. Receive packets until the filter answers "again" or "end of stream".
   Each received packet is pushed into the duration helper; a received
   packet whose DTS is below the helper's last is dropped with a warning; each chunk the
   helper emits is put into the filtered field as one run with the
   handler's source id, the filter's output time base and output
   parameters.

Packets keep their timestamps. The helper holds the last group back, so the
filtered field lags the other fields by up to one packet; the producer's
reading waits for the shortest field
([inline-codecs.md §3](inline-codecs.md#3-the-inline-producer)).

## 6. Drain

Draining a handler (a stage in the sense of [draining.md](draining.md)):

1. Send end of stream to the filter.
2. Receive and put packets as §5, step 5, until "end of stream".
3. Flush the duration helper ([common.md §6.3](common.md#63-flush-t)) and
   put its chunk, when it returns one, as one run. An empty helper returns
   nothing, and nothing is put.

The drained output of a handler precedes the first output of its
replacement.

Conformance:

- Flushing a handler that holds nothing adds nothing to the buffer.
- `h264_mp4toannexb` on a copy of a file: the last packet of the file
  reaches the output, and the output's parameters carry the Annex B form.
- A copy track switching from one source to another through a bitstream
  filter: the packets of the first source all precede those of the second,
  and the second source's output parameters are merged before its first
  packet.

## 7. Failure

Faults follow [faults.md](faults.md). Nothing raises out of the operator
while it streams, except the hard errors below.

**Fallibility.** The operator is fallible exactly when its input is
([inline-codecs.md §3](inline-codecs.md#3-the-inline-producer)). It never
invents content: no empty runs stand in for packets it drops.

**Source fault** of the operator
([faults.md §3](faults.md#3-source-faults-reach-the-script)), for one source
id:

1. the operator ends its track at the fault, after what it already
   buffered; the handler is drained when it can be, and discarded when the
   filter itself failed ([draining.md §5](draining.md#5-discarded-on-purpose));
2. a fallible operator is not ready until `fault_retry_delay`
   ([faults.md §3](faults.md#3-source-faults-reach-the-script)) has passed;
   it is then read again, and the next packet rebuilds the handler, for the
   same source id or another;
3. an infallible operator cannot be not ready and must not invent content:
   a fault that persists (the rebuilt handler faults again before emitting
   a packet) raises a hard error naming the operator and the cause, and the
   script fails.

`max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate))
bounds the item faults of one source id: that many consecutive dropped
packets escalate to a source fault.

**Operator documentation.** Each operator's documentation states that it is
fallible when its input is, and that with an infallible input a persistent
data fault is a hard error; a script that must keep streaming wraps the
operator in `mksafe` or a `fallback`.

| Situation                                                | Class                                                         | Result                                                                                                                                    |
| -------------------------------------------------------- | ------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| option value invalid                                     | configuration error                                           | error 7 at script evaluation ([filters.md §5.4](filters.md#54-validation-and-rendering))                                                  |
| option unused by the filter                              | configuration error                                           | hard error at the first initialisation (§3)                                                                                               |
| filter initialisation fails for a source                 | source fault                                                  | error logged with FFmpeg's message; the steps above                                                                                       |
| output parameters fail to merge                          | source fault                                                  | error naming both parameter sets; the steps above (§5)                                                                                    |
| send or receive fails other than "again"/"end of stream" | item fault ([faults.md §4](faults.md#4-item-faults-escalate)) | warning with FFmpeg's message; handler discarded, packet dropped; the next packet builds a handler                                        |
| run with packets and unknown parameters                  | item fault                                                    | the run's packets are dropped with a warning ([content.md §4.3](content.md#43-invalid-content)); counted against `max_consecutive_faults` |
| time-base change within one source id                    | item fault, repaired                                          | warning; handler drained and rebuilt (§5); nothing is lost                                                                                |
| decreasing DTS within one source id                      | item fault                                                    | packet dropped with a warning (§5); counted against `max_consecutive_faults`                                                              |

Repeated warnings of one kind are counted and logged with their count once
per `fault_log_interval` ([faults.md §4](faults.md#4-item-faults-escalate)).
