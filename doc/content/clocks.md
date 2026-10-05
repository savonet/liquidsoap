# Clocks in Liquidsoap

Every source in Liquidsoap is attached to a _clock_, assigned at startup and fixed
for the lifetime of the script. At regular intervals determined by the configured
frame duration, the clock asks the active sources it controls to generate the next
frame. The clock's job is to make sure this happens at the right rate.

In simple scripts, a single clock governs everything and you never have to think
about it. But as soon as your script involves hardware audio, network streams, or
operators like `crossfade` that manipulate the flow of time, the picture becomes
more complex. This page explains why multiple clocks exist, what happens when
they conflict, and how to manage them explicitly when needed.

Before reading on, it helps to be familiar with [sources](./sources.md) and
with [the streaming loop](./script_lifecycle.md#the-streaming-loop), which
clocks drive. When you want to find out why a clock falls behind, see
[performance and monitoring](./performance.md).

## Why multiple clocks?

There are two distinct reasons why a script may require more than one clock.

The first is **external**: the real world does not run on a single clock.
Different soundcards have their own hardware oscillators, ticking at slightly
different rates than the CPU and from each other. Network protocols like SRT
embed timing information in their packet stream and use it to regulate delivery.
For a system expected to run continuously — a radio station, for instance — a
discrepancy of even one millisecond per second accumulates to 43 minutes of
drift over a month. Left undetected, this would eventually require inserting
silence or dropping content to resynchronize. Liquidsoap makes this a hard
error rather than a silent problem: sources that control their own timing are
assigned to separate clocks so that any incompatibility is caught immediately.

The second reason is **internal**: some operators need to consume data from
their input at a different rate than they produce output. The `stretch` operator
changes playback speed explicitly. The `crossfade` operator is more subtle:
during a transition, it needs to read ahead into the next track while still
outputting the current one, temporarily pulling data at twice the normal rate.
After ten crossfaded tracks of six seconds each, the input source will have
advanced by a full minute more than the output. If the input and the downstream
output shared a clock, this would be impossible to schedule correctly. Running
the input in a separate clock resolves the ambiguity cleanly.

## Automatic clock mode

Each clock produces data one _frame_ at a time. A frame lasts
`settings.frame.duration` seconds, 0.02 seconds by default. If the clock
produces a frame in less time than the frame lasts, it is ahead and waits
before producing the next frame. If the clock takes longer, it falls behind
and produces the next frames without waiting, to catch up.

By default, Liquidsoap clocks operate in _automatic mode_. At the start of
each streaming cycle, the clock inspects its source graph looking for a
_synchronization source_ — an operator that controls the pace of data flow by
its own means:

- Hardware audio operators (`input.alsa`, `output.pulseaudio`, etc.) block on
  the hardware timer, waiting until the soundcard is ready for the next chunk.
- Network inputs like `input.srt` use timestamps embedded in SRT packets to
  regulate delivery. Some sources, such as `input.ffmpeg`, have to be set as
  self-sync manually with `self_sync=true`. Typically, `input.ffmpeg` should
  be self-sync when decoding a `libsrt` or `rtmp` input.
- File-based and generator sources (`playlist`, `single`, `sine`, `blank`,
  etc.) declare no synchronization source. Icecast and Shoutcast outputs
  (`output.icecast`, `output.shoutcast`) do not control time either. When no
  sync source is present, the
  clock is CPU-led: it advances at real-time speed, sleeping when ahead and
  logging a warning when it falls behind.

Consider this script:

```{.liquidsoap include="clock-alsa-file.liq"}

```

At startup you will see:

```
[clock.output.file:3] Starting top-level clock, sync: auto, sources: input.alsa (active), amplify (passive), output.file (output), animated by a task (rests)
```

Sources marked `active` are animated every streaming cycle regardless of
whether they are being pulled downstream — `input.alsa` must consume incoming
audio continuously even when nothing is asking for it. Sources marked `passive`
are only animated when something downstream requests data. Once the clock finds
a sync source, it hands over timing control. A sound card paces the stream by
blocking, so the clock also moves to a thread of its own:

```
[clock.output.file:3] Animator changes from task to thread (blocks: alsa)
[clock.output.file:3] Now paced by sync source alsa
```

By contrast, a script with no hardware or network source:

```{.liquidsoap include="clock-sine-file.liq"}

```

produces no sync source, so the clock runs under CPU control. When a sync
source disappears, the clock takes the pacing back and logs:

```
[clock.output.file:3] Sync source alsa left: the clock paces the stream (latency: 0.10s, maximum latency: 60.00s)
```

### Switching between time sources

Your stream may switch between different types of sources. For example:

```liquidsoap
fallback([input.srt(...), playlist("~/music")])
```

In this case:

- `playlist` is CPU-controlled.
- `input.srt` is self-sync.

Liquidsoap decides which component controls the clock at every round of the
streaming loop. It follows the self-sync sources that are producing data in
that round, and uses the CPU the rest of the time.

`playlist` reads files from the disk, so its data is available at any speed.

`input.srt` is self-sync: the SRT library delivers each packet at the time set
by the sender, and a read blocks until then. This wait paces the clock.

`input.srt` is also an active source: the clock reads from it on every round,
whichever source the fallback plays. It controls the clock while a sender is
connected:

- With a sender connected, the clock follows `input.srt`. This starts at the
  connection, before the fallback has switched to it.
- With no sender, the clock is CPU-controlled.

The log shows each change:

```
[clock.output:3] Now paced by sync source srt
[clock.output:3] Sync source srt left: the clock paces the stream (latency: 0.10s, maximum latency: 60.00s)
```

Which of the two is playing at a given moment is a separate question, answered
by [source composition](./composition.md): `input.srt` is a live source, so it
cuts into `playlist` mid-track rather than waiting for the end of the file.

## Catchup warnings

When a clock falls behind real time — because frame computation is taking longer
than the frame duration — it will attempt to catch up by running faster than real
time, and log a warning:

```
[clock.pulseaudio:2] Latency is too high: we must catchup 0.86 seconds!
[clock.pulseaudio:3] Since the last warning: 50 ticks, producing 1.860s, resting 0.000s, released 0.000s, no worker 0.000s, slowest source: output.pulseaudio
```

The second line says where the clock's time went since the previous warning:
`producing` is the time spent computing frames, and `no worker` the time spent
waiting for a free core. The slowest source is named at the end.

This usually indicates CPU overload, a slow network operation blocking the
streaming loop, or a source that is consistently too slow. Buffers help absorb
short-lived disturbances. For persistent overload, reducing the number of
simultaneous effects or encodings is the right approach.
[Diagnosing latency issues](./performance.md#diagnosing-latency-issues) walks
through the usual causes. If you find the
catchup messages noisy without indicating a real problem, you can reduce their
frequency:

```{.liquidsoap include="clock-log-delay.liq"}

```

This limits the warning to at most once per minute. See
[what is latency?](./performance.md#what-is-latency) for the other settings
involved.

## Clock conflicts

A conflict occurs when two sources that each control their own latency are
simultaneously active in the same clock. The clock has no way to honor two
different paces at once.

Two synchronization sources can coexist in the same clock without conflict,
as long as only one is producing data at a time.
A `fallback` between an SRT input and a local microphone is perfectly fine:

```{.liquidsoap include="clock-srt-alsa-fallback.liq"}

```

Since only one branch is active at any moment, the clock always sees exactly
one sync source. The situation becomes problematic when both are simultaneously
active — as in:

```{.liquidsoap include="clock-alsa-pulseaudio-conflict.liq"}

```

This fails with:

```
Error 17: clock output.alsa has multiple synchronization sources. Do you need to set self_sync=false?

Sync sources:
 alsa from source output.alsa
 pulseaudio from source output.pulseaudio
```

Both ALSA and PulseAudio try to control the clock simultaneously, and
Liquidsoap refuses to proceed.

A different kind of conflict arises with operators like `crossfade` and
`stretch`, which run in their own dedicated clock and require that their input
has no synchronization source. Passing a hardware or network source directly
will fail:

```{.liquidsoap include="clock-srt-crossfade-conflict.liq"}

```

```
Error 7: Invalid value:
This source may control its own latency and cannot be used with this operator.
```

The problem here is that `s` would need to be produced simultaneously at two
different rates — normal speed and an accelerated speed for the crossfade
lookahead — which is impossible for a source that controls its own timing.

## Resolving conflicts with buffers

The standard solution for clock conflicts is the `buffer` operator. It sits
between two clock domains and pre-computes a small reserve of audio (one second
by default), absorbing the timing differences between them. Because it decouples
the clocks of its input and output, Liquidsoap allows the two sides to belong to
different clocks.

For the dual-output conflict above, wrapping one side in a buffer resolves it:

```{.liquidsoap include="clock-alsa-pulseaudio-buffer.liq"}

```

The ALSA output will lag approximately one second behind PulseAudio — an
acceptable price for decoupling two independent hardware clocks. The `buffer`
parameter controls how much audio is pre-buffered; `max` sets the upper limit
(10 seconds by default). For persistent timing drift between two devices,
`buffer.adaptative` can compensate by slightly adjusting the playback rate to
keep the buffer full, at the cost of a small pitch shift.

For the `crossfade` conflict, wrapping the input in a buffer breaks the
coupling:

```{.liquidsoap include="clock-srt-crossfade-buffer.liq"}

```

The buffer places `input.srt` in its own clock, leaving the downstream
crossfade free to control its own timing.

## Disabling self-synchronization

An alternative fix for some conflicts is to pass `self_sync=false` to one of
the conflicting operators, explicitly surrendering its synchronization role:

```{.liquidsoap include="clock-alsa-pulseaudio-self-sync.liq"}

```

This tells Liquidsoap to follow the timing of the PulseAudio output. The same fix
applies when a self-sync input feeds a self-sync output, for instance
`input.srt` played through `output.ao`: pass `self_sync=false` to the side
that should follow the other.

This avoids any added latency, but it comes with a caveat: the two devices are
running on slightly different hardware clocks. Without a buffer to absorb the
drift, timing differences will accumulate and eventually cause glitches. This
approach is convenient for development and testing, but is not recommended for
production.

## Decoupling latencies

Beyond resolving conflicts, explicit clock separation is a useful design tool.
Consider a microphone being recorded to a file and simultaneously streamed to
Icecast:

```{.liquidsoap include="clock-decoupling-conflict.liq"}

```

Here all three operators share the ALSA clock. If the Icecast connection stalls,
the entire clock stalls with it — including the file recording. Network hiccups
will cause gaps in what should be a pristine local backup.

Wrapping the Icecast output in a buffer moves it to its own clock:

```{.liquidsoap include="clock-decoupling.liq"}

```

The ALSA clock now advances independently of the network. File recording and
any other local processing are unaffected by Icecast latency or connection
problems. The `mksafe` is necessary because the buffered side runs in a
different clock: if `mic` becomes unavailable, the Icecast output needs a safe
fallback.

## Parallel encoding with `clock.assign_new`

Each clock is animated on its own, as a scheduler task by default, which means
sources assigned to different clocks can run on separate CPU cores. This matters most for video encoding,
which is typically the most CPU-intensive part of a streaming workflow.

When two outputs share the same default clock, they encode sequentially:

```{.liquidsoap include="clock-parallel-sequential.liq"}

```

Assigning `b` to a dedicated clock allows both encoders to run in parallel:

```{.liquidsoap include="clock-parallel.liq"}

```

If both outputs encode a shared source, a `buffer` is still needed to bridge
the clocks — but be aware that if the two clocks drift enough to overflow or
underflow the buffer, occasional glitches may result.

## Inspecting clocks and sources

When a script uses several clocks, many sources, transitions or
time-dependent operators, you may want to see how everything is wired.
Liquidsoap can print text graphs of the clocks and sources of a running
script. Use them to write or debug a script, to investigate unexpected timing
behavior, or to check how a setup is connected.

For a single source, the `.clock` method returns its clock:

```{.liquidsoap include="clock-inspect.liq" from="BEGIN" to="END"}

```

### What is displayed

Liquidsoap can generate two related graphs:

- The _clock graph_ shows the clocks, their parent and child relationships,
  and the sources attached to each clock.
- The _source graph_ shows the sources and how they are connected to each
  other.

Clocks and sources only exist once the script runs, so Liquidsoap has to run
the script for a moment before it can print these graphs.

### Clock graphs

For each clock, the clock graph displays:

- its child clocks,
- its internal time and tick count, and whether it is self-synchronized,
- its outputs, its active sources and its passive sources.

After each source, the names in brackets are the sources that activate it,
that is, the sources that pull data from it.

The internal time of a clock is useful when you look at operators such as
`crossfade` and `stretch`. These operators run their inputs on a child clock,
and that clock can run faster than real time to prepare transitions ahead of
time. A self-synchronized source (`self_sync` set to `true`) controls its own
timing and cannot run faster than real time, which is why these operators
reject it as input (see [clock conflicts](#clock-conflicts)).

### Active sources

An active source is animated by its clock on every streaming cycle, even when
nothing is currently pulling data from it. For example, `input.http` and
`input.ffmpeg` are active sources: they keep reading from the remote stream
so that their buffer stays filled.

Active sources explain background activity, such as network traffic from an
input that is not currently on air.

### Source graphs

The source graph shows, for each clock, how sources are connected and how they
are animated. Animation flows from top to bottom. The top of each tree is an
output, or a source animated from outside its clock, marked
`[external activation]`. Sources lower in the graph are animated by the
sources above them.

For example, in the source graph below, `audio.producer` is animated by the
`output.icecast` clock. It appears at the top of its own clock's tree, marked
`[external activation]`.

### Using the command line

You can generate these graphs from the command line:

```
liquidsoap --describe-clocks script.liq
liquidsoap --describe-sources script.liq
```

With these options, Liquidsoap starts the script, runs it for a short while
(0.1 seconds by default), prints the requested graph and stops. The script
runs for real during that time: outputs start, connect and write data. You
can change how long the script runs before the graph is printed with:

```
--dump-delay <seconds>
```

These options are experimental.

### Using the telnet interface

If your script is already running with the telnet server enabled, you can
request graphs interactively:

- `clock.dump` displays the clock graph.
- `clock.dump_all_sources` displays the source graph.

### Using the scripting API

You can also get the graphs from within a script. Both functions return a
string:

- `clock.dump()` returns the clock graph.
- `clock.dump_all_sources()` returns the source graph.

You can, for instance, log them or send them to your own monitoring tools.

### Examples

The following outputs come from a script that uses `crossfade` for
transitions. To compute a transition, `crossfade` reads data from its input
before that data is due to play. It does this by running its input on a child
clock, `cross`, which advances faster than real time when needed. The
top-level clock, `output.icecast`, drives the outputs at real time.

#### Clock graph

```
· output.icecast (ticks: 3, time: 0.06s, self_sync: false)
  ├── outputs: output.icecast [output.icecast], output.file [output.file]
  ├── active sources:
  ├── passive sources: audio.producer [ffmpeg_encode_audio],
  │                    ffmpeg_encode_audio [output.icecast, output.file]
  └── audio.producer (ticks: 6, time: 0.12s, self_sync: false)
      ├── outputs: audio.consumer [audio.producer, audio.consumer]
      ├── active sources:
      ├── passive sources: safe_blank.1 [mksafe.1], safe_blank [mksafe],
      │                    cross [track_metadata_deduplicate, metadata_deduplicate],
      │                    track_metadata_deduplicate [metadata_deduplicate],
      │                    metadata_deduplicate [mksafe, insert_initial_track_mark.6],
      │                    mksafe [metadata_map.2, metadata_map.3],
      │                    metadata_map.2 [metadata_map.3],
      │                    metadata_map.3 [mksafe.1, insert_initial_track_mark.1],
      │                    mksafe.1 [audio.consumer], insert_initial_track_mark [],
      │                    insert_initial_track_mark.1 [mksafe.1],
      │                    insert_initial_track_mark.6 [mksafe]
      └── cross (ticks: 132, time: 2.64s, self_sync: false)
          ├── outputs:
          ├── active sources:
          └── passive sources: source [switch, switch.1],
                               audio [switch, switch.1, insert_initial_track_mark.2],
                               switch [switch.1, insert_initial_track_mark.3],
                               switch.1 [switch.2, insert_initial_track_mark.4],
                               request_queue [switch.2],
                               switch.2 [switch.3, insert_initial_track_mark.5],
                               request_queue_1 [switch.3], switch.3 [metadata_map, metadata_map.1],
                               metadata_map [metadata_map.1],
                               metadata_map.1 [track_amplify, amplify], track_amplify [amplify],
                               amplify [cross, cross], insert_initial_track_mark.2 [switch],
                               insert_initial_track_mark.3 [switch.1],
                               insert_initial_track_mark.4 [switch.2],
                               insert_initial_track_mark.5 [switch.3], cross.eos_buffer [cross]
```

The `cross` clock is at 2.64s while its parent clock is at 0.12s: the
crossfade has run its input ahead of real time to prepare transitions.

#### Source graph

```
Clock output.icecast:
Outputs:
· output.icecast [output]
  └── ffmpeg_encode_audio [passive]
      └── audio.producer [passive]
· output.file [output]
  └── ffmpeg_encode_audio [passive] (*)

Clock audio.producer (controlled by output.icecast):
Outputs:
· audio.producer [external activation]
  └── audio.consumer [output]
      └── mksafe.1 [passive]
          ├── safe_blank.1 [passive]
          ├── metadata_map.3 [passive]
          │   ├── mksafe [passive]
          │   │   ├── safe_blank [passive]
          │   │   ├── metadata_deduplicate [passive]
          │   │   │   ├── cross [passive]
          │   │   │   └── track_metadata_deduplicate [passive]
          │   │   │       └── cross [passive] (*)
          │   │   └── insert_initial_track_mark [passive]
          │   │       └── safe_blank [passive] (*)
          │   └── metadata_map.2 [passive]
          │       └── mksafe [passive] (*)
          └── insert_initial_track_mark.1 [passive]
              └── metadata_map.3 [passive] (*)
```

Evaluation flows from top to bottom: outputs at the top drive the sources
below them. A `(*)` marks a source that already appears elsewhere in the graph
and is not expanded a second time.

### When to use this feature

Clock and source graphs help you to:

- design or refactor a complex script,
- debug timing, synchronization or activation issues,
- check how `self_sync` and child clocks interact,
- explain the structure of a script to someone else.
