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

Before reading on, it helps to be familiar with [sources](sources.html) and
[latency](latency_control.html), which tie closely into clock behavior.

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

By default, Liquidsoap clocks operate in _automatic mode_. At the start of
each streaming cycle, the clock inspects its source graph looking for a
_synchronization source_ — an operator that controls the pace of data flow by
its own means:

- Hardware audio operators (`input.alsa`, `output.pulseaudio`, etc.) block on
  the hardware timer, waiting until the soundcard is ready for the next chunk.
- Network inputs like `input.srt` use timestamps embedded in SRT packets to
  regulate delivery.
- File-based and generator sources (`playlist`, `single`, `sine`, `blank`,
  etc.) declare no synchronization source. When no sync source is present, the
  clock is CPU-led: it advances at real-time speed, sleeping when ahead and
  logging a warning when it falls behind.

Consider this script:

```{.liquidsoap include="liq/clock-alsa-file.liq"}

```

At startup you will see:

```
[clock:3] Starting top-level clock output.file with sources: output.file (output), amplify (passive), input.alsa (active) and sync: auto
```

Sources marked `active` are animated every streaming cycle regardless of
whether they are being pulled downstream — `input.alsa` must consume incoming
audio continuously even when nothing is asking for it. Sources marked `passive`
are only animated when something downstream requests data. Once the clock finds
a sync source, it hands over timing control:

```
[clock.output.file:3] Switching to self-sync mode with sync source: alsa
```

By contrast, a script with no hardware or network source:

```{.liquidsoap include="liq/clock-sine-file.liq"}

```

produces no sync source, so the clock runs under CPU control. You will also see
this message whenever a sync source disappears and the clock reverts to CPU-led
mode:

```
[clock.output.file:3] Switching to non self-sync mode
```

## Catchup warnings

When a clock falls behind real time — because frame computation is taking longer
than the frame duration — it will attempt to catch up by running faster than real
time, and log a warning:

```
[clock.pulseaudio:2] We must catchup 0.86 seconds!
```

This usually indicates CPU overload, a slow network operation blocking the
streaming loop, or a source that is consistently too slow. Buffers help absorb
short-lived disturbances. For persistent overload, reducing the number of
simultaneous effects or encodings is the right approach. If you find the
catchup messages noisy without indicating a real problem, you can reduce their
frequency:

```{.liquidsoap include="liq/clock-log-delay.liq"}

```

This limits the warning to at most once per minute.

## Clock conflicts

A conflict occurs when two sources that each control their own latency are
simultaneously active in the same clock. The clock has no way to honor two
different paces at once.

It is worth noting that two synchronization sources can coexist in the same
clock without conflict, as long as only one is ever producing data at a time.
A `fallback` between an SRT input and a local microphone is perfectly fine:

```{.liquidsoap include="liq/clock-srt-alsa-fallback.liq"}

```

Since only one branch is active at any moment, the clock always sees exactly
one sync source. The situation becomes problematic when both are simultaneously
active — as in:

```{.liquidsoap include="liq/clock-alsa-pulseaudio-conflict.liq"}

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

```{.liquidsoap include="liq/clock-srt-crossfade-conflict.liq"}

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

```{.liquidsoap include="liq/clock-alsa-pulseaudio-buffer.liq"}

```

The ALSA output will lag approximately one second behind PulseAudio — an
acceptable price for decoupling two independent hardware clocks. The `buffer`
parameter controls how much audio is pre-buffered; `max` sets the upper limit
(10 seconds by default). For persistent timing drift between two devices,
`buffer.adaptative` can compensate by slightly adjusting the playback rate to
keep the buffer full, at the cost of a small pitch shift.

For the `crossfade` conflict, wrapping the input in a buffer breaks the
coupling:

```{.liquidsoap include="liq/clock-srt-crossfade-buffer.liq"}

```

The buffer places `input.srt` in its own clock, leaving the downstream
crossfade free to control its own timing.

## Disabling self-synchronization

An alternative fix for some conflicts is to pass `self_sync=false` to one of
the conflicting operators, explicitly surrendering its synchronization role:

```{.liquidsoap include="liq/clock-alsa-pulseaudio-self-sync.liq"}

```

This avoids any added latency, but it comes with a caveat: the two devices are
running on slightly different hardware clocks. Without a buffer to absorb the
drift, timing differences will accumulate and eventually cause glitches. This
approach is convenient for development and testing, but is not recommended for
production.

## Decoupling latencies

Beyond resolving conflicts, explicit clock separation is a useful design tool.
Consider a microphone being recorded to a file and simultaneously streamed to
Icecast:

```{.liquidsoap include="liq/clock-decoupling-conflict.liq"}

```

Here all three operators share the ALSA clock. If the Icecast connection stalls,
the entire clock stalls with it — including the file recording. Network hiccups
will cause gaps in what should be a pristine local backup.

Wrapping the Icecast output in a buffer moves it to its own clock:

```{.liquidsoap include="liq/clock-decoupling.liq"}

```

The ALSA clock now advances independently of the network. File recording and
any other local processing are unaffected by Icecast latency or connection
problems. The `mksafe` is necessary because the buffered side runs in a
different clock: if `mic` becomes unavailable, the Icecast output needs a safe
fallback.

## Parallel encoding with `clock.assign_new`

Each clock runs in its own thread, which means sources assigned to different
clocks can run on separate CPU cores. This matters most for video encoding,
which is typically the most CPU-intensive part of a streaming workflow.

When two outputs share the same default clock, they encode sequentially:

```{.liquidsoap include="liq/clock-parallel-sequential.liq"}

```

Assigning `b` to a dedicated clock allows both encoders to run in parallel:

```{.liquidsoap include="liq/clock-parallel.liq"}

```

If both outputs encode a shared source, a `buffer` is still needed to bridge
the clocks — but be aware that if the two clocks drift enough to overflow or
underflow the buffer, occasional glitches may result.

## Inspecting clocks

A source's clock is accessible via the `.clock` method:

```{.liquidsoap include="liq/clock-inspect.liq" from="BEGIN" to="END"}

```
