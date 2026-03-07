# Visualizing clocks and sources

As Liquidsoap scripts grow more complex — multiple clocks, many sources, transitions, time-dependent behaviors — it can become difficult to reason about how everything fits together. To help with this, Liquidsoap can generate text-based graphs of clocks and sources, giving you a structural overview of how time flows, how sources are connected, and which sources are actively animated.

These graphs are useful when writing or debugging a script, investigating unexpected timing behavior, or simply understanding how a setup is wired.

## What is displayed

Liquidsoap can generate two related graphs:

- **Clock graph** — shows clocks, their relationships, and which sources are attached to each clock
- **Source graph** — shows sources and how they are connected to each other

To build these graphs, Liquidsoap runs the script briefly so it can observe clocks, sources, and activations. No actual output needs to be produced, but time must advance for clocks to become observable.

## Clocks

Clocks control how time advances for sources. The clock graph displays:

- Parent/child relationships between clocks
- Each clock's internal time and tick count
- Which sources are active or passive on each clock

The internal time is particularly useful when reasoning about operators such as `crossfade` and `stretch`, which may run their clock faster than real time in order to buffer and prepare transitions ahead of when they are needed. When a source is marked with `self_sync = true`, it controls its own timing and cannot be accelerated — an important detail when reasoning about transitions.

## Active sources

An active source is one that is continuously animated by its clock even when it is not currently selected or producing output.

For example:

- `input.http`, which must actively pull data from a remote stream at all times to keep its buffer filled
- `input.ffmpeg`, which may need to be configured as active or passive depending on whether it is reading from a file or a live URL

Knowing which sources are active helps explain background activity that might otherwise be surprising.

## Source graphs

The source graph shows how sources are connected and how they are animated. Animation flows from top to bottom: the top is typically an output, or a source animated by an external driver such as `crossfade`. Sources lower in the graph are driven by whatever is above them.

For example, a `crossfade` or `stretch` operator may run its own faster clock and animate its inputs at an accelerated rate to prepare transitions or perform resampling before they are needed in real time.

## Using the command line

You can generate these graphs directly from the command line:

```
liquidsoap --describe-clocks script.liq
liquidsoap --describe-sources script.liq
```

When these options are used, the script starts, runs briefly so clock and source information can be gathered, then stops and prints the requested graph. You can adjust how long the script runs before dumping with:

```
--dump-delay <seconds>
```

This provides a quick, non-intrusive way to inspect a script without running it fully.

## Using the telnet interface

If your script is already running with the telnet server enabled, you can request graphs interactively:

- `clock.dump` — display the clock graph
- `clock.dump_all_sources` — display the source graph

## Using the scripting API

Graphs can also be generated from within a script:

- `clock.dump()` — dump the clock graph
- `clock.dump_all_sources()` — dump the source graph

This makes it possible to integrate graph output into custom tooling or monitoring setups.

## Examples

The following outputs are from a script that uses `crossfade` for transitions. They illustrate something important: `crossfade` needs to read data from its input sources ahead of time in order to compute transitions before they are due to play. To do this, it runs its inputs on a separate, faster-running clock — its own internal timeline. This is why the clock graph shows not one but two clocks: the top-level clock, which drives the outputs and operates at real time, and the crossfade's own clock, which drives its input sources and can advance faster than real time to buffer and prepare transition data.

In other words, while the main stream experiences time normally, the sources feeding into the crossfade are living on a faster timeline — producing data in advance so the transition sounds seamless when it arrives.

### Clock graph

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

Notice that the `cross` clock has advanced to 2.64s while the parent clock is only at 0.12s — the crossfade has been accelerating its sources around transition times to pre-compute transition data ahead of real time.

### Source graph

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

The source graph shows evaluation flowing from top to bottom: outputs at the top drive the sources below them. A `(*)` marks a source that has already appeared elsewhere in the graph — it is referenced again rather than expanded a second time.

## When to use this feature

Clock and source graphs are most helpful when:

- Designing or refactoring a complex script
- Debugging timing, synchronization, or activation issues
- Understanding how `self_sync` and clock acceleration interact
- Explaining how a script is structured to someone else
