# 📊 Visualizing clocks and sources

Liquidsoap scripts can grow complex: multiple clocks, many sources, transitions, and time-dependent behaviors interacting together. To help reason about this, Liquidsoap can now **display graphs of clocks and sources**, giving you a visual overview of how time flows, how sources are connected, and which sources are actively animated.

These graphs are useful while **writing or debugging a script**, inspecting unexpected timing behavior, or **explaining a setup** to others.

## 🧠 What is displayed?

Liquidsoap can generate two related graphs:

- **Clock graph** — shows clocks, their relationships, and which sources are active on each clock
- **Source graph** — shows sources and how they are connected

To build these graphs, Liquidsoap needs to **run the script for a short time** so it can observe clocks, sources, activations, and time flows. No actual output needs to be produced, but time must advance for clocks to be observable.

## 🕰️ Understanding clocks

Clocks control how time advances for sources. The **clock graph** displays:

- Parent/child relationships between clocks
- Each clock’s **internal time** and tick count
- Which sources are **active** on each clock

The internal time is especially useful for reasoning about operators such as `crossfade` and `stretch`, which may accelerate their clock relative to real time to prepare transitions before they play. When a clock or source is marked with **`self_sync = true`**, it can _not_ be accelerated — an important detail when reasoning about timing and transitions.

## 🔥 Active sources

The clock graph also highlights **active sources**. Active sources are always animated by their clock even if they are not currently producing output.

This includes for example:

- `input.harbor`, which must actively pull data from a remote Icecast stream when connected
- `input.ffmpeg`, which may be active or passive depending on whether it’s reading from a file or a remote URL

Understanding which sources are active helps explain background activity you might otherwise miss.

## 🔄 Understanding source graphs

The **source graph** shows how sources are connected, and — crucially — how they are animated:

- The graph is animated **from top to bottom**
- The top is usually an output, but it can also be an operator that drives time

For example, a `crossfade` or `stretch` operator may accelerate its clock and animate its inputs faster than real time to prepare transitions or resampling.

This top-to-bottom animation direction makes it easier to see:

- Which source is driving evaluation
- How transitions or clock acceleration affect lower sources

## 💻 Using the CLI

You can generate these graphs directly from the command line:

```bash
liquidsoap --display-clocks script.liq
liquidsoap --display-sources script.liq
```

When these options are used:

1. The script is started
2. It runs briefly so clock and source information can be gathered
3. The script is stopped
4. The requested graph is displayed

Adjust run duration with:

```bash
--dump-delay <seconds>
```

This offers a quick, non-intrusive way to inspect a script.

## 🔌 Using the server / telnet interface

If your script is already running with the server enabled, you can request graphs interactively:

- `clocks.dump` — display the clock graph
- `clocks.dump_sources` — display the source graph

Great for **live inspection** without restarting.

## 📟 Using the scripting API

Graphs can also be accessed programmatically:

- `clock.dump()` — dump the clock graph
- `clock.dump_all_sources()` — dump the source graph

This makes it easy to integrate visualization into custom tooling or monitoring.

## 🌟 Examples

Here are example outputs showing typical clock and source graphs:

### 🕒 Clock graph

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

### 🔗 Source graph

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

These outputs make it easy to see how clocks relate, which sources are active, and how evaluation flows from **top (outputs)** to **bottom (inputs)**.

## ✅ When to use this feature

Clock and source graphs are particularly helpful when:

- ✏️ Designing or refactoring a script
- 🐞 Debugging timing, synchronization, or activation issues
- 🧠 Understanding `self_sync` and clock acceleration
- 📢 Explaining how a script works to others

By making time, structure, and activation visible, these graphs provide a new way to reason about Liquidsoap scripts — beyond reading code alone.
