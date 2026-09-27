# Controlling memory usage

When using liquidsoap in production, it can be important to understand how to control the memory footprint of the application.
This is not an easy topic as there are several layers of memory management inside the application and also some
trade-off considerations between memory footprint and CPU usage.

This page gives an overview: what uses memory, how to measure it and which settings you can change. The [blog posts](#further-reading)
listed at the end go into the details, with measurements.

## What uses memory

A liquidsoap process uses memory in a few places:

- **Script loading.** Typechecking the script and the standard library is the most memory-intensive step. Most of this memory is freed once the script is running.
- **The OCaml heap.** Liquidsoap is written in OCaml, and OCaml values live in a heap managed by a garbage collector. This includes the default audio data, which is stored as OCaml floating point numbers.
- **C memory.** External libraries allocate their own memory.
- **Shared libraries.** The code of every library linked into liquidsoap, `ffmpeg` in particular, is loaded in memory. This memory is shared between all the processes using the same libraries.

### C memory allocations

Not all the memory in the application is allocated by the OCaml garbage collector. External libraries such as `ffmpeg`, `libmp3lame`,
etc. need to allocate their own memory. This is usually referred to as _C memory allocations_ though it does not have
to be allocated by a program written in `C`. Another, more technically appropriate term is _heap memory_, though memory dynamically allocated
by the OCaml garbage collector also lives in the program's heap. 😅

This type of memory is also cleaned up by the OCaml garbage collector: when the OCaml value holding a C memory block is no longer in use,
the garbage collector frees the corresponding C memory. A small OCaml value can hold a large amount of C memory, for instance a decoded
video frame. This is why video scripts use much more memory than audio scripts.

## Measuring memory usage

Measuring the memory used by a process is tricky. The numbers reported by common tools also include memory that belongs to the system or is shared with other processes:

- `RSS`, the resident set size reported by `top` and similar tools, includes shared memory. If `10` liquidsoap processes each report
  `100MB`, the system uses much less than `1000MB` in total.
- Some tools, typically in virtualized environments, also count the operating system's page cache.

Liquidsoap reports its own memory usage with `runtime.memory()`. Use `process_private_memory` to see the memory owned by the process, and
`process_managed_memory` to see the size of the OCaml heap:

```{.liquidsoap include="memory-usage.liq" from=1}

```

The same information is available from the telnet server with the `runtime.memory` command.

If the private memory grows while the OCaml heap stays flat, the growth comes from C memory, usually `ffmpeg` buffers. If both grow,
the growth comes from OCaml values.

When reading these numbers, keep in mind how the OCaml runtime behaves:

- Memory used to load the script is freed before the script starts. By default, liquidsoap compacts the OCaml heap right before starting
  (`settings.init.compact_before_start`). Loading the script from the [cache](./script_lifecycle.md#cache-and-memory-usage) skips typechecking and
  uses much less memory at startup.
- The OCaml runtime keeps freed memory to reuse it, and each domain (the OCaml unit of parallelism) has its own minor heap. Memory usage
  grows after startup and then levels off. Memory that grows and levels off is expected. Memory that keeps growing for hours is worth
  reporting.

## Reducing memory usage

### Build only the features you use

Each optional feature links more libraries into liquidsoap. `ffmpeg` has the largest footprint. If memory is a concern, build liquidsoap
with only the features your script uses. See the [optional packages](./build.md#optional-packages) in the build instructions.

### Frame settings

Liquidsoap processes media in frames, and each streaming cycle allocates the content of one frame. The frame settings change how much
memory each cycle allocates:

- `settings.frame.duration` sets the duration of a frame, `0.02` seconds by default. A longer frame means fewer cycles per second and less
  CPU usage, with larger allocations per cycle.
- `settings.frame.audio.samplerate` and `settings.frame.audio.channels` set the size of the audio data.
- `settings.frame.video.width` and `settings.frame.video.height` set the size of the video data. Video memory grows with the number of pixels.

See the [settings reference](./settings.md) for all the frame settings.

### Garbage collector parameters

The OCaml garbage collector trades CPU for memory: _to minimize unused memory, more CPU cycles have to be dedicated to tracking it_.
Inside liquidsoap scripts, the operations that the OCaml runtime provides to control the garbage collector are available within the
`runtime.gc` module. The documentation for these operations can be found in the [OCaml Gc module documentation](https://ocaml.org/manual/latest/api/Gc.html).

The two parameters that matter most are:

- `space_overhead`: lower values make the major collector run more often. This reduces memory usage and increases CPU usage. The OCaml default is `120`.
- `minor_heap_size`: the size of the minor heap, in words. Short-lived values, such as the content of a frame, are cheap to collect when
  they fit in the minor heap.

Typically, to change the garbage collector parameters, one can do:

```{.liquidsoap include="space_overhead.liq" from=1}

```

Liquidsoap runs on OCaml 5. The OCaml 4 parameters `allocation_policy`, `major_heap_increment`, `window_size` and `max_overhead` are ignored.

The best values depend on your script. Change one parameter at a time and measure the result on your own workload. The
[GC tuning blog post](https://www.liquidsoap.info/blog/2026-03-24-gc-tuning/) describes such an experiment and the tools used to run it.

### Audio data format

Another source of memory usage is the audio data format. By default, we store audio data using OCaml's native floating point numbers in order to be able to run the application,
including audio processing (crossfade, filters, fades, etc.) at the best possible speed and CPU usage. However, OCaml's native floats are stored using 64 bits (8 bytes), which is a large
amount of memory per number.

If you are concerned with reducing your audio memory footprint, for instance if your application has a lot of audio sources with buffers, you can
do a couple of things:

1. Use the [ffmpeg raw content](./ffmpeg.md).

This means storing all the audio content as ffmpeg audio frames. This is an opaque format that works very well if your script can use ffmpeg end-to-end, for instance processing
audio using [ffmpeg filters](./ffmpeg_filters.md).

2. Use one of the `pcm_f32` or `pcm_s16` audio formats.

These formats are less opaque. Their data is stored in a C memory array and can be accessed by the OCaml program. Some, but not all, of our operators
support them transparently, for instance `amplify`. When using `pcm_s16`, audio samples are stored as 16 bit signed integers (2 bytes, the audio CD format). When using `pcm_f32`, audio samples are stored as
32 bit float (4 bytes). 16 bit signed integers is probably enough for most applications and consumes 4 times less memory than OCaml's native floating point numbers.

The `%ffmpeg` encoder can require the `pcm_*` formats by adding `pcm_s16` or `pcm_f32` to the parameters of its audio stream. This will, in turn, inform all operators
and decoders to operate with this format, if they support it. The `ffmpeg` decoder, for instance, decodes directly into these formats:

```liquidsoap
# FFmpeg AAC encoder, pcm_s16
encoder = %ffmpeg(format="mp4", %audio(pcm_s16, codec="aac"))

# FFmpeg FLAC encoder, pcm_f32
encoder = %ffmpeg(format="flac", %audio(pcm_f32, codec="flac"))
```

For both `pcm_*` and ffmpeg raw formats, you can also use conversion functions (`ffmpeg.raw.decode.*`, `ffmpeg.raw.encode.*`, `audio.decode.pcm_*`, `audio.encode.pcm_*`) to convert
content back and forth.

In general, working with the `pcm_*` formats is easier. If you know what you are doing, though, working with raw FFmpeg frames can also have some advantages. In both cases,
there might be an increase in CPU usage if your script needs to process audio (for instance via a `crossfade`) when converting these formats back and forth.

Finally, if you need to store a large amount of audio data, for instance to create a one hour delay, you should consider using the `defer` operator which was designed for
this purpose. `defer` stores its buffer as `pcm_s16`. If your source already uses `pcm_s16`, `defer.pcm_s16` skips the conversion.

## Further reading

These blog posts cover memory usage in more detail:

- [Memory management](https://www.liquidsoap.info/blog/2023-07-09-memory-management/): the OCaml memory model, C memory allocations and the trade-off between memory and CPU.
- [A faster Liquidsoap (part 1)](https://www.liquidsoap.info/blog/2024-06-13-a-faster-liquidsoap/): script loading, typechecking and script caching.
- [A faster Liquidsoap (part 2)](https://www.liquidsoap.info/blog/2024-10-26-a-faster-liquidsoap-part-deux/): how to measure memory usage, shared memory, page cache, optional features and script caching.
- [Tuning Liquidsoap memory and CPU usage with OCaml GC parameters](https://www.liquidsoap.info/blog/2026-03-24-gc-tuning/): an experiment tuning frame duration, `minor_heap_size` and `space_overhead`.
- [Full concurrency is here!](https://www.liquidsoap.info/blog/2026-09-04-full-concurrency/): OCaml 5, and memory measurements on demanding video scripts.
