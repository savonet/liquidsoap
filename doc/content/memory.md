## Controlling memory usage

When using liquidsoap in production, it can be important to understand how to control the memory footprint of the application.
This is not an easy topic as there are several layers of memory management inside the application and also some
trade-off considerations between memory footprint and CPU usage.

As of writing (version `2.2.0`), some of the trade-off that we are making with the OCaml garbage collector do not seem
satisfactory in some memory-intensive conditions. Hopefully, this will improve in future major release (`2.3.x` and later).

But first, let's look at what's going on.

### The OCaml memory model

The OCaml compiler provides a garbage collector. This module is able to track memory blocks used by the OCaml program and free
them when they are not used without the programmer's intervention.

This is done by scanning the memory currently allocated by the OCaml program to identify the memory blocks that are not in
use anymore. While this is transparent to the user (you!), this also means that there will be extra CPU cycles dedicated to
this operation.

How often these cycle occur help controlling the growth of unused memory but with the understanding that _to minimize unused memory,
more CPU cycles have to be dedicated to tracking it_.

You can find more information about the OCaml garbage collection on [this page](https://ocaml.org/docs/garbage-collection).

Inside liquidsoap scripts, the operations that the OCaml compiler provides to control the garbage collector are available within the
`runtime.gc` module. The documentation for these operations can be found in the [OCaml Gc module documentation](https://v2.ocaml.org/api/Gc.html).

Typically, to change the garbage collector parameters, one can do:

```{.liquidsoap include="space_overhead.liq" from=1}

```

These parameters and functions make it possible to experiment and see if you can find better parameters for your application.

### C memory allocations

Not all the memory in the application is allocated by the OCaml garbage collector. External libraries such as `ffmpeg`, `libmp3lame`
and etc. need to allocate their own memory. This is usually referred to as _C memory allocations_ though it does not have
to be allocated by a program written in `C`.. Another, more technically appropriate is _heap memory_ though, dynamically memory allocated
by the OCaml garbage collector also lives in the program's heap.. 😅

This type of memory is also cleaned up by the OCaml garbage collector. To do so, a _custom block_ is passed to the OCaml program with
a reference to a C memory pointer and how to clean it up. When the OCaml program detects that this custom block is no longer
in use, it triggers the required operations to clean its corresponding C memory.

However, things get complicated when considering how to fine-tune the garbage collector to account for memory allocated on
the C side..

Remember that, as we discussed in the previous section, the garbage collector has to consume CPU cycles to free up memory. And, in the case of memory allocated on the
C side, a single OCaml value (usually a small amount of memory) can actually refer to a much larger amount of C memory. This
is typically the case when the corresponding C memory represents decoded video frames, which is usually a fairly large amount of memory.

In general, the trade-off is: if the garbage collector does not run often enough, a lot of these rather larger C memory blocks are lingering
longer, which leads to potentially huge amount of memory needlessly consumed by the application.

Conversely, if the garbage collector runs too often, memory usage is controlled but CPU usage is increased.

As of now, the strategy implemented by the OCaml compiler consists in tracking the ratio of OCaml held memory vs. its corresponding C memory and running the garbage collector
more often when this ratio increases. However, this is not optimal in cases where the application purposefully holds large amount
of C memory such as when doing video processing.

In the future, we would like to explore tightening up our control of this mechanism. It should be possible trick the garbage collector
by not declaring the full anmount of allocated C memory to make it possible to run the memory cleaning operations on purpose and at specific times,
typically after a streaming cycle has ended.

Most of the tools for that are already exported in the scripting language so, we will make sure to report our progress on the [blog](https://liquidsoap.info/blog)
for anyone to test it.

### Audio data format

Another source of memory usage is the audio data format. By default, we store audio data using OCaml's native floating point numbers in order to be able to run the application,
including audio processing (crossfade, filters, fades etc) at the best possible speed and CPU usage. However, OCaml's native float are stored using 64 bits (8 bytes), which is a large
amount of memory per number.

If you are concerned with reducing your audio memory footprint, for instance if your applications has a lot of audio sources with buffers, you can
do a couple of things:

1. Use the [ffmpeg raw content](ffmpeg.html).

This means storing all the audio content as ffmpeg audio frames. This is an opaque format that works very well if your script can use ffmpeg end-to-end, for instance processing
audio using [ffmpeg filters](ffmpeg_filters.html)..

2. Use one of the `pcm_f32` or `pcm_s16` audio format.

These formats are less opaque. Their data is stored in a C memory array and can be accessed by the OCaml program. Some, but not all, of our operators
do support them transparently. When using `pcm_s16`, audio samples are stored as 16 bit signed integers (2 bytes, the audio CD format). When using `pcm_f32`, audio samples are stored as
32 bit float (4 bytes). 16 bit signed integers is probably enough for most applications and consumes 4 times less memory than OCaml's native floating point numbers.

The `pcm_*` formats can be required by the encoders by adding `pcm_s16` or `pcm_f32` to their list of parameters. This will, in turn, inform all operators
and decoders to operate with this format, if they support it:

```liquidsoap
# Mp3 encoder, pcm_s16
encoder = %mp3(pcm_s16, channels=2)

# Ogg/opus encoder, pcm_f32
encoder = %ogg(%vorbis(pcm_f32))

# FFmpeg AAC encoder, pcm_s16
encoder = %ffmpeg(format="mp4",%audio(pcm_s16, codec="aac"))
```

For both `pcm_*` and ffmpeg raw formats, you can use also conversion functions (`ffmpeg.raw.decode.*`, `ffmpeg.raw.encode.*`, `audio.decode.pcm_*`, `audio.encode.pcm_*`) to convert
content back and forth.

In general, working with the `pcm_*` formats is easier. If you know what you are doing, though, working with raw FFmpeg frames can also have some advantages. In both cases,
there might be an increase in CPU usage if your script needs to process audio (for instance via a `crossfade`) when converting these formats back and forth.

Finally, if you need to store large amount of audio data, for instance to create a one hour delay, you should consider using the `track.audio.defer` operator which was designed for
this purpose.

### `jemalloc`

Lastly, the user-land memory allocator [jemalloc](https://github.com/jemalloc/jemalloc) can be used to control all memory allocations (C and OCaml). This allocator is particularly
good at preventing memory fragmentation, which is an important topic for an application like liquidsoap running short streaming cycle involving small amount of memory (FFmpeg frames etc).

The allocator is enabled by installing the `jemalloc` opam package and is included in all our production builds (except windows). It also comes with a lot of customization options
that are exported via the [runtime.jemalloc.\*](https://www.liquidsoap.info/doc-dev/reference.html#runtime.jemalloc.epoch) functions.

If you want to explore more, we recommend [reading about it](https://engineering.fb.com/2011/01/03/core-data/scalable-memory-allocation-using-jemalloc/) and
then exploring the [manual page](http://jemalloc.net/jemalloc.3.html) which contains details about all the available settings.
