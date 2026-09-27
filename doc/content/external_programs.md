# External programs

Liquidsoap supports many formats natively, through its own decoders and encoders and through FFmpeg. Sometimes you have a file format or a tool that Liquidsoap does not support: a tracker module format, a specific encoder binary, or an in-house tagging tool. In that case, Liquidsoap can call an external program to do the work.

An external program runs as a separate process. Liquidsoap starts the process, sends it data and reads its output. This gives you access to any command-line tool, at a cost:

- Liquidsoap has to manage the process: start it, restart it when needed and stop it cleanly.
- Starting a process and passing data through it adds latency.
- The stream depends on the behavior of the program. If the program crashes or hangs, Liquidsoap has to detect it and recover.

This page covers the two main uses: decoding files and encoding streams.

## Decoding files with external programs

You can use external programs in liquidsoap to decode audio files.

### Basic operators

External decoders are registered using `decoder.add`.

```liquidsoap
decoder.add(name="my_decoder",description="My custom decoder",
            file_extensions=["foo"], decoder)
```

- `file_extensions` is a list of file extensions that the decoder can handle.
- `decoder` is a function used to return the decoded file.
- You can also use the `mimes` argument to find files based on their mime-type.

The `decoder` function has a similar signature to protocol resolution functions. This is because
file decoding happens as part of the protocol resolution. It takes an `rlog` function, a `maxtime`
maximum execution time stamp and an input file and returns a decoded URI, or `null` if the file could not be decoded.

Decoded URI can be any url to pass down to the protocol resolution pipeline. Most of the time, it should be
a decoded file but it could also be an `annotate` uri if you wish to also pass down decoded metadata along with
the decoded file.

```{.liquidsoap include="decoder-openmpt.liq" from="BEGIN"}

```

The standard library ships this decoder as `enable_external_openmpt123_decoder()`, and an FFmpeg-based one as `enable_external_ffmpeg_decoder()`. See the [request lifecycle](./requests.md#the-request-lifecycle) for where decoders run during request resolution.

### Metadata decoders

An external program can also read metadata from a file. Register a metadata decoder with `decoder.metadata.add`. The decoder function receives the current metadata and the file name, and returns a list of `(name, value)` pairs to add to the metadata.

The following example reads tags from FLAC files with `metaflac`, which prints one `NAME=value` line per tag:

```{.liquidsoap include="decoder-metadata-metaflac.liq"}

```

Use `file_extensions` and `mime_types` to restrict the decoder to some files. Both default to `null`, which accepts any file.

## External encoders

You can use any external program that accepts wav or raw PCM data to encode audio data and use the resulting compressed
stream as an output, either to a file, a pipe, or even Icecast.

When using an external encoding process, uncompressed PCM data will be sent to the process through its standard input (`stdin`), and encoded data will be read through its standard output (`stdout`). When using a process that only does file input or output, `/dev/stdin` and `/dev/stdout` can be used, though this may generate issues if the encoding process expects to be able to go backward/forward in the file.

The main operators that can be used with external encoders are:

- `output.file`
- `output.icecast`

In order to use external encoders with these operators, you have to use the
`%external` [encoding format](./encoding_formats.md).
Its syntax is:

```liquidsoap
%external(channels=2,samplerate=44100,header=true,
          restart_on_crash=false,
          restart_on_metadata,
          restart_after_delay=30,
          process="progname")
```

The available options are:

- `process`: the command line of the process to start. It can also be passed as an unlabeled string.
- `header`: if set to `false` then no WAV header will be added to the data fed to the encoding process, thus the encoding process shall operate on RAW data.
- `restart_on_crash`: whether to restart the encoding process if it crashed. Useful when the external process fails to properly encode data after some time.
- `restart_on_metadata`: restart the encoding process on each new metadata. Useful for audio formats that need a new header, possibly with metadata, for each new track. This is the case for the ogg container.
- `restart_after_delay`: restart the encoder after some delay, in seconds. This can be useful for encoders that cannot operate on infinite streams, or are buggy after some time, like the `lame` binary.

Only one of `restart_after_delay` or `restart_on_metadata` should be used.

The restart mechanism strongly relies on the good behavior of the encoding process. The restart operation will
close the standard input of the encoding process. The encoding process is then expected to finish its own operations and
close its standard output. If it does not close its standard output, the encoding task will not finish.

If your encoding process has this issue, you should turn the `restart_on_crash` option to `true` and kill the encoding
process yourself.

If you use an external encoder with the `output.icecast` operator,
you should also use the following options of `output.icecast`:

- `send_icy_metadata`: send new metadata as ICY updates. This is needed for headerless formats, such as MP3 or AAC, and it appears to work also for ogg/vorbis streams.
- `format`: content-type (mime) of the data sent to Icecast. For instance, for ogg data, it is one of `"application/ogg"`, `"audio/ogg"` or `"video/ogg"` and for mp3 data it is `"audio/mpeg"`.

### Video support

Videos can also be encoded by programs able to read files in avi format from
standard input. To use it, the flag `video=true` of `%external` should be
used. For instance, a compressed avi file can be generated with `ffmpeg` using

```{.liquidsoap include="external-output.file.liq" from="BEGIN"}

```

## Other uses

Protocols can also call external programs. A protocol turns a custom URI into a file, for example by downloading it with a command-line tool. See [writing your own protocol](./requests.md#writing-your-own-protocol).

To play a live stream produced by another program, have the program publish the stream over a network protocol such as HTTP, SRT or RTMP, and read it with `input.ffmpeg`.
