# FFmpeg Support

Since the `2.0.x` release cycle, liquidsoap integrates a tight support of ffmpeg. This includes:

* [Decoders](#decoders)
* [Encoders](#dncoders)
* [Filters](#filters)
* [Bitstream filters](#bitstream-filters)

Ffmpeg support includes 3 types of content:

* **Internal content**, that is content available to all liquidsoap operators: `PCM` audio and `YUV420p` video
* **Raw content**, that is decoded content but stored as ffmpeg internal frame.
This type of content is only available to ffmpeg filters and raw encoders. It can be used to avoid data copies back and forth between liquidsoap and ffmpeg.
* **Copy content**, that is encoded content stored as ffmpeg internal packets.
This type of content is only available to ffmpeg copy encoder and bitstream filters and requires a fairly good understanding of media codecs and containers.
Copy contents can be used to avoid transcoding and pass encoded data end-to-end inside liquidsoap scripts.

## Enabling ffmpeg support

FFmpeg support is available via the external [ocaml-ffmpeg](https://github.com/savonet/ocaml-ffmpeg) binding package. If you are using any binary asset from
our release pages or via docker, this should already be included.

If you are installing via [opam](https://opam.ocaml.org/), installing the `ffmpeg` package should do the trick:

```sh
% opam install ffmpeg
```

## Decoders

For the most part, you should never have to worry about the `ffmpeg` decoder. When enabled, it should be the preferred decoder for all supported media.
When using raw or copied content, the decoder is able to produce the required content without the need of any intervention on the user part.

Should you need to tweak it, here are a couple of pointers:

The `settings.decoder.decoders` settings controls which decoders are to be used when trying to decode media files.
You can use it to restrict which decoders are being used, for instance making sure only the ffmpeg decoder is used:

```liquidsoap
settings.decoder.decoders.set(["FFMPEG"])
```

Priority for the decoder is set via:

```liquidsoap
settings.decoder.priorities.ffmpeg.set(10)
```

You can use this setting to adjust wether or not the ffmpeg decoder should be tried first when decoding media files, in particular in
conjunction with the other `settings.decoder.priorities.*` settings.

For each type of media codec, the `settings.decoder.ffmpeg.codecs.*` settings can be used to tell `ffmpeg` which decoder to use to 
decode this type of content (there could more than one decoder for a given codec).

For instance, for the `aac` codec:

* `settings.decoder.ffmpeg.codecs.aac.available()` returns the list of available decoders, typically `["aac", "aac_fixed"]`.
* `settings.decoder.ffmpeg.codecs.aac.set` can be used to choose which decoder should be used, typically: `settings.decoder.ffmpeg.codecs.aac.set("aac")`


When debugging issues with `ffmpeg`, it can be useful to increase the log verbosity. 

```liquidsoap
settings.ffmpeg.log.verbosity.set("warning")
```

This settings sets the verbosity of `ffmpeg` logs. Possible values, from less verbose to more verbose are:
`"quiet"`, `"panic"`, `"fatal"`, `"error"`, `"warning"`, `"info"`, `"verbose"` or `"debug"`

Please note that, due to a technical limitation, we are not yet able to route `ffmpeg` logs through 
the liquidsoap logging facilities, which means that `ffmpeg` logs are currently only printed to the 
process's standard output and that the `settings.ffmpeg.log.level` is currently not used.


## Encoders

See detailed [ffmpeg encoders](ffmpeg_encoder.html) article.

## Filters

See detailed [ffmpeg filters](ffmpeg_filters.html) article.

## Bitstream filters

See detailed [ffmpeg filters](ffmpeg_filters.html) article.

## Examples

See detailed [ffmpeg cookbook](ffmpeg_cookbook.html) article.
