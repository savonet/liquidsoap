# Frequently Asked Questions

## Contents

- [Error messages](#error-messages)
  - [Type error](#type-error)
  - [That source is fallible!](#that-source-is-fallible)
  - [Clock error](#clock-error)
  - [We must catchup x.xx!](#we-must-catchup-xxx)
  - [Unable to decode a file](#unable-to-decode-a-file)
  - [Runtime exceptions](#runtime-exceptions)
  - [Crashes](#crashes)
- [Troubleshooting](#troubleshooting)
  - [PulseAudio](#pulseaudio)
  - [Listeners are disconnected at the end of every track](#listeners-are-disconnected-at-the-end-of-every-track)
  - [Encoding blank](#encoding-blank)
  - [Temporary files](#temporary-files)

## Error messages

### Type error

Liquidsoap may reject a script with a series of errors of the form `this value has type ... but it should be a subtype of ...`. Usually the last error tells you what the problem is, but earlier errors may help pinpoint where it comes from.

For example, the error might indicate that a value of type `int` has been passed where a `float` was expected, in which case you should use a conversion, or more likely change an integer value such as `13` to a float `13.`.

A type error can also indicate that you're trying to use a source of a certain content type (e.g., audio) in a place where another content type (e.g., pure video) is required. In that case the last error in the list may not be the most useful one, but you will see something like this above it:

```
At ...:
Error 5: this value has type
  source(video=canvas(_),...)
but it should be a subtype of
  source(audio=pcm(_),...)
```

Sometimes, a type error indicates a mistake in the order or labels of arguments. For example, given `output.icecast(mount="foo.ogg",source)` liquidsoap will complain that the second argument is a source (`source(?A)`) but should be a format (`format(?A)`): indeed, the first unlabelled argument is expected to be the encoding format, e.g., `%vorbis`, and the source comes second.

Finally, a type error can indicate that you have forgotten to pass a mandatory parameter to some function. For example, on the code `fallback([source.mux.audio(x),...])`, liquidsoap will complain as follows:

```
At line ...:
Error 5: this value has type
  [(?id : _, audio : _) -> _]
but it should be a subtype of the type of the value at ../libs/switches.liq, line 11, char 11-18
  [source(_)] (inferred at ../libs/list.liq, line 102, char 29)
```

Indeed, `fallback` expects a source, but `source.mux.audio(x)` is still a function expecting the `audio` parameter.

### That source is fallible!

See the [quickstart](quick_start.html), or read more about [sources](sources.html).

### Clock error

Read about [clocks](clocks.html) for the errors
`a source cannot belong to two clocks`
and
`cannot unify two nested clocks`.

### We must catchup x.xx!

This error means that a clock is falling behind in liquidsoap. This can
be caused by an overloaded CPU — if your script is doing too much encoding
or processing, you should reduce the load on your machine or simplify your
liquidsoap script. Latency can also be caused by network lag: for example,
a slow connection to an Icecast server can cause the output to stall,
making the clock fall behind.

The first kind of latency is problematic because it tends to accumulate,
eventually leading to the restarting of outputs:

```
Too much latency!
Resetting active source...
```

The second kind can often be ignored: if you are streaming to an Icecast
server, there are several buffers between you and your listeners which make
this invisible to them. In more real-time applications, however, even small
lags will result in glitches.

In some situations, it is possible to isolate parts of a script from the
latency caused by other parts. For example, you can produce a clean stream
and back it up to a file, independently of the output to Icecast (which is
sensitive to network lag). For more details, read about [clocks](clocks.html).

### Unable to decode a file

The log message `Unable to decode "file" as {audio=pcm}!` means that
liquidsoap failed to decode a file — not necessarily because the format is
unsupported, but possibly because the file does not contain the expected
media type. For example, if audio and video are both expected, an audio-only
file will be rejected.

Liquidsoap can convert audio channels in most situations. Typically, if stereo
data is expected but the file contains mono audio, liquidsoap will use the
single channel as both left and right.

### Runtime exceptions

Liquidsoap scripts can raise runtime errors of the form:

```
At line 3, char 45:
Error 14: Uncaught runtime error:
type: not_found, message: "File not found!"
```

These are errors that a script can catch and handle — they typically occur
when trying to read a file that does not exist. The [language page](language.html)
has more details about errors, how to raise them, and how to catch them.

### Crashes

Liquidsoap dies with messages such as these at the end of the log:

```
... [threads:1] Thread "XXX" aborts with exception YYY!
... [stderr:3] Thread 2 killed on uncaught exception YYY.
... [stderr:3] Raised at file ..., line ..., etc.
```

These internal errors fall into two categories:

- **Bug**: This usually means you've found a bug. Please report it on [GitHub Issues](https://github.com/savonet/liquidsoap/issues).
- **User error**: In some cases, an exception is raised on a user error rather than being reported cleanly. By looking at the surrounding log messages, you may find that liquidsoap crashed for an identifiable reason. You can still report it as a bug — you should not have seen a raw exception and backtrace.

In either case, once this kind of error occurs there is no way to prevent
liquidsoap from crashing. These exceptions cannot be caught or handled at the
level of liquidsoap scripts.

## Troubleshooting

### PulseAudio

When using ALSA input or output — or more generally any audio I/O that does
not go through PulseAudio — you should make sure PulseAudio is not interfering.
PulseAudio is often installed by default and emulates ALSA, but this can cause
bugs, in particular errors of this form:

```
Alsa.Unknown_error(1073697252)!
```

There are two ways to address this:

- Route your ALSA input/output to bypass PulseAudio
- Disable PulseAudio on your system

**Bypassing PulseAudio:** First find out which sound card you want to use with `aplay -l`. Example output:

```
**** List of PLAYBACK Hardware Devices ****
card 0: Intel [HDA Intel], device 0: STAC92xx Analog [STAC92xx Analog]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
```

In this case the card is device `0`, subdevice `0`, i.e. `hw:0,0`. Create a file
`/etc/asound.conf` (or `~/.asoundrc` for a single-user setup) with:

```liquidsoap
pcm.liquidsoap {
        type plug
        slave { pcm "hw:0,0" }
}
```

This creates a new ALSA device that you can use with liquidsoap. The `plug` operator
works around hardware limitations (mixing multiple outputs, resampling, etc.). In some
cases you may need to define your own PCM device.

You can then use the device in liquidsoap as follows:

```liquidsoap
input.alsa(device="pcm.liquidsoap", ...)
```

**Disabling PulseAudio:** Edit `/etc/pulse/client.conf` and add or change:

```
autospawn = no
```

Then kill any running PulseAudio process:

```
killall pulseaudio
```

Alternatively, on Debian or Ubuntu, you can remove it entirely:

```
apt-get remove pulseaudio libasound2-plugins
```

### Listeners are disconnected at the end of every track

Several media players, including some popular ones, do not properly support
Ogg/Vorbis streams: they treat the end of a track as an end of file,
causing listeners to disconnect.

Affected players include VLC. Unaffected players include ogg123 and liquidsoap itself.

One workaround is to avoid producing track boundaries within a Vorbis stream.
This can be done by dropping both metadata and track marks, for example using
`source.drop.metadata_track_marks`.

### Encoding blank

Encoding pure silence is often too effective for streaming: the data is so
compressed that there is almost nothing to send, and listener clients may
eventually disconnect. It is better to use a non-silent jingle instead of
`blank()` to fill gaps. You can also use synthesis sources such as
`noise()`, `sine()`, etc.

### Temporary files

Liquidsoap uses OCaml's `Filename.tmp_dir_name` variable to determine where
to store temporary files. It works as follows:

On Unix, the value of the `TMPDIR` environment variable, or `"/tmp"` if the
variable is not set. On Windows, the value of the `TEMP` environment variable,
or `"."` if the variable is not set.
