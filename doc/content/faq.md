Frequently Asked Questions
==========================
What does this message means?
-----------------------------
### Type error
Liquidsoap might also reject a script with a series of errors of the form ```
this value has type ... but it should be a subtype of ...```
. Usually the last error tells you what the problem is, but the previous errors might provide a better information as to where the error comes from.

For example, the error might indicate that a value of type `int` has been passed where a float was expected, in which case you should use a conversion, or more likely change an integer value such as `13` into a float `13.`.

A type error can also show that you're trying to use a source of a certain content type (e.g., audio) in a place where another content type (e.g., pure video) is required. In that case the last error in the list is not the most useful one, but you will read something like this above:

```
At ...:
  this value has type
    source(audio=?A+1,video=0,midi=0)
    where ?A is a fixed arity type
  but it should be a subtype of
    source(audio=0,video=1,midi=0)
```

Sometimes, the type error actually indicates a mistake in the order or labels of arguments. For example, given `output.icecast(mount="foo.ogg",source)` liquidsoap will complain that the second argument is a source (`source(?A)`) but should be a format (`format(?A)`): indeed, the first unlabelled argument is expected to be the encoding format, e.g., `%vorbis`, and the source comes only second.

Finally, a type error can indicate that you have forgotten to pass a mandatory parameter to some function. For example, on the code `fallback([crossfade(x),...])`, liquidsoap will complain as follows:

```
At line ...:
  this value has type
    (?id:string, ~start_next:float, ~fade_in:float,
     ~fade_out:float)->source(audio=?A,video=?B,midi=0)
    where ?B, ?A is a fixed arity type
  but it should be a subtype of
    source(audio=?A,video=?B,midi=0)
    where ?B, ?A is a fixed arity type
```

Indeed, `fallback` expects a source, but `crossfade(x)` is still a function expecting the parameters `start_next`, `fade_in` and `fade_out`.

### That source is fallible!

See the [quickstart](quick_start.html), or read more about 
[sources](sources.html).

### Clock error

Read about [clocks](clocks.html) for the errors
`a source cannot belong to two clocks`
and
`cannot unify two nested clocks`.

### We must catchup x.xx!

This error means that a clock is getting late in liquidsoap. This can
be caused by an overloaded CPU, if your script is doing too much encoding
or processing: in that case, you should reduce the load on your machine
or simplify your liquidsoap script. The latency may also be caused by
some lag, for example a network lag will cause the icecast output to
hang, making the clock late.

The first kind of latency is problematic because it tends to accumulate,
eventually leading to the restarting of outputs:
```
Too much latency!
Resetting active source...
```


The second kind of latency can often be ignored: if you are streaming to
an icecast server, there are several buffers between you and your
listeners which make this problem invisible to them. But in more realtime
applications, even small lags will result in glitches.

In some situations, it is possible to isolate some parts of a script
from the latency caused by other parts. For example, it is possible to
produce a clean script and back it up into a file, independently of
its output to icecast (which again is sensitive to network lags).
For more details on those techniques, read about [clocks](clocks.html).

### Unable to decode ``file'' as {audio=2;video=0;midi=0}!

This log message informs you that liquidsoap failed to decode a file, not 
necessarily because it cannot handle the file, but also possibly because
the file does not contain the expected media type. For example, if video
is expected, an audio file will be rejected.

The case of mono files is often surprising. Since liquidsoap does not
implicitly convert between media formats, input files must be stereo
if the output expects stereo data. As a result, people often get this
error message on files which they expected to play correctly. The
simple way to fix this is to use the `audio_to_stereo()` operator
to allow any kind of audio on its input, and produce stereo as expected
on its output.

### Exceptions

Liquidsoap dies with messages such as these by the end of the log:

```
... [threads:1] Thread "XXX" aborts with exception YYY!
... [stderr:3] Thread 2 killed on uncaught exception YYY.
... [stderr:3] Raised at file ..., line ..., etc.
```

Those internal errors can be of two sorts:

* **Bug**: Normally, this means that you've found a bug, which you should report on the mailing list or bug tracker.
* **User error**: In some cases, we let an exception go on user errors, instead of nicely reporting and handling it. By looking at the surrounding log messages, you might realize that liquidsoap crashed for a good reason, that you are responsible for fixing. You can still report a bug: you should not have seen an exception and its backtrace.

In any case, once that kind of error happens, there is no way for the
user to prevent liquidsoap from crashing. Those exceptions cannot be
caught or handled in any way at the level of liquidsoap scripts.

Troubleshooting
---------------

### Pulseaudio

When using ALSA input or output or, more generally any audio input or output 
that is not using pulseaudio, you should disable pulseaudio, which is often installed
by default. Pulseaudio emulates ALSA but this also generates bugs, 
in particular errors of this form:
```
Alsa.Unknown_error(1073697252)!
```

There are two things you may do:

* Make sure your alsa input/output does not use pulseaudio
* Disable pulseaudio on your system

In the first case, you should first find out which sound card you want to use,
with the command `aplay -l`. An example of its output is:

```
**** List of PLAYBACK Hardware Devices ****
card 0: Intel [HDA Intel], device 0: STAC92xx Analog [STAC92xx Analog]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
```

In this case, the card we want to use is: device `0`, subdevice `0`, thus:
`hw:0,0`. We now create a file `/etc/asound.conf` (or `~/.asoundrc` for single-user
configuration) that contains the following:

```liquidsoap
pcm.liquidsoap {
        type plug
        slave { pcm "hw:0,0" }
}
```

This creates a new alsa device that you can use with liquidsoap. The `plug` operator
in ALSA is used to work-around any hardware limitations in your device (mixing multiple
outputs, resampling etc.). In some cases you may need to read more about ALSA and define
your own PCM device.

Once you have created this device, you can use it in liquidsoap as follows:

```liquidsoap
input.alsa(device="pcm.liquidsoap", ...)
```

In the second case -- disabling pulseaudio, you can edit the file `/etc/pulse/client.conf` and
change or add this line:

```
autospawn = no
```

And kill any running pulseaudio process:

```
killall pulseaudio
```

Otherwise you may simply remove pulseaudio's packages, if you use Debian or Ubuntu:

```
apt-get remove pulseaudio libasound2-plugins
```

### Listeners are disconnected at the end of every track

Several media players, including renowned ones, do not properly support
Ogg/Vorbis streams: they treat the end of a track as an end of file,
resulting in the disconnection.

Players that are affected by this problem include VLC.
Players that are not affected include ogg123, liquidsoap.

One way to work around this problem is to not use Ogg/Vorbis (which we
do not recommend) or to not produce tracks within a Vorbis stream.
This is done by merging liquidsoap tracks (for example using
`add(normalize=false,[blank(),source])`)
and also not passing any metadata
(which is also a result of the previous snippet).

### Encoding blank

Encoding pure silence is often too effective for streaming: data is so
compressed that there is nothing to send to listeners, whose clients
eventually disconnect. Therefore, it is a good idea to use a non-silent
jingle instead of `blank()` to fill in the blank. You can
also achieve various effects using synthesis sources such as
`noise()`, `sine()`, etc.

### Temporary files

Liquidsoap relies on OCaml's `Filename.tmp_dir_name` variable to store temporary 
files. It is documented as follows:

The name of the temporary directory: Under Unix, the value of the `TMPDIR` environment 
variable, or `"/tmp"` if the variable is not set. Under Windows, the value of the `TEMP` 
environment variable, or `"."` if the variable is not set.


