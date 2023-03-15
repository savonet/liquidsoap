# Stereotool support

Start with version `2.2.0`, liquidsoap supports the shared library distributed by [Thimeo Audio Technology](https://www.thimeo.com/stereo-tool/)
using the `stereotool` operator (and `track.audio.stereotool` for the low-level, track-specific equivalent).

This operator can replace the use of the `stereotool` binary in your script and offers multiple benefits. In particular, it has a **very low latency**
compared to using the binary and also operates synchronously.

The operator should be quits easy to use. Here's an example:

```liquidsoap
# Define a source
s = ...

# Apply stereotool to it:
s = stereotool(
  library_file="/path/to/stereotool/shared/lib",
  license_key="my_license_key",
  preset="/path/to/preset/file"
)
```

That's it! You can apply as many `stereotool` operators as you wish and at any stage in the script, thanks
ts its synchronous nature. However, a current limitation is that **track marks and metadata are slightly
delayed**.

This is because the operator has an internal processing buffer. We do plan on delaying metadata and track marks
to match this delay but this is not yet implemented and will probably have to wait for the `2.3.x` release cycle.

This means that, until then, track switches and metadata updates might happen slightly earlier than the corresponding
signal. We're talking about `50` to `100ms` earlier, though, so that might not be a super big deal.

For the same reason, the source returned by `stereotool` is an _audio-only_ source. Otherwise, other concurrent tracks
such as video and etc would be slightly out of sync. If you need to use the operator in this kind of situation, you
might want to use a `ffmpeg` filter to e.g. adjust the video's PTS to match the audio delay.

In such case, you can refer to the `latency` method that is available on the source returned by the operator which
should indicate the delay to compensate from the processed audio signal.

The operator's `preset` parameter has a companion `load_type` parameter that can optionally be used to only load a subset of
the preset. You might refer to the upstream documentation if you need to use it.

Lastly, `stereotool` is a **proprietary software**. While we actively promote open source, we also want to meet
our users where they are and, for a lot of them, this means supporting the sound processing provided by the tool.

However, to use it, you will need a license. Using the operator without the proper license will _not_ result in an
error in your script but the audio signal will have spoken text and/or beeps.

Using the operator with an invalid license will be reported in the log. You might also use the `valid_license`
method available on the source returned by the operator, which returns `false` if the license is invalid. In this case, the `unlincensed_used_features` returns a string
indicating which unlicensed features are being used.
