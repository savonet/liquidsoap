# Seeking in liquidsoap

Starting with Liquidsoap `1.0.0-beta2`, it is now possible to seek within sources!
Not all sources support seeking though: currently, they are mostly file-based sources
such as `request.queue`, `playlist`, `request.dynamic.list` etc..

The basic function to seek within a source is `source.seek`. It has the following type:

```
(source('a),float)->float
```

The parameters are:

- The source to seek.
- The duration in seconds to seek from current position.

The function returns the duration actually seeked.

Please note that seeking is done to a position relative to the _current_
position. For instance, `source.seek(s,3.)` will seek 3 seconds forward in
source `s` and `source.seek(s,(-4.))` will seek 4 seconds backward.

Since seeking is currently only supported by request-based sources, it is recommended
to hook the function as close as possible to the original source. Here is an example
that implements a server/telnet seek function:

```{.liquidsoap include="seek-telnet.liq"}

```

## Cue points

File-based sources support cue-points to cut the beginning and end of tracks
The values of cue-in and cue-out points are given in absolute
position through the source's metadata. For instance, the following
source will cue-in at 10 seconds and cue-out at 45 seconds on all its tracks:

```liquidsoap
s = playlist(prefix="annotate:liq_cue_in=\"10.\",liq_cue_out=\"45\":",
             "/path/to/music")
```

As in the above example, you may use the `annotate` protocol to pass custom cue
points along with the files passed to Liquidsoap. This is particularly useful
in combination with `request.dynamic` as an external script can build-up
the appropriate URI, including cue-points, based on information from your
own scheduling back-end.

Alternatively, you may use `metadata.map` to add those metadata. The operator
`metadata.map` supports seeking and passes it to its underlying source.
