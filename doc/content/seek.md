# Seeking in liquidsoap

Liquidsoap can seek within sources!
Not all sources support seeking though: currently, they are mostly file-based sources
such as `request.queue`, `playlist`, `request.dynamic`, etc.

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

Request-based sources also support cue points, which cut the beginning and end
of each track. The decoder applies the cue-in point by seeking in the file when
it opens it, so cue points work with the same sources as `source.seek`. See
[cue points](./requests.md#cue-points) for how to set them.
