Sources
=======
Using liquidsoap is about writing a script describing how to build what you
want. It is about building a stream using elementary streams and stream
combinators, etc. Actually, it's a bit more than streams, we call them
*sources*.

A source is a stream with metadata and track annotations. It is discretized as
a stream of fixed-length buffers of media samples, the frames. Every frame may
have metadata inserted at any point, independently of track boundaries. At
every instant, a source can be asked to fill a frame of data.

The liquidsoap API provides plenty of functions for building sources.
Some of those functions build elementary sources from scratch, others are
operators that combine sources into more complex ones. An important class of
sources is that of *active sources*, they are the sources that actively
trigger the computation of your stream. Typically, active sources are built
from output functions, because outputting a stream is the only reason why you
want to compute it.

All sources, operators and outputs are listed in the
[scripting API reference](reference.html).

How does it work?
-----------------
To clarify the picture let's study in more details an example:
```liquidsoap
radio =
  output.icecast(
    %vorbis,mount="test.ogg",
    random(
      [ jingle ,
        fallback([ playlist1,playlist2,playlist3 ]) ]))
```

At every cycle of the [clock](clocks.html), the output asks the `random` node for data,
until it gets a full frame of raw audio.
Then, it encodes the frame and sends it to the Icecast server.
Suppose `random` has chosen the `fallback` node,
and that only `playlist2` is available, and thus played.
At every cycle, the buffer is passed from `random` to
`fallback` and then to `playlist2` which fills it,
returns it to `fallback` which returns it to `random`
which returns it to the output.

At some point, `playlist2` ends a track.
The fallback detects that on the returned buffer,
and selects a new child for the next filling,
depending on who's available.
But it doesn't change the buffer, and returns it to `random`,
which also (randomly) selects a new child at this point,
before returning the buffer to the output.
On next filling, the route of the frame can be different.

Note that it is also possible to have the route changed inside a track,
for example using the `track_sensitive` option of fallback,
which is typically done for instant switches to live shows when they start.

The important point here is that **all of the above steps are local**.
Everything takes place between one operator and its immediate children source;
operators do not see beyond that point.

Fallibility
-----------
By default, liquidsoap outputs are meant to emit a stream without
discontinuing. Since this stream is provided by the source passed to the
output operator, it is the source responsibility to never fail.
Liquidsoap has a mechanism to verify this, which helps you think of
all possible failures, and prevent them.
Elementary sources are either *fallible* or *infallible*, and this
*liveness type* is propagated through operators to finally
compute the type of any source.
For example,
a `fallback` or `random` source is infallible
if an only if at least one of its children is infallible,
and a `switch` is infallible if and only if it has one infallible
child guarded by the trivial predicate `{ true }`.

On startup, each output checks the liveness type of its input source,
and issues an error if it is fallible. The typical fix for such problems
is to add one fallback to play a default file (`single()`)
or a checked playlist (`playlist.safe()`) if the normal source
fails.
One can also use the `mksafe` operator that will insert silence
during failures.

If you do not care about failures, you can pass the parameter
`fallible=true` to most outputs. In that case, the output
will accept a fallible source, and stop whenever the source fails,
to restart when it is ready to emit a stream again.

Caching mode
------------
In some situations, a source must take care of the consistency of its
output. If it is asked twice to fill buffers during the same cycle, it
should fill them with the same data. Suppose for example that a playlist is
used by two outputs, and that it gives the first frame to the first
output, the second frame to the second output: it would give the third frame
to the first output during the second cycle,
and the output will have missed one frame.

It is sometimes useful to keep this is mind to understand the behaviour
of some complex scripts. The high-level picture is enough for users,
more details follow for developers and curious readers.

The sources detect if they need to remember (cache) their previous output in
order to replay it. To do that, clients of the source must register in
advance. If two clients have registered, then caching should be enabled.
Actually that's a bit more complicated, because of transitions. Obviously the
sources which use a transition involving some other source must register to
it, because they may eventually use it. But a jingle used in two transitions
by the same switching operator doesn't need caching. The solution involves two
kinds of registering: *dynamic* and *static activations*. Activations are
associated with a path in the graph of sources' nesting. The dynamic
activation is a pre-registration allowing a single real *static activation*
to come later, possibly in the middle of a cycle.
Two static activations trigger caching. The other reason for enabling caching
is when there is one static activation and one dynamic activation which
doesn't come from a prefix of the static activation's path. It means that the
dynamic activation can yield at any moment to a static activation and that the
source will be used by two sources at the same time.

Execution model
---------------
In your script you define a bunch of sources interacting together. Each
source belongs to a [clock](clocks.html), but clocks only have direct access
to *active sources*, which are mostly outputs.
At every cycle of the clock, active sources are animated: a chunk of stream
(frame) is computed, and potentially outputted one way or another.

This streaming task is the most important and shouldn't be disturbed.
Thus, other tasks are done in auxiliary threads:
file download, audio validity checking, http polling, playlist reloading...
No blocking or expensive call should be done in streaming threads.
Remote files are completely downloaded to a local temporary file
before use by the root thread. It also means that you shouldn't access NFS
or any kind of falsely local files.
