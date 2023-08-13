# Clocks

In the [quickstart](quick_start.html) and in the introduction to liquidsoap
[sources](sources.html), we have described a simple world in which sources
communicate with each other, creating and transforming data that
composes multimedia streams.
In this simple view, all sources produce data at the same rate,
animated by a single clock: at every cycle of the clock,
a fixed amount of data is produced.

While this simple picture is useful to get a fair idea of what's going on
in liquidsoap, the full picture is more complex: in fact, a streaming
system might involve _multiple clocks_, or in other words several
time flows.

It is only in very particular cases that liquidsoap scripts
need to mention clocks explicitly. Otherwise, you won't even notice
how many clocks are involved in your setup: indeed, liquidsoap can figure
out the clocks by itself, much like it infers types.
Nevertheless, there will sometimes be cases where your script cannot
be assigned clocks in a correct way, in which case liquidsoap will
complain. For that reason, every user should eventually get a minimum
understanding of clocks.

In the following, we first describe why we need clocks.
Then we go through the possible errors that any user might encounter
regarding clocks.
Finally, we describe how to explicitly use clocks,
and show a few striking examples of what can be achieved that way.

## Why multiple clocks

The first reason is **external** to liquidsoap: there is simply
not a unique notion of time in the real world.
Your computer has an internal clock which indicates
a slightly different time than your watch or another computer's clock.
Moreover, when communicating with a remote computer, network
latency causes extra time distortions.
Even within a single computer there are several clocks: notably, each
soundcard has its own clock, which will tick at a slightly different
rate than the main clock of the computer.
Since liquidsoap communicates with soundcards and remote computers,
it has to take those mismatches into account.

There are also some reasons that are purely **internal** to liquidsoap:
in order to produce a stream at a given speed,
a source might need to obtain data from another source at
a different rate. This is obvious for an operator that speeds up or
slows down audio (`stretch`). But it also holds more subtly
for `cross`, `cross` as well as the
derived operators: during the lapse of time where the operator combines
data from an end of track with the beginning of the other other,
the crossing operator needs twice as much stream data. After ten tracks,
with a crossing duration of six seconds, one more minute will have
passed for the source compared to the time of the crossing operator.

In order to avoid inconsistencies caused by time differences,
while maintaining a simple and efficient execution model for
its sources, liquidsoap works under the restriction that
one source belongs to a unique clock,
fixed once for all when the source is created.

The graph representation of streaming systems can be adapted
into a good representation of what clocks mean.
One simply needs to add boxes representing clocks:
a source can belong to only one box,
and all sources of a box produce streams at the same rate.
For example,

```liquidsoap
output.icecast(fallback([crossfade(playlist(...)),jingles]))
```

yields the following graph:

![Graph representation with clocks](/assets/img/graph_clocks.png)

Here, clock_2 was created specifically for the crossfading
operator; the rate of that clock is controlled by that operator,
which can hence accelerate it around track changes without any
risk of inconsistency.
The other clock is simply a CPU-based clock, so that the main stream
is produced following the ``real'' time rate.

## Error messages

Most of the time you won't have to do anything special about clocks:
operators that have special requirements regarding clocks will do
what's necessary themselves, and liquidsoap will check that everything is
fine. But if the check fails, you'll need to understand the error,
which is what this section is for.

### Disjoint clocks

On the following example, liquidsoap will issue the fatal error
`a source cannot belong to two clocks`:

```liquidsoap
s = playlist("~/media/audio")
output.alsa(s) # perhaps for monitoring
output.icecast(mount="radio.ogg",%vorbis,crossfade(s))
```

Here, the source `s` is first assigned the ALSA clock,
because it is tied to an ALSA output.
Then, we attempt to build a `crossfade` over `s`.
But this operator requires its source to belong to a dedicated
internal clock (because crossfading requires control over the flow
of the of the source, to accelerate it around track changes).
The error expresses this conflict:
`s` must belong at the same time to the ALSA clock
and `crossfade`'s clock.

### Nested clocks

On the following example, liquidsoap will issue the fatal error
`cannot unify two nested clocks`:

```liquidsoap
jingles = playlist("jingles.lst")
music = rotate([1,10],[jingles,playlist("remote.lst")])
safe = rotate([1,10],[jingles,single("local.ogg")])
q = fallback([crossfade(music),safe])
```

Let's see what happened.
The `rotate` operator, like most operators, operates
within a single clock, which means that `jingles`
and our two `playlist` instances must belong to the same clock.
Similarly, `music` and `safe` must belong to that
same clock.
When we applied crossfading to `music`,
the `crossfade` operator created its own internal clock,
call it `cross_clock`,
to signify that it needs the ability to accelerate at will the
streaming of `music`.
So, `music` is attached to `cross_clock`,
and all sources built above come along.
Finally, we build the fallback, which requires that all of its
sources belong to the same clock.
In other words, `crossfade(music)` must belong
to `cross_clock` just like `safe`.
The error message simply says that this is forbidden: the internal
clock of our crossfade cannot be its external clock -- otherwise
it would not have exclusive control over its internal flow of time.

The same error also occurs on `add([crossfade(s),s])`,
the simplest example of conflicting time flows, described above.
However, you won't find yourself writing this obviously problematic
piece of code. On the other hand, one would sometimes like to
write things like our first example.

The key to the error with our first example is that the same
`jingles` source is used in combination with `music`
and `safe`. As a result, liquidsoap sees a potentially
nasty situation, which indeed could be turned into a real mess
by adding just a little more complexity. To obtain the desired effect
without requiring illegal clock assignments, it suffices to
create two jingle sources, one for each clock:

```liquidsoap
music = rotate([1,10],[playlist("jingles.lst"),
                       playlist("remote.lst")])
safe  = rotate([1,10],[playlist("jingles.lst"),
                       single("local.ogg")])
q = fallback([crossfade(music),safe])
```

There is no problem anymore: `music` belongs to
`crossfade`'s internal clock, and `crossfade(music)`,
`safe` and the `fallback` belong to another clock.

## The clock API

There are only a couple of operations dealing explicitly with clocks.

The function `clock.assign_new(l)` creates a new clock
and assigns it to all sources from the list `l`.
For convenience, we also provide a wrapper, `clock(s)`
which does the same with a single source instead of a list,
and returns that source.
With both functions, the new clock will follow (the computer's idea of)
real time, unless `sync=false` is passed, in which case
it will run as fast as possible.

The old (pre-1.0.0) setting `root.sync` is superseded
by `clock.assign_new()`.
If you want to run an output as fast as your CPU allows,
just attach it to a new clock without synchronization:

```liquidsoap
clock.assign_new(sync="none",[source])
```

This will automatically attach the appropriate sources to that clock.

Another important use case of this operator is if your script involves multiple sources from the same external clock, typically multiple ALSA input or output from the same sound card or multiple jack input and output. By default (the so-called `clock_safe` mode), liquidsoap will assign a dedicated clock to each of those sources, leading either to an error or forcing the use of an unnecessary `buffer` (see below). Instead, you can allocate each source with `clock_safe=false` and assign them a single clock:

```
s1 = input.jack(clock_safe=false, ...)
s2 = input.jack(clock_safe=false, ...)

clock.assign_new([s1,s2])
```

However, you may need to do it for other operators if they are totally
unrelated to the first one.

The `buffer()` operator can be used to communicate between
any two clocks: it takes a source in one clock and builds a source
in another. The trick is that it uses a buffer: if one clock
happens to run too fast or too slow, the buffer may empty or overflow.

Finally, `get_clock_status` provides information on
existing clocks and track their respective times:
it returns a list containing for each clock a pair
`(name,time)` indicating
the clock id its current time in _clock cycles_ --
a cycle corresponds to the duration of a frame,
which is given in ticks, displayed on startup in the logs.
The helper function `log_clocks` built
around `get_clock_status` can be used to directly
obtain a simple log file, suitable for graphing with gnuplot.
Those functions are useful to debug latency issues.

## External clocks: decoupling latencies

The first reason to explicitly assign clocks is to precisely handle
the various latencies that might occur in your setup.

Most input/output operators (ALSA, AO, Jack, OSS, etc)
require their own clocks. Indeed, their processing rate is constrained
by external sound APIs or by the hardware itself.
Sometimes, it is too much of an inconvenience,
in which case one can set `clock_safe=false` to allow
another clock assignment --
use at your own risk, as this might create bad latency interferences.

Currently, `output.icecast` does not require to belong
to any particular clock. This allows to stream according to the
soundcard's internal clock, like in most other tools:
in `output.icecast(%vorbis,mount="live.ogg",input.alsa())`,
the ALSA clock will drive the streaming of the soundcard input via
icecast.

Sometimes, the external factors tied to Icecast output cannot be
disregarded: the network may lag. If you stream a soundcard input
to Icecast and the network lags, there will be a glitch in the
soundcard input -- a long enough lag will cause a disconnection.
This might be undesirable, and is certainly disappointing if you
are recording a backup of your precious soundcard input using
`output.file`: by default it will suffer the same
latencies and glitches, while in theory it could be perfect.
To fix this you can explicitly separate Icecast (high latency,
low quality acceptable) from the backup and soundcard input (low latency,
high quality wanted):

```liquidsoap
input = input.oss()

# Icecast source, with its own clock:
icecast_source = mksafe(buffer(input))
clock.assign_new(id="icecast", [icecast_source])

# Output to icecast:
output.icecast(%mp3,mount="blah",icecast_source)

# File output:
output.file(
  %mp3,{time.string("record-%Y-%m-%d-%H-%M-%S.mp3")},
  input)
```

Here, the soundcard input and file output end up in the OSS
clock. The icecast output
goes to the explicitly created `"icecast"` clock,
and a buffer is used to
connect it to the soundcard input. Small network lags will be
absorbed by the buffer. Important lags and possible disconnections
will result in an overflow of the buffer.
In any case, the OSS input and file output won't be affected
by those latencies, and the recording should be perfect.
The Icecast quality is also better with that setup,
since small lags are absorbed by the buffer and do not create
a glitch in the OSS capture, so that Icecast listeners won't
notice the lag at all.

## Internal clocks: exploiting multiple cores

Clocks can also be useful even when external factors are not an issue.
Indeed, several clocks run in several threads, which creates an opportunity
to exploit multiple CPU cores.
The story is a bit complex because OCaml has some limitations on
exploiting multiple cores, but in many situations most of the computing
is done in C code (typically decoding and encoding) so it parallelizes
quite well.

Typically, if you run several outputs that do not share much (any) code,
you can put each of them in a separate clock.
For example the following script takes one file and encodes it as MP3
twice. You should run it as `liquidsoap EXPR -- FILE`
and observe that it fully exploits two cores:

```liquidsoap
def one()
  s = single(argv(1))
  clock.assign_new(sync="none",[s])
  output.file(%mp3,"/dev/null",s)
end
one()
one()
```
