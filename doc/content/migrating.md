# Migrating to a new Liquidsoap version

This page lists the changes you may need to make to your script when you upgrade Liquidsoap, one section per release, from 2.1.x to 2.2.x onward. Each section covers the breaking changes and the new behaviors that can affect an existing script.

## Generalities

If you are installing via `opam`, it can be useful to create a [new switch](https://opam.ocaml.org/doc/Usage.html) to install
the new version of `liquidsoap`. This lets you test the new version while keeping
the old version around in case you need to revert.

More generally, we recommend keeping a backup of your script and testing it in a staging
environment close to production before going live. Streaming issues can build up over time.
We do our best to release stable code, but problems can arise for many reasons.
Always do a trial run before putting things into production.

## From 2.4.x to 2.5.x

### Video content type renamed from `canvas` to `yuv420p`

The internal video content type has been renamed from `canvas` to `yuv420p` to
better reflect what it actually contains: planar YUV420 images. Internally the
content is still organized as a _canvas_ (a superposition of layers), but the
type name visible in type annotations and error messages is now `yuv420p`.

If your scripts use type annotations with `canvas`, rename them:

| Old                                       | New                                        |
| ----------------------------------------- | ------------------------------------------ |
| `source(video=canvas)`                    | `source(video=yuv420p)`                    |
| `source(video=canvas(width=W, height=H))` | `source(video=yuv420p(width=W, height=H))` |

The `video.canvas` API (for positioning video elements) is unaffected by this change.

### Automatic video dimensions detection

Video dimensions (`video.frame.width`/`height`) are now automatically detected from the first decoded video file. This means you no longer need to manually set dimensions in most cases.

To disable this behavior, either set `settings.frame.video.detect_dimensions` to `false` or explicitly set the video dimensions yourself.

### Implicit integer to float casting

Integers can now be implicitly converted to floats when a float is expected. This makes numerical code easier to write:

```{.liquidsoap include="implicit-float-ok.liq"}

```

Previously, you would need to explicitly use `5.` or `float_of_int(5)`.

This conversion only applies when the type checker can safely determine that a float is expected. The following cases are rejected:

```liquidsoap
# if-then-else with mixed types - ERROR
x = if true then 1. else 2 end

# List with mixed int and float - ERROR
l = [1., 2, 3.]

# Function returning mixed types - ERROR
def f(b) = if b then 1. else 2 end end
```

In these cases, the type checker cannot safely reconcile the mixed `int` and `float` types. Use explicit float literals (`1.`, `2.`, etc.) when you encounter these situations.

### Metadata in `add` operators

The `add` operator (and related track-level `track.audio.add` and `track.video.add` operators) now relays metadata from all the sources being summed.

Previously, only metadata from the first source effectively added was relayed. This was a long-standing behavior that could be surprising when mixing multiple sources with distinct metadata.

If you were relying on the old behavior of only getting metadata from the first source, you may need to filter or prioritize metadata manually using `metadata.map`.

### `output.harbor` authentication

The `user` and `password` parameters have been removed from `output.harbor`.
Authentication must now be configured exclusively through the `auth` function:

```liquidsoap
# Old
output.harbor(mount="stream", user="source", password="hackme", ...)

# New
output.harbor(
  mount="stream",
  auth=fun({address, login, password}) ->
    login == "source" and password == "hackme",
  ...
)
```

When no authentication is needed, simply omit the `auth` parameter (it defaults to `null`).

The `burst` parameter is now nullable. Pass `null` to disable the initial burst:

```liquidsoap
output.harbor(mount="stream", burst=null, ...)
```

### `output.harbor` listener callbacks

`on_connect` and `on_disconnect` on `output.harbor` now receive the same listener record, which has new `id`, `connected_at`, `duration` and `bytes_sent` fields. The `ip` field no longer includes the client port: use `id` to tell apart connections from the same address.

```liquidsoap
# Old
o.on_disconnect(fun (ip) -> log("#{ip} disconnected"))

# New
o.on_disconnect(fun (listener) -> log("#{listener.ip} disconnected"))
```

### Crossfade simplification

The `cross` and `crossfade` operators have been simplified. The separate `start_duration` and `end_duration` parameters have been replaced by a single unified `duration` parameter. The crossfade now buffers the same duration from both ending and starting tracks.

If you use autocue (via `enable_autocue_metadata()` or external autocue implementations like those used in AzuraCast), your script works as before.

If you do not use autocue, the transition will now be computed using the same duration for both tracks. If you were previously using different `start_duration` and `end_duration` values, you'll need to adjust your script to use a single `duration` value.

The following changes were made:

| Old                                         | New                               |
| ------------------------------------------- | --------------------------------- |
| `start_duration` parameter                  | Removed, use `duration`           |
| `end_duration` parameter                    | Removed, use `duration`           |
| `override_start_duration` parameter         | Removed, use `override_duration`  |
| `override_end_duration` parameter           | Removed, use `override_duration`  |
| `liq_cross_start_duration` metadata         | Removed, use `liq_cross_duration` |
| `liq_cross_end_duration` metadata           | Removed, use `liq_cross_duration` |
| `s.start_duration()` method                 | Removed, use `s.cross_duration()` |
| `s.end_duration()` method                   | Removed, use `s.cross_duration()` |
| `assume_autocue` parameter                  | Removed                           |
| `settings.crossfade.assume_autocue` setting | Removed                           |

The `add` operator now relays metadata from all sources being summed (see above). To keep metadata from the ending track out of crossfade transitions, metadata is removed from the sources passed to the transition and passed explicitly via the transition arguments. In the transition function, use `ending.metadata` and `starting.metadata` to access the metadata from each track.

Also, remember that the `add` operator removes all track marks.

### Per-source methods on `switch`, `fallback`, `rotate`, `random`

The `switch` operator and its wrappers `fallback`, `rotate` and `random` now take their per-branch settings as methods on each source. The old parallel list parameters are gone. See [source composition](./composition.md) for how these methods work together.

| Old                                            | New                                                          |
| ---------------------------------------------- | ------------------------------------------------------------ |
| `fallback(replay_metadata=false, [s1, s2])`    | `fallback([s1.{replay_metadata = false}, s2])`               |
| `switch(single=[true, false], [...])`          | `s1.{single = true}` on the source                           |
| `rotate(weights=[3, 1], [music, jingles])`     | `rotate([music.{weight = 3}, jingles.{weight = 1}])`         |
| `random(weights=[2, 1], [music, jingles])`     | `random([music.{weight = 2}, jingles])`                      |
| `transitions`, `transition_length`             | `on_select` on the source being entered                      |
| `override`                                     | Removed                                                      |
| `track_sensitive` on the switch                | `track_sensitive` on each source                             |
| `fallback.skip(main, fallback=backup)`         | `fallback([main, backup.{track_sensitive = getter(false)}])` |
| Switching mid-track with no fade (old default) | `s1.{on_select = source.composition.legacy_on_select}`       |

Each source now carries its own `weight`, defaulting to `1`. The old `weights` list was positional and was padded with `1` when shorter than the source list.

A `transitions` function becomes an `on_select` method on the source being entered:

**Before:**

```liquidsoap
def my_transition(old, new) =
  add([fade.out(old), fade.in(new)])
end
s = fallback(transitions=[my_transition], transition_length=3., [s1, s2])
```

**After:**

```liquidsoap
def my_on_select({ending, starting, replay_metadata=_}) =
  if null.defined(ending) then
    let old = max_duration(3., null.get(ending))
    (add([fade.out(duration=3., old), fade.in(duration=3., starting)]) : source)
  else
    starting
  end
end
s = fallback([s1.{on_select = my_on_select}, s2])
```

Bound `ending` with `max_duration` as above. The source being left is cleaned up only once your transition stops pulling from it. See [writing your own transition](./composition.md#writing-your-own-transition).

Switching now fades out the ending source by default. The default `on_select` sequences the ending source with the starting one when the ending source has between `0.` and `settings.source.composition.max_fade` seconds left, so it finishes naturally. When more time is left and the ending source carries only PCM audio, it is faded out over `max_fade` seconds (default `1.`). Otherwise switching is immediate.

`input.http` is a live source, so it now cuts in immediately. For a relay that carries a playlist, set `s.composition_type := "file"` on it. See [a relay that carries a playlist](./composition.md#a-relay-that-carries-a-playlist).

`rotate.merge` no longer takes `transitions` or `weights`. Set `weight` on the sources, as with `rotate`. `rotate.merge` installs its own `on_select` on the first source, so an `on_select` you set on that source is ignored.

#### Stdlib operators that used to pin `track_sensitive`

Several operators are built on top of `fallback` and used to pass a fixed `track_sensitive`. They now inherit it from the source they wrap, so their behavior follows that source's composition type: `append`, `prepend`, `map_first_track`, `overlap_sources` and the deprecated `fade.final`. If you relied on one of them switching (or not switching) mid-track regardless of its input, set `track_sensitive` on the source you pass in.

`mksafe` is the exception: it pins `source.composition.legacy_on_select` on both branches, so switching to `safe_blank` and back is immediate, with no fade.

The deprecated `mkavailable` lost its `track_sensitive` parameter with no replacement. Use `source.available`, as the deprecation warning suggests.

### Source callbacks return a release

Registering a callback on a source now returns a value with a `release` method:

```liquidsoap
announce = s.on_metadata(synchronous=true, fn)
announce.release()
```

Nothing to change in existing scripts: the value is still `unit` underneath and can be ignored. It matters if you register callbacks repeatedly on long-lived sources, typically when handling dynamic sources. See [source callbacks](./callbacks.md).

### Scheduled work runs concurrently

Request resolutions, harbor clients, `thread.run` handlers and asynchronous source callbacks used to run on a handful of queue threads, one at a time each. They now run on several cores at once, and in parallel with the streaming loop.

Most scripts need no change. A script that reads and writes shared state from several handlers can now observe it half-done: a reader sees one of two references updated and not the other, or two handlers both see a flag unset and both do the work.

Group such writes with `atomic`. The reads need grouping too: `atomic` only holds back other atomic sections, so a reader outside one still sees the writes land one at a time.

```{.liquidsoap include="atomic-now-playing.liq"}

```

A single reference holding a record needs none of this, since the update and the read are each one operation. Prefer it when the values can live together.

For a check-and-set, `r.exchange(v)` writes and returns the previous value in one step:

```{.liquidsoap include="atomic-once.liq"}

```

See [sharing state between tasks](./scheduling.md#sharing-state-between-tasks) for the rules a section must follow.

`settings.scheduler.generic_queues`, `settings.scheduler.fast_queues` and `settings.scheduler.non_blocking_queues` are deprecated: the scheduler sizes itself from the number of cores. Setting them logs a warning. `settings.scheduler.blocking_tasks` caps how many slow tasks run at once.

If concurrent execution breaks a script and it cannot be fixed right away, `settings.scheduler.legacy := true` brings back the previous behavior, threads and queues included. It is a fail-safe and will be removed in a later version.

### Daemon mode removed

The `-d`/`--daemon` option and the `settings.init.daemon` settings are gone, pidfile writing included. Detaching from the terminal required forking, which is not safe now that liquidsoap runs on several cores.

Run liquidsoap in the foreground under a service manager instead, such as `systemd` on Linux or `launchd` on macOS. Both keep track of the process, restart it, and capture its output, so a pidfile is not needed. A script still setting `init.daemon`, `init.daemon.pidfile` or `init.daemon.change_user` no longer typechecks (`this value has no method daemon`); drop those lines, and let the service manager set user and group.

### Prometheus latency metrics renamed

The metrics exported by `prometheus.latency` are ratios of the frame duration: a value below `1` means that liquidsoap keeps up. Their names now end in `_ratio` to say so. `liquidsoap_input_latency_seconds` is now `liquidsoap_input_latency_ratio`, and likewise for the `peak` and `max` variants and for the `output` and `overall` modes. Update your dashboards and alerts to the new names.

## From 2.3.x to 2.4.x

See our [2.4.0 blog post](https://www.liquidsoap.info/blog/2025-08-11-liquidsoap-2.4.0/) for a detailed presentation
and cheatsheet of the new features and changes in this release.

### `insert_metadata`

`insert_metadata` is now available as a source method. You do not need to use the
`insert_metadata` operator anymore and the operator has been deprecated.

You can now directly do:

```liquidsoap
s = some_source()

s.insert_metadata([("title","bla")])
```

### Stream-related callbacks

Stream-related callbacks are the biggest change in this release. They are now fully documented
in their own [section](./callbacks.md), and can be executed asynchronously by setting `synchronous=false`
when registering them.

When `synchronous=false`, the callback is placed in a `thread.run` task, keeping it off the
streaming cycle. This matters because if a callback takes too long, the streaming cycle falls
behind, causing catchup errors.

Callbacks have also been moved to source methods to unify the API. In most cases, callbacks
previously passed as arguments are still accepted, but trigger a deprecation warning.

Summary of changes:

- Callbacks now have their own documentation section.
- Use `synchronous=false` to run a callback asynchronously via `thread.run`.
- Most old-style callback arguments still work with a deprecation warning.
- `blank.detect` could not be updated in a backward-compatible way.
- `on_file_change` in `output.*.hls` now passes a single record argument.
- `on_connect` in `output.harbor` now passes a single record argument.

With the new source-related callbacks, instead of doing:

```liquidsoap
s = on_metadata(s, fn)
```

You should now do:

```liquidsoap
# Set synchronous to false if function fn can potentially
# take a while to execute to prevent blocking the main streaming
# thread.
s.on_metadata(synchronous=true, fn)
```

Additionally, `on_end` and `on_offset` have been merged into a single `on_position` source method. Here is the new syntax:

```liquidsoap
# Execute a callback after current track position:
s.on_position(
  # See above
  synchronous=false,
  # This is the default
  remaining=false,
  position=1.2,
  # Allow execution even if current track does not reach position `1.2`:
  allow_partial=true,
  fn
)

# Execute a callback when remaining position is less
# than the given position:
s.on_position(
  synchronous=false,
  remaining=true,
  position=1.2,
  fn
)
```

With the other callbacks, e.g. `on_start`, instead of doing:

```liquidsoap
output.ao(on_start=fn, ...)
```

You should now do:

```liquidsoap
o = output.ao(...)
o.on_start(synchronous=false, fn)
```

Use `synchronous=true` for fast or timing-sensitive callbacks.
Use `synchronous=false` for slow, non-time-sensitive work like submitting to a remote HTTP server.

When `synchronous=false`, callbacks run via `thread.run`, which means
there may be a slight delay and execution order is not guaranteed.

### Error methods

Error methods have been removed by default from the error types to avoid cluttering the documentation.

If you need to access error methods, you can use `error.methods`:

```liquidsoap
# Add back error methods
err = error.methods(err)

# Access them
print("Error kind: #{err.kind}")
```

### Warnings when overwriting top-level variables

The typechecker is now able to detect when top-level variables are overridden.

This prevents situations like this:

```liquidsoap
request = ...
# Later...
request.create(...)  # Cryptic type error
```

Previously, it was far too easy to overwrite important built-in modules (like `request`) and end up with confusing type errors.

No script changes needed for this but if you see:

```
Warning 6: Top-level variable request is overridden!
```

…consider renaming your variable.

### `null()` replaced by `null`

Previously, `null` was a function: you had to call `null()` to get a null value, or `null(value)` to wrap something.

Now, `null` can be used directly:

```liquidsoap
my_var = null
```

The function form still works for wrapping a value in a nullable:

```liquidsoap
my_var = null("some value")
```

## From 2.2.x to 2.3.x

### Script caching

A mechanism for caching scripts was added. There are two caches, one for the standard library
that is shared by all scripts, and one for individual scripts.

Scripts run the same way with or without caching. However, caching your script has two advantages:

- The script starts much faster.
- Much less memory is used at startup. The cache stores the result of typechecking and other initialization work done on first run.

You can pre-cache a script using the `--cache-only` option:

```
$ liquidsoap --cache-only /path/to/script.liq
```

The location of the two caches can be found by running `liquidsoap --build-config`. You can also set them using the
`$LIQ_CACHE_USER_DIR` and `$LIQ_CACHE_SYSTEM_DIR` environment variables.

Typically, inside a docker container, to pre-cache a script you would set `$LIQ_CACHE_USER_DIR` to the appropriate
location and then run `liquidsoap --cache-only`:

```dockerfile
ENV LIQ_CACHE_USER_DIR=/path/to/liquidsoap/cache

RUN mkdir -p $LIQ_CACHE_USER_DIR && \
    liquidsoap --cache-only /path/to/script.liq
```

See [caching](./script_lifecycle.md#caching) for more details.

### Default frame size

Default frame size has been set to `0.02s`, down from `0.04s` in previous releases. This should lower the latency
of your liquidsoap script.

See [this PR](https://github.com/savonet/liquidsoap/pull/4033) for more details.

### Crossfade transitions and track marks

Track marks can now be properly passed through crossfade transitions. This means that you also have to make sure
that your transition function is fallible! For instance, this silly transition function:

```liquidsoap
def transition(_, _) =
  blank(duration=2.)
end
```

Will never terminate!

Typically, to insert a jingle you would do:

```liquidsoap
def transition(old, new) =
  sequence([old.source, single("/path/to/jingle.mp3"), new.source])
end
```

### Replaygain

- There is a new `metadata.replaygain` function that extracts the replay gain value in _dB_ from the metadata.
  It handles both `r128_track_gain` and `replaygain_track_gain` internally and returns a single unified gain value.

- The `file.replaygain` function now takes a new compute parameter:
  `file.replaygain(id=null, compute=true, ratio=50., file_name)`.
  The compute parameter determines if gain should be calculated when the metadata does not already contain replaygain tags.

- The `enable_replaygain_metadata` function now accepts a compute parameter to control replaygain calculation.

- The `replaygain` function no longer takes an `ebu_r128` parameter. The signature is now simply: `replaygain(~id=null, s)`.
  Previously, `ebu_r128` allowed controlling whether EBU R128 or standard replaygain was used.
  However, EBU R128 data is now extracted directly from metadata when available.
  So `replaygain` cannot control the gain type via this parameter anymore.

### Regular expressions

The regular expression backend was replaced in `2.3.0`. Most existing patterns work as before,
but subtle differences can arise with advanced expressions.

One known behavior change is that `string.split` with a capture group no longer returns the matched separator:

```
# 2.2.x: matched separator was included in the result
% string.split(separator="(:|,)", "foo:bar")
["foo", ":", "bar"]

# 2.3.x: matched separator is not included
% string.split(separator="(:|,)", "foo:bar")
["foo", "bar"]
```

Named capture groups using `(?P<name>pattern)` are no longer supported.
Use `(?<name>pattern)` instead.

### Static requests

Static requests detection can now work with nested requests.

Typically, a request for this URI: `annotate:key="value",...:/path/to/file.mp3` will be
considered static if `/path/to/file.mp3` can be decoded.

Practically, this means that more sources will now be considered infallible, for instance
a `single` using the above URI.

In most cases, this should improve the user experience when building new scripts and streaming
systems.

In rare cases where you actually wanted a fallible source, you can still pass `fallible=true` to e.g.
the `single` operator or use the `fallible:` protocol.

### String functions

Some string functions have been updated to account for string encoding. In particular, `string.length` and `string.sub` now assume that their
given string is in `utf8` by default.

While this is what most users expect, it can lead to backward incompatibilities and new exceptions. You can revert to the previous default by
passing `encoding="ascii"` to these functions or setting `settings.string.default_encoding`.

### `check_next`

`check_next` in playlist operators is now called _before_ the request is resolved, so unwanted
requests can be skipped before consuming process time. If you need to inspect the request's metadata
or check whether it resolves into a valid file, call `request.resolve` inside your `check_next` function.

### `segment_name` in HLS outputs

The `segment_name` function now receives a single record argument instead of individual parameters.
Two new fields have been added: `duration` (segment duration in seconds) and `ticks` (exact duration in Liquidsoap ticks).

```liquidsoap
def segment_name(metadata) =
  "#{metadata.stream_name}_#{metadata.position}.#{metadata.extname}"
end
```

### `on_air` metadata

The `on_air` and `on_air_timestamp` request metadata are deprecated. These values were never reliable:
they are set at the request level when `request.dynamic` starts playing, but a request can be used in
multiple sources, and a source may not actually be on the air if excluded by a `switch` or `fallback`.

Instead, it is recommended to get this data directly from the outputs.

Starting with `2.3.0`, all outputs add `on_air` and `on_air_timestamp` to the metadata returned by `on_track`, `on_metadata`, `last_metadata`, and the telnet `metadata` command.

For the telnet `metadata` command, these metadata need to be added to the `settings.encoder.metadata.export` setting first.

If you are looking for an event-based API, you can use the output's `on_track` methods to track the metadata currently being played and the time at which it started being played.

For backward compatibility and easier migration, `on_air` and `on_air_timestamp` metadata can be enabled using the `settings.request.deprecated_on_air_metadata` setting:

```liquidsoap
settings.request.deprecated_on_air_metadata := true
```

However, it is strongly recommended to migrate your script to use one of the new methods.

### `last_metadata`

`last_metadata` now clears when a new track begins, which aligns with the expected behavior:
it reflects the metadata of the current track, not the previous one.

If you need to, you can revert to the previous behavior using the source's `reset_last_metadata_on_track` method:

```liquidsoap
s.reset_last_metadata_on_track := false
```

### Gstreamer

`gstreamer` was removed after a long deprecation period. The `ffmpeg` integration covers most, if not all,
of the same functionality. See [this PR](https://github.com/savonet/liquidsoap/pull/4036) for more details.

### Prometheus

The default port for the Prometheus metrics exporter has changed from `9090` to `9599`.
As before, you can change it with `settings.prometheus.server.port := <your port value>`.

### `source.dynamic`

Operators such as `single` and `request.once` have been reworked to use `source.dynamic` internally.

The operator is now considered production-ready, though it should be used with care.

If you were already using it, note that the `set` method has been removed in favor of a callback API.

## From 2.1.x to 2.2.x

### References

The `!x` notation for getting the value of a reference is now deprecated. You
should write `x()` instead. And `x := v` is now an alias for `x.set(v)` (both
can be used interchangeably).

### Icecast and Shoutcast outputs

`output.icecast` and `output.shoutcast` are some of our oldest operators and were in dire need of some
cleanup so we did it!

We applied the following changes:

- You should now use `output.icecast` only for sending to icecast servers and `output.shoutcast` only for sending to shoutcast servers. All shared options have been moved to their respective specialized operator.
- Old `icy_metadata` argument was renamed to `send_icy_metadata` and changed to a nullable `bool`. `null` means guess.
- New `icy_metadata` argument now returns a list of metadata to send with ICY updates.
- Added a `icy_song` argument to generate default `"song"` metadata for ICY updates. Defaults to `<artist> - <title>` when available, otherwise `artist` or `title` if available, otherwise `null`, meaning don't add the metadata.
- Cleaned up and removed parameters that were irrelevant to each operator, e.g. `icy_id` in `output.icecast`.
- Made `mount` mandatory and `name` nullable. Use `mount` as `name` when `name` is `null`.

### HLS events

Starting with version `2.2.1`, on HLS outputs, `on_file_change` events are now `"created"`, `"updated"` and `"deleted"`. This breaking change
was required to reflect the fact that file changes are now atomic. See [this issue](https://github.com/savonet/liquidsoap/issues/3284)
for more details.

### `cue_cut`

Starting with version `2.2.4`, the `cue_cut` operator has been removed. Cue-in and cue-out processing
is now integrated directly into request resolution. In most cases, you can simply remove the operator
from your script. In some cases, you may need to disable `cue_in_metadata` and `cue_out_metadata`
when creating requests or `playlist` sources.

### Harbor HTTP server and SSL support

The API for registering HTTP server endpoints and using SSL was completely rewritten. It should be more flexible and
provide a node/express-like API for registering endpoints and middleware. You can check out [the harbor HTTP documentation](./harbor_http.md)
for more details. The [Https support](./harbor_http.md#https-support) section also explains the new SSL/TLS API.

### Timeout

Timeout values were previously inconsistent: some were named `timeout_ms` (integer, milliseconds),
others `timeout` (float, seconds). All `timeout` settings and arguments are now unified: they are
named `timeout` and hold a floating-point number of seconds.

In most cases your script will fail to run until you update your custom `timeout` values.
Review all of them to make sure they follow the new convention.

### Metadata overrides

Some metadata overrides now reset on track boundaries. Previously they were permanent, despite
being documented as track-scoped. To keep the old behavior, use the `persist_overrides` parameter
(`persist_override` for `cross`/`crossfade`).

The list of concerned metadata is:

- `"liq_fade_out"`
- `"liq_fade_skip"`
- `"liq_fade_in"`
- `"liq_cross_duration"`
- `"liq_fade_type"`

### JSON rendering

The confusing `let json.stringify` syntax has been removed as it did not provide any feature not already covered by either
the `json.stringify()` function or the generic `json()` object mapper. Please use either of those now.

### Default character encoding in `output.{harbor,icecast,shoutcast}`

Default metadata encoding for `output.harbor`, `output.icecast`, and `output.shoutcast` has changed to `UTF-8`.

Legacy systems expected `ISO-8859-1` (`latin1`) for ICY metadata in MP3 streams, but most modern clients
now expect `UTF-8`, including those that previously defaulted to other encodings.

If you use these outputs, verify that your listeners' clients handle `UTF-8` correctly. If needed, the
encoding can be set explicitly via the operator's parameters.

### Decoder names

Decoder names are now lowercase. If you have customized decoder priority or ordering, update the names accordingly:

```
settings.decoder.decoders.set(["FFMPEG"])
```

becomes:

```
settings.decoder.decoders.set(["ffmpeg"])
```

Actually, because of the above change in references, this even becomes:

```
settings.decoder.decoders := ["ffmpeg"]
```

### `strftime`

File-based operators no longer support `strftime` format strings directly. Use `time.string` explicitly instead:

```liquidsoap
output.file("/path/to/file%H%M%S.wav", ...)
```

becomes:

```liquidsoap
output.file({time.string("/path/to/file%H%M%S.wav")}, ...)
```

### Other breaking changes

- `reopen_on_error` and `reopen_on_metadata` in `output.file` and related outputs are now callbacks.
- `request.duration` now returns a `nullable` float, `null` being the value returned when the request duration could not be computed.
- `getenv` (resp. `setenv`) has been renamed to `environment.get` (resp. `environment.set`).
