# Migrating to a new Liquidsoap version

This page lists the most common issues when migrating to a new version of Liquidsoap.

### Generalities

If you are installing via `opam`, it can be useful to create a [new switch](https://opam.ocaml.org/doc/Usage.html) to install
the new version of `liquidsoap`. This lets you test the new version while keeping
the old version around in case you need to revert.

More generally, we recommend keeping a backup of your script and testing it in a staging
environment close to production before going live. Streaming issues can build up over time.
We do our best to release stable code, but problems can arise for many reasons —
always do a trial run before putting things into production.

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
in their own dedicated section, and can be executed asynchronously by setting `synchronous=false`
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

**Asynchronous or synchronous?** Use `synchronous=true` for fast or timing-sensitive callbacks.
Use `synchronous=false` for slow, non-time-sensitive work like submitting to a remote HTTP server.

**Note on execution order:** When `synchronous=false`, callbacks run via `thread.run`, which means
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
request.create(...)  # 💥 Cryptic type error!
```

Previously, it was far too easy to overwrite important built-in modules (like `request`) and end up with confusing type errors.

No script changes needed for this but if you see:

```
Warning 6: Top-level variable request is overridden!
```

…consider renaming your variable.

### `null()` replaced by `null`

Previously, `null` was a function — you had to call `null()` to get a null value, or `null(value)` to wrap something.

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

A mechanism for caching script was added. There are two caches, one for the standard library
that is shared by all scripts, and one for individual scripts.

Scripts run the same way with or without caching. However, caching your script has two advantages:

- The script starts much faster.
- Much less memory is used at startup. The cache stores the result of typechecking and other initialization work done on first run.

You can pre-cache a script using the `--cache-only` command:

```liquidsoap
$ liquidsoap --cache-only /path/to/script.liq
```

The location of the two caches can be found by running `liquidsoap --build-config`. You can also set them using the
`$LIQ_CACHE_USER_DIR` and `$LIQ_CACHE_SYSTEM_DIR` environment variables.

Typically, inside a docker container, to pre-cache a script you would set `$LIQ_CACHE_SYSTEM_DIR` to the appropriate
location and then run `liquidsoap --cache-only`:

```dockerfile
ENV LIQ_CACHE_USER_DIR=/path/to/liquidsoap/cache

RUN mkdir -p $LIQ_CACHE_USER_DIR && \
    liquidsoap --cache-only /path/to/script.liq
```

See [the language page](language.html#caching) for more details!

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

**Known behavioral change** — `string.split` with a capture group no longer returns the matched separator:

```
# 2.2.x: matched separator was included in the result
% string.split(separator="(:|,)", "foo:bar")
["foo", ":", "bar"]

# 2.3.x: matched separator is not included
% string.split(separator="(:|,)", "foo:bar")
["foo", "bar"]
```

**Known incompatibility** — Named capture groups using `(?P<name>pattern)` are no longer supported.
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

The operator is now considered production-ready, though it is very powerful and should be used with care.

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
- Cleaned up and removed parameters that were irrelevant to each operator, i.e. `icy_id` in `output.icecast` and etc.
- Made `mount` mandatory and `name` nullable. Use `mount` as `name` when `name` is `null`.

### HLS events

Starting with version `2.2.1`, on HLS outputs, `on_file_change` events are now `"created"`, `"updated"` and `"deleted"`. This breaking
was required to reflect the fact that file changes are now atomic. See [this issue](https://github.com/savonet/liquidsoap/issues/3284)
for more details.

### `cue_cut`

Starting with version `2.2.4`, the `cue_cut` operator has been removed. Cue-in and cue-out processing
is now integrated directly into request resolution. In most cases, you can simply remove the operator
from your script. In some cases, you may need to disable `cue_in_metadata` and `cue_out_metadata`
when creating requests or `playlist` sources.

### Harbor HTTP server and SSL support

The API for registering HTTP server endpoint and using SSL was completely rewritten. It should be more flexible and
provide node/express like API for registering endpoints and middleware. You can checkout [the harbor HTTP documentation](harbor_http.html)
for more details. The [Https support](harbor_http.html#https-support) section also explains the new SSL/TLS API.

### Timeout

Timeout values were previously inconsistent — some were named `timeout_ms` (integer, milliseconds),
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
now expect `UTF-8` — including those that previously defaulted to other encodings.

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

- `reopen_on_error` and `reopen_on_metadata` in `output.file` an related outputs are now callbacks.
- `request.duration` now returns a `nullable` float, `null` being value returned when the request duration could not be computed.
- `getenv` (resp. `setenv`) has been renamed to `environment.get` (resp. `environment.set`).

## From 2.0.x to 2.1.x

### Regular expressions

First-class [regular expression](language.html#regular-expressions) are introduced and are used to replace the following operators:

- `string.match(pattern=<regexp>, <string>` is replaced by: `r/<regexp>/.test(<string>)`
- `string.extract(pattern=<regexp>, <string>)` is replaced by: `r/<regexp>/.exec(<string>)`
- `string.replace(pattern=<regexp>, <string>)` is replaced by: `r/<regexp>/g.replace(<string>)`
- `string.split(separator=<regexp>, <string>)` is replaced by: `r/<regexp>/.split(<string>)`

### Partial application

In order to improve performance, avoid some programming errors and simplify the
code, the support for partial application of functions was removed (from our
experience it was not used much anyway). This means that you should now provide
all required arguments for functions. The behavior corresponding to partial
application can of course still be achieved by explicitly abstracting (with
`fun(x) -> ...`) over some arguments.

For instance, suppose that we defined the addition function with two arguments
with

```liquidsoap
def add(x,y) =
  x + y
end
```

and defined the successor function by partially applying it to the first
argument

```liquidsoap
suc = add(1)
```

We now need to explicitly provide the second argument, and the `suc` function
should now be defined as

```liquidsoap
suc = fun(x) -> add(1, x)
```

or

```liquidsoap
def suc(x) =
  add(1, x)
end
```

### JSON parsing

JSON parsing was greatly improved and is now much more user-friendly.
You can check out our detailed presentation [here](json.html).

### Runtime evaluation

Runtime evaluation of strings has been re-implemented as a type-safe
eval `let` decoration. You can now do:

```liquidsoap
let eval x = "[1,2,3]"
```

And, just like with JSON parsing, the recommended use is with a
_type annotation_:

```liquidsoap
let eval (x: [int]) = "[1,2,3]"
```

### Deprecations and breaking changes

- The argument `streams_info` of `output.file.hls` is now a record.
- Deprecated argument `timeout` of `http.*` operators.
- `source.on_metadata` and `source.on_track` now return a source as this was the case in previous versions, and associated handlers are triggered only when the returned source is pulled
- `output.youtube.live` renamed `output.youtube.live.rtmp`, remove `bitrate` and `quality` arguments and added a single encoder argument to allow stream copy and more.
- `list.mem_assoc` is replaced by `list.assoc.mem`
- `timeout` argument in `http.*` operators is replaced by `timeout_ms`.
- `request.ready` is replaced by `request.resolved`

## From 1.4.x to 2.0.0

### `audio_to_stereo`

`audio_to_stereo` should not be required in most situations anymore. `liquidsoap` can handle channels conversions transparently now!

### `auth` function in `input.harbor`

The type of the `auth` function in `input.harbor` has changed. Where before, you would do:

```liquidsoap
def auth(user, password) =
  ...
end
```

You would now do:

```liquidsoap
def auth(params)
  user     = params.user
  password = params.password
  ...
end
```

### Type errors with lists of sources

Now that sources have their own methods, the actual list of methods attached to each source can vary from one to the next. For instance,
`playlist` has a `reload` method but `input.http` does not. This currently confuses the type checker and leads to errors that look like this:

```liquidsoap
At script.liq, line xxx, char yyy-zzz:
Error 5: this value has type
  _ * source(audio=?A, video=?B, midi=?C)
  .{
    time : () -> float,
    shutdown : () -> unit,
    fallible : bool,
    skip : () -> unit,
    seek : (float) -> float,
    is_active : () -> bool,
    is_up : () -> bool,
    log :
    {level : (() -> int?).{set : ((int) -> unit)}
    },
    self_sync : () -> bool,
    duration : () -> float,
    elapsed : () -> float,
    remaining : () -> float,
    on_track : ((([string * string]) -> unit)) -> unit,
    on_leave : ((() -> unit)) -> unit,
    on_shutdown : ((() -> unit)) -> unit,
    on_metadata : ((([string * string]) -> unit)) -> unit,
    is_ready : () -> bool,
    id : () -> string,
    selected : (() -> source(audio=?D, video=?E, midi=?F)?)
  }
but it should be a subtype of the type of the value at radio.liq, line 122, char 2-21
  _ * _.{reload : _}
```

Use the `(s:source)` type annotation to tell the type checker to ignore source-specific methods
and treat the value simply as a source:

```liquidsoap
s = fallback([
  (s1:source),
  (s2:source),
  (s3:source)
])
```

### Http input and operators

HTTP support has been delegated to external libraries for broader protocol compatibility.
If you installed `liquidsoap` via `opam`:

- You need to install the `ocurl` package to enable all HTTP request operators, `http.get`, `http.post`, `http.put`, `http.delete` and `http.head`
- You need to install the `ffmpeg` package (version `1.0.0` or above) to enable `input.http`
- You do not need to install the `ssl` package anymore to enable their `https` counter-part. These operators have been deprecated.

### Crossfade

The `cross` transition function signature changed: instead of individual arguments for each track's
properties, they are now grouped into two records (`ending` and `starting`). For example:

```liquidsoap
def transition(
  ending_dB_level, starting_dB_level,
  ending_metadata, starting_metadata,
  ending_source,   starting_source) =
...
end
```

You would now do:

```liquidsoap
def transition(ending, starting) =
  # Now you can use:
  #  - ending.db_level, ending.metadata, ending.source
  #  - starting.db_level, starting.metadata, starting.source
...
end
```

### Settings

Settings are now exported as records. Where you would before write:

```liquidsoap
set("decoder.decoders", ["MAD", "FFMPEG"])
```

You can now write:

```liquidsoap
settings.decoder.decoders.set(["MAD", "FFMPEG"])
```

Likewise, to get a setting's value you can now do:

```liquidsoap
current_decoders = settings.decoder.decoders()
```

This provides many good features, in particular type-safety.

For convenience, we have added shorter versions of the most used settings. These are all shortcuts to their respective `settings` values:

```liquidsoap
log.level.set(4)
log.file.set(true)
log.stdout.set(true)
init.daemon.set(true)
audio.samplerate.set(48000)
audio.channels.set(2)
video.frame.width.set(720)
video.frame.height.set(1280)
```

The `register` operator was removed as it could not be adapted to the new API. Backward-compatible
`set` and `get` operators are provided, but should be replaced as they will be removed in a future version.

### Metadata insertion

`insert_metadata` no longer returns a pair. It now returns a source with an `insert_metadata` method.
Update your code from:

```liquidsoap
fs = insert_metadata(s)
# The function to insert metadata
f = fst(ms)
# The source with inserted metadata
s = snd(ms)
...
# Using the function
f([("artist", "Bob")])
...
# Using the source
output.pulseaudio(s)
```

to:

```liquidsoap
s = insert_metadata(s)
...
# Using the function
s.insert_metadata([("artist", "Bob")])
...
# Using the source
output.pulseaudio(s)
```

### Request-based queueing

Queueing for request-based sources has been simplified. The `default_duration` and `length` parameters
have been removed. Use `prefetch` instead to specify how many requests to queue in advance.

For more advanced queueing, `request.dynamic.list` and `request.dynamic` now expose functions to
inspect and set their own request queues.

### JSON import/export

`json_of` has been renamed `json.stringify` and `of_json` has been renamed `json.parse`.

JSON export has been enhanced with a new generic object exporter. Associative lists of type `(string, 'a)` are
now exported as objects. See the [JSON documentation page](json.html) for more details.

Convenience functions have been added to convert metadata to and from JSON object format: `metadata.json.stringify` and
`metadata.json.parse`.

### Returned types from output operators

Output operators now return `()` instead of a source, enforcing that outputs are end-points of the
signal graph. If your script used the return value of an output, apply the operator directly to the
source instead. For example:

```liquidsoap
s = ...

clock.assign_new([output.icecast(..., s)])
```

Should now be written:

```liquidsoap
s = ...

clock.assign_new([s], ...)

output.icecast(..., s)
```

### Deprecated operators

Some operators have been deprecated. Most have backward-compatible replacements. You will see
deprecation warnings in your logs. Here's a list of the most important ones:

- `playlist.safe` is replaced by: `playlist(mksafe(..))`
- `playlist.once` is replaced by: `playlist`, setting `reload_mode` argument to `"never"` and `loop` to `false`
- `rewrite_metadata` should be rewritten using `metadata.map`
- `fade.initial` and `fade.final` are not needed anymore
- `get_process_output` is replaced by: `process.read`
- `get_process_lines` is replaced by: `process.read.lines`
- `test_process` is replaced by: `process.test`
- `system` is replaced by: `process.run`
- `add_timeout` is replaced by: `thread.run.recurrent`
- `on_blank` is replaced by: `blank.detect`
- `skip_blank` is replaced by: `blank.skip`
- `eat_blank` is replaced by: `blank.eat`
- `strip_blank` is replaced by: `blank.strip`
- `which` is replaced by: `file.which`
- `register_flow`: flow is no longer maintained
- `empty` is replaced by: `source.fail`
- `file.unlink` is replaced by: `file.remove`
- `string.utf8.escape` is replaced by: `string.escape`
- `map_metadata` is replaced by: `metadata.map`

### Windows build

The Windows binary is statically built, which means both `%ffmpeg` and any encoder sharing its
underlying libraries (e.g. `libmp3lame` for MP3) cannot be enabled simultaneously — they export
conflicting C symbols.

Since `%ffmpeg` covers all conflicting encoders and more, the Windows build enables `%ffmpeg` and
disables all other encoders. If you were using a different encoder, switch to `%ffmpeg`. For example,
for MP3 encoding with variable bitrate:

```liquidsoap
%ffmpeg(format="mp3", %audio(codec="libmp3lame", q=7))
```
