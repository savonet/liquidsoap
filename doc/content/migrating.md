# Migrating to a new Liquidsoap version

In this page, we list the most common catches when migrating to a new version of
Liquidsoap.

### Generalities

If you are installing via `opam`, it can be useful to create a [new switch](https://opam.ocaml.org/doc/Usage.html) to install
the new version of `liquidsoap`. This will allow to test the new version while keeping
the old version around in case you to revert to it.

More generally, we recommend to always keep a version of your script around and also
to make sure that you test your new script with a staging environment that is
close to production. Streaming issues can build up over time. We do our best to
release the most stable possible code but problems can arise from many reasons
so, always best to first to a trial run before putting things to production!

## From 2.4.x to 2.5.x

### Automatic video dimensions detection

Video dimensions (`video.frame.width`/`height`) are now automatically detected from the first decoded video file. This means you no longer need to manually set dimensions in most cases.

To disable this behavior, either set `settings.video.detect_dimensions` to `false` or explicitly set the video dimensions yourself.

### Implicit integer to float casting

Integers can now be implicitly converted to floats when a float is expected. This makes numerical code easier to write:

```{.liquidsoap include="implicit-float-ok.liq"}

```

Previously, you would need to explicitly use `5.` or `float_of_int(5)`.

**Limitations:** This conversion only works when the type checker can safely determine that a float is expected. There are inherent limitations to where this can be applied. The following cases do **not** work:

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

The `add` operator (and related track-level `track.audio.add` and `track.video.add` operators) now relays metadata from **all** sources being summed, not just the first one.

Previously, only metadata from the first source effectively added was relayed. This was a long-standing behavior that could be surprising when mixing multiple sources with distinct metadata.

If you were relying on the old behavior of only getting metadata from the first source, you may need to filter or prioritize metadata manually using `metadata.map`.

### Crossfade simplification

The `cross` and `crossfade` operators have been simplified. The separate `start_duration` and `end_duration` parameters have been replaced by a single unified `duration` parameter. The crossfade now buffers the same duration from both ending and starting tracks.

**If you use autocue** (via `enable_autocue_metadata()` or external autocue implementations like those used in AzuraCast): No changes are required. Everything should work as before.

**If you don't use autocue**: The transition will now be computed using the same duration for both tracks. If you were previously using different `start_duration` and `end_duration` values, you'll need to adjust your script to use a single `duration` value.

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

The `add` operator now relays metadata from all sources being summed (see above). To prevent metadata from the ending track from being surfaced in crossfade transitions, they have been removed from the source passed to the transition. Instead, they are passed explicitly via the transition arguments. In the transition function, use `ending.metadata` and `starting.metadata` to access the metadata from each track.

Also, remember that the `add` operator removes all track marks.

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

tl;dr:

- Callbacks have been moved to their own section of the documentation
  for better standardization and information.
- Callbacks can be executed using `thread.run` by passing `synchronous=false`
  when registering them.
- Most callback arguments should be accepted as deprecated arguments.
- `blank.detect` could not be updated in a backward-compatible manner.
- `on_file_change` in `output.*.hls` has been updated to pass a single record.
- `on_connect` callback on `output.harbor` has been updated to pass a single record.

Stream-related callbacks is the biggest change with this release. They are now fully documented, with their own dedicated section
in the doc and can now be executed in an asynchronous task when asked by setting the mandatory `synchronous` argument to `false`.

When setting `synchronous=false`, the function to be executed by the callback is placed in a `thread.run` task. This is done to
make sure that the functions executed during the streaming cycle do not impact the streaming latency. Otherwise,
a callback function takes too long, the streaming cycle gets late, causing issues with the runtime system typically resulting in catchup errors.

Callbacks have also been moved to source methods in order to unify the codebase, options and more. In most cases, callback previously passed as
arguments are still accepted, triggering a deprecation warning.

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

❓ **Asynchronous or synchronous?** When registering callbacks, you have to specify if you want the function to be called synchronously or asynchronously. If the function is fast to execute or requires precise timing (it should still be fast to execute though!) then you should register with `synchronous=true`. Slow tasks that are not time-sensitive like submitting to a remote HTTP server should be registered with `synchronous=false`.

⚠️ **Asynchronous callbacks execution order** When registered with `synchronous=false`, callbacks are executed using `thread.run`. This means that there can be a slight delay in their execution. Also, execution order is not guaranteed to be respected.

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

## `null()` replaced by `null`

Previously, `null` was a function — you had to call `null()` to get a null value, or `null(value)` to wrap something. This confused a lot of people (and the typechecker wasn’t helping).

Now, `null` can be used directly:

```liquidsoap
my_var = null
```

Function form still works if you need it:

```liquidsoap
my_var = null("some value")  # Explicit nullable
```

## From 2.2.x to 2.3.x

### Script caching

A mechanism for caching script was added. There are two caches, one for the standard library
that is shared by all scripts, and one for individual scripts.

Scripts should run the same way with or without caching. However, caching your script has two advantage:

- The script starts much faster.
- Much less memory is used when starting. This memory is used the first time running the script to typecheck it and more. This is what we're caching.

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
  `file.replaygain(~id=null, ~compute=true, ~ratio=50., file_name)`.
  The compute parameter determines if gain should be calculated when the metadata does not already contain replaygain tags.

- The `enable_replaygain_metadata` function now accepts a compute parameter to control replaygain calculation.

- The `replaygain` function no longer takes an `ebu_r128` parameter. The signature is now simply: `replaygain(~id=null, s)`.
  Previously, `ebu_r128` allowed controlling whether EBU R128 or standard replaygain was used.
  However, EBU R128 data is now extracted directly from metadata when available.
  So `replaygain` cannot control the gain type via this parameter anymore.

### Regular expressions

The library providing regular expressions has been switched with `2.3.0`. This means that subtle differences
can arise with the evaluation of some regular expressions.

Here's an example that was recently reported:

In `2.2.x`, this was true:

```
# When using a regular expression with a capture pattern to split, the value matched for splitting is returned:
% string.split(separator="(:|,)", "foo:bar")
["foo", ":", "bar"]

# But not when using a regular expression without matching:
% string.split(separator=":|,", "foo:bar")
["foo", "bar"]
```

In `2.3.x`, the matched pattern is not returned:

```
% string.split(separator="(:|,)", "foo:bar")
["foo", "bar"]

% string.split(separator=":|,", "foo:bar")
["foo", "bar"]
```

### Static requests

Static requests detection can now work with nested requests.

Typically, a request for this URI: `annotate:key="value",...:/path/to/file.mp3` will be
considered static if `/path/to/file.mp3` can be decoded.

Practically, this means that more source will now be considered infallible, for instance
a `single` using the above uri.

In most cases, this should improve the user experience when building new scripts and streaming
systems.

In rare cases where you actually wanted a fallible source, you can still pass `fallible=true` to e.g.
the `single` operator or use the `fallible:` protocol.

### String functions

Some string functions have been updated to account for string encoding. In particular, `string.length` and `string.sub` now assume that their
given string is in `utf8` by default.

While this is what most user expect, this can lead to backward incompatibilities and new exceptions. You can change back to the old default by
passing `encoding="ascii"` to these functions or using the `settings.string.default_encoding` settings.

### `check_next`

`check_next` in playlist operators is now called _before_ the request is resolved, to make it possible to cut out
unwanted requests before consuming process time. If you need to see the request's metadata or if the request resolves
into a valid tile, however, you might need to call `request.resolve` inside your `check_next` script.

### Regular expressions

The backend to interpret regular expressions has been changed. For the most part, all existing regular expressions should be supported
but you might experience some incompatibilities with advanced/complex ones.

Known incompatibilities include:

- `(?P<name>pattern)` for named captures is not supported. `(?<name>pattern)` should be used instead.

### `segment_name` in HLS outputs

To make segment name more flexible, `duration` (segment duration in seconds) and `ticks` (segment exact duration in liquidsoap's main ticks) have been added
to the data available when calling `segment_name`.

To prevent any further breakage of this function, its arguments have been changed to a single record containing all the available attributes:

```liquidsoap
def segment_name(metadata) =
  "#{metadata.stream_name}_#{metadata.position}.#{metadata.extname}"
end
```

### `on_air` metadata

Request `on_air` and `on_air_timestamp` metadata are deprecated. These values were never reliable. They are set at the request level when `request.dynamic`
and all its derived sources start playing a request. However, a request can be used in multiple sources and the source using it can be used in multiple
outputs or even not be actually being on the air if, for instance, it not selected by a `switch` or `fallback`.

Instead, it is recommended to get this data directly from the outputs.

Starting with `2.3.0`, all output now add `on_air` and `on_air_timestamp` to the metadata returned by `on_track`, `on_metadata` and `last_metadata` and the telnet `metadata` command.

For the telnet `metadata` command, these metadata need to be added to the `settings.encoder.metadata.export` setting first.

If you are looking for an event-based API, you can use the output's `on_track` methods to track the metadata currently being played and the time at which it started being played.

For backward compatibility and easier migration, `on_air` and `on_air_timestamp` metadata can be enabled using the `settings.request.deprecated_on_air_metadata` setting:

```liquidsoap
settings.request.deprecated_on_air_metadata := true
```

However, it is highly recommended to migrate your script to use one of the new method.

### `last_metadata`

The implementation of `last_metadata` was updated to clear the last metadata when a new track begins. This is more in line with most user's expectation: last metadata
is intended to reflect the metadata of the current track.

If you need to, you can revert to the previous behavior using the source's `reset_last_metadata_on_track` method:

```liquidsoap
s.reset_last_metadata_on_track := false
```

### Gstreamer

`gstreamer` was removed. It had been deprecated for a while. We expect `ffmpeg` to carry most, if not all
of gstreamer's features. See [this PR](https://github.com/savonet/liquidsoap/pull/4036) for more details.

### Prometheus

The default port for the Prometheus metrics exporter has changed from `9090` to `9599`.
As before, you can change it with `settings.prometheus.server.port := <your port value>`.

### `source.dynamic`

Many operators such as `single` and `request.once` have been reworked to use `source.dynamic` as their underlying
implementation.

The operator is now considered usable in production although we urge caution when using it: it is very powerful but can
also break things!

If you were (boldly!) using this operator before, the most important change is that its `set` method has been removed in
favor of a unique callback API.

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

Starting with version `2.2.4`, the `cue_cut` operator has been removed. Requests cue-in and cue-out processing has been integrated
directly into requests resolution. In most cases, you simply can remove the operator from your script. In some cases, you might
need to disable `cue_in_metadata` and `cue_out_metadat` either when creating new requests or when creating `playlist` sources.

### Harbor HTTP server and SSL support

The API for registering HTTP server endpoint and using SSL was completely rewritten. It should be more flexible and
provide node/express like API for registering endpoints and middleware. You can checkout [the harbor HTTP documentation](harbor_http.html)
for more details. The [Https support](harbor_http.html#https-support) section also explains the new SSL/TLS API.

### Timeout

We used to have timeout values labelled `timeout` or `timeout_ms`, some of these would be integer and
in milliseconds, other floating point and in seconds etc. This was pretty confusing so, now all `timeout`
settings and arguments have been unified to be named `timeout` and hold a floating point value representing
a number of seconds.

In most cases, your script will not execute until you have updated your custom `timeout`
values but you should also review all of them to make sure that they follow the new
convention.

### Metadata overrides

Some metadata overrides have been made to reset on track boundaries. Previously, those were permanent even though they
were documented as only applying to the current track. If you need to keep the previous behavior, you can used the
`persist_overrides` parameters (`persis_override` for `cross`/`crossfade`).

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

Default encoding for `output.harbor`, `output.icecast` and `output.shoutcast` metadata has been changed to `UTF-8` in all cases.

Legacy systems used to expect `ISO-8859-1` (also known as `latin1`) for metadata inserted into `mp3` streams via the `icy`
mechanism.

It seems that, nowadays, most software expect `UTF-8` out of the box, including for legacy systems that previously
assumed other encodings. Therefore, by changing this default value, we try to match expectations of the largest
number of users of our software.

If you are using one of these outputs, make sure to test this assumptions with your listners' clients. If needed, the
characters encoding can be set to a different value using the operator's parameters.

### Decoder names

Decoder names have been converted to lowercase. If you were relying on specific settings for decoders priority/ordering, you
will need to convert them to lowercase, for instance:

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

Add file-based operators do not support `strftime` type conversions out of the box anymore. Instead, you should use explicit conversions using `time.string`. This means that this script:

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

In such cases, we recommend to give a little nudge to the typechecker by using the `(s:source)` type annotation where a list of source is causing the issue. For instance:

```liquidsoap
s = fallback([
  (s1:source),
  (s2:source),
  (s3:source)
])
```

This tells the type checker not to worry about the source methods and just focus on what matters, that they are actually sources.. 🙂

### Http input and operators

In order to provide as much compatibility as possible with the different HTTP protocols and implementation, we have decided
to delegate HTTP support to external libraries which have large scale support and implementation. This means that,
if you have installed `liquidsoap` using `opam`:

- You need to install the `ocurl` package to enable all HTTP request operators, `http.get`, `http.post`, `http.put`, `http.delete` and `http.head`
- You need to install the `ffmpeg` package (version `1.0.0` or above) to enable `input.http`
- You do not need to install the `ssl` package anymore to enable their `https` counter-part. These operators have been deprecated.

### Crossfade

The parameters for `cross` transitions was changed to take advantage of the new module system. Instead of passing multiple arguments
related to the ending and starting track, those are regrouped into a single record. So, if you had a transition like this:

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

The `register` operator could not be adapted to this new API and had to be removed, however, backward-compatible
`set` and `get` operators are provided. Make sure to replace them as they should be removed in a future version.

### Metadata insertion

The function `insert_metadata` does not return a pair anymore, but a source with
a method named `insert_metadata`. This means that you should change the code

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

to

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

Queueing for request-based sources has been simplified. The `default_duration` and `length` have been removed in favor of
a simpler implementation. You can now pass a `prefetch` parameter which tells the source how many requests should be queued
in advance.

Should you need more advanced queueing strategy, `request.dynamic.list` and `request.dynamic` now export functions to retrieve
and set their own queue of requests.

### JSON import/export

`json_of` has been renamed `json.stringify` and `of_json` has been renamed `json.parse`.

JSON export has been enhanced with a new generic json object export. Associative lists of type `(string, 'a)` are now
exported as lists. See our [JSON documentation page](json.html) for more details.

Convenience functions have been added to convert metadata to and from JSON object format: `metadata.json.stringify` and
`metadata.json.parse`.

### Returned types from output operators

Starting with liquidsoap `2.0.0`, output operators return the empty value `()` while they previously returned a source.

This helps enforce the fact that outputs should be end-points of your scripting graphs. However, in some cases, this can cause
issues while migrating old scripts, in particular if the returned value of an output was used in the script.

The way to fix this is to apply your operator to the source directly underneath the output. For instance, the following clock assignment:

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

Some operators have been deprecated. For most of them, we provide a backward-compatible support
but it is good practice to update your script. You should see logs in your script when running
deprecated operatords. Here's a list of the most important ones:

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
- `metadata.map` is replaced by: `metadata.map`

### Windows build

The windows binary is statically built and, for this reason, we cannot enable both the `%ffmpeg` encoder and any encoder that
uses the same underlying libraries, for instance `libmp3lame` for `mp3` encoding. The technical reason is that both libraries
import the same C symbols, which makes compilation fail.

The `%ffmpeg` encoder provides all the functionalities of the internal encoders that conflict with it along with many more format
we do not support otherwise. For this reason, it was decided to enable the `%ffmpeg` encoder and disable all other encoders.

This means that, if you were previously using a different encoder than `%ffmpeg`, you will need to adapt your script to
use it. For instance, for mp3 encoding with variable bitrate:

```liquidsoap
%ffmpeg(format="mp3", %audio(codec="libmp3lame", q=7))
```
