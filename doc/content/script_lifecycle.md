# How scripts run

This page follows your script from the moment you type `liquidsoap script.liq`
to a running stream, and then to shutdown. Knowing these steps helps you read
the startup logs, understand when an error can happen, and make your script
start faster.

Here are the steps, in order:

1. Liquidsoap loads the standard library and your scripts.
2. Liquidsoap parses and typechecks the scripts, or loads them from the cache.
3. Liquidsoap evaluates the scripts: this creates the sources and outputs.
4. The clocks start and run the streaming loop.
5. Liquidsoap shuts down.

## Script loading

When you run liquidsoap for streaming, the command line has the following form:

```
$ liquidsoap script_or_expr_1 ... script_or_expr_N
```

This allows you to ask liquidsoap to load definitions and settings from
some scripts so that they become available when processing the next ones.

For example you can store your passwords by defining the variable `xxx`
in `secret.liq`, and then refer to that variable in your main script
`main.liq`. You would then run `liquidsoap secret.liq main.liq`. If you ever
need to communicate `main.liq` there won't be any risk of divulging your
password.

When available, the variable `liquidsoap.script.path` contains the path of the current script's
file and `null` otherwise.

### The pervasive script library

In fact, liquidsoap also implicitly loads scripts before those that you specify
on the command-line. These scripts are meant to contain standard utilities.
Liquidsoap finds them in its library directory, which depends on how
liquidsoap was built (it is typically `/usr/share/liquidsoap/libs` for
distribution packages). Its path is available in scripts as
`configure.libdir` and is displayed by `liquidsoap --build-config`. You can
load the library from another location with the `--stdlib` option.

Currently, liquidsoap loads `stdlib.liq` from the library directory,
and this file includes some others.
You can add your personal standard library in that directory
if you find it useful.

### Includes

A script can also pull in other files with `%include "file.liq"`. Liquidsoap
reads the included file while parsing, as if you had pasted its contents in
place of the `%include` line. A relative path is resolved from the directory of
the file that contains the `%include`. See
[including other files](./language.md#including-other-files) for examples.

## Parsing and typechecking

Liquidsoap first parses each script. Parsing reads the text, expands the
`%include` lines and reports syntax errors.

Liquidsoap then typechecks the script. The typechecker infers the type of every
expression, including the content of each stream, and reports type errors
before anything runs. Typechecking the standard library is the slowest part of
startup, which is why Liquidsoap caches its result.

## Caching

Type-checking scripts can take a lot of time and consume memory. To optimize things, this step can be cached.

You want to know about the cache for several reasons:

- Your script starts much faster when it is loaded from the cache.
- Your script uses much less memory when it is loaded from the cache.
- In Docker images and other production setups, you can fill the cache when you
  build the image, so that the container starts quickly every time.
- Cache files take up disk space and can contain your secrets, so you should know
  where they are.

During the first execution, the script is parsed, type checked and evaluated. On the second and any following execution, a cache of the script is used, reducing the typechecking phase, sometimes by a `100x` factor!

Here's a log without caching on an M3 MacBook Pro:

```
2024/07/03 14:31:41 [startup:3] main script hash computation: 0.03s
2024/07/03 14:31:41 [startup:3] main script cache retrieval: 0.03s
2024/07/03 14:31:41 [startup:3] stdlib hash computation: 0.03s
2024/07/03 14:31:41 [startup:3] stdlib cache retrieval: 0.03s
2024/07/03 14:31:41 [startup:3] Typechecking stdlib: 3.37s
2024/07/03 14:31:41 [startup:3] Typechecking main script: 0.00s
```

And the same log after caching:

```
2024/07/03 14:32:59 [startup:3] main script hash computation: 0.02s
2024/07/03 14:32:59 [startup:3] Loading main script from cache!
2024/07/03 14:32:59 [startup:3] main script cache retrieval: 0.05s
```

Scripts can be cached ahead of time without executing them, for instance while compiling a docker image, using `--cache-only`. Caching can also be disabled using `--no-cache`.

Caching happens at two different times:

- First the standard library is cached
- Then the script itself is cached

Caching the standard library makes it possible to run the type-checker faster on new scripts. Here's an example of a log from running a new script with
a cached standard library:

```
2024/07/03 14:33:27 [startup:3] main script hash computation: 0.02s
2024/07/03 14:33:27 [startup:3] main script cache retrieval: 0.02s
2024/07/03 14:33:27 [startup:3] stdlib hash computation: 0.03s
2024/07/03 14:33:27 [startup:3] Loading stdlib from cache!
2024/07/03 14:33:27 [startup:3] stdlib cache retrieval: 0.10s
2024/07/03 14:33:27 [startup:3] Typechecking main script: 0.00s
```

Caching can be disabled by setting `LIQ_CACHE` to anything other than `1` or `true`.

### When the cache is used

Liquidsoap parses your script on every run and computes a hash of the parsed
script. The hash covers the included files too, since they are part of the
parsed script. Liquidsoap then looks for a cache file with that hash:

- When you edit your script or one of its included files, the hash changes and
  Liquidsoap typechecks the script again, then writes a new cache file.
- When you upgrade Liquidsoap, the old cache files do not match the new binary.
  Liquidsoap logs `Liquidsoap binary changed: main script cache invalidated!`
  and typechecks again.

The cache only replaces typechecking. Liquidsoap still evaluates your script on
every run, so your sources, settings and environment variables are read fresh
each time.

### Caching in production and Docker images

The first run of a new script pays the full typechecking cost. In production,
you want to pay it ahead of time, when you install the script or build your
image. `--cache-only` parses and typechecks your script, writes the cache files
and exits without streaming:

```
$ liquidsoap --cache-only /path/to/script.liq
```

The run that uses the cache must look in the same cache directory as the run
that filled it. The user cache lives in the home directory of the user running
liquidsoap, so a build step running as `root` and a service running as
`liquidsoap` look in different places. Set `$LIQ_CACHE_USER_DIR` to the same
directory for both. Inside a docker container, `ENV` sets it for the build steps
and for the running container:

```dockerfile
ENV LIQ_CACHE_USER_DIR=/path/to/liquidsoap/cache

RUN mkdir -p $LIQ_CACHE_USER_DIR && \
    liquidsoap --cache-only /path/to/script.liq
```

Make sure that the user running the container can read that directory.

`--cache-only` also caches the standard library in the system cache. Our Debian,
Ubuntu, Fedora and Alpine packages already fill the system cache in
`/var/cache/liquidsoap`, see [using in production](./in_production.md). When
Liquidsoap cannot write a cache file, it logs `Error while storing cache: ...`
and keeps running without the cache.

### Cache locations

Cache files can accumulate and also take up disk space so it is important to know where they are located!

There are two types of cache locations:

- System cache for cached files that should be shared with all liquidsoap scripts. This is where the standard library cache is located. This location is a system-wide path on Unix systems such as `/var/cache/liquidsoap`.
- User cache for cached files that are specific to the user running liquidsoap scripts. On Unix systems, this location is at `$HOME/.cache/liquidsoap`.

On Windows, the default cache directory for both types of cache locations is in the same directory as the binary.

`liquidsoap --build-config` displays both locations. At runtime, `liquidsoap.cache(mode=<mode>)` returns the cache directory. `mode` should be one of: `"user"` or `"system"`.

### Cache maintenance

There is a cache maintenance routine which runs after each new cache file is written. It deletes cache files that have not been used for `10` days and keeps at most `20` files in each cache directory, removing the least recently used first.

You can run the cache maintenance routine by calling `liquidsoap.cache.maintenance(mode=<mode>)` manually. Here, too, `mode` should be one of: `"user"` or `"system"`.

### Cache security

Please be aware that the cache does _not_ encrypt its values. As such, user cache files should be considered sensitive as they may contain passwords and other runtime secrets
that are available through your scripts. We recommend that you:

- Use environment variables as much as possible when passing secrets
- Secure your user script and cache files.

The default creation permissions for user cache files are: `0o600` so only the user creating them should be able to read them. You should make sure that your script permissions are also similarly restricted.

### Cache and memory usage

One side-benefit from loading a script from cache is that the entire typechecking process is skipped.

This leads to a significant reduction in initial memory consumption, typically down from about `375MB` to about `80MB`!

Additionally, the OCaml memory compaction algorithm is executed after typechecking your script but before running it.
This results in additional memory usage reduction with a slight delay in initial startup time.

To minimize your script startup time you should:

- Cache it before running it to skip the initial typechecking
- Set `settings.init.compact_before_start` to `false` to skip the initial memory compaction:

```liquidsoap
settings.init.compact_before_start := false
```

### Cache environment variables

The following environment variables control the cache behavior:

- `LIQ_CACHE`: disable the cache when set to anything other than `1` or `true`
- `LIQ_CACHE_SYSTEM_DIR`: set the cache system directory
- `LIQ_CACHE_SYSTEM_DIR_PERMS`: set the permission used when creating cache system directory (and its parents when needed). Default: `0o755`
- `LIQ_CACHE_SYSTEM_FILE_PERMS`: set the permissions used when creating a system cache file. Default: `0o644`
- `LIQ_CACHE_USER_DIR`: set the cache user directory
- `LIQ_CACHE_USER_DIR_PERMS`: set the permission used when creating cache user directory (and its parents when needed). Default: `0o700`.
- `LIQ_CACHE_USER_FILE_PERMS`: set the permissions used when creating a user cache file. Default: `0o600`
- `LIQ_CACHE_MAX_DAYS`: set the maximum days a cache file can be stored before it is eligible to be deleted during the next cache maintenance pass. Default: `10`.
- `LIQ_CACHE_MAX_FILES`: set the maximum number of files in each cache directory. The least recently used files are removed first. Default: `20`.
- `LIQ_DEBUG_CACHE`: when set, errors while loading or storing a cache file are logged with a backtrace.

## Phases of execution

There are various stages of running liquidsoap:

- **Parsing**: read scripts and scripting expressions, can fail with syntax errors.
- **Static analysis**: infer the type of all expressions, may fail with type
  errors. This phase is skipped when the script is loaded from the
  [cache](#caching).
- **Evaluation**: the script runs from top to bottom and sources get created.
  Each source joins the [clock](./clocks.md) of the sources it reads from, so
  sources connected to each other share a clock. Operators such as `buffer` or
  `crossfade` put their input in a separate clock. Outputs check that their
  source is [infallible](./sources.md#fallible-sources). Each of these steps may
  raise an error.
- **Startup**: once all scripts are evaluated, every clock that has at least one
  output starts. On its first tick, a clock activates its sources: each output
  wakes up the sources it reads from, and remaining unknown
  [stream types](./stream_content.md) are fixed according to the
  `settings.frame.audio.channels` and related settings. Streaming has started.

When a clock starts, you will see a log line like:

```
[clock:3] Starting top-level clock output.file with sources: output.file (output), amplify (passive), input.alsa (active) and sync: auto
```

Usually, liquidsoap is run by passing one or several scripts and expressions to execute. Those expressions set up some sources, and outputs typically don't change anymore. If those initially provided active sources fail to be initialized (invalid parameter, failure to connect, etc.) liquidsoap will terminate with an error.

When the scripts define no output, Liquidsoap prints
`No output defined, nothing to do.` and exits. Pass `--force-start` or set
`settings.init.force_start := true` if your script creates its outputs later,
for instance from a server command.

It is however possible to **dynamically** create active sources,
through registered server commands, event handlers, etc.
They will be initialized and run as statically created ones:
a new clock with an output starts when the callback that created it returns,
and a source added to a running clock is activated on the next tick of that
clock.
In **interactive** mode (passing the `--interactive` option)
it is also possible to input expressions in a liquidsoap prompt,
and their execution can trigger the creation of new outputs.

Outputs can be deactivated using their `shutdown()` method:
they will stop streaming and will be destroyed. A clock stops when it has no
output and no active source left, and logs
`Clock has stopped: no more sources to process.`

## The streaming loop

Once the clocks are started, each clock runs the streaming loop. Each iteration
of the loop is a _tick_, and each tick produces one _frame_: a small chunk of
media, `settings.frame.duration` long. The default frame duration is `0.02`
seconds, so a clock ticks 50 times per second.

During a tick, the clock:

1. Activates sources created since the last tick.
2. Asks each output and each active source to run for one frame. An output asks
   its source for a frame, that source asks its own sources, and so on down to
   the elementary sources that read files, decode streams or capture audio.
3. Runs the callbacks registered for this tick.

A source computes its frame at most once per tick. When two outputs read from
the same source, the second output gets the frame that the source already
computed for the first one.

Sources are either passive or active:

- A **passive** source computes data only when an output or another source asks
  for it. Most sources are passive, for instance `playlist` or `amplify`.
- An **active** source runs at every tick, even when no output uses its data.
  `input.harbor` and `input.alsa` receive data continuously and must consume it
  at every tick to keep their buffers from overflowing.

See [sources](./sources.md) for more on how sources produce data, and
[source callbacks](./callbacks.md) for the functions you can run when a source
starts a track, receives metadata or stops.

### Keeping time

The clock keeps track of how much media it has produced: after `n` ticks, it
has produced `n` times the frame duration. The clock compares that amount with
the time that has passed since it started:

- When the clock is ahead by at least `settings.clock.latency` (`0.1` seconds by
  default), it rests until real time catches up.
- When the clock is behind, it produces the next frames without resting, to
  catch up.

The time that the clock uses depends on its synchronization source:

- **CPU-driven**: when no source controls the timing, for instance with
  `playlist`, `single` or `sine`, the clock follows the computer's own clock.
- **Self-sync**: some sources control the pace of the data by their own means.
  `input.alsa` and other sound card operators wait for the hardware, and
  `input.srt` uses the timestamps in its packets. When such a source is active,
  the clock follows that source and logs
  `Switching to self-sync mode (...)`. When the source goes away, the clock logs
  `Switching to non-self-sync mode` and goes back to the CPU.

At most one self-sync source can produce data in a clock at a time. See
[clocks](./clocks.md) for the conflicts this causes and how to fix them with
`buffer`.

### When a tick takes too long

A frame of `0.02` seconds must be computed in less than `0.02` seconds on
average. When the computation takes longer, because of CPU overload, slow disk
or network access, or a callback that blocks, the clock falls behind and
catches up:

- When the clock is behind by more than `settings.clock.log_delay_threshold`
  (`0.2` seconds by default), it logs a warning, at most once every
  `settings.clock.log_delay` seconds (`1` by default):

  ```
  [clock.pulseaudio:2] Latency is too high: we must catchup 0.86 seconds! ...
  ```

- When the clock is behind by more than `settings.clock.max_latency` (`60`
  seconds by default), it gives up catching up. It logs
  `Too much latency! Resetting active sources...` and resets its outputs and
  active sources, which reconnects Icecast outputs for instance.

See [performance and monitoring](./performance.md#understanding-latency) to tune these settings and find
out what slows your script down.

## Shutdown

Liquidsoap shuts down when:

- your script calls `shutdown()`, optionally with an exit code, for instance
  `shutdown(code=1)`,
- your script calls `restart()`,
- the process receives `SIGINT` (Ctrl-C) or `SIGTERM`.

Liquidsoap then goes through these steps, and the log shows `Shutdown started!`:

1. Runs the functions registered with `on_shutdown`.
2. Stops each clock at the end of its current tick. The outputs stop, which
   closes their files and network connections. Liquidsoap waits up to
   `settings.clock.max_latency` seconds for the clocks to stop.
3. Runs the functions registered with `on_cleanup`, and removes the files it
   downloaded for remote requests.
4. Exits with the given exit code. After `restart()`, Liquidsoap starts again
   with the same command line.

When a shutdown hangs, send the signal three times: the third signal exits
immediately with code `128`. The `exit` function also stops
Liquidsoap immediately, skipping these steps. Use it only when `shutdown` is not
an option.
