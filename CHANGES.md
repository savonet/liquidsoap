# 2.4.3 (unreleased)

## Changed:

- DEPRECATED telnet/server `request.on_air` command is enabled when
  `settings.request.deprecated_on_air_metadata` is `true`.
- Add back missing `"status"` telnet/server command for outputs.
- Reduced default buffer size throughout the app to improve memory efficiency
  (#4919)
- Refactored XML parsing to be type-driven, support optional fields with
  custom element name aliases (#4926, #4927)

## Fixed:

- Fixed `??` operator type inference in conditional branches (#4922)
- Prevent double nullable types (#4925)
- Make active `stereotool` really be active.. (#4882)
- Fixed `fMP4` HLS support for audio+video streams (#4841)
- Fixed crossfade clocks inconsistencies leading to weird caching audio glitches (#4851)
- Fixed first frame of new track getting lost when crossfading with `fade.in` duration is set to `0.` (#4887)
- Fixed crash with concurrent `stop`/`start` operations (#4849)
- Fixed metadata leak from underlying ffmpeg `ogg` demuxer in `ffmpeg` stream
  decoder (#4848)
- Fixed performance regression when initializing `playlist` (#4913)
- Make sure that we read all stdout/stderr before considering a process closed. (#4918)
- Make sure `output.file` does not create files without data (#4899)

---

# 2.4.2 (2025-01-17)

## New:

- Added pretty-print of clocks and source graphs!
  Use via `--describe-<clocks/sources>` in the command
  line or `clocks.dump` and `clocks.dump_sources` on
  telnet/server! (#4837, #4836)

## Changed:

- Batch calls to `thread.when` to preserve performances,
  warn when using `thread.when` too intensively (#4832)
- Added pretty-print of telnet `help` list of commands.
  This might break compatibility with automated telnet
  readers! (Remember: telnet is a user-facing API, not designed
  for machine consumption!).

## Fixed:

- Fixed sources leaks in `source.dynamic` (#4835)
- Optimized `Queues` implementation: fast, atomic access,
  slow, mutex-protected mutation (#4831)

---

# 2.4.1 (2025-01-10)

## New:

- Added support for explicit metadata to pass to the output
  when creating a `%ffmpeg` encoder (#4667)
- Added start/stop telnet commands for outputs.
- Added `settings.syslog.level` to set syslog level (#4686)
- Added `active` parameter to `stereotool` and `output.stereotool` (#4749)
- Added `file.mime.extension` to get the file extension's associated
  with a MIME/content-type as registered in `settings.http.mime.extnames`

## Changed:

- Do not normalize sources in `mix` by default.
- Added `json.value` to make it possible to mix different value
  types when returning json values (#4712)
- Allow referring to pulseaudio devices by name (#4732)
- `output.shoutcast`: make `dj` argument a getter.
- Exclude metadata listed in `settings.encoder.metadata.cover`
  from automatic metadata recoding. (#4690)

## Fixed:

- Fix `input.harbor` initial metadata when switching with
  fades (#4736)
- Fix audio artifact in crossfade transitions (#4739)
- Make sure that sources re-used in `source.dynamic` are never
  inadvertently cleaned up (#4713)
- Fix sources not being properly collected (#4670)
- Raise a proper error on parse error raised during `%include`
  parsing.
- Don't print metadata coverart string (#4730)
- Make sure `fetch` always fetches a new request in `request.dynamic` (#4745)
- Fixed start/stop logic in `output.harbor` (#4666)
- Fixed `fade.in`/`fade.out` logic w.r.t. override metadata (#4759)
- Fixed `video.cover`

---

# 2.4.0 (2025-09-01)

## New:

### Language:

- Allow destructing function arguments using the same patterns as for
  variable assignment (#4562)
- Enhanced labeled arguments syntax (#4526)
- Add warning when erasing top-level variables (#4518)
- `null` can now be used directly without having to call `null()`.
  `null(value)` calls are still valid and can be used to create
  non-null values with nullable types. Calls to `null()` are marked
  as deprecated (#4516)
- Added `liquidsoap.script.path` that contains the path to the current
  script's file, if available.

### Core:

- BREAKING: Most callbacks have been moved to source methods and are now
  executed asynchronously by default. See migration notes for all the
  details.
- BREAKING: Added new file-to-file external `decoder.add` API. This
  makes it much easier and safer to write external decoders (#4531)
- Deprecated `insert_metadata`, added default `insert_metadata` method on
  every source (#4541)
- Added client certificate support to `http.transport.tls` (#4589, @DelilahHoare)

### Utilities:

- Added `cron.parse`, `cron.{add,remove}` and a cron thread
  to allow registration of cron-like asynchronous tasks (#4579)
- Added LUFS-based per-track loudness correction (#4545)

## Changed:

### Language:

- BREAKING: Error methods have been removed by default.
  Use `error.methods` to get them! (#4537)
- Make sure that `let { foo = gni } = v` assigns a value to
  `gni` but not to `foo` (#4561)

### Core:

- Turned initial memory compaction on by default. This
  has show to greatly reduce initial memory consumption.
- Improved source and clock naming (#4497)
- Support for `ImageLib` has been removed. The library is not
  maintained anymore and causes issues with dangling external
  processes (#4595)

### Utilities:

- Deprecated `replaygain` operator, introduced unified
  `normalize_track_gain` which works with both ReplayGain
  and LUFS (#4545)

## Fixed:

- Fixed issues with autocue `start_next`. This brings autocue's behavior
  inline with its expectation but can change existing script's output (#4605)
- Fix error when loading script path having non-ascii characters in them
  (#4343)
- Fix concurrent inline encoders (#4638)
- Fix error with interactive variables (#4592, @myeungdev)
- Prevent concurrent reload and request fetch in playlists (#4316)
- Don't mark source as ready until their clock has started. (#4496)
- Fixed mutex deadlock caused by aggressive inlining (#4540)
- Fixed segfault when using SRT on windows (#4538)
- Fixed memleak in ffmpeg inline encoder (#4501)
- Fixed `tmp:` protocol logic (#4544)
- Fixed dangling file descriptors with `file.*.stream` (#4639, reported by @limebar)

---

# 2.3.3 (2025-05-16)

New:

- `input.srt`: add `ipv6only` to allow to bind only to ipv6 addresses.
  Set it to `true` when `bind_address` is ipv6.
- Allow HLS segment names to contain sub-directories.
- Implicitly convert ffmpeg raw audio data, making it much more practical
  to write scripts using the raw ffmpeg format (#4478)

Changed:

- Made `defer` more user-friendly by operating directly on generic `source(audio=pcm('a))`
  sources. Renamed old `defer` to `defer.pcm_s16`
- `dtools`, `duppy` and `xmlplaylist` have been moved into the liquidsoap code
  base and will no longer be developed or required as stand-alone packages (#12582)
- Changed default value of `metadata.map` `strip` argument to `true` and `insert_missing`
  to `false` Added `settings.metadata.map.strip` and `settings.metadata.map.insert_missing`
  configuration keys to revert to previous defaults. (#4447)

Fixed:

- Do not send empty metadata to shoutcast servers (#4408)
- Automatically close file descriptor opened via scripted values with a log message
  warning of file descriptor leaks (#4481)
- Fixed segfault in new `lufs` C code introduced with release `2.3.2` (#4490)

---

# 2.3.2 (2025-04-01) 🃏

New:

- Added support for multiple metadata fields in
  ogg and flac metadata
- Added support for track-level REM ALBUM in cue file parsing
  (#4381)

Changed:

- Added `"pic"` to list of excluded metadata for automatic charset conversion.
- Added `settings.charset.max_string_length` setting to prevent automatic charset
  conversions of strings over that length.

Fixed:

- Optimized CPU usage (#4369, #4370)
- Fixed empty initial HLS segment (#4401)
- Fixed support for `duration` metadata in image decoder (#4397)
- Fixed cue-out bug in cue file parsing (#4381)
- Bring back parse error location. (#4362)
- Fixed SRT encoding when restarting a stream with reverse data flow
  (#4399)
- Make sure that audioscrobbler `on_track`/`on_end` operations are
  sent to a asynchronous task queue.
- Fixed resources accumulation leading to catchup when using `crossfade`
  (#4419, #4410)
- Fixed source reselection logic issue that was causing crashes when using
  `switch` and `fallback` operators (#4420)
- Fixed self-sync logic with pulse audio outputs (#4429)
- Fixed script caching on windows.

---

# 2.3.1 (2025-02-05)

New:

- Added support for address resolution preference in SRT (#4317)
- Added global address resolution settings for SRT and Icecast (#4317)
- Added support for parsing and rendering XML natively (#4252)
- Added support for `WAVE_FORMAT_EXTENSIBLE` to the internal
  wav dexcoder.
- Added optional `buffer_size` parameter to `input.alsa` and
  `output.alsa` (#4243)
- Reimplemented audioscrobbler support natively using the more
  recent protocol (#4250)
- Added boolean getter to disable/enable normalization
  (#4308)

Changed:

- Make alsa I/O work with buffer size different than
  liquidsoap internal frame (#4236)
- Reimplemented CUE file parser in native liquidsoap script,
  added support for multiple files and EAC non-compliant extension
  (#1373, #4330)
- Make `"song"` metadata mapping to `"title"` metadata in
  `input.harbor` disabled when either `"artist"` or `"title"`
  is also passed. Add a configuration key to disable this mechanism.
  (#4235, #2676)
- `output.icecast` now re-sends the last metadata when connecting to the
  remote server unless explicitly disabled using the `send_last_metadata_on_connect`
  option (#3906)
- Add full explicit support for `ipv4` vs. `ipv6` resolution in SRT inputs and outputs,
  add global `settings.srt.prefer_address` and `settings.icecast.prefer_address` (#4317)
- Added generic SRT socket get/set API. Added new socket options, including `latency`
  and `ipv6only`.

Fixed:

- Fixed request resolution loop when enabling both `autocue`
  and `replaygain` metadata resolvers (#4245, fixed in #4246)
- Fixed `flac` encoding segfault (#4286, #4274)
- Fixed source `last_metadata` not being properly updated (#4262)
- Convert all ICY (icecast) metadata from `input.http` to `utf8`.
- Fixed `inotify` unwatching due to GC cleanup (#4275)
- Fixed `delay` initial conditions (#4281)

---

# 2.3.0 (2024-11-27)

New:

- Rewrote the streaming API to work with immutable frame content. This
  should greatly impact impredictable side-effect of the previous models w.r.t.
  track marks, content sharing and more. This also impacts multiple operators
  behavior. Mostly, things should be roughly the same with differences around
  behaviors related to track marks (`source.on_track` and etc). (#3577)
- Added script caching layer for faster script startup time. See: https://www.liquidsoap.info/blog/2024-06-13-a-faster-liquidsoap/ for details (#3924, #3949, #3959 and #3977)
- Rewrote the clock/streaming loop layer. This prepares our streaming system to
  support multicore when the OCaml compiler is mature enough to allow it. Clocks
  are now attached to sources via their `clock` methods. Returned value is a stripped
  down `clock` variable. Users can use the `clock` function to retrieve the full
  methods, e.g. `s = sine(); c = clock(s.clock)`. This value has advanced functions
  for clock control such as `start`/`stop`, `ticks` and `self_sync` to check for
  `self-sync`. (#3781)
- Allow frames duration shorter than one video frames, typically values under `0.04s`.
  Smaller frames means less latency and memory consumption at the expense of
  a higher CPU usage (#3607)
- Change default frame duration to `0.02s` (#4033)
- Optimized runtime (#3927, #3928, #3919)
- Added NDI output support (#4181)
- Added `finally` to execute code regardless of whether or not an exception is raised
  (see: #3895 for more details).
- Added support for Spinitron submission API (#4158)
- Removed gstreamer support. Gstreamer's architecture was never a good fit for us
  and created a huge maintenance and debugging burden and it had been marked as
  deprecated for a while. Most, if not all of its features should be available using
  `ffmpeg`. (#4036)
- Removed `taglib` support. It is superseded by the internal `ocaml-metadata` module
  and taglib, with its dependency on the C++ runtime library, has been causing issues
  with binary builds portability and crashes with the (not yet supported) OCaml 5
  compiler. (#4087)
- Added `video.canvas` to make it possible to position video elements independently
  of the rendered video size ([#3656](https://github.com/savonet/liquidsoap/pull/3656), [blog post](https://www.liquidsoap.info/blog/2024-02-10-video-canvas-and-ai/))
- Added cover manager from an original code by @vitoyucepi (#3651)
- Added non-interleaved API to `%ffmpeg` encoder, enabled by default when only
  one stream is encoded.
- Allow trailing commas in record definition (#3300).
- Added `metadata.getter.source.float` (#3356).
- BREAKING: Added `duration` and `ticks` to metadata available when computing HLS segment names (#4135)
- Added optional `main_playlist_writer` to `output.file.hls` and
  derivated operator (#3484)
- Added `is_nan`, `is_infinite`, `ceil`, `floor`, `sign` and `round` (#3407)
- Added `%track.drop` to the `%ffmpeg` encoder to allow partial encoding
  of a source's available tracks (#3480)
- Added `let { foo? } = ...` pattern matching (#3481)
- Added `metadata.replaygain` method to extract unified replay gain value from metadata (#3438).
- Added `metadata.parse.amplify` to manually parse amplify override metadata.
- Added `compute` parameter to `file.replaygain` to control gain calculation (#3438).
- Added `compute` parameter to `enable_replaygain_metadata` to control replay gain calculation (#3438).
- Added `copy:` protocol (#3506)
- Added `file.touch`.
- Added support for sqlite databases (#3575).
- Added `string.of_int` and `string.spaces`.
- Added `list.assoc.nullable`.
- Added `source.cue` (#3620).
- Added `string.chars` (#4111)
- Added atomic file write operations.
- Added new `macos_say` speech synthesis protocol. Make it the default implementation for the `say:`
  protocol on `macos`.
- Added `settings.request.timeout` to set the request timeout globally.

Changed:

- Reimplemented `request.once`, `single` and more using `source.dynamic`. Removed experiment
  flag on `source.dynamic`. The operator is considered stable enough to define advanced sources
  but the user should be careful when using it.
- Mute SDL startup messages (#2913).
- `int` can optionally raises an error when passing `nan` or `infinity`, `int(infinity)`
  now returns `max_int` and `int(-infinity)` returns `min_int`. (#3407)
- Made default font a setting (#3507)
- Changed internal metadata format to be immutable (#3297).
- Removed `source.dump` and `source.drop` in favor of safer `request.dump` and `request.drop`.
  `source.{dump, drop}` can still be implemented manually when needed and with the proper
  knowledge of what's going on.
- Allow a getter for the offset of `on_offset` and dropped the metadata
  mechanism for updating it (#3355).
- `string.length` and `string.sub` now default to `utf8` encoding (#4109)
- Disable output paging when `TERM` environment variable is not set.
- Allow running as `root` user inside `docker` container by default (#3406).
- Run `check_next` before playlist's requests resolutions (#3625)
- Set `force` to `true` by default in `file.copy` to make operator behave
  as expected.
- BREAKING: Float comparison now follows the expected specs, in particular: `nan == x` is always `false` and
  `nan != x` is always `true`. Use `float.is_nan` to test if a float is `nan`.
- BREAKING: `replaygain` no longer takes `ebu_r128` parameter (#3438).
- BREAKING: assume `replaygain_track_gain` always stores volume in _dB_ (#3438).
- BREAKING: protocols can now check for nested static uri. Typically, this means
  that requests for an uri of the form: `annotate:key="value",...:/path/to/file.mp3`
  is now considered infallible if `/path/to/file.mp3` can be decoded.
- Added `parents` option of `file.mkdir` (#3600, #3601).
- Added `forced_major_collections` record field to the result of `runtime.gc.stat()` and
  `runtime.gc.quick_stat()` (#3783).
- Changed the port for the built-in Prometheus exporter to `9599` (#3801).
- Set `segments_overheader` in HLS outputs to disable segments cleanup altogether.
- Added support for caching LV2 and LADSPA plugins (#3959).
- Pulseaudio input and output now restart on pulseaudio errors (#4174).

Fixed:

- Fixed type generalization on values returned from function applications. Most notably,
  this should help with HTTP endpoint registration (#3303, fixed in #4030)

---

# 2.2.5 (2024-05-01) (Mayday!)

New:

- Added `enable_autocue_metadata` and `autocue:` protocol to automatically compute cue points and crossfade parameters (#3753, #3811, @RM-FM and @Moonbase59)
- Added various ffmpeg timestamps when exporting ffmpeg metadata from filters.
- Added `db_levels` method to `blank.*` sources (#3790)
- Added `excluded_metadata_resolvers` to `request.create` to make it possible to selectively disable specific metadata resolvers when resolving requests.
- Normalized expected API from `autocue`, allow multiple implementation and adapt `cross`/`crossfade` to work with it out of the box with workaround for short tracks.
- Added private and swapped memory reporting when compiled with `mem_usage`.
- Added priorities to metadata deocoders, allows finer-control of metadata overriding. (#3887)

Changed:

- Allow to disable `http.*` url normalization. Add warning when url normalization changes the url (#3789)
- Add `namespace` and optional source IDs to `mix`.

Fixed:

- Prevent request metadata from overriding root metadata (#3813)
- Fixed `source.drop` and `source.dump` clock initialization.
- Fixed bogus report of non-monotonous PTS content when using raw ffmpeg content.
- Fixed streaming errors when disconnecting `input.harbor`.
- Fixed issues with rendered id3v2 frame that contain binary data (#3817)
- Fixed memory leaks with SRT listen socket polling callbacks.
- Fixed `%ffmpeg` copy muxer logic with some audio/video streams (#3840)
- Fixed `duration` metadata calculation in the presence of `cue_in`/`cue_out` metadata.

---

# 2.2.4 (2024-02-04)

New:

- Added support for `id3v2` metadata in `output.*.hls` when using `%mp3`, `%shine` or `%fdkaac` encoders (#3604)
- Added option to set preferred address class (`ipv4`, `ipv6` or system default) when resolving hostnames in http transports and `output.icecast`
- Added `self_sync` option to `input.srt` to accommodate for streams in file mode (#3684)
- Added `curve` parameter to fade functions and `liq_fade_{in,skip,out}_curve` metadata override (#3691)
- Added `delay` parameter to fade functions to make it possible to add delay before fade happens. Add `liq_fade_{in,skip,out}_delay` metadata override.
- Added `single_track` option to allow `sequence` to play each source until they are unavailable while keeping track marks.

Changed:

- `cue_cut` operator has been removed. Cueing mechanisms have been moved to underlying file-based sources. See migration notes for more details.

Fixed:

- Fix pop/click at the end of fade out/in (#3318)
- Fix audio/video synchronization issues when decoding live streams using ffmpeg.
- Fix issues with TLS connecting clients not being properly timed out (#3598)
- Make sure reconnection errors are router through the regulat `on_error` callback in `output.icecast` (#3635)
- Fixed discontinuity count after a restart in HLS outputs.
- Fixed file header logic when reopening in `output.file` (#3675)
- Fixed memory leaks when using dynamically created sources (`input.harbor`, `input.ffmpeg`, SRT sources and `request.dynamic`)
- Fixed invalid array fill in `add` (#3678)
- Fixed deadlock when connecting to a non-SSL icecast using the TLS transport (#3681)
- Fixed crash when closing external process (#3685)

---

# 2.2.2 (2023-11-02)

New:

- Added `string.escape.html` (#3418, @ghostnumber7)
- Add support for getters in arguments of `blank.detect` (#3452).
- Allow float in source content type annotation so that it possible
  to write: `source(audio=pcm(5.1))`

Changed:

- Trim urls in `input.ffmpeg` by default. Disable using
  `trim_url=false` (#3424)
- Automatically add HLS-specific ffmpeg parameters to
  `%ffmpeg` encoder (#3483)
- BREAKING: default `on_fail` removed on `playlist` (#3479)

Fixed:

- Allow `channel_layout` argument in ffmpeg encoder to set the
  number of channels.
- Improved support for unitary minus, fix runtime call of optional
  methods (#3498)
- Fixed `map.metadata` mutating existing metadata.
- Fixed reloading loop in playlists with invalid files (#3479)
- Fixed main HLS playlist codecs when using `mpegts` (#3483)
- Fixed pop/clicks in crossfade and source with caching (#3318)
- Fixed pop/clicks when resampling using `libsamplerate` (#3429)
- Fixed gstreamer compilation. Remember that gstreamer features are
  DEPRECATED! (#3459)
- Fixed html character escaping in `interactive.harbor` (#3418, @ghostnumber7)
- Fixed icecast not reconnecting after erroring out while closing connection
  in some circumstances (#3427)
- Fixed parse-only mode (#3423)
- Fixed ffmpeg decoding failing on files with unknown codecs.
- Fixed a crash due to `wait_until` timestamp being in the past when
  using `posix-time2`
- Make sure that temporary files are always cleaned up in HLS outputs (#3493)

---

# 2.2.1 (2023-09-05)

Changed:

- BREAKING: on HLS outputs, `on_file_change` events are
  now `"created"`, `"updated"` and `"deleted"`, to better
  reflect the new atomic file operations (#3284)
- Added `compact` argument to the `http.response.json` function. `http.response.json` will produce minified JSON by
  default. Added a newline symbol to the end of the JSON data produced by `http.response.json`. (#3299)
- Bumped internal ogg decoder to make sure that it is used over the ffmpeg decoder whenever possible.
  FFmpeg has issues with metadata in chained streams which needs to be fixed upstream. Unfortunately,
  `input.http` can only use the ffmpeg decoder at the moment.
- Cleanup `output.file` encoding and file handling logic (#3328)
- Added `ratio` to `source.{dump,drop}` to make it possible to control its CPU peaks.
- Enhanced clock error reporting (#3317)

Fixed:

- Fixed slow memory leak in muxer operator (#3372, #3181, #3334)
- Fixed discontinuity logic error in HLS outputs after a restart.
- Fixed HTTP response status in `output.harbor` (#3255)
- Make sure main HLS playlist is regenerated after being
  unlinked (#3275)
- Fixed hard crash on icecast disconnection errors.
- Fix `output.harbor` encoder header when encoding with
  `%ogg`, `%vorbis` and etc. (#3276)
- Fixed quality argument parsing in ffmpeg encoders (#3267)
- Make all HLS file write atomic (#3284)
- Allow seek and cue operators to work with muxed sources
  using a single underlying source (#3252)
- Fixed export of cover art metadata (#3279)
- Remove use of `stereo:` protocol in `say:` protocol:
  this is now handled automatically by the decoder and generates latency via
  high CPU usage peak.
- Fixed `output.file` reopening with flac encoding (#3328)

---

# 2.2.0 (2023-07-21)

New:

- Added support for less memory hungry audio formats, namely
  `pcm_s16` and `pcm_f32` (#3008)
- Added support for native osc library (#2426, #2480).
- SRT: added support for passphrase, pbkeylen, streamid,
  added native type for srt sockets with methods, moved stats
  to socket methods, added `socket()` method on srt input/outputs
  (#2556)
- HLS: Added support for ID3 in-stream metadata (#3154) and
  custom tags (#2898).
- Added support for FLAC metadata (#2952)
- Added support for YAML parsing and rendering (#2855)
- Added support for the proprietary shared stereotool library (#2953)
- Added TLS support via `ocaml-tls` (#3074)
- Added `video.align`.
- Added `string.index`.
- Added support for ffmpeg decoder parameters to allow decoding of
  raw PCM stream and file (#3066)
- Added support for unit interactive variables: those call a handler when their
  value is set.
- Added support for id3v2 `v2.2.0` frames and pictures.
- Added `track.audio.defer` to be used to buffer large amount of audio data (#3136)
- Added `runtime.locale.force` to force the system's locale (#3231)
- Added support for customizable, optimized `jemalloc` memory allocator (#3170)
- Added `source.drop` to animate a source as fast as possible..
- Added in house replaygain computation:
  - `source.replaygain.compute` to compute replaygain of a source
  - `file.replaygain` to compute the replaygain of a file
- Added support for ImageLib to decode images.
- Added support for completion in emacs based on company (#2652).
- Added syntactic sugar for record spread: `let {foo, gni, ..y} = x`
  and `y = { foo = 123, gni = "aabb", ...x}` (#2737)
- Added `file.{copy, move}` (#2771)
- Detect functions defining multiple arguments with the same label (#2823).
- Added `null.map`.
- References of type `'a` are now objects of type `(()->'a).{set : ('a) -> unit}`. This means that you should use `x()` instead of `!x` in order to get
  the value of a reference. Setting a reference can be done both by `x.set(v)`
  and `x := v`, which is still supported as a notation (#2881).
- Added `ref.make` and `ref.map`.
- Added `video.board`, `video.graph`, `video.info` (#2886).
- Added the `pico2wave` protocol in order to perform speech synthesis using
  [Pico TTS](https://github.com/naggety/picotts) (#2934).
- Added `settings.protocol.gtts.lang` to be able to select `gtts`' language,
  added `settings.protocol.gtts.options` to be able to add any other option (#3182)
- Added `settings.protocol.pico2wave.lang` to be able to select `pico2wav` language (#3182)
- Added `"metadata_url"` to the default list of exported metadata (#2946)
- Added log colors!
- Added `list.filter_map` and `list.flatten`.
- Added `medialib` in order to store metadata of files in a folder and query
  them (#3115).
- Added `--unsafe` option (#3113). This makes the startup much faster but
  disables some guarantees (and might even make the script crash...).
- Added `string.split.first` (#3146).
- Added `string.getter.single` (#3125).

Changed:

- Switched to `dune` for building the binary and libraries.
- Changed `cry` to be a required dependency.
- Changed default character encoding in `output.harbor`, `output.icecast`
  `output.shoutcast` to `UTF-8` (#2704)
- BREAKING: all `timeout` settings and parameters are now `float` values
  and in seconds (#2809)
- BREAKING: in `output.{shoutcast,icecast}`:
  - Old `icy_metadata` renamed to `send_icy_metadata` and changed to a nullable `bool`. `null` means guess.
  - New `icy_metadata` now returns a list of metadata to send with ICY updates.
  - Added `icy_song` argument to generate default `"song"` metadata for ICY updates. Defaults
    to `<artist> - <title>` when available, otherwise `artist` or `title` if available, otherwise
    `null`, meaning don't add the metadata.
  - Cleanup, removed parameters that were irrelevant to each operator, i.e. `icy_id` in `output.icecast` and etc.
  - Make `mount` mandatory and `name` nullable. Use `mount` as `name` when `name` is `null`.
- `reopen_on_error` and `reopen_on_metadata` in `output.file` and related operators are now callbacks to
  allow dynamic handling.
- Added `reopen` method to `output.file`.
- Added support for a Javascript build an interpreter.
- Removed support for `%define` variables, superseded by support for actual
  variables in encoders.
- Cancel pending append when skipping current track on `append` source.
- Errors now report proper stack trace via their `trace` method, making it
  possible to programmatically point to file, line and character offsets
  of each step in the error call trace (#2712)
- Reimplemented `harbor` http handler API to be more flexible. Added a new
  node/express-like registration and middleware API (#2599).
- Switched default persistence for cross and fade-related overrides
  to follow documented behavior. By default, `"liq_fade_out"`, `"liq_fade_skip"`,
  `"liq_fade_in"`, `"liq_cross_duration"` and `"liq_fade_type"` now all reset on
  new tracks. Use `persist_overrides` to revert to previous behavior
  (`persist_override` for `cross`/`crossfade`) (#2488).
- Allow running as root by default when docker container can be detected using
  the presence of a `/.dockerenv` file.
- `id3v2` argument of `%mp3` encoder changed to `"none"` or version number to allow
  to choose the metadata version. `true` is still accepted and defaults to version
  `3`. Switched to our internal implementation so that it does not require `taglib`
  anymore.
- Moved HLS outputs stream info as optional methods on their respective encoder.
- Changed `self_sync` in `input.ffmpeg` to be a boolean getter, changed `self_sync`
  in `input.http` to be a nullable boolean getter. Set `self_sync` to `true` in
  `input.http` when an icecast or shoutcast server can be detected.
- Add `sorted` option to `file.ls`.
- Add `buffer_length` method to `input.external.rawaudio` and
  `input.external.wav` (#2612).
- Added full `OCaml` backtrace as `trace` to runtime errors returned from OCaml code.
- Removed confusing `let json.stringify` in favor of `json.stringify()`.
- Font, font size and colors are now getters for text operators (`video.text`,
  `video.add_text`, etc.) (#2623).
- Add `on_cycle` option to `video.add_text` to register a handler when cycling
  (#2621).
- Renamed `{get,set}env` into `environment.{get,set}`
- Renamed `add_decoder`, `add_oblivious_decoder` and `add_metadata_resolver`
  into, respectively, `decoder.add`, `decoder.oblivious.add`, `decoder.metadata.add`
- Deprecated `get_mime`, added `file.mime.libmagic` and `file.mime.cli`, made
  `file.mime` try `file.mime.libmagic` if present and `file.mime.cli` otherwise,
  changed returned value when no mime was found to `null`.
- Return a nullable float in `request.duration`.
- Removed `--list-plugins-json` and `--list-plugins-xml` options.
- Added `--list-functions-json` option.
- Removed built-in use of `strftime` conversions in output filenames, replaced
  by an explicit call to `time.string` (#2593)
- Added nullable default to `{int,float,bool}_of_string` conversion functions, raise
  an exception if conversion fails and no default is given.
- Deprecated `string_of` in favor of `string` (#2700).
- Deprecated `string_of_float` in favor of `string.float` (#2700).
- Added `settings.protocol.youtube_dl.timeout` to specify timeout when using
  `youtube-dl` protocol (#2827). Use `yt-dlp` as default binary for the
  protocol.
- The `sleeper` operator is now scripted (#2899).
- Reworked remote request file extension resolution (#2947)
- REMOVED `osx-secure-transport`. Doubt it was ever used, API deprecated
  upstream (#3067)
- Renamed `rectangle` to `add_rectangle`, and similarly for `line`.

Fixed:

- The randomization function `list.shuffle` used in `playlist` was incorrect and
  could lead to incorrectly randomized playlists (#2507, #2500).
- Fixed srt output in listener mode to allow more than one listener at a time and
  prevent listening socket from being re-created on listener disconnection (#2556)
- Fixed race condition when switching `input.ffmpeg`-based urls (#2956)
- Fixed deadlock in `%external` encoder (#3029)
- Fixed crash in encoders due to concurrent access (#3064)
- Fixed long-term connection issues with SSL (#3067)

---

# 2.1.4 (2022-03-01)

New:

- Added `buffer_length` method to `buffer` operator.
- Always display error backtrace when a fatal exception
  is raised in the streaming loop.
- Added `umask()` to get the current `umask` and `umask.set(...)`
  to set the current `umask` (#2840)

Changed:

- Add break when restarting the external process in `input.external.{rawaudio,rawvideo}`
  (#2860, #2872)
- Removed `disconnect` method on `input.harbor`. This method was doing the same as
  the `stop` method. Added `shutdown` method to properly shutdown the
  source even when not connected to an output.
- Made process a string getter in `input.external.{rawaudio,rawvideo}` (#2877)

Fixed:

- Fixed parameter type for `stats_interval` in SRT I/O.
- Fixed type generalization on variable and pattern bindings
  (#2782)
- Fixed memory leak in http requests (#2935)
- Make sure that exception raised in `request.dynamic` never crash
  the process (#2897)
- Fixed `filename` getter being called multiple time in
  `output.file` (#2842)
- Fixed default directory permissions in `output.*.hls`
  operators (#2930)
- Space trim in interactive variables set on telnet (#2785)
- Fixed internal streaming logic in `max_duration` and `crossfade`.
- Make sure that there's at most one metadata at any given
  frame position (#2786)
- Fixed `metadata.json.parse` always returns an empty list (#2816).
- Fixed `icy_id` being ignored in `output.shoutcast` (#2819)
- Fixed shutdown livelock with some ffmpeg inline encoder,
  decoder and filter operators.
- Fixed input polling stop (#2769)
- Fixed parsed error report in `%include` directives (#2775)
- Fixed crash in external processes when received a `Unix.EINTR`
  event (#2861)
- Fixed crash in `string.interpolate` (#2883)
- Cleaned up srt support.

---

# 2.1.3 (2022-11-04)

New:

- Added `time.string`.
- Added `error.on_error` to report any error raised during the
  script's execution. Enhanced reported error positions (#2712)
- Added `device_id` and `latency` options to `input.portaudio`
  and `output.portaudio` to be able to choose the requested
  device. Use `liquidsoap --list-portaudio-devices` to see the
  list of devices (#2733)
- Added `disconnect` method to `input.harbor`, making it possible
  to disconnect a source client programmatically, including when
  a new client is trying to connect.

Changed:

- Send data in-memory in `http.{post,put}.file` when input data
  is already in memory. This allows to use plain `Content-Length`
  instead of `chunked` transfer encoding in these case, though
  `libcurl` seems to always prefer `chunked` encoding for `put`
  requests.
- Better error message when an encoder is not available on windows
  (#2665)
- Create output directory in HLS outputs when it does not exist
  using newly introduced `perms` permission argument (#2725)
- Removed `restart_on_error` argument on `output.url` and added
  `restart_delay` which implements a delayed restart. Added
  `on_error` argument to be notified of errors (#2731)
- Changed default `encoding` parameter in `string.{quote, escape}`
  to be `null`. Fallback to `"ascii"` encoding when no encoding is
  specified and `"utf8"` fails. This prevents unexpected script
  failures but might not be backward-compatible if you used a
  custom `escape_char` or `special_char` function (#2738)

Fixed:

- Enhanced methods typing support (#2659)
- Add support for `song` metadata (mapped to `title`) and `url` (mapped to
  `metadata_url`) in `input.harbor` (#2676)
- Fixed `blank.*` operator types.
- Fixed request metadata escaping (#2732)
- Fixed `input.external.rawadudio` mono input (#2742)
- Fixed `http` response body on redirect (#2758)

---

# 2.1.2 (2022-09-26)

New:

- Added `string.char`, `string.getter.flush` and `string.getter.concat`.
- Added `http.multipart_form_data` and `http.{post,put}.file`.

Changed:

- Allow sub-second values in `sleep()` (#2610)
- Allowed many new format for `taglib` (#2605)
- Add `settings.ffmpeg.content.copy.relaxed_compatibility_check.set` settings to allow relaxed
  compatibility check for ffmpeg copy content, making it possible to encode
  streams with various audio samplerate or video size when the container
  supports it.

Fixed:

- Stop error loop when opening a listening ssl socket with non-existent certificate. (#2590)
- Youtube HLS upload for live streams.
- Fixed `data:...` uri scheme to conform to RFC 2397 (#2491)
- Fixed multiple issues related to empty `ogg/opus` metadata (#2605)
- Ensure that `video.add_text` fails when the source does (#2609)
- Fixed metadata parsing in `server.insert_metadata` (#2619)
- Fixed `extract_replaygain` path (#2624, @parnikkapore)
- Fixed crash when terminating the process (#2585)
- Fixed channels conversion when using `input.rawaudio` (#2602)

Internal Change:

- `ref()` implementation switched to OCaml's `Atomic` to prevent race conditions,
  `thread.mutexify` and `mutexify` functions removed. (#2603)

---

# 2.1.1 (2022-08-28)

New:

- Added `process.quote.command` to generate complex quoted command strings suitable
  for use with `process.run` and os-independent.

Changed:

- Renamed `playlist.remaining` into `playlist.remaining_files` (#2524)
- Added `id` argument to `replaygain` operator (#2537).
- Made `ocurl` dependency required, added `uri` as required dependency (#2551)

Fixed:

- Fixed missing ffmpeg features on windows build.
- Fixed sync issues with `ffmpeg.encode.*` inline encoders (#2584)
- Fixed `http.get` issues when `user-agent` was not set (#2517)
- Fixed order of `playlist.next` returned requests.
- Fixed infinite loop when reloading a failed playlist (#2576)
- Fixed http requests with urls containing spaces (#2551)
- Fixed `on_connect` type for `srt` inputs and outputs.
- Fixed parsing issues with functions/variables definitions
  beginning with `rec` or `replaces` (#2560)
- Fixed infinite parse error loop (#2527)
- Fixed empty initial `mp4` HLS segment.
- Prevent initial start for autostart and fallible sources.

---

# 2.1.0 (2022-07-15)

New:

- Added support for variables in encoders (#1858)
- Added support for regular expressions (#1881)
- Added generalized support for value extraction patterns (#1970)
- Added support for string getter for `http.{post,put}` operations (#1984)
- Added `output.youtube.live.hls`
- Rewrote out internal JSON parser/renderer (#2011). **Breaking change** values
  that cannot be represented as `JSON` will now raise `error.json` when
  converted to `JSON`. `infinite` and `nan` floats can be exported using the
  `json5` export format.
- Added socket API (#2014).
- Added support for ffmpeg bitstream filters (#2387)
- Added `liquidsoap.version.at_least`.
- Added `video.rectangle`, `video.persistence`.
- Added `video.vumeter`.
- Added `video.slideshow`.
- Added `video.add_text.camlimages` (#2202).
- Added `video.text.*` and re-implemented `video.add_text.*` from those (#2226).
- Added `irc.channel` operator to retrieve the contents of an IRC channel
  (#2210).
- Added new in-house parsing of metadata for some image and video formats
  (#2236).
- Added `file.download`
- Added new options for `%ffmpeg` copy encoder: `ignore_keyframes`
  and `wait_for_keyframe` (#2382)

Changed:

- Removed support for partial application, which should avoid some type errors,
  improve performance and simplifies the code related to the reduction (#2204).
- Video dimensions (width and height) can now be specified per stream in the
  type and are then used instead of the default ones. For instance, you can now
  write
  ```
  s = (single("file.mp4") : source(video(width=300,height=200)))
  ```
  in order to force the decoding of a file to be performed at the 300×200
  resolution (#2212).
- Video images are now _canvas_, which means that they do not directly contain
  the images, but are constituted of multiple images placed at various
  positions. This should make much more efficient operations such as making
  videos from multiple ones, adding a logo, etc. (#2207)
- `output.youtube.live` renamed `output.youtube.live.rtmp`, remove `bitrate` and
  `quality` arguments and added a single encoder argument to allow stream copy
  and more.
- `source.on_metadata` and `source.on_track` now return a source as this was the
  case in previous versions, and associated handlers are triggered only when the
  returned source is pulled (#2103).
- Made `streams_info` parameter of `output.file.hls` a record (#2173).
- Disable scrolling by default in `video.add_text`. You can re-enable it by
  using `video.add_text(speed=70, ...)`.
- Added "example" sections to operators documentation, we now need to populate
  those (#2227).
- Default implementation of `video.testsrc` is now builtin, previous
  implementation can be found under `video.testsrc.ffmpeg`.
- Images can now generate blank audio if needed, no need to add
  `mux_audio(audio=blank(),image)` anymore (#2230).
- Removed deprecated `timeout` argument in `http.*` operators.
- Deprecated `request.ready` in favor of `request.resolved`.

Fixed:

- Fixed typo in `status` command of the `mix` operator.
- Fixed performances issues with `input.ffmpeg` and `input.http` (#2475)
- Fixed `list.shuffle` which was used to randomize playlists in `playlist`
  operator (#2507, #2500).

---

# 2.0.7 (2022-07-15)

Fixed:

- Fixed memory leaks with opus bindings.
- Make sure decoding buffer and samplerate converter are only created once. (#2475)
- Make sure first metadata is always sent in icecast/shoutcast output (#2506)

---

# 2.0.6 (2022-06-20)

New:

- Added `video/mp4` to list of recognized mime types for request resolutions.

Changed:

- Log errors when using `process.read` (##2420, @martinkirch)

Fixed:

- Memory leak when executing `process.run` (#2424)
- Delay harbor server endpoint registration until application has started (#1589)
- Print user-readable encoder parameter error report.
- Fixed m3u metadata parsing when artist has a comma in their name (#2449)
- Cleanup failed request in `playlist` operator.
- Make sure requests are always cleaned up, making `request.destroy` calls
  optionals.

  # 2.0.5 (24-05-2022)

New:

- Extended m3u EXTINF parser to support empty duration and annotations.

Changed:

- Brought back `mix` operator (#2401)

Fixed:

- Allow crossfade duration override of `0.`
- Buffer synchronization issues.
- Drop methods from ffmpeg filter input source
  types to avoid unnecessary conflicts.
- Fix evaluation of abstract values with methods.
- Prevent some sources from being consumed when not active,
  namely ffmpeg inline encoders, `soundtouch`, `resample` and all
  the muxing operators.
- Raise runtime exceptions in `string.replace` failures with
  useful message. (#2408)
- Prevent `request.dynamic` from raising exceptions when checking
  if the source is ready (#2381)

---

# 2.0.4 (23-04-2022)

New:

- Added `settings.video.add_text` to enforce consistent choice of
  `video.add_text` implementation (#2302)

Changed:

- Make sure source shutdown can only be called on sources that
  can actually be shutdown:
  - Remove generic `source.shutdown`
  - Keep `s.shutdown()` method only on sources that are active.
    Refs: #2259
- Optimized memory usage when accessing frame content (#2266)
- Optimized memory usage when accessing ground terms.
- Allow crossfade duration getter to override duration at the
  end of each track if duration isn't set via metadata.
- Make sure crossfade metadata are not duplicated (#2153)
- Renamed `map_metadata` into `metadata.map`, deprecated `map_metadata`.
- Deprecatdd `list.mem_assoc`
- Enhanced remaining time when using `add` (#2255)
- Added `timeout_ms` to `http.*` to provide time in milliseconds, deprecated
  `timeout` argument.
- Connect `output.icecast` when data is available instead of when
  operator starts to avoid useless connections when underlying source
  fails immediately.

Fixed:

- Prevent infinite loops when crossfade duration is negative (#2287)
- Prevent mutex deadlock when recursively locking mutexes (#2274)
- Mark method `add()` as internal in `request.queue`, fix method `length()` (#2274)
- Fixed `retry_delay` being ignored in some cases in `request.dynamic`.
- Prevent race condition in external process handler.
- Fixed A/V sync when streaming encoded data via ffmpeg encoder (#2159)
- Prevent stopped/iddle sources from being restarted when resetting
  `clock(s)` after too much latency (#2278)
- Fixed registration of `video.add_text.ffmpeg` as possible implementation
  for `video.add_text` (#2302)
- Fixed `http.*` calls preventing liquidsoap from shutting down.
- Fixed `http` protocol not returning an error when timing out (#2242)
- Reworked ffmpeg filters feeding mechanism.
- Fixed inconsistencies in `playlist.parser` (#2257)
- Fixed inconsistent reselect in `rotate` (#2300)
- Fixed special characters escaping in `video.add_text.ffmpeg` (#2324)
- Fixed `input.rawaudio` and `input.rawvideo` when handling non-stereo
  content.

  # 2.0.3 (11-02-2022)

New:

- Added support for memory debugging using `memtrace`
- Added `time.{zone,zone.set,make}` (#2178)
- Added `runtime.gc` module, rename `garbage_collect` as `runtime.gc.full_major`
  with deprecated compatibility wrapper, added `runtime.gc.stat`,
  `runtime.gc.quick_stat`, `runtime.gc.print_stat` and `runtime.gc.{get,set}`.
- Added `runtime.sys.word_size`
- Added optional support for `runtime.mem_usage`
- Added `runtime.memory` wrapper to get info about the system and process'
  memory usage.
- Added `configure.camomile_dir` to export expected location of camomile
  directory when packaging liquidsoap.
- Added `liquidsoap.chroot.make` to copy all files required for a liquidsoap
  install.

Changed:

- Bumped `input.harbor` default buffer to `12.` to make it possible
  to use it with `crossfade` transitions without changing default
  values (#2156)
- `year` method as returned in `time.local` and `time.utc` now
  returns the actual year instead of years since 1900 (#2178)
- `mday`, `mon`, `wday` and `yday` methods as returned in `time.local`
  and `time.utc` have been renamed to, resp., `day`, `month`, `week_day`
  and `year_day` (#2178)
- `month` method as returned in `time.local` and `time.utc` now
  returns the month as a number between `1` and `12` (#2178)
- `week_day` method as returned in `time.local` and `time.utc` now
  returns the week day as a number between `1` and `7` (#2178)
- `year_day` method as returned in `time.local` and `time.utc` now
  returns the week day as a number between `1` and `366` (#2178)
- Added option to choose if `input.rtmp` should behave as
  a server or a client (#2197)
- Allow dynamic text change in `video.add_text.ffmpeg` (#2189)
- Removed `thread_name` argument from `thread.on_error` callbacks.

Fixed:

- Make sure metadata are replayed when switching to a source for the
  first time in switches/fallback (#2138)
- Bring back `video.add_text.sdl` (#2187)
- Fixed `thread.on_error` implementation (#2171)
- Fixed `ffmpeg` video scaling to make sure it always is proportional (#2211)

  # 2.0.2 (28-12-2021)

New:

- Show code excerpts on errors (#2086)
- Added `on_get_ready` callback to sources, to be
  executed after a source's has initialized.
- Added `flush_and_skip` telnet command to `request.dynamic`
  to empty the request's queue before skipping the current
  track, forcing a full reload.
- Added `last_metadata` method on sources to return the
  last metadata produced by the source.

Fixed:

- Fixed ffmpeg copy encoder crash when switching between
  streams.
- Fixed unbound buffer in muxing operators (#2054)
- Return correct positions when parsing strings (#2095)
- Deadlock when shutting down with `input.rtmp` (#2089)
- Add timeout to srt operations (#2082)
- Fixed `request.queue` `queue` telnet command returning
  nothing (#2088)
- Fixed single quotes being escaped in json stringify.
  (#2120)
- Fixed frame caching issues when no initial break was
  present in the memoized frame. (#2109. AzuraCast/AzuraCast#4825)
- Fixed `replay_metadata` not replaying metadata from active sources
  (#2109)

  # 2.0.1 (27-11-2021)

New:

- Added `time.predicate` to parse time predicates at runtime.
- Added support for ffmpeg filter commands, unify `video.add_text.ffmpeg`
  with other operators, make it the default when available. (#2050)

Changed:

- Removed `encode_metadata` option in `input.file.hls` as it does nothing with
  the main encoder for HLS format, `%ffmpeg` (#2023)
- Converted `output.icecast` optional parameters to `nullable`.

Fixes:

- Fixed switch-based sources not respecting track boundaries when
  using default transitions one track only per selected source. (#1999)
- Fixed playlist annotation. (#2005)
- Raise a proper runtime exception when `string.escape` fails. (#2010)
- Account for internal caching in `request.dynamic.list`'s `queue` and
  `set_queue` methods.
- Keep buffering for crossfade when new source has track mark but is still ready.
- Added missing output `start`/`stop` commands.
- Fixed `perms`, `dir_perms` and `append` not bring honored when delegating file
  output to the encoder.
- Fixed base directory not being created when delegating file output to the
  encoder (#2069).
- Use `process.quote` in process calls (#2031)

  # 2.0.0 (03-10-2021)

New:

- Add support for errors with `error.*` and `try ... catch` (#1242).
- Add support for optional values with `null.*` (#1242).
- Add support for `x ? y : z` syntax (#1266).
- Added support for list spread and deconstruction syntax (#1269).
- Add support for generic JSON objects, map `(string, 'a)` lists to regular
  lists, add support for json5 floats (`NaN`, `Infinity`), return `null` for
  those otherwise, rename `json_of` into `json.stringify` and `of_json` into
  `json.parse` with deprecation (#1824)
- Added support for video encoding and decoding using `ffmpeg` (#1038).
- Added support for hardware-accelerated video encoding using `ffmpeg` (#1380)
- Added support for ffmpeg filters (#1038).
- Added video support to `output.hls` (#1391).
- Added mp4 support to `output.hls` (#1391).
- Added `output.url` for encoders that support handling data output (currently
  only `%ffmpeg`) (#1038).
- Added `output.file.dash.ffmpeg`.
- Added LV2 support (#906).
- Added `string.nth` (#970).
- Added `string.binary.to_int` (#970).
- Added `string.hex_of_int`.
- Added `file.ls` (#1011).
- Added native id3v2 tag parser, as well as associated function
  `file.mp3.metadata`, `file.mp3.parse_apic` and `file.cover` (#987).
- Use a pager to display long help results (#1017).
- Added new functions for lists: `lists.exists`, `list.for_all`, `list.init`,
  `list.ind`, `list.index`, `list.last`, `list.shuffle`.
- Added `request.id`.
- Added a profiler for the language. It can be enabled with `profiler.enable` and
  the results are obtained with `profiler.stats.string` (#1027).
- Added `gtts` protocol to use Google TTS (#1034).
- Added `liquidsoap.executable` to get the path of the currently running
  Liquidsoap.
- Added `source.dump`.
- Added `source.elapsed` and `source.duration`
- Added `synth` protocol (#1014).
- Added listener and caller mode for `input.srt` and `output.srt` (#1377)
- Added support for `srt.enforced_encryption` setting.
- Added support for prometheus reporting (#1000)
- Add `validate` parameter to `register`, which allows to validate a value
  before setting it (#1046, @CyberDomovoy)
- Add `string.null_terminated` (#960).
- Removed `string.utf8.escape` in favor or a unifited, utf8-aware `string.escape`.
- Add `string.unescape`.
- Add `file.metadata` (#1058).
- Add `predicate.activates`, `predicate.changes`, `predicate.first`,
  `predicate.once`, `predicate.signal` (#1075).
- Add `playlist.list.reloadable` and `playlist.list` (#1133).
- Make it possible to disable buffer overrun logs.
- Add `accelerate` operator (#1144).
- Add `video.resize`.
- Add `getter.int_of_float` and `getter.float_of_int`.
- Add `source.dump` (#1036).
- Add `stereo` and `synth` protocols (#1036).
- Add `video.add_text.ffmpeg`.
- Added support for `file:///path/to/file` and `file:/path/to/file`protocols.
- Added configure option to specify internal library install path (#1211).
- Add support for records and methods (#1197).
- Rename `unsafe.single.infallible` to `single.infallible`.
- Add `list.indexed`.
- Added optional support for high-resolution time and latency control on POSIX
  systems (#1050).
- Added syntax for `for` and `while` loops (#1252).
- Added a bunch of source-related methods (#1379).
- Added `min` and `max` functions.
- Added `lufs` to compute the LUFS loundness (#1497).
- Added `interactive.harbor` in order to expose interactive variables over
  harbor (#1495).
- Added `interactive.persistent` (as well as `interactive.save` and
  `interactive.load`) to make interactive variables persistent (#1495).
- Added `server.harbor` (#1502).
- Added `metronome`.
- Added `playlist.files`.
- Added `getter.is_constant`.
- Added `assert`.
- Added `source.available`.
- Added `request.once`.
- Added `file.getter`.
- A better `normalize` function (with more reasonable parameters, more
  customisable, and written in Liquidsoap) is now provided. The old one is
  renamed `normalize.old`.
- New and better `compress` function. The previous one was renamed
  `compress.old` (#868, #869).
- Added `stereo.width`.
- Added `file.mkdir`.
- Added support for harbor's connected address in auth function and as a method
  (#1364).
- Added `time.up`.
- Added `video.cover`.
- Added `video.still_frame`.
- Added `request.status`.
- Added `playlog` to record how long ago a song was last played (#333 and #1530).
- Added `clock.log_delay` to configure how often clock catchup error messages
  should be printed.
- Added `input.rtmp` (#1640).
- Added `%ifversion` and `%else` preprocessing commands (#1682).
- Added `dtmf` and `dtmf.detect` to generate and detect DTMF tones (#1796).
- Added `sine.detect` to detect sines (#1796).
- Added `on_air_timestamp` to request's metadata to get the request's `on_air` time
  as a Unix timestamp (#1871)

Changed:

- Implemented per-frame clock synchronization mechanism, should allow for more
  advanced flexibility when working with source synchronization while keeping
  the default safe behavior. (#1012)
- Remove `active_source` type, make all output return `unit` type. (#1671)
- Switch to YUV420 as internal image format, much more efficient (#848).
- Use bigarrays for audio buffers (#950).
- Re-implemented switch-derived operators (`fallback`, `rotate`, `random`) as
  scripted operators, removed `track_sensitive` argument from `rotate` and
  `random` as it does not have a sound meaning for them.
- Added optional exit `code` to `shutdown`.
- Renamed `verb` argument info `method` in `output.icecast`.
- Simplified `add` behavior, also fixing an clock issue (#668).
- Switch to more efficient callback API for decoders (#979).
- Use system pagesize for buffer allocation (#915).
- Use new Strings module in order to avoid concatenations (#984).
- Native Liquidsoap implementation of list functions (#920).
- Added `fallible` option to `single` operator.
- Allow `input.ffmpeg` to control its own clock or delegate to CPU clock (#1628)
- Reimplement `input.http` using `ffmpeg`, deprecate `input.https` in favor
  of unified `input.http` (#1628)
- Changed `input.http` and `input.ffmpeg` `url` parameter into a string getter
- Changed `request.queue` into a Liquidsoap implementation (#1013).
- Removed `request.equeue`, such a feature could be re-implemented in
  Liquidsoap, see `request.queue`.
- The `playlist` operator is now fully implemented in Liquidsoap (#1015).
- Removed `playlist.once`, its behavior can be achieved by passing `"never"` to
  the `reload_mode` argument of `playlist` (#1015).
- Removed `playlist.merged`: it is not that useful and can be achieved easily
  with `merge_tracks` on a `playlist` (#1015).
- Deprecated `playlist.safe` (#1015).
- Renamed `add_timeout` to `thread.run.recurrent`, added `thread.run` variant,
  renamed `exec_at` to `thread.when` and renamed `mutexify` to `thread.mutexify`
  (#1019).
- Changed the weights of `add` to float (#1022).
- Renamed `which` to `file.which`.
- Change `blank()` duration semantics to mean forever only on negative values.
- Get rid of numbering of universal variables (#1037).
- Renamed `base64.decode`/`base64.encode` to
  `string.base64.decode`/`string.base64.encode`.
- Vumeter is now implemented in Liquidsoap (#1103).
- Change `input.http` and `input.https` `url` parameter into a string getter
  (#1084).
- Added `path.home.unrelate`.
- Use getters for arguments of `video.add_image` (#1176).
- Add `x`, `y`, `width` and `height` argument to `image`, unify with
  `video.add_image`.
- Generalize `audio_to_stereo` to video frames and those without audio.
- Allow crossfading for video (#1132, #1135).
- Use getters for parameters of synthesizer sources (#1036).
- Renamed `empty` to `fail`.
- Restored `request.dynamic` (#1213).
- Requests are not typed anymore: their type is fixed at resolution time.
- Deprecated `request.create.raw`, you should use `request.create` instead.
- Reference setting and access are now handled as normal builtins instead of in
  the kernel.
- Use records as return type of `http.*`, `https.*`, `rms`, `peak` and
  `request.queue` (#1234).
- Indices of groups returned by `string.extract` are now integers instead of
  strings (#1240).
- Generalize the `l[k]` notation so that the key `k` can be of any type (on
  which we know how to compare).
- `ref` is not a keyword anymore: this means that `ref x` is not accepted
  anymore, you need to write `ref(x)` (#1254).
- Renamed `file.unlink` to `file.remove`.
- Deprecated `get_process_output`, `get_process_lines`, `test_process` and
  `system` in favor of `process.run`, `process.read`, `process.read.lines` and
  `process.test`.
- Renamed `http_codes` to `http.codes` and put first member as integer.
- Renamed `http.response` to `http.response` and `http.response.stream` to
  `http.response.stream`.
- `localtime` and `gmtime` now return a record.
- Deprecated `{eat,strip,skip,on}_blank` in favor of
  `blank.{eat,strip,skip,detect}`.
- `http{,s}.{get,post,push}` now perform redirections if needed, which can be
  disabled with the `redirect` parameter (#1319).
- Deprecated `gettimeofday` in favor or `time`, renamed `localtime` to
  `time.local` and `gmtime` to `time.utc`, and the argument of these two
  last functions is now optional (#1320).
- Dropped optional `gavl` video converter in favor of `ffmpeg`.
- Remove `persist` argument in `output.*.hls` and use nullable value for `persist_at`.
- Deprecated source server commands in favor or direct call to source
  methods. Added wrappers for some of the old commands (#1379).
- Deprecated catch-all `input` and `output` in favor or setting your desired
  input or output explicitly.
- Implement `interactive.*` on script side (#1493).
- `file.write` does not return a boolean anymore, exceptions are used for
  exceptional cases (#1500).
- `source.dynamic` now takes a nullable argument.
- Renamed `on_end` to `source.on_end`.
- Changed the name of the arguments of `fallback.skip`.
- Normalize ReplayGain handling:
  - we now use the standard `replaygain_track_gain` metadata
  - renamed the protocol from `replay_gain` to `replaygain`
  - added the `replaygain` operator to perform amplification
- `normalize` now handles all channels uniformly.
- First-order filter `filter.rc` now takes the cutoff frequency instead of the
  time constant as argument.
- `file.watch` now returns unit with `unwatch` method.
- Changed the interface for `bpm`: the bpm can now be retrieved using a method
  of the returned source instead of having a callback.
- Removed `server.read*` and `server.write*`.

Fixed:

- Set `cloexec` on all relevant Unix calls (#1192).
- Fix implementation of recursive functions (#934).
- Make `blank()` source unavailable past is expected duration (#668).
- Remove `video.add_text.gstreamer` shade in background (#1190).
- Improve the quality of `video.add_text.gd` (#1188).
- Exit with non-zero code on errors.
- Fixed parsing of http URI arguments with `=` in them (#1340).
- Fixed fade-out in crossfades when crossfade duration is the same as fade-out
  duration (#1351).
- Fixed osc server not working when daemonized (#1365).
- Fixed glitchy audio when using `input.harbor` (#1944)
- Fixed `"tracknumber"` and `"year"` returning `0` in taglib (#1901)

Removed:

- LiGuidsoap, the old Liquidsoap GUI. 🪦

  # 1.4.4 (27-02-2021)

New:

- Added `process.quote` to quote process' arguments (#1215)

Changed:

- Fetch mime type using curl first when available.
- Make override metadata name case-sensitive in `amplify` (#1323)
- Harnessed playlist file resolver to better support some combination of
  protocols and file resolution (#1362)

Fixed:

- Remote file resolution when passing URLs with spaces (#1410)
- Fixed empty `{http,https}` body (#1417)
- Fixed `input.harbor` shoutcast client connection (#1353)
- Fixed exception reporting when output fails to start (#1372)
- Fixed `random` track selection (#1468)
- Fixed playlist request leak when using `reload="watch"` with `inotify` on a
  folder (#1451)
- Deadlock when LO server thread crashes (#1409)

  # 1.4.3 (14-09-2020)

Fixed:

- Fixed exponential memory usage in clock unification algorithm (#1272).

  # 1.4.2 (03-05-2020)

New:

- Added `retry_delay` argument to `request.dynamic` (#1169).
- Renamed `request.dynamic` to `request.dynamic.list` and updated its
  callback function type to return an array of requests, making possible
  to return multiple requests at once but, more importantly,
  to return `[]` when no next requests are available. (#1169)

Changed:

- Set `audio/flac` as mime for flac (#1143).
- Deprecated `request.dynamic`.

Fixed:

- Fixed errors when installing bash-completion files (#1095)
- Fixed failures in `extract-replaygain` script (#1125)
- Do not crash when loading playlists using `~/path/to/..` paths.
- Set `set_default_verify_paths` for SSL (#450)
- Use 443 as default port for https (#1127)
- Fix implementation of `rotate` (#1129).
- Register audio/opus mime type for ogg decoding (#1089)
- Re-encode name, genre and description in `output.icecast` using the given
  encoding (#1092)
- Accept 24 bits per sample in %flac encoder (#1073).
- Fix rare stack overflow during clock unification (#1108).
- Prevent metadata inserted via `insert_metadata` from being visible to
  underlying sources (#1115)
- Fix `cross()` fallability.
- Fix decoder remaining time when decoding is done (#1159)
- Fixed crash when cleaning up `output.hls`
- Fix `get_process_lines` regexp logic (#1151)

  # 1.4.2 (03-05-2020)

New:

- Added `retry_delay` argument to `request.dynamic` (#1169).
- Renamed `request.dynamic` to `request.dynamic.list` and updated its
  callback function type to return an array of requests, making possible
  to return multiple requests at once but, more importantly,
  to return `[]` when no next requests are available. (#1169)

Changed:

- Set `audio/flac` as mime for flac (#1143).
- Deprecated `request.dynamic`.

Fixed:

- Fixed errors when installing bash-completion files (#1095)
- Fixed failures in `extract-replaygain` script (#1125)
- Do not crash when loading playlists using `~/path/to/..` paths.
- Set `set_default_verify_paths` for SSL (#450)
- Use 443 as default port for https (#1127)
- Fix implementation of `rotate` (#1129).
- Register audio/opus mime type for ogg decoding (#1089)
- Re-encode name, genre and description in `output.icecast` using the given
  encoding (#1092)
- Accept 24 bits per sample in %flac encoder (#1073).
- Fix rare stack overflow during clock unification (#1108).
- Prevent metadata inserted via `insert_metadata` from being visible to
  underlying sources (#1115)
- Fix `cross()` fallability.
- Fix decoder remaining time when decoding is done (#1159)
- Fixed crash when cleaning up `output.hls`
- Fix `get_process_lines` regexp logic (#1151)

  # 1.4.1 (18-02-2020)

Fixed:

- Fixed `fade.final` and `fade.initial` (#1009)

  # 1.4.0 (29-09-2019)

New:

- UTF8 parsing!
- Added support for tuples: `x = (1,"aa",false)` (#838)
- Added support for deconstructing tuples: `let (z,t,_) = x` (#838)
- Added `input.{file,harbor}.hls` to read HLS stream (#59, #295, #296).
- Added `output.hls` to natively stream in HLS (#758).
- Added `%ffmpeg` native encoder, only for audio encoding for now (#952)
- Added ffmpeg-based stream decoder, limited to mime type `application/ffmpeg`
  for now.
- Added `(to_){string,float,int,bool}_getter` operators to handle getters in
  script side.
- Made `p` parameter in `smooth_add` a `float` getter (#601)
- Added `source.time` to get a source's clock time.
- Added `max_duration` to limit a source's duration.
- Added `file.temp_dir` to create temporary directories.
- Added `file.{unlink,rmdir}` to remove, resp., file and directories.
- Added `file.write` to write content to a file.
- Added `file.read` to read contents of a file without loading all of it in memory.
- Added `youtube-pl:<ID>` protocol to resolve and parse youtube playlists (or
  any playlist supported by `youtube-dl`) (#761)
- Added `protocol.aws.endpoint` setting for the `s3://` protocol, thanks to
  @RecursiveGreen. (#778)
- Added support for sandboxing `run_process` calls. (#785)
- Added `harbor.{http,https}.static` to serve static path.
- Added `log.{critical,severe,important,info,warning,debug}`. Use aliases in
  code as well (#800, #801, #802)
- Added `sleep` function.
- Added `mkavailable` function.
- Added `fade.skip` function. (#804)
- Added `video.external.testsrc` function.
- Added `video.frame.*` and `audio.samplerate`.
- Added `input.external.ffmpeg` and `output.external.ffmpeg`.
- Added `output.youtube.live.ffmpeg`.
- Added `output.file.hls.ffmpeg`.
- Added `reopen` telnet command in `output.external`.
- Added `on_frame` (#886).
- Enabled external decoders in windows (#742)
- Added support for bash completion.
- Added `video.add_text.native`.
- Added `configure.bindir`
- Added `for` and `while` loop functions.
- Added `list.case`.
- Added `metadata.getter` and `metadata.getter.float`.
- Added `string.contains`.
- Added `request.uri`.
- Added `{input,output}.srt` (#898)
- Added `path.remove_extension`.
- Added SSL read/write timeout options, use it for incoming socket connections (#932)
- Added ffmpeg resampler (#947).
- Added `lsl` and `lsr`.

Changed:

- Depends on OCaml >= 4.08.0
- Changed return type of `http.*` and `run_process` to use tuples (#838)
- Better error reporting with coloring and uniform format. (#790)
- Improved reporting of file, line and character during parsing errors.
- Remove dynamic plugin build option.
- Made `on_end` delay a float getter.
- Reimplemented `fade.{in,initial,out,final}` as scripted operators. (#664)
- Removed `cross`/`crossfade` operators, superseded by
  `smart_cross`/`smart_crossfade`
- Rename `smart_cross`/`smart_crossfade` operators as `cross`/`crossfade`
- Default behavior of `crossfade` is old (simple) crossfade. Use `smart=true`
  to enable old `smart_crossfade` behavior.
- Rename `file.duration` as `request.duration`
- Removed duplicate `is_directory`
- Rename `{basename,dirname}` as `path.{is_directory,basename,dirname}`
- Empty playlists return by scripted resolvers is now considered a failure to
  resolve.
- Rewrite `smooth_add` to use new `mkcross` functions.
- Reimplemented `open_process_full` to get a hand on `pid` and finer-grained
  closing workflow (#703)
- Added `transition_length` to `switch`-based operators to limit transition
  lengths and allow garbage collection of transition sources.
- SDL renders text in UTF-8. (#712)
- Made `x` and `y` parameters in `video.add_text` `float` getters. (#730)
- Reimplemented `extract-replaygain` using `ffmpeg`, added an optional replay
  gain option to the `ffmpeg2wav` protocol. Thanks to @Yamakaky for contributing
  on this. (#749)
- The `ratio` parameter of `compress` and `limit` is a float getter. (#745)
- Removed `rewrite_metadata` which had been deprecated for a while now.
- Allow string getter for `harbor` HTTP responses.
- Renamed `get_clock_status` to `clock.status` and `log_clocks` to `clock.log`.
- Renamed `rms_window` parameter of `compress` to `window`. (#796)
- Added `chop` operator.
- Keep master tracks' boundaries in `mux_*` functions. (#795)
- Added `new_track` optional argument to callback in `insert_metadata`.
- Use getters for weights of `rotate`. (#808)
- Added `conservative`, `length` and `default_duration` params to
  `playlist.{reloadable,once,merge}` (#818)
- Renamed `input.external` into `input.external.rawaudio`, added
  `input.external.wav`.
- Renamed `gstreamer.hls` to `output.file.hls.gstreamer`.
- Raise an error when using a format (e.g. `%vorbis`, `%mp3`, ..) that is not
  enabled. (#857)
- Set default encoders and ladspa plugins samplerate and channels to configured
  internal `"frame.audio.samplerate"` and `"frame.audio.channels"`. (#870)
- Handle unary minus in the preprocessor instead of the parser in order to avoid
  duplicating the parser. (#860)
- Add `filter` option to `playlist.once`.
- Added a `replay_delay` option to the `pipe` operator to replay metadata and
  breaks after a delay instead of restart the piping process. (#885)
- Add `buffer_length` telnet command to `input.harbor`.
- Bumped default `length` parameter for request-based sources (`playlist`,
  `request.dynamic`, ..) to `40.` to assure that there always is at least
  one request ready to play when the current one ends.
- Added support for cue in/out and fade in/out/type metadata support in `ffmpeg2wav`
  protocol. Rename protocol to `ffmpeg`. (#909)
- `list.assoc` and `list.assoc.remove` require an ordered type as first
  component.
- Renamed `quote` to `string.quote`, removed `process.quote` in favor or
  `string.quote` (#1635)
- Added `phase_inversion={true/false}` to `%opus` encoder (#937)
- Fixed encoders forcing frame rate and audio channels too early (#933)
- Change filename to a string getter in file-based outputs. (#198)
- Changed `audio.converter.samplerate.preferred` option to
  `audio.converter.samplerate.converters` to give a list of possible converters.

Fixed:

- Lack of documentation for `cross`/`crossfade` (#743)
- Fixed before metadata being lost during crossfade not in conservative mode.
- Correct types and default values for `random.int` (#767).
- Allow changing pipeline in gstreamer functions. (#762)
- Script deadlock after a long time, most likely related to old crossfade
  transitions (#755)
- AVI export fixed. (#789)
- `%external` does not stop processes anymore on each metadata. (#789)
- Fixed exit getting stuck when using `input.jack` (#769)
- Stop lo server on shutdown. (#820)
- Fixed external process stop not detected on second and further calls (#833)
- Add `seek` in operators where implementation is clear (#853)
- Do not enter buffering mode between tracks in `buffer` (#836)
- Fixed file descriptor leak in external processes (#865)
- Fixed encoded output creating empty files from failing sources (#876)
- Fixed `cue_cut` not working when used before `cross`/`crossfade` (#874)
- Fixed audio glitches when seeking within a MP3 file.
- Fixed `insert_metadata` logic when insert new track and metadata (#903)
- Fixed `replay-gain` script default location.
- Fixed audio glitches at the end of crossfade transitions.
- Specify that `list.remove` removes only the first occurrence and avoid
  reversing the list (#922).
- File descriptor leak when using openssl-based operators.
- Fixed SSL read taking too long to timeout (#932)
- Fixed output starting when underlying source is not available (#393)
- Fixed `string.escape` also quoting its string.

  # 1.3.7 (09-04-2019)

Changed:

- Reimplemented `open_process_full` to get a hand on `pid` and finer-grained
  closing workflow (#703)
- Better log message when request download times out (#708)
- Drop `log.level` for `ffmpeg` messages to `5`

Fixed:

- Timeout when executing external processes (#691, #736, #726, #708)
- Set buffering only when frame is partial in time_wrap.ml. Makes it work with
  crossfade transitions (#695)
- Changed `Icy-MetaData:1` to `Icy-MetaData: 1` in HTTP source headers. Fixes
  some shoutcast implementations (#727)
- Fixed deadlock in `input.http` source status command (#367)

  # 1.3.6 (23-01-2019)

Fixed:

- Fixed `smart_crossfade` transitions skipping data after track marks. (#683,
  #652)
- Fixed `input.pulseaudio` parameters.
- Fixed crash when copying frame content (#684)

  # 1.3.5 (25-12-2018)

New:

- Added a bunch of base mathematics primitive, `exp`, `log`, `cos`, `sine`, ...
- Added `"extinf_duration"` to parsed `#EXTINF` metadata.

Fixed:

- Fixed inotify watch semantics (#677)
- Enhanced `#EXTINF` parsing in ambiguous cases (#625)
- Fixed `output.youtube.live` (#630)
- Make sure server writes are synchronous (#643)
- Fixed crash when loading some frei0r plugins (#435)
- Fixed compilation with `osx-secure-transport`
- Fixed invalid opus stream generated when no data was ever encoded (#180)

  # 1.3.4 (10-09-2018)

New:

- Added `FFMPEG` decoder using the new `ocaml-ffmpeg` API. Thanks for @gndl for
  the hard work there.
- Added `"init.allow_root"` setting to allow running liquidsoap as root.
- Added `on_track` callback for playlists. Can be used to force a reload.
- Added `server.condition`, `server.wait`, `server.broadcast` and
  `server.signal`. Used to control server command execution.
- Added `server.write`, `server.read{chars,line}` to write interactive server
  commands in conjunction with the above functions. (#544, #568)
- Added `output.youtube.live` as a wrapper around `output.gstreamer.audio_video`
  to stream live to Youtube (#498)
- Added metadata extraction to `ffmpeg2wav` protocol (#623).

Changed:

- Depends on OCaml >= 4.03.0
- Depends on camomile > 1.0.0
- Use `http{s}.head` when available to fetch remote file's mime type. (win32
  port)
- Better log messages for root exit and buffer override.
- Switch default log to stdout. Set to file when `log.file.path` is set (#612)
- Disabled Gstreamer stream decoder.
- Removed asynchronous mode for `output.gstreamer.audio_video`
- Reworked `smartcross` internal logic (#596)
- Enabled `replaygain` on `m4a` files, thanks to @gilou (#604)
- Added `encoding` parameter to `output.shoutcast` to allow alternative string
  encoding for metadata updates (#411)
- Deprecated `rewrite_metadata`

Fixed:

- Decouple dyntools compilation.
- Support for OCaml >= 4.06
- File descriptor leak in `output.icecast` (#548)
- Fixed URL regexp for `input.https` (#593)
- Multiple gstreamer fixes:
  - File decoder with video.
  - Memory leaks (#516, #511, #434, #318)
  - Process freeze (#608, 278)
- Duppy crash on exit (#160)
- Fixed audio glitches when using the `pipe` operator (#614)
- Deadlock in external decoder. (#611)

  # 1.3.3 (14-10-2017)

New:

- Added `on_change` to `register`
- Added IPv6 support for `input.harbor`. (#491)
- Added `time`, `localtime` and `gmtime` to help with time-predicates (#481)
- Added `on_start` to execute callback when liquidsoap starts.
- Added `enable_external_ffmpeg_decoder` to enable ffmpeg-base external decoder.
- Added `"decoder.external.{ffmpeg,ffprobe,flac,metaflac,faad,mpcdec}.path"`
  configuration settings.

Changed:

- Renamed secure transport harbor key paths to: `harbor.secure_transport.*`
- Renamed secure transport I/O to: `{input,output}.harbor.secure_transport`.
- Added `.wma` to `gstreamer` file decoder file extensions (#483)

Fixed:

- Fixed memory leak in `output.icecast` connection method. (#490)
- Fixed `mutexify`
- Make sure that metadata are always passed in increasing position order in
  `map_metadata` (#469)

  # 1.3.2 (02-09-2017)

Changed:

- Removed `kick` telnet/server command, duplicate of `stop`.
- Support `replaygain` for mp3 files, thanks to @d4h3r0 (#460)
- Implement `input.harbor.ssl` using SecureTransport for OSX.

Fixed:

- Fix scheduler loop causing high CPU usage when using Process_handler without
  some of the default callbacks. (#475)
- Revert `wait_for` implementation to pre-`1.3.0`, using a custom `select` loop (#453)
- Handle mime-type arguments in `input.harbor` streams. (#456)
- Tell ocaml to use the same C compiler at build and link time. Fixes build on
  FreeBSD when using C++-based bindings such as taglib. (#465)
- Accept any capitalization of HTTP(S) as regular HTTP URL (#464)
- Fix compilation with osx-secure-transport enabled.
- Fix deadlock calling logging functions from within `Gc.finalise` (#609)

  # 1.3.1 (28-05-2017)

New:

- Allow any tags allowed in `"encoder.encoder.export"` settings in vorbis
  streams (#418)
- Allow `"audio/mp3"` mime-type for mp3 in file resolution protocol. (#451)

Fixed:

- Fixed `run_process`, `get_process_lines`, `get_process_output` when compiling
  with OCaml <= 4.03 (#437, #439)
- Calls to `wait_for` while the scheduler isn't running (#442)
- Revert default handling of environment in `run_process`, `get_process_lines`,
  `get_process_output` to passing calling process' environment by default.

  # 1.3.0 (27-04-2017)

New:

- Added support for recursive functions (#406)
- Add peak and peak.stereo operators (#364)
- Change `track_sensitive` parameter to a boolean getter (fixed value or
  anonymous function).
- Add SSL support to the various harbor operators, either via openssl or OSX's
  SecureTransport.
- Add optional "dj" and "next" metadata for Shoutcast v2, wrap "dj" value in a
  callback in output.shoutcast (#370, #388)
- Allow partial parsing of JSON objects in `of_json`.
- Generalize list.assoc to allow default values. Legacy code must be updated:
  `list.assoc(k,l)` -> `list.assoc(default="",k,l)`
- Generalize list.hd to allow default values. Legacy code must be updated:
  `list.hd(l)` -> `list.hd(default="",l)`
- Allow to pass a default to list.nth. Legacy code must be updated:
  `list.nth(l,pos)` -> `list.nth(default=<..>,l,pos)`
- Added `on_offset` to execute a callback at a given offset within a source's tracks.
- Added mutexify to protect a function from being called concurrently.
- Added request.log to get log data associated with a request
- Added `overlap_sources` to rotate between sources with overlapping tracks.
- Added `replay_metadata` to `input.harbor()`
- Added `\<char code>` syntax for strings (#368)
- Added string.sub
- Added `run_process` to run a process with optional environment and return
  (`stdout`,`stderr`,`exit_status`)
- Added `add_playlist_parser` to register new playlist parsers
- Added optional static parameter to `protocol.add`
- Added file.temp to create fresh temporary filename
- Added process: protocol
- Reimplemented curl-based fetch process using process:
- Added s3:// protocol to fetch files from AWS S3 using the AWS CLI.
- Added polly: protocol to enable speech synthesis using AWS polly. Generated
  files are mono so make sure you use `audio_to_stereo()`.
- Added youtube-dl: protocol to resolved requests using youtube-dl
- Added `which()` to find an executable within the $PATH
- Added `register()` to allow to register new configuration settings

Changed:

- Reverted default value for `--error_as_warnings` option, renamed to `--strict`.
- Moved say: protocol registration to utils.liq.
- Moved `get_process_lines` and `get_process_output` to utils.liq, added
  optional env parameter
- Set `conservative=true` by default in `cross()` and `smartcross()`

Deprecated (can be removed in any future version):

- Dynamic plugins compilation, deprecated in favor of opam rebuild mechanism.

Removed:

- aac and aacplus encoders, removed in favor of fdk-aac.
- dirac/schroedinger video encoder: obsolete, abandoned upstream.
- `force_mpeg` option in taglib metadata decoder. Has not been used for years and
  allows to decouple taglib code from the mad decoder.

Bugfixes:

- Fix negative seek (#390)
- Prevent flows metadata updata from stalling sources (#377)
- Add revdns setting for telnet, set all revdns default to false (#372)
- Fix icy metadata in output.harbor (#358)
- Fix missing first line of headers from icy clients in `input.harbor` (#380)
- Fix timestamp in some logged output (#395)
- Fix crash in external (download) protocol.
- Fix `fade.{in,out}` metadata handling for new fade duration and type.
- Compute normalization regardless of child sources ready status in `add()` to
  avoid unexpected change of volume.

  # 1.2.1 (01-07-2016)

New:

- Support for https (SSL/TLS) icecast connections.
- Added `http.{put,head,delete}`, `https.{get,post,head,put,delete}`.
- Added `input.https`.
- Added `list.mapi`.
- Added `rotate.sequence`.
- New `pipe()` operator to pipe audio data through an external program.
- Switched to curl for request resolution/fetch.

Bugfixes:

- Fix metadata update for shoutcast v2 when sid <> 1 (#320).
- Fix connection to `input.harbor` using the shoutcast v1 protocol (#337).

  # 1.2.0 (12-01-2016)

New:

- Websocket server (#90): this means that you can stream to harbor directly from
  your browser!
- Add support for AIFF format (#112).
- Add `url.split_args` to split the argument of an url (#123).
- Add `buffer.adaptative` to cope with small network delays (#131).
- Add sleeper operator to simulate network delays and test robustness (#131).
- Add `stereo.left` and `stereo.right` to extract channels from a stereo stream.
- Add restart command to restart liquidsoap (#135).
- Add `file.contents` to read the contents of a file.
- Add `filter.rc` for first-order RC filters.

Enhancements:

- Add support for sending OSC data (`osc.send_*`).
- Native support for (some) AVI files (#256) which enables support for external
  video encoders (#233).
- Improve rms operator (#105) to have per channel rms (#102), the ability to
  dynamically set window duration (#103) and multiple monitors (#104).
- Icecast streaming can now use HTTP1.1 chunked encoding (#82, #107).
- Add support for multiple shoutcast extensions (#216).
- Fade type can be overridden by metadata in `fade.in` / `fade.out` (#64).
- Allow LADSPA plugins with arbitrary number of channels (#191).
- Rename shine encoder from `%mp3.fxp` to `%shine`.
- fdkaac: dynamic plugin (#79), set afterburner parameter, use MPEG4 by default
  (#83).
- Improved subtyping on lists (#125, #126).
- Add native simple JSON decoder.
- Better code: do not abusively use assertions (#137), issue more warnings and
  fix them (#162).

Bugfixes:

- Correctly close connection in http.get / http.post (#72).
- Remove `input.lastfm` which has been broken for a while.
- Lots of small bugfixes.

  # 1.1.1 (08-05-2013)

New:

- Add support for FDK-AAC, which seems to be the best AAC(+) encoder around for
  now. Replacement candidate for VO-AAC and AACPLUS
- Add %ifencoder to check whether Liquidsoap was compiled with support for a
  particular encoding format.
- There is now an emacs mode in scripts/liquidsoap-mode.el.
- Liquidsoap can be used as a Windows service.

Enhancements:

- Handle more OSC types (`float`, `float_pair`, `bool`, `string`, `string_pair`) and added
  `osc.on_*.`
- Better infrastructure for decoding images. `add_image` can now handle most image
  file types.
- Add `random.int` as well as `min_int` and `max_int` to standard library.
- Add `playlist.merge` to play a whole playlist as one track.
- Add `gstreamer.hls` to play http live streams (HLS).
- Add `say.program` to specify text-to-speech program in scripts.
- Add "random" transition type to `video.fade.*` in order to select a random
  transition each time.
- Add max parameter to drop data on buffer overrun with `input.gstreamer.*`.
- Add `bytes_per_page` parameter to ogg encoders.
- Add support for DTX in speex and opus, as well as VAD for speex.
- Localize some more parsing errors in files.

Bugfixes:

- Avoid deadlocks in harbor.
- Correctly flush lame encoder.
- Correct sequence operator when there is only one source.
- Handle relative URLs in http playlists.
- portaudio is now an active source.
- Avoid jack I/O lowering the volume.

  # 1.1.0 (04-03-2013)

  ** This version brings some new features as well as correcting bugs. **

New:

- Add support for GStreamer decoding, processing and encoding (%gstreamer
  format, v4l webcam input is now implemented using GStreamer).
- Add support for opus decoding and encoding.
- Add support for the shine encoder, which can efficiently work on architectures
  without FPU.
- Add support for automatically computing the duration of tracks in the
  "duration" metadata [LS-641]. It can be enabled with
  `set("request.metadata_decoders.duration",true)`
- Add support for frei0r video effects.
- Allow `%define`'d variables in encoding formats [LS-634], e.g.
  ```
  %define BITRATE 24
  %define STEREO true
  output.file(%mp3(bitrate = BITRATE, stereo = STEREO),"bla.mp3",s)
  ```

Enhancements:

- Taglib now reads all metadatas (even non-standard ones).
- Add a mode to automatically reload a playlist when the file was changed
  [LS-363,LS-523]. For instance, `s = playlist("~/Music",reload_mode="watch")`.
  Also, add `file.watch` to call a callback when a file is changed, with
  inotify support when present.
- Add support for FFMpeg as video converter, which you can use with
  `set("video.converter.preferred", "ffmpeg")`
- Add `back_time` argument to blank operators [LS-609].
- Add a metadata to override fade.final duration.
- MIME is computed at most once when extracting replaygain.
- Default samplerate converter is now "fast".
- BPM detection (bpm) now uses a callback.
- Add `clock.unify` to unify clocks of all sources from a list.
- Add `source_url` metadata to `input.http` streams.
- Improved error message when theora format is not supported.
- Add list.filter function.
- `video.add_image` can now take any image format as input.
- Add `mux_stereo`.
- Support for external decoders in streams.
- Move bugtracker to https://github.com/savonet/liquidsoap/issues

Bugfixes:

- Configure is now compatible with OCaml >= 4.0 and removed support for OCaml <
  3.11 [LS-625].
- Fix random memory access / memory leak when decoding AAC/MP4 files [LS-647].
- Correct resampling of wav files.
- Use the length for data indicated in header for wav files.
- `Argv.(0)` now returns the script name [LS-605].
- Liquidsoap should now operate fine when compiled with -noassert [LS-578].
- Better handling of inexistent MIDI channels.
- Video decoder now correctly handles videos from Icecast.
- Avoid visu.volume freezing Liquidsoap on shutdown.
- Fix a memory leak when decoding both audio and video in ogg [LS-636].
- More efficient handling of video converters, also fixes some crashes [LS-623].
- Have the soundtouch operator preserve tags [LS-621].
- Fix remaining time estimation in cross and `smart_cross`.
- Avoid deadlocks in harbor and `input.http`.
- Remove leftover files in configure [LS-567].
- Handle wav files with padded fmt headers.
- Handle end-of-stream when seeking mp3 with mad.

  # 1.0.1 (04-07-2012)

  ** This version brings bug fixes and minor enhancements over 1.0.0. **

Fixes:

- correct type for the "flush" parameter in `output.external()`
  thanks to Romaric Petion for pointing it out
- fix bug where MP3 encoder would discard initial ID3v2 rendering
- fix bug where `smart_cross()` would stop before the end of tracks,
  thanks to Edward Kimber for raising the issue
- load libraries in --interactive [LS-618]
- update examples, notably the installed radio.liq
  thanks to Emery Hemingway for noticing the problem
- generalize the types of `input.http()` and `input.harbor()` to allow variable
  content kinds, and also allow video for harbor [LS-601]
- `request.equeue()` now allows to remove requests from the primary queue
- fix compilation of lame dynamic plugin.

New:

- new values for metadata fields does not override old one anymore;
  use setting `request.metadata_decoders.override` to restore old behavior
- `stereo_mode` and `internal_quality` parameters for %mp3 encoder
- enable mad decoder for MP1 and MP2 in addition to MP3, create aliased
  configuration keys "decoder.file_extensions/mime_types.mad"
- support for CUE sheet playlists and metadata in M3U
- setting "decoder.taglib.force_mpeg" to force taglib to consider files as MPEG
- scripting builtins `getenv()`, `setenv()` and `environment()`
- scripting builtin `source.fallible()`
- harbor is now verb-oriented, supporting GET, POST, PUT, HEAD, DELETE, OPTIONS
- load DSSI plugins from environment variables and using `dssi.register()`
- also display the type of the whole expression when -i is passed
- generalized custom path support for facilitating standalone distributions
- and as usual, various improvements in the code, log and error messages, etc.

  # 1.0.0 (08-10-2011)

Finally, the 1.0.0 release! It brings several important fixes, but
also some nice novelties.
The most outstanding difference concerns `output.icecast()`: its restart
and `restart_delay` parameters are gone, replaced by a new `on_stop` handler
which is called on every error (failed connection or disconnection) and
returns the new restart delay. The `on_error` handler receives a string
describing the error which enables user-friendly reporting, adaptative
delays, etc.
Note that `on_error` defaults to `fun(_)->3`. which is equivalent to having
restart=true, restart_delay=3. in previous versions, NOT the same as the
former restart=false default. As a result, liquidsoap won't fail to startup
if an initial connection attempt fails.

Fixes:

- LS-532,527: avoid freeze after errors in streaming threads or source
  initialization routines
- LS-542: race condition in `playlist*()` breaking randomness
- LS-489: double expiration lead to illegal queue length and freeze of
  request-based sources
- Avoid multiple simultaneous reloading in `playlist*()`,
  thanks to Fabio Costa for his help on this one!
- Pass charset information to icecast server to avoid encoding bugs
- LS-555: timeout for icecast connection attempts
- LS-559: permanent stop after disconnection on Ogg streams
- LS-565: efficient and crash-free error handling in `input.http/harbor()`
  when the input stream has an invalid number of channels
- LS-431: proper handling of duration in `blank()` avoids abusive empty tracks
- LS-556: rework conversion operators, optimizations used to be unsafe & broken
- LS-574: silent MIDI synthesis operators
- LS-396: `drop*()`'s types reflect that they don't support variable arities
- LS-442: allow comments not terminated by newline at end of file
  New:
- `on_error` handler in `output.icecast()`, see above
- New msg param in %mp3 for marking frame headers, defaults to version string
- `output.file()`: new `on_close` parameter, may be used to write exact duration
- %mp3.vbr/abr for variable bitrate MP3, %mp3 is now a synonym of %mp3.cbr
- MP3 encoders now support ID3v2 tags
- `input.http()`: new "status" command
- LS-556: `mux_mono()` for adding a single audio channel into a stream
- `video.add_text()` using libgd (gd4o) for environments without X
  Dependency on graphics can be disabled (to work around erreneous detection)
- script language: add infix operator mod (patch by Fabio Costa)
- `delay()` now has an "initial" parameter
- LS-557: "server.timeout" setting can now be disabled by setting it to -1
- LS-532: `source.init()` for selective init with a way to handle errors,
  plus settings "clock.allow_streaming_errors" and "init.force_stat" (or
  --force-start on the command line) for easing dynamic uses of liquidsoap
  Enhancements:
- Panic crash to avoid frozen liquidsoap after duppy crashes
- Text-to-speech: festival and sox are now only runtime dependencies
- LS-475,516: better support for dynamic URL change in `input.http()`
- LS-484: display user-friendly error messages in interactive mode
- LS-308: use seconds internally in request sources, avoid overflow and
  display more user-friendly debug messages
- Cleanup `visu.volume()` and `video.vis_volume()`
- LS-573: replace " " by "\_" in identifiers to make them valid in the server
- Script syntax: unary minus now usable without parenthesis after semicolon
- Two generic queues by default, to avoid deadlocks in advanced situations
- Documentation, build & install system, etc.

  # 1.0.0 beta3 (05-08-2011)

- Feature: Added `of_json` to parse json data. Depends
  on json-wheel.
- Feature: Added file.exists and is_directory.
- Feature: Added timeout options for:
  telnet, harbor (server), input.icecast
- Enhancement: finer-grained timeout detection
  for `input.harbor` and `input.http`
- Fix: deadlock when disconnecting harbor users
  through server/telnet command.
- Fix: dynlink detection in native mode with old
  versions of ocaml
- Fix: deadlock when an exception is raised during
  startup while the clock is owned by a source
  (e.g. `input.alsa`). See LS-527 for more details.

  # 1.0.0 beta2.1 (07-07-2011)

- Fix: `playlist.safe()` was unusable in beta2, as a side effect of removing
  duplicate "timeout" parameter in `playlist()`.
- Minor enhancements to documentation, settings and reference.

  # 1.0.0 beta2 (04-07-2011)

This release introduces lots of fixes and cleanup, but also some new features.
Major novelties: support for fast seeking and cue points, FLAC and improved
AAC+ support, introduction of the liquidsoap yellowpages "flows",
plugin support and improved messages for scripting errors
Compatibility warning: `insert_metadata` has changed, and `clock.assign_new()`
should be used instead of `clock()` to avoid some of the new static checks

Decoders:

- New support for seeking and fast computation of durations in most formats
- New decoders: FLAC (native & Ogg) and images using Camlimages
- Fixes in Ogg decoding: LS-515 (loss of data) and LS-537 (segfault).
- Fix LS-337: periodical failures when decoding AAC(+) streams
- AAC(+): use new ocaml-faad with builtin mp4ff, easier to build
- New detection mechanism mixing extensions and MIME types (when available),
  with corresponding settings `decoder.file_extensions.<format>` and
  `decoder.mime_types.<format>`.
- Decoder order can be user-defined thanks to new settings
  "decoder.file_decoders", "decoder.stream_decoders" and
  "decoder.metadata_decoders".
- Indicate which decoder is used in the "decoder" metadata
- More helpful log for various errors
- Fix segfault with SdlImage image decoder

Encoders:

- New FLAC encoders %flac (native) and %ogg(%flac)
- New AAC+ 2.0 and vo-aacenc
- New settings to theora: keyframes make files much smaller!
- New settings for WAV encoding: headerless, samplesize.
- Fix segfaults with ocaml-aacplus
- Enhancement LS-441: filter metadata before encoding,
  based on the "encoder.metadata.export" setting.
- Rework infrastructure of encoded outputs to fit all formats, outputs
  and styles of metadata handling, file reopening (#386)

Harbor:

- New: `output.harbor()` which acts as a mini icecast server,
  accepting listeners directly. Encoding is shared among users,
  and is only performed when needed.
- New: ability to register HTTP GET/POST handlers to create simpler
  web services, using `harbor.http.register/remove()`.
- Make all settings local: port, user and password can be set independently
  for each `input.harbor()` source
- New: "metadata_charset" and "icy_metadata_charset" in `input.harbor()`
- Fix: race condition possibly leading to abusive "source taken" (LS-500)

Icecast:

- Add support for streaming native flac, only works when streaming to
  `input.harbor()`, not supported by actual Icecast servers
- Fix bugs in ICY protocol support (header parsing, user name)
- Use ICY metadata updates when streaming AAC(+)
- New: "encoding" parameter for `output.icecast()`, used for recoding metadata
  Defaults to "latin1" with shoutcast servers
- New: `icy.update_metadata()` function for manual updates
- Enhance default "song" metadata, avoiding " - " when unnecessary (#467)

Input/output:

- New experimental `input.v4l/v4l2()` for webcams
- New experimental `input/output.udp()` for unchecked UDP streaming,
  available with most formats (at your own risk)
- Restore `output.pipe.external()`, now called `output.external()`
- New parameters for most outputs and inputs (start, on_start,
  on_stop, fallible); cleanup and uniformize implementations (LS-365)
- New ALSA settings alsa.alsa_buffer, alsa.buffer_length and alsa.periods
  Setting periods=0 allows to not attempt to set the number periods,
  which is impossible on some devices
- New preference order in `input/output.preferred()`: pulseaudio, portaudio,
  oss, alsa, ao, dummy

Operators:

- New: support for cue points with `cue_cut()`
- Change `insert.metadata()` which is now more script friendly,
  returning an insertion function rather than register a server
  command. The old functionality is available as `server.insert_metadata()`.
- New: `rms()` operator for getting RMS of a stream, and `server.rms()` which
  makes this information available as a server command.
- New: `track_sensitive` mode for blank detection operators
- New: `playlist.reloadable()` for playing a list once, with a command
  for restarting it.
- Remove `id.*()` which can be replaced by type annotations

Scripting API:

- New: OSC support through `osc.bool()`, `osc.float()` and `osc.float_pair()`
- New: JSON export `json_of()`
- New: `http.get()` and `http.post()`
- New: `url.encode/decode()`, `base64.encode/decode()`
- New: `string.recode()` for charset conversions using camomile
- New: `notify_metadata()` and `osd_metadata()`, suitable for use with
  `on_track()` and `on_metadata()`
- New: `request.metadata()` for getting a request's metadata
- New: `string.length()`
- Enhance `log_clocks()` with parameter for delaying startup
- Enhance `get_clock_status()` with "uptime" reference time

Server interface:

- Print the playlist's URI when calling `<playlist>.uri` without an
  argument.
- Enhance `<queue>.ignore` now works also in the primary queue
- New command for changing the URL of an `input.http()`, ref #466. The
  command is `<id>.url` and it needs a restart (`<id>.stop`, then start)
  to take effect.
- Fixed double registration of server commands which resulted in broken
  "help" command (LS-521)

Script language:

- Option "-i" doesn't show types for pervasives anymore
- Pretty printing of types (LS-474) with indentation, also used
  in the documentation
- Enhanced type errors (LS-459): no more traces, only the relevant
  part of types is displayed, plus a few special friendly messages.
- Enhanced static checks (LS-123) thanks to the introduction of the
  `active_source` subtype, for unused variables and ignored values
  This should avoid common errors, help troubleshooting
  If needed, advanced users can work around errors using --check-lib
  and --errors-as-warnings

General:

- Do not attempt to install "daemon" files if user/group have not been
  set, unless forced using "make INSTALL_DAEMON= install"
- Fix several core "source protocol" bugs, causing assert failures and
  other crashes: LS-460 (source becomes not ready without operator knowing)
  #403 (information about being ready is not precise enough)
- Fix incorrect image accesses (LS-430) by introducing a safer VFrame API
  Applies to most video operators ( `video.fade()`, `video.text()`, effects...)
- Cleanup resource (de)allocation, which is becoming critical with dynamic
  reconfigurations (e.g., dynamic output creation, `source.dynamic()`)
  Enforce that server commands are always deallocated (LS-495)
  Attempt to stop sources when initialization fails, so they cleanup as
  much as possible (LS-503)
  Avoid deadlocks upon crashes in IoRing-based operators
  Share code for stoppable feeding threads, use it in `input.harbor()`
  Avoid useless initialization of SDL systems
- Dynamic loading of lame and aacplus libraries, making it possible to
  ship them as separate binary packages. This is particularly useless
  for non-free libraries and for those that depend on X (e.g., SDL)
  which is often undesirable on servers
- Support for separate compilation of most optional features as plugins
  Use `--enable-<feature>-dynamic-plugin` in ./configure and
  --dynamic-plugins-dir in liquidsoap
- Better Win32 support, more version checks, separate compilation of
  C files and compliance with Debian's OCaml standards
- Externalize many audio and video functions in the new ocaml-mm library
  Factorize and optimize various conversions
- Rewrite harbor and server code using the new Duppy monad,
  introducing camlp4 into the build system
- New regression tests (make test)
- More user-friendly exception printing
- Avoid problems preventing backtrace printing

Miscellaneous:

- Update liguidsoap, make microphone input optional (LS-496)
- Do not crash upon charset-recoding failures [LS-473]
- Fix in `source.dynamic()`: missing source re-selection [LS-354]
- Avoid deadlock on startup in daemon mode [LS-229]
- Fixes in LADSPA and SDL causing early freezing of Frame parameters.
- Fullscreen mode for `output.sdl()`
- Fix: SIGPIPE used to cause crashes (LS-53,LS-287)
- Fix: `video.volume()` could crash upon some end-of-track situationos
- Fix: properly escape filenames in external file duration methods
- Rework timeout management for various sockets (notably icecast & harbor)
  Set nodelay, remove `TCP_*TIMEOUT` [LS-508,LS-509]

  # 1.0.0 beta1 (06-09-2010)

This beta version introduces two major new features: heterogeneous stream
types and clocks.

New:

- Different sources can carry different types of content.
- Encoding formats have been introduced to help infer stream content types.
  This brings static checking for bounds in encoding parameters.
- Introduce conversions between stream contents (mono, stereo, drop
  audio, video, etc) and muxing.
- Allow explicit type annotations in scripts.
- Introduce clocks, cleanly allowing for the coexistence of different
  time flows, and avoiding inconsistencies that can result from it.
  Soundcard I/O and cross-based operators notably make use of it.
- Remove root.sync, replaced by attaching a source s to clock(sync=false,s).
- Enable dynamic source creation and `source.shutdown()`.
- Extend and adapt MIDI and video operators.
- Introduce purely metadata streams (audio=video=midi=0 channels) and
  metadata stream decoder.
- Support WAV streams in `input.http/harbor()`.
- Introduce static image decoder using SDL image.
- Remove bound of request identifies (RID).
- Experimental: `source.dynamic()` for advanced dangerous hacking.
- Make path relative to script in `%include "PATH"`, and introduce
  `%include <...>` where path is relative to liquidsoap library directory.
- Add `channels_matrix` parameter to `output.ao()`.
- Add `on_(dis)connect` hooks in `input.harbor()`.

Cleanup and fixes:

- Lots of cleanup and fixes as with all major code rewriting.
- Optimize video and stabilize it a little bit... still not perfect.
- Rewrite stream and file decoding API, as well as file format detection.
- Enhance shutdown and error message reporting, notably for icecast
  and request-based sources.
- Avoid quasi-infinite loop in failed request resolving.

  # 0.9.3 (04-09-2010)

This release is a bugfix of the latest snapshot (0.9.2).
It will be the last bugfix before 1.0.

Bugs fixed:

- Add "audio/mpegurl" to the list of mime-type for basic playlist parsing.
- Decode arguments passed to `input.harbor`.
- Use Camomile framework to try to recode arguments and user/password
  passed to `input.harbor`.
- Support Theora 1.1 API via ocaml-theora 0.2.0.
- Fixed `input.lastfm`.
- Fixed SDL output.
- Allow magic file type detection to follow symlinks.

  # 0.9.2 (29-10-2009)

This release is a SNAPSHOT of upcoming features. It also contains several
important bugfixes. As a snapshot, it contains experimental or unpolished
features, and also breaks compatibility with previous versions.
You should in particular notice the two "New" items below:

- `random(strict=true)` is now called `rotate()`;
- request sources (`playlists`, `request.*`) have a new queuing behavior,
  check the doc (request-sources.html) or revert to conservative=true.

Bugs fixed:

- Ogg encoder now muxes pages according to their ending time.
- Support "ogg/audio" and "ogg/video" mime types for HTTP ogg streams.
- Changed external encoder's "restart_encoder" to the more relevant
  "restart_on_new_track". Added optional "restart_after_delay"
  to restart the encoder after some delay. Also completely rewrite
  stop mechanism. Stop operation now waits for the encoding process
  to finish, allowing proper restart_on_new_track like for ogg encoded
  data.
- Factorized buffered I/O code. Also added a blocking API, which avoids
  "no available frame" and "reader not ready" messages and audio glitches.
  It enforces that `root.sync` be deactivated for these sources, such that
  synchronization is done by the source. (#203)
- Factorized file decoding code.
- Fixed reversed order when parsing playlists using `playlist.parse()`.
- Avoid bad crashes when resources lack, e.g. no more memory.
- Tighten and enforce the inter-source protocol.
- All outputs: fix the autostart server command. With the former code,
  a modification of the autostart parameter was only taken into account
  one start/stop cycle later.
- `on_blank()`: fix a bug that prevented the first call to on_noise.
- Fixed estimated remaining samples on ogg files, fixes issues with
  operators relying on this value, in particular `crossfade()` and
  request-based sources when operating in non-conservative mode.
- Fixed socket descriptor leak in `input.http`. (#318)
- Fixed deadlock at init when an exception was raised at wake_up
  phase. (#319)
- Fix `delay()` which could sometimes incorrectly declare itself ready,
  and thus try to get some data from its unavailable input source.
- Bug in queue duration estimation led to infinite feeding of the queue,
  until all request IDs are taken.
- Several fixes enforcing clean (non-deadlocking) sleep/shutdown.

New:

- The operator `rotate()` replaces random(strict=true), and `random()`
  does not have a strict parameter anymore.
- Switch to new behaviour in request-based sources.
  Use conservative=true to get to the old behaviour.
- `on_blank()`: provide an `on_noise` handler.
- `playlist*()`: add server commands for reloading the playlist and changing
  its URI.
- Fallible outputs: all outputs now have a fallible mode, in which
  they accept fallible sources, and automatically start/stop when the child
  fails to stream.
- EXPERIMENTAL ogg/dirac encoding support.
- `output.*.aacplus()`: native outputs encoding in AAC+ using ocaml-aacplus.
- Switched to a custom implementation of the various shout protocols.
  It notably allows arbitrary content-type settings which enables
  AAC+ streaming. Enabled wrappers for external encoders for AAC+ format
  aacplusenc (#220 and #136). Also added custom IRC, AIM and ICQ headers
  to shoutcast wrappers (#192).
- Harbor now supports Shoutcast/ICY headers properly. This allows in
  particular the use of any supported data format for the source client.
- Harbor sources now also consider "song" field when updating metadata.
- Added built-in support for m4a audio files and metadata.
  Made external support optional through enable_faad.
- Added optional resampling for output.external operators (#273).
- Added optional host and port parameters for audioscrobbling
  submissions. Allows to use alternate systems, such as libre.fm
  of jamendo's compatibility API. Added nowplaying submission,
  and various wrappers, including a full submission process
  (now playing at beginning, submit some times before the end).
- New variables in the script language: `liquidsoap.version` and
  `configure.{libdir,pidfile,logdir}`.
- Added built-in support for replay_gain, through the replay_gain protocol
  (enabled by default) and the replay gain metadata resolver (to be enabled
  using `enable_replaygain_metadata()`). (#103 & #317)
- Reverse DNS operations can be disabled using settings keys
  `server.telnet.reverse_dns` and `harbor.reverse_dns`.

Experimental:

- MIDI: file decoding, synthesis, virtual keyboard.

Removed:

- RTP input and output.
- Removed decoders using ocaml-natty. Slow, unmaintained and superseded
  by the mplayer decoder.

  # 0.9.1 (18-06-2009)

Bugs fixed:

- Fixed request task not ending properly for request-driven sources (playlist,
  request.queue, request.equeue). Fixes a problem reported when reloading
  an empty playlist multiple times. (#269)
- Fixed math.h usage in rgb_c.c.
- Fixed append operator. (#284)
- Fixed OSS compilation for non-linux systems.
- Disconnect idle harbor sources after timeout has expired.
  Thanks to Roman Savrulin for reporting and fixing !
- Taglib metadata resolver is only used on files decoded
  by the MP3 decoder.

New:

- Get a node's striping status when stripping
  blank with `strip_blank` (#260).
- `on_connect` function for `input.harbor` now receives the
  list of headers given by the connected source (#266).
- Added `on_end` operator, to execute a handler when a track ends.
- Added estimated remaining time in the queue length for request-driven
  sources (`request.{equeue,queue}`, `playlist`). This allows these sources
  to prepare less files in advance. In particular, primary queue may
  only contain the file currently played. Default behaviour has been
  set to the old behaviour, and a conservative option has
  been added to switch to the new behaviour. New behavivour
  will be the default for the next release.
  fixes #169, references #146

  # 0.9.0 (01-03-2009)

Bugs fixed:

- Fixed byte swapping function.
- Fixed unix server socket failure (#160).
- Fixed mp3 audio glitches when decoding
  files with picture id3v2 tags using ocaml-mad (#162).
- Fixed liquidsoap crash on weird telnet and harbor input (#164).
- Fixed `request.queue()` not considering initial queue on wake-up (#196).
- Fixed source leak in `append()`.
- Fixed `after_output` propagation in the transitions of switches (#208).
- Fixed compilation for ocaml 3.11 (#216).
- Fixed (again) Vorbis mono output (#211).
- Avoid crashing on broken symlinks when parsing directories (#154).
- Use random temporary file in liGuidsoap.
- Fixed liGuidsoap to use the (not so) new metadata field initial_uri.
- Fix bugs in the life cycle (sleep/wake_up) of queued request sources,
  which made `say_metadata` unfunctional.
- Remove buggy unbuffered `{input,output}.jack`. (#231)
- Fixed audio information sent to icecast. (#226)
- Fixed global reset starting inactive sources. (#227)
- Fixed shoutcast source initial answer in harbor. (#254)
- Fixed frame and metadata duplication in cross operators. (#257)
- Fixed several sources (outputs, external streams) that were not going
  to sleep correctly.

Changes:

- Warning: `interactive_float()` is now `interactive.float()`.

New:

- Compatible with OCaml 3.09.
- Faster shutdown.
- Rewrote ogg decoding, for files and streams.
- Support for ogg skeletons, currently only used for theora.
- Cleanup icecast class hierarchy and restart mechanism, especially
  with respect to the encoder.
- Support for breaks and metadata in generators.
  As a result, `input.http()` and `input.harbor()` now fully support them.
  See new_track_on_metadata parameters there.
- Switch operators (fallback,random and switch) can now replay the metadata
  of a source that has been left in the middle of a track.
- New `force_mime` parameter for `input.http()`.
- New `insert_missing` parameter for `append()`.
- Multi-line strings in liq scripts, with a caml-like syntax.
- Slight modification of the scripting syntax, handling unary minus.
- Added `user_agent` parameter for `input.http` and `input.lastfm`
- Added speex support for files and HTTP streams.
- Added EXPERIMENTAL support for AU, AIFF and NSV mp3 audio files
  using ocaml-natty.
- Added "prefix" parameter to the playlist operators. Allows to prefix
  each uri in order to force resolution through a specific protocol,
  like replaygain: for instance. (#166)
- Support for external processes as audio encoder:
  - Added output.icecast.lame to output to icecast using the lame binary.
  - Added output.icecast.flac to output to icecast using the flac binary.
  - Full generic support awaits some changes in libshout.
- Support for external processes as audio stream decoder:
  - Added `input.mplayer` to stream data coming from mplayer.
- Support for external processes as audio file decoder:
  - Added support for flac audio files using the flac binary.
  - Added support for m4a files using the faad binary.
  - Added optional support for mplayer, enabled with `enable_mplayer()`.
- Support for external metadata decoders.
- Support for generic authentication function in harbor,
  also available for ICY (shoutcast) clients.
- Added optional support for the samplerate library,
  and dynamic configuration of resampling routine.
- Added `lag()` operator, which delays a stream by a constant time.
- Initial support for PulseAudio.
- Added experimental support for `{input,output}.marshal`, allowing
  raw communication between various liquidsoap instances.
- Added optional alternatives to store buffered audio data:
  - raw: in memory, s16le format
  - disk: on disk, several big files
  - disk_manyfiles: on disk, a lot of small files
    See documentation for more details.
- Added EXPERIMENTAL video support:
  - Support for ogg/theora file input.
  - Support for ogg/theora file and icecast output
  - Support for SDL output.
  - Optional support for ocaml-gavl as video converter.
  - Support for video in _some_ existing operators, including switches, `add()`,
    metadata/track manipulations.
  - Added operators: `video.fade.*`, `video.fill`, `video.greyscale`, `video.image`,
    `video.invert`, `video.lomo`, `video.noise`, `video.opacity`, `video.opacity.blur`,
    `video.rotate`, `video.scale`, `video.sepia`, `video.text`, `video.tile`,
    `video.transparent`, `video.volume`.

  # 0.3.8.1 (11-08-2008)

- Fixed metadata propagation during default transition
  in smart_crossfade
- Changed transition evaluation order in smart_crossfade
- Fixed transition function in smart_crossfade

# 0.3.8 (30-07-2008)

Bugs fixed:

- Vorbis mono output is now working
- Fixed parameter parameter description
  in the documentation
- Propagate new delay in add_timeout
- Fixed inter-thread mutex lock/unlock in playlist.ml
- Fixed "next" playlist command
- Fixed race conditions in request_source.ml feeding task
- cross/smartcross: raise the default for inhibition as
  setting it to exactly duration is not enough
- Don't fail when $HOME is not set
- Fixed metadata update in `input.harbor` with icecast2 source protocol
- Fixed shutdown function. Fixes #153
- Fixed `input.oss`. Liquidsoap now works with OSS4 ! Fixes #158

New:

- Added a hook to execute a function when playlist.once ends
- Enhanced smart_crossfade
- Added string.case and string.capitalize
- New "exec_at" operator, to execute a function depending on a given predicate
- Added script example in the documentation to listen to radio Nova and get
  the metadata from a web page.
- Changed parameters name in fallback.skip to reflect who are the fallback
  and input source.
- Added a dump file parameter to `input.harbor`, for debugging purpose.
- Added an auth function parameter to `input.harbor` for custom
  authentifications. Fixes #157
- Added "primary_queue" and "secondary_queue" commands to request.queue and
  request.equeue sources. Also set the metadata "queue" to either "primary"
  or "secondary" for each request in the queue. Documented it too.
- Insert latest metadata for a node of a switch source when switching
  back to it in a middle of a track.
- Added a 'conservative' parameter to cross, smilar to the one in smartcross.

Internal:

- Enhanced liqi documentation parser to build the website.

  # 0.3.7 (03-06-2008)

Bugs fixed:

- Now works on FreeBSD and hopefully other unices that are stricter than
  Linux about Mutex usage.
- `input.http()` now has a `bind_address` parameter.
- Harbor socket now has a timeout for lost connections.
- `smartcross()` is now more compliant with the inter-sources protocol,
  fixes several "get frame didn't change the buffer" bugs.
- Ogg packeting bugs.
- Buffering policy in `input.http/harbor()`.
- No "." in IDs and labels.
- Resources: FD leaks, useless threads (threads leaks?) in `input.http()`.
- `fade.out()` used to run into infinite loops when the delay was 0.

New:

- New documentation system and website.
- Self-documenting server with a more helpful "help" command.
- Moved to duppy: less threads, lighter load, and an configurable scheduler.
- Moved to Taglib for more reliable access to MP3 metadata.
- MIME types, notably for playlists and MP3 files.
- New Jack support. The old one has been renamed to `in/output.jack.legacy()`.
- Harbor: per-mount passwords and the stop command to kick a source client.
- Official Last.FM client.
- Metadata is no more punctual but interval-based, which suppresses some
  surprising behaviours.
- Perfected daemon behaviour.
- All `output.file.*()` now have the features that used to be only in
  `output.file.vorbis()`, notable re-opening. Added %w to the strftime-like
  format codes allowed in their filename parameter.
- Add `clear_metadata()` and `map_metadata()`. Now, `rewrite_metadata()` is a simple
  specialization of `map_metadata()`, written in utils.liq.
- Dynamic amplification factor in `amplify()`, e.g. useful for replay gain.
- Lots of new functions in the scripting API: for lists, requests, system
  interaction, shutdown, command-line parsing, scripted server commands, etc.

As always:

- code cleanup, style, etc.

  # 0.3.6 (17-12-2007)

Bugfix release:

- Close Http socket
- Add http socket timeout
- Close playlist file after reading its content
- Fix http redirect support for lastfm files
- Fix file open leak in camomile support
- Fix playlist uri ending with "/"

  # 0.3.5 (08-11-2007)

Bugfix release:

- Fixed #57: scpls and mpegurl playlist parsing
- Fixed #46: Late cross-scripts bindings

  # 0.3.4 (25-09-2007)

Notation: "-" stands for a change, "+" for an addition.

- Language
  - Support for polymorphism, subtyping and basic ad-hoc polymorphism,
    which allows a much simpler API, notably for maths and serialization.
  * Added `interactive_*()` for mutable values.
  - The right syntax for settings is now set("var", value) and can be used
    anywhere in the scripts.
  - The volume parameters of most operators are now in dB.
  - Many builtin functions added.
  - Nicer type error messages.
- Sources
  - Added `input.lastfm()` to relay last.fm streams.
  - Added `input.harbor()` to received Icecast2 source streams.
  - Added `noise()` to generate white noise
  * Reimplemented playlist support, added various xml and text formats.
  * Added mpd protocol to find files using mpd.
- Operators
  - New effects: `compress()`, `flanger()`, `pan()`.
  - New filters: `filter.fir.*()`, `filter.iir.*()`, `filter.biquad.*()`, `comb()`.
  - Added support for LADSPA effects.
  - Added `eat_blank()` to remove blanks.
- Outputs
  - Added non-default restart option for `output.icecast.*()`.
  - Added the possibility to tweak some settings at runtime.
  - Split `output.icecast.vorbis()` into `output.icecast.vorbis.*()` to distinguish
    between encoding modes -- and similarly for output.file.vorbis and mp3.
  - Better handling of Icecast disconnections.
- IO
  - Added portaudio support.
  * Jack support is now somewhat working.
- As usual, lots of bug fixes, careful polishing & much more...

  # 0.3.3 (06-06-2007)

- Major cleanup of the core stream representation; moved to float arrays,
  removing several back-and-forth conversions and enhancing the perfs a lot;
  reviewed all sources and operators, made many minor enhancements btw.
- Lots of sound processing operators: compand, compress, normalize,
  pitch, bpm, soundtouch, saw, square, etc. Add more shapes to `fade.*()`.
- New track processing operators: insert_metadata, on_track.
- Smart cross: allows to select a transition based on the volumes around the
  end-of-track.
- Support for AAC encoding/decoding.
- Several fixes to output.icecast.mp3 in order to support shoutcast servers.
- Automatic format recognition for `input.http()`, support for playlists.
- OSS I/O.
- Unbuffered ALSA I/O for low latency.
- Server interface via UNIX domain sockets.
- Better output.file.vorbis with support for re-opening the file, appending,
  interpolate strftime format codes, etc.
- Add pre-processing and math primitives to the language, new `_[_]` notation for
  `assoc()`, ruby-style anti-quotation `("..#{..}..")`, `add_timeout()`, `execute()`,
  `log()`...
- Ability to tweak the internal PCM stream format.
- Classify sources and operators in categories for more structured doc.
- Started a few visualization operators, text and graphics based.
- Several bug fixes: request leaks, sine frequency, switch, etc.

  # 0.3.2 (16-03-2007)

- New portable output to speakers using `libao()`.
- Updated liGuidsoap to use it until ALSA gets enhanced.
- Implemented a decent estimation of the remaining time in a track.
- Added the `cross()` operator allowing cross-fading.
- Generalized `say_metadata()` into `append()` and `prepend()`.
- Per-track settings for `cross()`, `fade.*()`, `prepend()` and `append()`
  using requests' metadatas.
- Implemented `input.http.mp3()`, including support for icy metadata.
- New `pipe()` operator which allows one to filter the raw audio through an
  external program. However, sox and other common tools aren't suitable for that
  because they don't flush their output often enough.
- New `on_blank()` operator for calling a callback on excessive blanks.
- Restart outputs on insane latencies.
- Type checkings for settings.
- Setting for not starting the internal telnet server.
- Now handles old and new versions of Camomile correctly.
- Internal fixes and polishing (switches' cached selection, empty tracks..)

  # 0.3.1 (17-11-2006)

- More standards-compliant tarball
- Generate doc with locally built liquidsoap
- Try to cope with ill-formed mp3
- Updated for newer versions of Camomile
- So-called "strict" random-mode

  # 0.3.0 (27-08-2006)

- Many minor and major fixes at every level!
- Conversion of metadata to UTF8.
- Got rid of too many threads by scheduling all download tasks
  in a single thread, and handling all of the server's clients
  in another single thread.
- Simplified the time interval syntax and integrated it to the script language.
- New protocol: Wget for FTP, HTTP and HTTPS.
- Ability to define a new protocol from the script,
  typically using an external app/script for resolution, such as bubble.
- Ability to use an external app/script for dynamically creating requests.
- New `on_metadata` operator for performing arbitrary actions (typically
  a call to an external script) when metadata packets occur in a stream.
- MP3 encoding, to file or shout.
- API renamings and simplification.
- Supports transition, as functions of type (source,source) -> source
  in all switching operators: schedule, random, fallback.
- Restart icecast2 outputs on failures.
- Major changes to the scripting language which is more uniform and flexible,
  has functions, a helpful (?) type inference, and a simple Ruby-like syntax.
- Timing constraints and synchronization are managed by Root
  in a centralized way, no more by the many outputs which need it.
- Audio decoding is no more based on file extensions,
  but on the existence of a valid decoder.
- Added the equeue operators which allows interactive building of playlists,
  supporting insertion and moving of queued requests -- queue only allows
  you to push requests and cancel queued requests.
- A Python-Gtk GUI for controlling operators, or more specifically as a console
  for creating your live show -- to be updated, still unstable.
- Alsa input and output.
- Blank detection, automatically skips on too long blanks, or strip them.
- Http ogg/vorbis relay, the way to relay live shows.
- Interactive mixer.
- The request system was mostly rewritten to really fulfill its specification.
- The server is no more associated to the queue operator but is now something
  external, in which all operators can plug commands. This much more logical
  design lead to many more interactive controls. The syntax of command outputs
  was also simplified for easier automated processing.
- Dynamic loading of plugins.
- Outputs are now operators. It makes it possible to output different streams
  in a single instance of Liquidsoap, which RadioPi needed. As a consequence
  we removed the restriction that one source must have at most one father,
  without any extra burden for the user.

  # 0.2.0 (20-04-2005)

- Proper initial release.

  # 0.1.0 (2004)

- Release for academic demonstration, not functional.
