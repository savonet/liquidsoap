1.5.0 (unreleased)
=====

New:

- Added `output.file.dash.ffmpeg`.
- Added LV2 support (#906).
- Added `string.nth` (#970).
- Added `string.binary.to_int` (#970).
- Added `file.ls` (#1011).
- Added native id3v2 tag parser, as well as associated function `file.mp3.tags`,
  `file.mp3.parse_apic` and `file.cover` (#987).
- Added `lists.exists` and `list.for_all`.
- Use a pager to display long help results (#1017).
- Added `list.init`.
- Added `list.ind`.
- Added `request.id`.
- Added a profiler for the language. It can be enabled with `profiler.enable` and
  the results are obtained with `profiler.stats.string` (#1027).
- Added `gtts` protocol to use Google TTS (#1034).
- Added `liquidsoap.executable` to get the path of the currently running
  Liquidsoap.
- Added `source.dump`.
- Added `synth` protocol (#1014).
- Added support for `srt.enforced_encryption` setting.
- Added support for prometheus reporting (#1000)
- Add `validate` parameter to `register`, which allows to validate a value
  before setting it (#1046, @CyberDomovoy)

Changed:

- Implemented per-frame clock synchronization mechanism, should allow for more
  advanced flexibility when working with source synchronization while keeping
  the default safe behavior. (#1012)
- Switch to YUV420 as internal image format, much more efficient (#848).
- Use bigarrays for audio buffers (#950).
- Simplified `add` behavior, also fixing an clock issue (#668).
- Switch to more efficient callback API for decoders (#979).
- Use system pagesize for buffer allocation (#915).
- Use new Strings module in order to avoid concatenations (#984).
- Native Liquidsoap implementation of list functions (#920).
- Added `fallible` option to `single` operator.
- Changed `request.queue` into a Liquidsoap implementation (#1013).
- Removed `request.equeue`, such a feature could be re-implemented in
  Liquidsoap, see `request.queue`.
- The `playlist` operator is now fully implemented in Liquidsoap (#1015).
- Removed `playlist.once`, its behavior can be achieved by passing `"once"` to
  the `reload_mode` argument of `playlist.once` (#1015).
- Removed `playlist.merged`: it is not that useful and can be achieved easily
  with `merge_tracs` on a `playlist` (#1015).
- Deprecated `playlist.safe` (#1015).
- Renamed `add_timeout` to `thread.run.recurrent`, added `thread.run` variant,
  renamed `exec_at` to `thread.when` and renamed `mutexify` to `thread.mutexify`
  (#1019).
- Changed the weights of `add` to float (#1022).
- Renamed `which` to `file.which`.
- Change `blank()` duration semantics to mean forever only on negative values.
- Get rid of numbering of universal variables (#1037).

Fixed:

- Fix implementation of recursive functions (#934).
- Fix opam install error with some bash-completion configuration (#980).
- Make `blank()` source unavailable past is expected duration (#668).
- Fixed implementation details with `cross` operator.

1.4.0 (29-09-2019)
=====

New:

- UTF8 parsing!
- Added support for tuples: `x = (1,"aa",false)` (#838)
- Added support for deconstructing tuples: `let (z,t,_) = x` (#838)
- Added `input.{file,harbor}.hls` to read HLS stream (#59, #295, #296).
- Added `output.hls` to natively stream in HLS (#758).
- Added `%ffmpeg` native encoder, only for audio encoding for now (#952)
- Added ffmpeg-based stream decoder, limited to mime type `application/ffmpeg` for now.
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
- Added `log.{critical,severe,important,info,warning,debug}`. Use aliases in code as well (#800, #801, #802)
- Added `sleep` function.
- Added `mkavailable` function.
- Added `fade.skip` function. (#804)
- Added `video.external.testsrc` function.
- Added `video.frame.*` and `audio.samplerate`.
- Added `input.external.ffmpeg` and `output.external.ffmpeg`.
- Added `output.youtube.live.ffmpeg`.
- Added `output.file.hls.ffmpeg`.
- Added `reopen` telnet command in `output.external`.
- Enabled external decoders in windows (#742)
- Added support for bash completion.
- Added `video.add_text.native`.
- Added `configure.bindir`
- Added `for` and `while` loop functions.
- Added `list.case`.
- Added `metadata.string_getter` and `metadata.float_getter`.
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
- Removed `cross`/`crossfade` operators, superseeded by
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
- `list.assoc` and `list.remove_assoc` require an ordered type as first
  component.
- Renamed `quote` to `string.quote`.
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

1.3.7 (09-04-2019)
=====

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

1.3.6 (23-01-2019)
=====

Fixed:

- Fixed smart_crossfade transitions skipping data after track marks. (#683,
  #652)
- Fixed `input.pulseaudio` parameters.
- Fixed crash when copying frame content (#684)

1.3.5 (25-12-2018)
=====

New:

- Added a bunch of base mathematics primitive, `exp`, `log`, `cos`, `sine`, ...
- Added `"extinf_duration"` to parsed `#EXTINF` metadata.

Fixed:

- Fixed inotify watch semantics (#677)
- Enhanced `#EXTINF` parsing in ambigious cases (#625)
- Fixed `output.youtube.live` (#630)
- Make sure server writes are synchronous (#643)
- Fixed crash when loading some frei0r plugins (#435)
- Fixed compilation with `osx-secure-transport`
- Fixed invalid opus stream generated when no data was ever encoded (#180)

1.3.4 (10-09-2018)
=====

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
- Added `encoding` parameter to `output.shoutcast` to allow alternative string encoding for metadata updates (#411)
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

1.3.3 (14-10-2017)
=====

New:

- Added `on_change` to `register`
- Added IPv6 support for `input.harbor`. (#491)
- Added `time`, `localtime` and `gmtime` to help with time-predicates (#481)
- Added `on_start` to execute callback when liquidsoap starts.
- Added `enable_external_ffmpeg_decoder` to enable ffmpeg-base external decoder.
- Added `"decoder.external.{ffmpeg,ffprobe,flac,metaflac,faad,mpcdec}.path"`
  configuration settings.

Changed:

- Renamed secure transport harbor key paths to: harbor.secure_transport.*
- Renamed secure transport I/O to: {input,output}.harbor.secure_transport.
- Added `.wma` to `gstreamer` file decoder file extensions (#483)

Fixed:

- Fixed memory leak in `output.icecast` connection method. (#490)
- Fixed `mutexify`
- Make sure that metadata are always passed in increasing position order in `map_metadata` (#469)

1.3.2 (02-09-2017)
=====

Changed:

- Removed `kick` telnet/server command, duplicate of `stop`.
- Support `replaygain` for mp3 files, thanks to @d4h3r0 (#460)
- Implement `input.harbor.ssl` using SecureTransport for OSX.

Fixed:

- Fix scheduler loop causing high CPU usage when using Process_handler without
  some of the default callbacks. (#475)
- Revert `wait_for` implementation to pre-`1.3.0`, using a custom `select` loop (#453)
- Handle mime-type arguments in input.harbor streams. (#456)
- Tell ocaml to use the same C compiler at build and link time. Fixes build on
  FreeBSD when using C++-based bindings such as taglib. (#465)
- Accept any capitalization of HTTP(S) as regular HTTP URL (#464)
- Fix compilation with osx-secure-transport enabled.
- Fix deadlock calling logging functions from within `Gc.finalise` (#609)

1.3.1 (28-05-2017)
=====

New:

- Allow any tags allowed in `"encoder.encoder.export"` settings in vorbis
  streams (#418)
- Allow `"audio/mp3"` mime-type for mp3 in file resolution protocol. (#451)

Fixed:

- Fixed `run_process`, `get_process_lines`, `get_process_output` when compiling with
  OCaml <= 4.03 (#437, #439)
- Calls to wait_for while the scheduler isn't running (#442)
- Revert default handling of environment in run_process, get_process_lines,
  get_process_output to passing calling process' environment by default.

1.3.0 (27-04-2017)
=====

New:

- Added support for recursive functions (#406)
- Add peak and peak.stereo operators (#364)
- Change `track_sensitive` parameter to a boolean getter (fixed value or
  anonymous function).
- Add SSL support to the varous harbor operators, either via openssl or OSX's
  SecureTransport.
- Add optional "dj" and "next" metadata for Shoutcast v2, wrap "dj" value in a
  callback in output.shoutcast (#370, #388)
- Allow partial parsing of JSON objects in of_json.
- Generalize list.assoc to allow default values. Legacy code must be updated:
  list.assoc(k,l) -> list.assoc(default="",k,l)
- Generalize list.hd to allow default values. Legacy code must be updated:
  list.hd(l) -> list.hd(default="",l)
- Allow to pass a default to list.nth. Legacy code must be updated:
  list.nth(l,pos) -> list.nth(default=<..>,l,pos)
- Added on_offset to execute a callback at a given offset within a source's tracks.
- Added mutexify to protect a function from being called concurrently.
- Added request.log to get log data associated with a request
- Added overlap_sources to rotate between sources with overlapping tracks.
- Added replay_metadata to input.harbor()
- Added \<char code> syntax for strings (#368)
- Added string.sub
- Added run_process to run a process with optional environment and return (stdout,stderr,exit_status)
- Added add_playlist_parser to register new playlist parsers
- Added optional static parameter to add_protocol
- Added file.temp to create fresh temporary filename
- Added process: protocol
- Reimplemented curl-based fetch process using process:
- Added s3:// protocol to fetch files from AWS S3 using the AWS CLI.
- Added polly: protocol to enable speech synthesis using AWS polly. Generated files are mono so make sure you use audio_to_stereo().
- Added youtube-dl: protocol to resolved requests using youtube-dl
- Added which() to find an exectuable within the $PATH
- Added register() to allow to register new configuration settings

Changed:

- Reverted default value for --error_as_warnings option, renamed to --strict.
- Moved say: protocol registration to utils.liq.
- Moved get_process_lines and get_process_output to utils.liq, added optional env parameter
- Set conservative=true by default in cross() and smartcross()

Deprecated (can be removed in any future version):

- Dynamic plugins compilation, deprecated in favor of opam rebuild mechanism.

Removed:

- aac and aacplus encoders, removed in favor of fdk-aac.
- dirac/schroedinger video encoder: obsolete, abandoned upstream.
- force_mpeg option in taglib metadata decoder. Has not been used for years and allows to decouple taglib code from the mad decoder.

Bugfixes:

- Fix negative seek (#390)
- Prevent flows metadata updata from stalling sources (#377)
- Add revdns setting for telnet, set all revdns default to false (#372)
- Fix icy metadata in output.harbor (#358)
- Fix missing first line of headers from icy clients in input.harbor (#380)
- Fix timestamp in some logged output (#395)
- Fix crash in external (download) protocol.
- Fix fade.{in,out} metadata handling for new fade duration and type.
- Compute normalization regardless of child sources ready status in add() to
  avoid unexpected change of volume.


1.2.1 (01-07-2016)
========

New:

- Support for https (SSL/TLS) icecast connections.
- Added http.{put,head,delete}, https.{get,post,head,put,delete}.
- Added input.https.
- Added list.mapi.
- Added rotate.sequence.
- New pipe() operator to pipe audio data through an external program.
- Switched to curl for request resolution/fetch.

Bugfixes:

- Fix metadata update for shoutcast v2 when sid <> 1 (#320).
- Fix connection to input.harbor using the shoutcast v1 protocol (#337).


1.2.0 (12-01-2016)
==================

New:

- Websocket server (#90): this means that you can stream to harbor directly from
  your browser!
- Add support for AIFF format (#112).
- Add url.split_args to split the argument of an url (#123).
- Add buffer.adaptative to cope with small network delays (#131).
- Add sleeper operator to simulate network delays and test robustness (#131).
- Add stereo.left and stereo.right to extract channels from a stereo stream.
- Add restart command to restart liquidsoap (#135).
- Add file.contents to read the contents of a file.
- Add filter.rc for first-order RC filters.


Enhancements:

- Add support for sending OSC data (osc.send_*).
- Native support for (some) AVI files (#256) which enables support for external
  video encoders (#233).
- Improve rms operator (#105) to have per channel rms (#102), the ability to
  dynamically set window duration (#103) and multiple monitors (#104).
- Icecast streaming can now use HTTP1.1 chunked encoding (#82, #107).
- Add support for multiple shoutcast extensions (#216).
- Fade type can be overridden by metadata in fade.in / fade.out (#64).
- Allow LADSPA plugins with arbitrary number of channels (#191).
- Rename shine encoder from %mp3.fxp to %shine.
- fdkaac: dynamic plugin (#79), set afterburner parameter, use MPEG4 by default
  (#83).
- Improved subtyping on lists (#125, #126).
- Add native simple JSON decoder.
- Better code: do not abusively use assertions (#137), issue more warnings and
  fix them (#162).

Bugfixes:

- Correctly close connection in http.get / http.post (#72).
- Remove input.lastfm which has been broken for a while.
- Lots of small bugfixes.

1.1.1 (08-05-2013)
==================

New:

- Add support for FDK-AAC, which seems to be the best AAC(+) encoder around for
  now. Replacement candidate for VO-AAC and AACPLUS
- Add %ifencoder to check whether Liquidsoap was compiled with support for a
  particular encoding format.
- There is now an emacs mode in scripts/liquidsoap-mode.el.
- Liquidsoap can be used as a Windows service.

Enhancements:

- Handle more OSC types (float, float_pair, bool, string, string_pair) and added
  osc.on_*.
- Better infrastructure for decoding images. add_image can now handle most image
  file types.
- Add random.int as well as min_int and max_int to standard library.
- Add playlist.merge to play a whole playlist as one track.
- Add gstreamer.hls to play http live streams (HLS).
- Add say.program to specify text-to-speech program in scripts.
- Add "random" transition type to video.fade.* in order to select a random
  transition each time.
- Add max parameter to drop data on buffer overrun with input.gstreamer.*.
- Add bytes_per_page parameter to ogg encoders.
- Add support for DTX in speex and opus, as well as VAD for speex.
- Localize some more parsing errors in files.

Bugfixes:

- Avoid deadlocks in harbor.
- Correctly flush lame encoder.
- Correct sequence operator when there is only one source.
- Handle relative URLs in http playlists.
- portaudio is now an active source.
- Avoid jack I/O lowering the volume.

1.1.0 (04-03-2013)
==================
** This version brings some new features as well as correcting bugs. **

New:

- Add support for GStreamer decoding, processing and encoding (%gstreamer
  format, v4l webcam input is now implemented using GStreamer).
- Add support for opus decoding and encoding.
- Add support for the shine encoder, which can efficiently work on architectures
  without FPU.
- Add support for automatically computing the duration of tracks in the
  "duration" metadata [LS-641]. It can be enabled with
  set("request.metadata_decoders.duration",true)
- Add support for frei0r video effects.
- Allow %define'd variables in encoding formats [LS-634], e.g.
  %define BITRATE 24
  %define STEREO true
  output.file(%mp3(bitrate = BITRATE, stereo = STEREO),"bla.mp3",s)

Enhancements:

- Taglib now reads all metadatas (even non-standard ones).
- Add a mode to automatically reload a playlist when the file was changed
  [LS-363,LS-523]. For instance, s =
  playlist("~/Music",reload_mode="watch"). Also, add file.watch to call a
  callback when a file is changed, with inotify support when present.
- Add support for FFMpeg as video converter, which you can use with
  set("video.converter.preferred", "ffmpeg")
- Add back_time argument to blank operators [LS-609].
- Add a metadata to override fade.final duration.
- MIME is computed at most once when extracting replaygain.
- Default samplerate converter is now "fast".
- BPM detection (bpm) now uses a callback.
- Add clock.unify to unify clocks of all sources from a list.
- Add "source_url" metadata to input.http streams.
- Improved error message when theora format is not supported.
- Add list.filter function.
- video.add_image can now take any image format as input.
- Add mux_stereo.
- Support for external decoders in streams.
- Move bugtracker to https://github.com/savonet/liquidsoap/issues

Bugfixes:

- Configure is now compatible with OCaml >= 4.0 and removed support for OCaml <
  3.11 [LS-625].
- Fix random memory access / memory leak when decoding AAC/MP4 files [LS-647].
- Correct resampling of wav files.
- Use the length for data indicated in header for wav files.
- Argv.(0) now returns the script name [LS-605].
- Liquidsoap should now operate fine when compiled with -noassert [LS-578].
- Better handling of inexistent MIDI channels.
- Video decoder now correctly handles videos from Icecast.
- Avoid visu.volume freezing Liquidsoap on shutdown.
- Fix a memory leak when decoding both audio and video in ogg [LS-636].
- More efficient handling of video converters, also fixes some crashes [LS-623].
- Have the soundtouch operator preserve tags [LS-621].
- Fix remaining time estimation in cross and smart_cross.
- Avoid deadlocks in harbor and input.http.
- Remove leftover files in configure [LS-567].
- Handle wav files with padded fmt headers.
- Handle end-of-stream when seeking mp3 with mad.

1.0.1 (04-07-2012)
==================
** This version brings bug fixes and minor enhancements over 1.0.0. **

Fixes:

- correct type for the "flush" parameter in output.external()
  thanks to Romaric Petion for pointing it out
- fix bug where MP3 encoder would discard initial ID3v2 rendering
- fix bug where smart_cross() would stop before the end of tracks,
  thanks to Edward Kimber for raising the issue
- load libraries in --interactive [LS-618]
- update examples, notably the installed radio.liq
  thanks to Emery Hemingway for noticing the problem
- generalize the types of input.http() and input.harbor() to allow variable
  content kinds, and also allow video for harbor [LS-601]
- request.equeue() now allows to remove requests from the primary queue
- fix compilation of lame dynamic plugin.

New:

- new values for metadata fields does not override old one anymore;
  use setting "request.metadata_decoders.override" to restore old behavior
- stereo_mode and internal_quality parameters for %mp3 encoder
- enable mad decoder for MP1 and MP2 in addition to MP3, create aliased
  configuration keys "decoder.file_extensions/mime_types.mad"
- support for CUE sheet playlists and metadata in M3U
- setting "decoder.taglib.force_mpeg" to force taglib to consider files as MPEG
- scripting builtins getenv(), setenv() and environment()
- scripting builtin source.fallible()
- harbor is now verb-oriented, supporting GET, POST, PUT, HEAD, DELETE, OPTIONS
- load DSSI plugins from environment variables and using dssi.register()
- also display the type of the whole expression when -i is passed
- generalized custom path support for facilitating standalone distributions
- and as usual, various improvements in the code, log and error messages, etc.

1.0.0 (08-10-2011)
==================

Finally, the 1.0.0 release! It brings several important fixes, but
also some nice novelties.
The most outstanding difference concerns output.icecast(): its restart
and restart_delay parameters are gone, replaced by a new on_stop handler
which is called on every error (failed connection or disconnection) and
returns the new restart delay. The on_error handler receives a string
describing the error which enables user-friendly reporting, adaptative
delays, etc.
Note that on_error defaults to fun(_)->3. which is equivalent to having
restart=true, restart_delay=3. in previous versions, NOT the same as the
former restart=false default. As a result, liquidsoap won't fail to startup
if an initial connection attempt fails.

Fixes:

- LS-532,527: avoid freeze after errors in streaming threads or source
  initialization routines
- LS-542: race condition in playlist*() breaking randomness
- LS-489: double expiration lead to illegal queue length and freeze of
  request-based sources
- Avoid multiple simultaneous reloading in playlist*(),
  thanks to Fabio Costa for his help on this one!
- Pass charset information to icecast server to avoid encoding bugs
- LS-555: timeout for icecast connection attempts
- LS-559: permanent stop after disconnection on Ogg streams
- LS-565: efficient and crash-free error handling in input.http/harbor()
  when the input stream has an invalid number of channels
- LS-431: proper handling of duration in blank() avoids abusive empty tracks
- LS-556: rework conversion operators, optimizations used to be unsafe & broken
- LS-574: silent MIDI synthesis operators
- LS-396: drop*()'s types reflect that they don't support variable arities
- LS-442: allow comments not terminated by newline at end of file
New:
- on_error handler in output.icecast(), see above
- New msg param in %mp3 for marking frame headers, defaults to version string
- output.file(): new on_close parameter, may be used to write exact duration
- %mp3.vbr/abr for variable bitrate MP3, %mp3 is now a synonym of %mp3.cbr
- MP3 encoders now support ID3v2 tags
- input.http(): new "status" command
- LS-556: mux_mono() for adding a single audio channel into a stream
- video.add_text() using libgd (gd4o) for environments without X
  Dependency on graphics can be disabled (to work around erreneous detection)
- script language: add infix operator mod (patch by Fabio Costa)
- delay() now has an "initial" parameter
- LS-557: "server.timeout" setting can now be disabled by setting it to -1
- LS-532: source.init() for selective init with a way to handle errors,
  plus settings "clock.allow_streaming_errors" and "init.force_stat" (or
  --force-start on the command line) for easing dynamic uses of liquidsoap
Enhancements:
- Panic crash to avoid frozen liquidsoap after duppy crashes
- Text-to-speech: festival and sox are now only runtime dependencies
- LS-475,516: better support for dynamic URL change in input.http()
- LS-484: display user-friendly error messages in interactive mode
- LS-308: use seconds internally in request sources, avoid overflow and
  display more user-friendly debug messages
- Cleanup visu.volume() and video.vis_volume()
- LS-573: replace " " by "_" in identifiers to make them valid in the server
- Script syntax: unary minus now usable without parenthesis after semicolon
- Two generic queues by default, to avoid deadlocks in advanced situations
- Documentation, build & install system, etc.

1.0.0 beta3 (05-08-2011)
========================
- Feature: Added of_json to parse json data. Depends
  on json-wheel.
- Feature: Added file.exists and is_directory.
- Feature: Added timeout options for:
    telnet, harbor (server), input.icecast
- Enhancement: finer-grained timeout detection
  for input.harbor and input.http
- Fix: deadlock when disconnecting harbor users
  through server/telnet command.
- Fix: dynlink detection in native mode with old
  versions of ocaml
- Fix: deadlock when an exception is raised during
  startup while the clock is owned by a source
  (e.g. input.alsa). See LS-527 for more details.

1.0.0 beta2.1 (07-07-2011)
==========================
- Fix: playlist.safe() was unusable in beta2, as a side effect of removing
  duplicate "timeout" parameter in playlist().
- Minor enhancements to documentation, settings and reference.

1.0.0 beta2 (04-07-2011)
========================

This release introduces lots of fixes and cleanup, but also some new features.
Major novelties: support for fast seeking and cue points, FLAC and improved
  AAC+ support, introduction of the liquidsoap yellowpages "flows",
  plugin support and improved messages for scripting errors
Compatibility warning: insert_metadata has changed, and clock.assign_new()
  should be used instead of clock() to avoid some of the new static checks

Decoders:

- New support for seeking and fast computation of durations in most formats
- New decoders: FLAC (native & Ogg) and images using Camlimages
- Fixes in Ogg decoding: LS-515 (loss of data) and LS-537 (segfault).
- Fix LS-337: periodical failures when decoding AAC(+) streams
- AAC(+): use new ocaml-faad with builtin mp4ff, easier to build
- New detection mechanism mixing extensions and MIME types (when available),
  with corresponding settings "decoder.file_extensions.<format>" and
  "decoder.mime_types.<format>".
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

- New: output.harbor() which acts as a mini icecast server,
  accepting listeners directly. Encoding is shared among users,
  and is only performed when needed.
- New: ability to register HTTP GET/POST handlers to create simpler
  web services, using harbor.http.register/remove().
- Make all settings local: port, user and password can be set independently
  for each input.harbor() source
- New: "metadata_charset" and "icy_metadata_charset" in input.harbor()
- Fix: race condition possibly leading to abusive "source taken" (LS-500)

Icecast:

- Add support for streaming native flac, only works when streaming to
  input.harbor(), not supported by actual Icecast servers
- Fix bugs in ICY protocol support (header parsing, user name)
- Use ICY metadata updates when streaming AAC(+)
- New: "encoding" parameter for output.icecast(), used for recoding metadata
  Defaults to "latin1" with shoutcast servers
- New: icy.update_metadata() function for manual updates
- Enhance default "song" metadata, avoiding " - " when unnecessary (#467)

Input/output:

- New experimental input.v4l/v4l2() for webcams
- New experimental input/output.udp() for unchecked UDP streaming,
  available with most formats (at your own risk)
- Restore output.pipe.external(), now called output.external()
- New parameters for most outputs and inputs (start, on_start,
  on_stop, fallible); cleanup and uniformize implementations (LS-365)
- New ALSA settings alsa.alsa_buffer, alsa.buffer_length and alsa.periods
  Setting periods=0 allows to not attempt to set the number periods,
  which is impossible on some devices
- New preference order in input/output.prefered(): pulseaudio, portaudio,
  oss, alsa, ao, dummy

Operators:

- New: support for cue points with cue_cut()
- Change insert.metadata() which is now more script friendly,
  returning an insertion function rather than register a server
  command. The old functionality is available as server.insert_metadata().
- New: rms() operator for getting RMS of a stream, and server.rms() which
  makes this information available as a server command.
- New: track_sensitive mode for blank detection operators
- New: playlist.reloadable() for playing a list once, with a command
  for restarting it.
- Remove id.*() which can be replaced by type annotations

Scripting API:

- New: OSC support through osc.bool(), osc.float() and osc.float_pair()
- New: JSON export json_of()
- New: http.get() and http.post()
- New: url.encode/decode(), base64.encode/decode()
- New: string.recode() for charset conversions using camomile
- New: notify_metadata() and osd_metadata(), suitable for use with
  on_track() and on_metadata()
- New: request.metadata() for getting a request's metadata
- New: string.length()
- Enhance log_clocks() with parameter for delaying startup
- Enhance get_clock_status() with "uptime" reference time

Server interface:

- Print the playlist's URI when calling <playlist>.uri without an
  argument.
- Ehance <queue>.ignore now works also in the primary queue
- New command for changing the URL of an input.http(), ref #466. The
  command is <id>.url and it needs a restart (<id>.stop, then start)
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
  active_source subtype, for unused variables and ignored values
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
  Applies to most video operators (video.fade(), video.text(), effects...)
- Cleanup resource (de)allocation, which is becoming critical with dynamic
  reconfigurations (e.g., dynamic output creation, source.dynamic())
  Enforce that server commands are always deallocated (LS-495)
  Attempt to stop sources when initialization fails, so they cleanup as
  much as possible (LS-503)
  Avoid deadlocks upon crashes in IoRing-based operators
  Share code for stoppable feeding threads, use it in input.harbor()
  Avoid useless initialization of SDL systems
- Dynamic loading of lame and aacplus libraries, making it possible to
  ship them as separate binary packages. This is particularly useless
  for non-free libraries and for those that depend on X (e.g., SDL)
  which is often undesirable on servers
- Support for separate compilation of most optional features as plugins
  Use --enable-<feature>-dynamic-plugin in ./configure and
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
- Fix in source.dynamic(): missing source re-selection [LS-354]
- Avoid deadlock on startup in daemon mode [LS-229]
- Fixes in LADSPA and SDL causing early freezing of Frame parameters.
- Fullscreen mode for output.sdl()
- Fix: SIGPIPE used to cause crashes (LS-53,LS-287)
- Fix: video.volume() could crash upon some end-of-track situationos
- Fix: properly escape filenames in external file duration methods
- Rework timeout management for various sockets (notably icecast & harbor)
  Set nodelay, remove TCP_*TIMEOUT [LS-508,LS-509]

1.0.0 beta1 (06-09-2010)
========================

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
- Enable dynamic source creation and source.shutdown().
- Extend and adapt MIDI and video operators.
- Introduce purely metadata streams (audio=video=midi=0 channels) and
  metadata stream decoder.
- Support WAV streams in input.http/harbor().
- Introduce static image decoder using SDL image.
- Remove bound of request identifies (RID).
- Experimental: source.dynamic() for advanced dangerous hacking.
- Make path relative to script in %include "PATH", and introduce
  %include <...> where path is relative to liquidsoap library directory.
- Add channels_matrix parameter to output.ao().
- Add on_(dis)connect hooks in input.harbor().

Cleanup and fixes:

- Lots of cleanup and fixes as with all major code rewriting.
- Optimize video and stabilize it a little bit... still not perfect.
- Rewrite stream and file decoding API, as well as file format detection.
- Enhance shutdown and error message reporting, notably for icecast
  and request-based sources.
- Avoid quasi-infinite loop in failed request resolving.

0.9.3 (04-09-2010)
==================

This release is a bugfix of the latest snapshot (0.9.2).
It will be the last bugfix before 1.0.

Bugs fixed:

- Add "audio/mpegurl" to the list of mime-type for basic playlist parsing.
- Decode arguments passed to input.harbor.
- Use Camomile framework to try to recode arguments and user/password
  passed to input.harbor.
- Support Theora 1.1 API  via ocaml-theora 0.2.0.
- Fixed input.lastfm.
- Fixed SDL output.
- Allow magic file type detection to follow symlinks.

0.9.2 (29-10-2009)
==================

This release is a SNAPSHOT of upcoming features. It also contains several
important bugfixes. As a snapshot, it contains experimental or unpolished
features, and also breaks compatibility with previous versions.
You should in particular notice the two "New" items below:

- random(strict=true) is now called rotate();
- request sources (playlists, request.*) have a new queuing behavior,
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
  It enforces that "root.sync" be deactivated for these sources, such that
  synchronisation is done by the source. (#203)
- Factorized file decoding code.
- Fixed reversed order when parsing playlists using playlist.parse().
- Avoid bad crashes when resources lack, e.g. no more memory.
- Tighten and enforce the inter-source protocol.
- All outputs: fix the autostart server command. With the former code,
  a modification of the autostart parameter was only taken into account
  one start/stop cycle later.
- on_blank(): fix a bug that prevented the first call to on_noise.
- Fixed estimated remaining samples on ogg files, fixes issues with
  operators relying on this value, in particular crossfade() and
  request-based sources when operating in non-conservative mode.
- Fixed socket descriptor leak in input.http. (#318)
- Fixed deadlock at init when an exception was raised at wake_up
  phase. (#319)
- Fix delay() which could sometimes incorrectly declare itself ready,
  and thus try to get some data from its unavailable input source.
- Bug in queue duration estimation led to infinite feeding of the queue,
  until all request IDs are taken.
- Several fixes enforcing clean (non-deadlocking) sleep/shutdown.

New:

- The operator rotate() replaces random(strict=true), and random()
  does not have a strict parameter anymore.
- Switch to new behaviour in request-based sources.
  Use conservative=true to get to the old behaviour.
- on_blank(): provide an on_noise handler.
- playlist*(): add server commands for reloading the playlist and changing
  its URI.
- Fallible outputs: all outputs now have a fallible mode, in which
  they accept fallible sources, and automatically start/stop when the child
  fails to stream.
- EXPERIMENTAL ogg/dirac encoding support.
- output.*.aacplus(): native outputs encoding in AAC+ using ocaml-aacplus.
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
- New variables in the script language: liquidsoap.version and
  configure.{libdir,pidfile,logdir}.
- Added built-in support for replay_gain, through the replay_gain protocol
  (enabled by default) and the replay gain metadata resolver (to be enabled
  using enable_replaygain_metadata()). (#103 & #317)
- Reverse DNS operations can be disabled using settings keys
  "server.telnet.reverse_dns" and "harbor.reverse_dns".

Experimental:

- MIDI: file decoding, synthesis, virtual keyboard.

Removed:

- RTP input and output.
- Removed decoders using ocaml-natty. Slow, unmaintained and superseded
  by the mplayer decoder.

0.9.1 (18-06-2009)
==================

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
  blank with strip_blank (#260).
- on_connect function for input.harbor now receives the
  list of headers given by the connected source (#266).
- Added on_end operator, to execute a handler when a track ends.
- Added estimated remaining time in the queue length for request-driven
  sources (request.{equeue,queue}, playlist). This allows these sources
  to prepare less files in advance. In particular, primary queue may
  only contain the file currently played. Default behaviour has been
  set to the old behaviour, and a conservative option has
  been added to switch to the new behaviour. New behavivour
  will be the default for the next release.
  fixes #169, references #146

0.9.0 (01-03-2009)
==================

Bugs fixed:

- Fixed byte swapping function.
- Fixed unix server socket failure (#160).
- Fixed mp3 audio glitches when decoding
  files with picture id3v2 tags using ocaml-mad (#162).
- Fixed liquidsoap crash on weird telnet and harbor input (#164).
- Fixed request.queue() not considering initial queue on wake-up (#196).
- Fixed source leak in append().
- Fixed after_output propagation in the transitions of switches (#208).
- Fixed compilation for ocaml 3.11 (#216).
- Fixed (again) Vorbis mono output (#211).
- Avoid crashing on broken symlinks when parsing directories (#154).
- Use random temporary file in liGuidsoap.
- Fixed liGuidsoap to use the (not so) new metadata field initial_uri.
- Fix bugs in the life cycle (sleep/wake_up) of queued request sources,
  which made say_metadata unfunctional.
- Remove buggy unbuffered {input,output}.jack. (#231)
- Fixed audio information sent to icecast. (#226)
- Fixed global reset starting inactive sources. (#227)
- Fixed shoutcast source initial answer in harbor. (#254)
- Fixed frame and metadata duplication in cross operators. (#257)
- Fixed several sources (outputs, external streams) that were not going
  to sleep correctly.

Changes:

- Warning: interactive_float() is now interactive.float().

New:

- Compatible with OCaml 3.09.
- Faster shutdown.
- Rewrote ogg decoding, for files and streams.
- Support for ogg skeletons, currently only used for theora.
- Cleanup icecast class hierarchy and restart mechanism, especially
  with respect to the encoder.
- Support for breaks and metadata in generators.
  As a result, input.http() and input.harbor() now fully support them.
  See new_track_on_metadata parameters there.
- Switch operators (fallback,random and switch) can now replay the metadata
  of a source that has been left in the middle of a track.
- New force_mime parameter for input.http().
- New insert_missing parameter for append().
- Multi-line strings in liq scripts, with a caml-like syntax.
- Slight modification of the scripting syntax, handling unary minus.
- Added user_agent parameter for input.http and input.lastfm
- Added speex support for files and HTTP streams.
- Added EXPERIMENTAL support for AU, AIFF and NSV mp3 audio files
  using ocaml-natty.
- Added "prefix" parameter to the playlist operators. Allows to prefix
  each uri in order to force resolution through a specific protocol,
  like replaygain: for instance. (#166)
- Support for external processes as audio encoder:
  * Added output.icecast.lame to output to icecast using the lame binary.
  * Added output.icecast.flac to output to icecast using the flac binary.
  * Full generic support awaits some changes in libshout.
- Support for external processes as audio stream decoder:
  * Added input.mplayer to stream data coming from mplayer.
- Support for external processes as audio file decoder:
  * Added support for flac audio files using the flac binary.
  * Added support for m4a files using the faad binary.
  * Added optional support for mplayer, enabled with enable_mplayer().
- Support for external metadata decoders.
- Support for generic authentication function in harbor,
  also available for ICY (shoutcast) clients.
- Added optional support for the samplerate library,
  and dynamic configuration of resampling routine.
- Added lag() operator, which delays a stream by a constant time.
- Initial support for PulseAudio.
- Added experimental support for {input,output}.marshal, allowing
  raw communication between various liquidsoap instances.
- Added optional alternatives to store buffered audio data:
  * raw: in memory, s16le format
  * disk: on disk, several big files
  * disk_manyfiles: on disk, a lot of small files
  See documentation for more details.
- Added EXPERIMENTAL video support:
  * Support for ogg/theora file input.
  * Support for ogg/theora file and icecast output
  * Support for SDL output.
  * Optional support for ocaml-gavl as video converter.
  * Support for video in _some_ existing operators, including switches, add(),
     metadata/track manipulations.
  * Added operators: video.fade.*, video.fill, video.greyscale, video.image,
     video.invert, video.lomo, video.noise, video.opacity, video.opacity.blur,
     video.rotate, video.scale, video.sepia, video.text, video.tile,
     video.transparent, video.volume.

0.3.8.1 (11-08-2008)
====================

- Fixed metadata propagation during default transition
   in smart_crossfade
 - Changed transition evaluation order in smart_crossfade
 - Fixed transition function in smart_crossfade

0.3.8 (30-07-2008)
==================

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
- Fixed metadata update in input.harbor with icecast2 source protocol
- Fixed shutdown function. Fixes #153
- Fixed input.oss. Liquidsoap now works with OSS4 ! Fixes #158

New:

- Added a hook to execute a function when playlist.once ends
- Enhanced smart_crossfade
- Added string.case and string.capitalize
- New "exec_at" operator, to execute a function depending on a given predicate
- Added script example in the documentation to listen to radio Nova and get
  the metadata from a web page.
- Changed parameters name in fallback.skip to reflect who are the fallback
  and input source.
- Added a dump file parameter to input.harbor, for debugging purpose.
- Added an auth function parameter to input.harbor for custom
  authentifications. Fixes #157
- Added "primary_queue" and "secondary_queue" commands to request.queue and
  request.equeue sources. Also set the metadata "queue" to either "primary"
  or "secondary" for each request in the queue. Documented it too.
- Insert latest metadata for a node of a switch source when switching
  back to it in a middle of a track.
- Added a 'conservative' parameter to cross, smilar to the one in smartcross.

Internal:

- Enhanced liqi documentation parser to build the website.

0.3.7 (03-06-2008)
==================

Bugs fixed:

- Now works on FreeBSD and hopefully other unices that are stricter than
  Linux about Mutex usage.
- input.http() now has a bind_address parameter.
- Harbor socket now has a timeout for lost connections.
- smartcross() is now more compliant with the inter-sources protocol,
  fixes several "get frame didn't change the buffer" bugs.
- Ogg packeting bugs.
- Buffering policy in input.http/harbor().
- No "." in IDs and labels.
- Resources: FD leaks, useless threads (threads leaks?) in input.http().
- fade.out() used to run into infinite loops when the delay was 0.

New:

- New documentation system and website.
- Self-documenting server with a more helpful "help" command.
- Moved to duppy: less threads, lighter load, and an configurable scheduler.
- Moved to Taglib for more reliable access to MP3 metadata.
- MIME types, notably for playlists and MP3 files.
- New Jack support. The old one has been renamed to in/output.jack.legacy().
- Harbor: per-mount passwords and the stop command to kick a source client.
- Official Last.FM client.
- Metadata is no more punctual but interval-based, which suppresses some
  surprising behaviours.
- Perfected daemon behaviour.
- All output.file.*() now have the features that used to be only in
  output.file.vorbis(), notable re-opening. Added %w to the strftime-like
  format codes allowed in their filename parameter.
- Add clear_metadata() and map_metadata(). Now, rewrite_metadata() is a simple
  specialization of map_metadata(), written in utils.liq.
- Dynamic amplification factor in amplify(), e.g. useful for replay gain.
- Lots of new functions in the scripting API: for lists, requests, system
  interaction, shutdown, command-line parsing, scripted server commands, etc.

As always:

- code cleanup, style, etc.

0.3.6 (17-12-2007)
==================

Bugfix release:

- Close Http socket
- Add http socket timeout
- Close playlist file after reading its content
- Fix http redirect support for lastfm files
- Fix file open leak in camomile support
- Fix playlist uri ending with "/"

0.3.5 (08-11-2007)
==================

Bugfix release:
- Fixed #57: scpls and mpegurl playlist parsing
- Fixed #46: Late cross-scripts bindings

0.3.4 (25-09-2007)
==================

Notation: "-" stands for a change, "+" for an addition.

* Language
  - Support for polymorphism, subtyping and basic ad-hoc polymorphism,
    which allows a much simpler API, notably for maths and serialization.
  + Added interactive_*() for mutable values.
  - The right syntax for settings is now set("var", value) and can be used
    anywhere in the scripts.
  - The volume parameters of most operators are now in dB.
  - Many builtin functions added.
  - Nicer type error messages.
* Sources
  + Added input.lastfm() to relay last.fm streams.
  + Added input.harbor() to received Icecast2 source streams.
  + Added noise() to generate white noise
  - Reimplemented playlist support, added various xml and text formats.
  - Added mpd protocol to find files using mpd.
* Operators
  + New effects: compress(), flanger(), pan().
  + New filters: filter.fir.*(), filter.iir.*(), filter.biquad.*(), comb().
  + Added support for LADSPA effects.
  + Added eat_blank() to remove blanks.
* Outputs
  - Added non-default restart option for output.icecast.*().
  - Added the possibility to tweak some settings at runtime.
  - Split output.icecast.vorbis() into output.icecast.vorbis.*() to distinguish
    between encoding modes -- and similarly for output.file.vorbis and mp3.
  - Better handling of Icecast disconnections.
* IO
  + Added portaudio support.
  - Jack support is now somewhat working.
* As usual, lots of bug fixes, careful polishing & much more...

0.3.3 (06-06-2007)
==================

* Major cleanup of the core stream representation; moved to float arrays,
  removing several back-and-forth conversions and enhancing the perfs a lot;
  reviewed all sources and operators, made many minor enhancements btw.
* Lots of sound processing operators: compand, compress, normalize,
  pitch, bpm, soundtouch, saw, square, etc. Add more shapes to fade.*().
* New track processing operators: insert_metadata, on_track.
* Smart cross: allows to select a transition based on the volumes around the
  end-of-track.
* Support for AAC encoding/decoding.
* Several fixes to output.icecast.mp3 in order to support shoutcast servers.
* Automatic format recognition for input.http(), support for playlists.
* OSS I/O.
* Unbuffered ALSA I/O for low latency.
* Server interface via UNIX domain sockets.
* Better output.file.vorbis with support for re-opening the file, appending,
  interpolate strftime format codes, etc.
* Add pre-processing and math primitives to the language, new _[_] notation for
  assoc(), ruby-style anti-quotation ("..#{..}.."), add_timeout(), execute(),
  log()...
* Ability to tweak the internal PCM stream format.
* Classify sources and operators in categories for more structured doc.
* Started a few visualization operators, text and graphics based.
* Several bug fixes: request leaks, sine frequency, switch, etc.

0.3.2 (16-03-2007)
==================

* New portable output to speakers using libao().
* Updated liGuidsoap to use it until ALSA gets enhanced.
* Implemented a decent estimation of the remaining time in a track.
* Added the cross() operator allowing cross-fading.
* Generalized say_metadata() into append() and prepend().
* Per-track settings for cross(), fade.*(), prepend() and append()
  using requests' metadatas.
* Implemented input.http.mp3(), including support for icy metadata.
* New pipe() operator which allows one to filter the raw audio through an
  external program. However, sox and other common tools aren't suitable for that
  because they don't flush their output often enough.
* New on_blank() operator for calling a callback on excessive blanks.
* Restart outputs on insane latencies.
* Type checkings for settings.
* Setting for not starting the internal telnet server.
* Now handles old and new versions of Camomile correctly.
* Internal fixes and polishing (switches' cached selection, empty tracks..)

0.3.1 (17-11-2006)
==================

* More standards-compliant tarball
* Generate doc with locally built liquidsoap
* Try to cope with ill-formed mp3
* Updated for newer versions of Camomile
* So-called "strict" random-mode

0.3.0 (27-08-2006)
==================

* Many minor and major fixes at every level!
* Conversion of metadata to UTF8.
* Got rid of too many threads by scheduling all download tasks
  in a single thread, and handling all of the server's clients
  in another single thread.
* Simplified the time interval syntax and integrated it to the script language.
* New protocol: Wget for FTP, HTTP and HTTPS.
* Ability to define a new protocol from the script,
  typically using an external app/script for resolution, such as bubble.
* Ability to use an external app/script for dynamically creating requests.
* New on_metadata operator for performing arbitrary actions (typically
  a call to an external script) when metadata packets occur in a stream.
* MP3 encoding, to file or shout.
* API renamings and simplification.
* Supports transition, as functions of type (source,source) -> source
  in all switching operators: schedule, random, fallback.
* Restart icecast2 outputs on failures.
* Major changes to the scripting language which is more uniform and flexible,
  has functions, a helpful (?) type inference, and a simple Ruby-like syntax.
* Timing constraints and synchronization are managed by Root
  in a centralized way, no more by the many outputs which need it.
* Audio decoding is no more based on file extensions,
  but on the existence of a valid decoder.
* Added the equeue operators which allows interactive building of playlists,
  supporting insertion and moving of queued requests -- queue only allows
  you to push requests and cancel queued requests.
* A Python-Gtk GUI for controlling operators, or more specifically as a console
  for creating your live show -- to be updated, still unstable.
* Alsa input and output.
* Blank detection, automatically skips on too long blanks, or strip them.
* Http ogg/vorbis relay, the way to relay live shows.
* Interactive mixer.
* The request system was mostly rewritten to really fulfill its specification.
* The server is no more associated to the queue operator but is now something
  external, in which all operators can plug commands. This much more logical
  design lead to many more interactive controls. The syntax of command outputs
  was also simplified for easier automated processing.
* Dynamic loading of plugins.
* Outputs are now operators. It makes it possible to output different streams
  in a single instance of Liquidsoap, which RadioPi needed. As a consequence
  we removed the restriction that one source must have at most one father,
  without any extra burden for the user.

0.2.0 (20-04-2005)
==================

* Proper initial release.

0.1.0 (2004)
============

* Release for academic demonstration, not functional.
