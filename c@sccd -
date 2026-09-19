[1mdiff --git a/doc/content/external_encoders.md b/doc/content/external_encoders.md[m
[1mindex 215b88a2f..23e8c780b 100644[m
[1m--- a/doc/content/external_encoders.md[m
[1m+++ b/doc/content/external_encoders.md[m
[36m@@ -30,9 +30,9 @@[m [mThe available options are:[m
 - `header`: if set to `false` then no WAV header will be added to the data fed to the encoding process, thus the encoding process shall operate on RAW data.[m
 - `restart_on_crash`: whether to restart the encoding process if it crashed. Useful when the external process fails to encode properly data after some time.[m
 - `restart_on_metadata`: restart encoding process on each new metadata. Useful in conjunction with the `process` parameter for audio formats that need a new header, possibly with metadatas, for each new track. This is the case for the ogg container.[m
[31m-- `restart_encoder_delay`: Restart the encoder after some delay. This can be useful for encoders that cannot operate on infinite streams, or are buggy after some time, like the `lame` binary. The default for `lame` and `accplusenc`-based encoders is to restart the encoder every hour.[m
[32m+[m[32m- `restart_after_delay`: Restart the encoder after some delay, in seconds. This can be useful for encoders that cannot operate on infinite streams, or are buggy after some time, like the `lame` binary. The default for `lame` and `accplusenc`-based encoders is to restart the encoder every hour.[m
 [m
[31m-Only one of `restart_encoder_delay` or `restart_on_new_track` should be used.[m
[32m+[m[32mOnly one of `restart_after_delay` or `restart_on_metadata` should be used.[m
 [m
 The restart mechanism strongly relies on the good behaviour of the encoding process. The restart operation will[m
 close the standard input of the encoding process. The encoding process is then expected to finish its own operations and[m
[1mdiff --git a/doc/content/harbor.md b/doc/content/harbor.md[m
[1mindex ed03648c2..100e513d8 100644[m
[1m--- a/doc/content/harbor.md[m
[1m+++ b/doc/content/harbor.md[m
[36m@@ -77,15 +77,21 @@[m [mWhen ICY is enabled on port `n`, Shoutcast clients should connect to port `n+1`.[m
 ### SSL / HTTPS[m
 [m
 SSL support requires one of the following opam packages: `ssl` or[m
[31m-`osx-secure-transport`. When available via `ssl`, use `input.harbor.ssl`;[m
[31m-when available via `osx-secure-transport`, use `input.harbor.secure_transport`.[m
[32m+[m[32m`osx-secure-transport`. Every harbor operator takes a `transport` argument.[m
[32m+[m[32mPass it `http.transport.ssl` when SSL support comes from `ssl`, or[m
[32m+[m[32m`http.transport.secure_transport` when it comes from `osx-secure-transport`:[m
 [m
[31m-The corresponding settings are under `harbor.ssl.*` or[m
[31m-`harbor.secure_transport.*`:[m
[32m+[m[32m```liquidsoap[m
[32m+[m[32mtransport =[m
[32m+[m[32m  http.transport.ssl([m
[32m+[m[32m    certificate="/path/to/cert.pem", key="/path/to/key.pem"[m
[32m+[m[32m  )[m
[32m+[m[32ms = input.harbor(transport=transport, port=8005, "live")[m
[32m+[m[32m```[m
 [m
[31m-- `harbor.ssl.certificate`: Path to the SSL certificate.[m
[31m-- `harbor.ssl.private_key`: Path to the SSL private key.[m
[31m-- `harbor.ssl.password`: Optional password to unlock the private key.[m
[32m+[m[32mThe `key` argument can be omitted when the certificate file also contains the[m
[32m+[m[32mprivate key. The same transport is accepted by `output.harbor` and[m
[32m+[m[32m`harbor.http.register`.[m
 [m
 For a free, valid certificate, see [Let's Encrypt](https://letsencrypt.org/).[m
 For local testing, a self-signed certificate can be generated with:[m
[1mdiff --git a/doc/content/migrating.md b/doc/content/migrating.md[m
[1mindex bb221a968..4c9b8d9eb 100644[m
[1m--- a/doc/content/migrating.md[m
[1m+++ b/doc/content/migrating.md[m
[36m@@ -35,7 +35,7 @@[m [mThe `video.canvas` API (for positioning video elements) is unaffected by this ch[m
 [m
 Video dimensions (`video.frame.width`/`height`) are now automatically detected from the first decoded video file. This means you no longer need to manually set dimensions in most cases.[m
 [m
[31m-To disable this behavior, either set `settings.video.detect_dimensions` to `false` or explicitly set the video dimensions yourself.[m
[32m+[m[32mTo disable this behavior, either set `settings.frame.video.detect_dimensions` to `false` or explicitly set the video dimensions yourself.[m
 [m
 ### Implicit integer to float casting[m
 [m
[1mdiff --git a/doc/content/stream_content.md b/doc/content/stream_content.md[m
[1mindex 68d8a3ef3..e6685c66d 100644[m
[1m--- a/doc/content/stream_content.md[m
[1m+++ b/doc/content/stream_content.md[m
[36m@@ -72,20 +72,20 @@[m [mYou might have noticed that our description of internal stream contents is[m
 missing some information, such as sample rate, video size, etc.[m
 Indeed, that information is not part of the stream types, which is[m
 local to each source/request/format, but global in liquidsoap.[m
[31m-You can change it using the `frame.audio/video.*`[m
[32m+[m[32mYou can change it using the `settings.frame.audio/video.*`[m
 settings, shown here with their default values:[m
 [m
 ```liquidsoap[m
[31m-audio.samplerate := 44100[m
[31m-video.frame.width := 320[m
[31m-video.frame.height := 240[m
[31m-video.frame.rate := 25[m
[32m+[m[32msettings.frame.audio.samplerate := 44100[m
[32m+[m[32msettings.frame.video.width := 1280[m
[32m+[m[32msettings.frame.video.height := 720[m
[32m+[m[32msettings.frame.video.framerate := 25[m
 ```[m
 [m
 By default, video dimensions are automatically detected from the first decoded[m
 video file. This behavior can be disabled by setting[m
[31m-`settings.video.detect_dimensions` to `false` or by explicitly setting[m
[31m-`video.frame.width` or `video.frame.height`.[m
[32m+[m[32m`settings.frame.video.detect_dimensions` to `false` or by explicitly setting[m
[32m+[m[32m`settings.frame.video.width` or `settings.frame.video.height`.[m
 [m
 ## Checking stream contents[m
 [m
[1mdiff --git a/src/core/builtins/builtins_request.ml b/src/core/builtins/builtins_request.ml[m
[1mindex 3e4c792dd..8f58e3030 100644[m
[1m--- a/src/core/builtins/builtins_request.ml[m
[1m+++ b/src/core/builtins/builtins_request.ml[m
[36m@@ -305,7 +305,7 @@[m [mlet _ =[m
 let _ =[m
   Lang.add_builtin ~base:request "status" ~category:`Liquidsoap[m
     ~descr:[m
[31m-      "Current status of a request. Can be idle, resolving, ready, playing or \[m
[32m+[m[32m      "Current status of a request. Can be idle, resolving, ready, failed or \[m
        destroyed."[m
     [("", Request.Value.t, None, None)][m
     Lang.string_t[m
[1mdiff --git a/src/lang/runtime/lang_regexp.ml b/src/lang/runtime/lang_regexp.ml[m
[1mindex f8b9120cc..f9db89e1d 100644[m
[1m--- a/src/lang/runtime/lang_regexp.ml[m
[1m+++ b/src/lang/runtime/lang_regexp.ml[m
[36m@@ -29,7 +29,7 @@[m [mtype regexp = {[m
   regexp : Re.re;[m
 }[m
 [m
[31m-let all_regexp_flags = [`i; `g; `m][m
[32m+[m[32mlet all_regexp_flags = [`i; `g; `s; `m][m
 [m
 let string_of_regexp_flag = function[m
   | `i -> "i"[m
[1mdiff --git a/src/libs/extra/server.liq b/src/libs/extra/server.liq[m
[1mindex 1a39f4a32..3b86a1767 100644[m
[1m--- a/src/libs/extra/server.liq[m
[1m+++ b/src/libs/extra/server.liq[m
[36m@@ -27,8 +27,8 @@[m [mend[m
 # @category Source / Track processing[m
 # @param ~id Force the value of the source ID.[m
 def server.insert_metadata(s) =[m
[31m-  def insert(s) =[m
[31m-    let (meta, _) = string.annotate.parse("#{s}:")[m
[32m+[m[32m  def insert(arg) =[m
[32m+[m[32m    let (meta, _) = string.annotate.parse("#{arg}:")[m
     if[m
       meta != [][m
     then[m
[1mdiff --git a/src/libs/replaygain.liq b/src/libs/replaygain.liq[m
[1mindex 6bc891c04..6d51b9e89 100644[m
[1m--- a/src/libs/replaygain.liq[m
[1m+++ b/src/libs/replaygain.liq[m
[36m@@ -104,8 +104,10 @@[m [mdef replaces file.replaygain(~id=null, ~compute=true, ~ratio=50., file_name) =[m
 end[m
 [m
 # Enable ReplayGain metadata resolver. This resolver will process any file[m
[31m-# decoded by Liquidsoap and add a `replaygain_track_gain` metadata when this[m
[31m-# value could be computed. For a finer-grained replay gain processing, use the[m
[32m+[m[32m# decoded by Liquidsoap and add the metadata named by[m
[32m+[m[32m# `settings.normalize_track_gain_metadata` (`liq_normalize_track_gain` by[m
[32m+[m[32m# default) when the value could be computed, so that `normalize_track_gain`[m
[32m+[m[32m# applies it. For a finer-grained replay gain processing, use the[m
 # `replaygain:` protocol.[m
 # @param ~compute Compute replaygain if metadata tag is empty.[m
 # @param ~ratio Decoding ratio. A value of `50.` means try to decode the file `50x` faster than real time, if possible. Use this setting to lower CPU peaks when computing replaygain tags.[m
[1mdiff --git a/tests/regression/dune.inc b/tests/regression/dune.inc[m
[1mindex 4d070e548..c4819239a 100644[m
[1m--- a/tests/regression/dune.inc[m
[1m+++ b/tests/regression/dune.inc[m
[36m@@ -2328,6 +2328,29 @@[m
  (action[m
   (run %{run_test} seek_track_map liquidsoap %{test_liq} seek_track_map.liq)))[m
 [m
[32m+[m[32m(rule[m
[32m+[m[32m (alias test_server.insert_metadata)[m
[32m+[m[32m (package liquidsoap)[m
[32m+[m[32m (deps[m
[32m+[m[32m  server.insert_metadata.liq[m
[32m+[m[32m  (glob_files ../media/**)[m
[32m+[m[32m  ../liquidsoap-test-assets[m
[32m+[m[32m  ../../src/bin/liquidsoap.exe[m
[32m+[m[32m  ../streams/file1.png[m
[32m+[m[32m  ../streams/file1.mp3[m
[32m+[m[32m  ./theora-test.mp4[m
[32m+[m[32m  (package liquidsoap)[m
[32m+[m[32m  (source_tree ../../src/libs)[m
[32m+[m[32m  (:test_liq ../test.liq)[m
[32m+[m[32m  (:run_test ../run_test.exe))[m
[32m+[m[32m (action[m
[32m+[m[32m  (run[m
[32m+[m[32m   %{run_test}[m
[32m+[m[32m   server.insert_metadata[m
[32m+[m[32m   liquidsoap[m
[32m+[m[32m   %{test_liq}[m
[32m+[m[32m   server.insert_metadata.liq)))[m
[32m+[m
 (rule[m
  (alias test_shoutcast-args)[m
  (package liquidsoap)[m
[36m@@ -2853,6 +2876,7 @@[m
   (alias test_scheduler_legacy)[m
   (alias test_scheduler_parallel)[m
   (alias test_seek_track_map)[m
[32m+[m[32m  (alias test_server.insert_metadata)[m
   (alias test_shoutcast-args)[m
   (alias test_source_cleanup)[m
   (alias test_source_dynamic)[m
[1mdiff --git a/tests/regression/server.insert_metadata.liq b/tests/regression/server.insert_metadata.liq[m
[1mnew file mode 100644[m
[1mindex 000000000..fe2e509b9[m
[1m--- /dev/null[m
[1m+++ b/tests/regression/server.insert_metadata.liq[m
[36m@@ -0,0 +1,22 @@[m
[32m+[m[32ms = server.insert_metadata(sine(id="src"))[m
[32m+[m
[32m+[m[32ms.on_metadata([m
[32m+[m[32m  synchronous=true,[m
[32m+[m[32m  fun (m) ->[m
[32m+[m[32m    if m["title"] == "New title" and m["artist"] == "Liquidsoap" then test.pass() end[m
[32m+[m[32m)[m
[32m+[m
[32m+[m[32moutput.dummy(s)[m
[32m+[m
[32m+[m[32mthread.run([m
[32m+[m[32m  delay=1.,[m
[32m+[m[32m  {[m
[32m+[m[32m    ignore([m
[32m+[m[32m      server.execute([m
[32m+[m[32m        'src.insert title="New title",artist="Liquidsoap"'[m
[32m+[m[32m      )[m
[32m+[m[32m    )[m
[32m+[m[32m  }[m
[32m+[m[32m)[m
[32m+[m
[32m+[m[32mthread.run(delay=5., {test.fail("metadata was not inserted")})[m
