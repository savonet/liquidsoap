
(rule
  (alias citest)
  (package liquidsoap)
  (target @fdkaac[aot='mpeg4_aac_lc',channels=1]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%fdkaac(aot=\"mpeg4_aac_lc\",channels=1)" sine "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @fdkaac[channels=2]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%fdkaac(channels=2)" sine "@fdkaac[channels=2].aac"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @shine[channels=1]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%shine(channels=1)" sine "@shine[channels=1].mp3"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @shine[channels=2]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%shine(channels=2)" sine "@shine[channels=2].mp3"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @flac[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(stereo)" sine "@flac[stereo].flac"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @flac[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(mono)" sine "@flac[mono].flac"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @wav[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(stereo)" sine "@wav[stereo].wav"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @wav[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(mono)" sine "@wav[mono].wav"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @mp3[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(mono)" sine "@mp3[mono].mp3"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @mp3[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(stereo)" sine "@mp3[stereo].mp3"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@vorbis[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(mono))" sine "@ogg[@vorbis[mono]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@vorbis[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(stereo))" sine "@ogg[@vorbis[stereo]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@flac[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(mono))" sine "@ogg[@flac[mono]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@flac[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(stereo))" sine "@ogg[@flac[stereo]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@opus[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(mono))" sine "@ogg[@opus[mono]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ogg[@opus[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(stereo))" sine "@ogg[@opus[stereo]].ogg"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"))" sine "@ffmpeg[format='mp4',@audio[codec='aac']].mp4"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%audio_2(codec=\"aac\",channels=1),%video(codec=\"libx264\"),%video_2(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%gno(audio_content,codec=\"aac\",channels=2),%gni(audio_content,codec=\"aac\",channels=1),%bar(video_content,codec=\"libx264\"),%foo(video_content,codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4"))))
(rule
  (alias citest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@video[codec='libx264']].mp4"))))
(rule
 (alias citest)
 (package liquidsoap)
 (target @fdkaac[aot='mpeg4_aac_lc',channels=1].aac)
 (deps
  (:encoder @fdkaac[aot='mpeg4_aac_lc',channels=1]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%fdkaac(aot=\"mpeg4_aac_lc\",channels=1)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @fdkaac[channels=2].aac)
 (deps
  (:encoder @fdkaac[channels=2]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%fdkaac(channels=2)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @shine[channels=1].mp3)
 (deps
  (:encoder @shine[channels=1]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%shine(channels=1)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @shine[channels=2].mp3)
 (deps
  (:encoder @shine[channels=2]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%shine(channels=2)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @flac[stereo].flac)
 (deps
  (:encoder @flac[stereo]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%flac(stereo)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @flac[mono].flac)
 (deps
  (:encoder @flac[mono]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%flac(mono)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @wav[stereo].wav)
 (deps
  (:encoder @wav[stereo]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%wav(stereo)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @wav[mono].wav)
 (deps
  (:encoder @wav[mono]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%wav(mono)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @mp3[mono].mp3)
 (deps
  (:encoder @mp3[mono]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%mp3(mono)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @mp3[stereo].mp3)
 (deps
  (:encoder @mp3[stereo]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%mp3(stereo)")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@vorbis[mono]].ogg)
 (deps
  (:encoder @ogg[@vorbis[mono]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%vorbis(mono))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@vorbis[stereo]].ogg)
 (deps
  (:encoder @ogg[@vorbis[stereo]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%vorbis(stereo))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@flac[mono]].ogg)
 (deps
  (:encoder @ogg[@flac[mono]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%flac(mono))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@flac[stereo]].ogg)
 (deps
  (:encoder @ogg[@flac[stereo]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%flac(stereo))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@opus[mono]].ogg)
 (deps
  (:encoder @ogg[@opus[mono]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%opus(mono))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ogg[@opus[stereo]].ogg)
 (deps
  (:encoder @ogg[@opus[stereo]]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%opus(stereo))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%audio_2(codec=\"aac\",channels=1),%video(codec=\"libx264\"),%video_2(codec=\"libx264\"))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%gno(audio_content,codec=\"aac\",channels=2),%gni(audio_content,codec=\"aac\",channels=1),%bar(video_content,codec=\"libx264\"),%foo(video_content,codec=\"libx264\"))")))
(rule
 (alias citest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@video[codec='libx264']]_encoder.liq)
  (source_tree ../../src/libs)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%video(codec=\"libx264\"))")))
(rule
  (alias citest)
  (package liquidsoap)
  (target all_media_files)
  (deps
    @fdkaac[aot='mpeg4_aac_lc',channels=1].aac
@fdkaac[channels=2].aac
@shine[channels=1].mp3
@shine[channels=2].mp3
@flac[stereo].flac
@flac[mono].flac
@wav[stereo].wav
@wav[mono].wav
@mp3[mono].mp3
@mp3[stereo].mp3
@ogg[@vorbis[mono]].ogg
@ogg[@vorbis[stereo]].ogg
@ogg[@flac[mono]].ogg
@ogg[@flac[stereo]].ogg
@ogg[@opus[mono]].ogg
@ogg[@opus[stereo]].ogg
@ffmpeg[format='mp4',@audio[codec='aac']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4
@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4
@ffmpeg[format='mp4',@video[codec='libx264']].mp4)
  (action (run touch %{target})))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} test_mono.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} test_stereo.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} test_mono.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} test_stereo.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @shine[channels=1].mp3" liquidsoap %{test_liq} test_mono.liq -- "@shine[channels=1].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @shine[channels=1].mp3" liquidsoap %{test_liq} test_stereo.liq -- "@shine[channels=1].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @shine[channels=1].mp3" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@shine[channels=1].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @shine[channels=2].mp3" liquidsoap %{test_liq} test_mono.liq -- "@shine[channels=2].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @shine[channels=2].mp3" liquidsoap %{test_liq} test_stereo.liq -- "@shine[channels=2].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @shine[channels=2].mp3" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@shine[channels=2].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @flac[stereo].flac" liquidsoap %{test_liq} test_mono.liq -- "@flac[stereo].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @flac[stereo].flac" liquidsoap %{test_liq} test_stereo.liq -- "@flac[stereo].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @flac[stereo].flac" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@flac[stereo].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @flac[mono].flac" liquidsoap %{test_liq} test_mono.liq -- "@flac[mono].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @flac[mono].flac" liquidsoap %{test_liq} test_stereo.liq -- "@flac[mono].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @flac[mono].flac" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@flac[mono].flac")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @wav[stereo].wav" liquidsoap %{test_liq} test_mono.liq -- "@wav[stereo].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @wav[stereo].wav" liquidsoap %{test_liq} test_stereo.liq -- "@wav[stereo].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @wav[stereo].wav" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@wav[stereo].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @wav[mono].wav" liquidsoap %{test_liq} test_mono.liq -- "@wav[mono].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @wav[mono].wav" liquidsoap %{test_liq} test_stereo.liq -- "@wav[mono].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @wav[mono].wav" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@wav[mono].wav")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @mp3[mono].mp3" liquidsoap %{test_liq} test_mono.liq -- "@mp3[mono].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @mp3[mono].mp3" liquidsoap %{test_liq} test_stereo.liq -- "@mp3[mono].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @mp3[mono].mp3" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@mp3[mono].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @mp3[stereo].mp3" liquidsoap %{test_liq} test_mono.liq -- "@mp3[stereo].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @mp3[stereo].mp3" liquidsoap %{test_liq} test_stereo.liq -- "@mp3[stereo].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @mp3[stereo].mp3" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@mp3[stereo].mp3")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} test_mono.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} test_stereo.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} test_mono.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} test_stereo.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} test_ffmpeg_audio_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_video_size.liq -- "@ffmpeg[format='mp4',@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_video_size.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} test_ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  multitrack.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "multitrack.liq" liquidsoap %{test_liq} multitrack.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_ffmpeg_inline_encode_decode.liq" liquidsoap %{test_liq} test_ffmpeg_inline_encode_decode.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode_audio.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_ffmpeg_inline_encode_decode_audio.liq" liquidsoap %{test_liq} test_ffmpeg_inline_encode_decode_audio.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode_video.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_ffmpeg_inline_encode_decode_video.liq" liquidsoap %{test_liq} test_ffmpeg_inline_encode_decode_video.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_distributed_hls.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_ffmpeg_distributed_hls.liq" liquidsoap %{test_liq} test_ffmpeg_distributed_hls.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_hls.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_ffmpeg_raw_hls.liq" liquidsoap %{test_liq} test_ffmpeg_raw_hls.liq -- "")))
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  all_media_files
  test_taglib.liq
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "test_taglib.liq" liquidsoap %{test_liq} test_taglib.liq -- "")))
