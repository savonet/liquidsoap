
(rule
  (target @flac[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(stereo)" sine "@flac[stereo].flac"))))
(rule
  (target @flac[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(mono)" sine "@flac[mono].flac"))))
(rule
  (target @wav[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(stereo)" sine "@wav[stereo].wav"))))
(rule
  (target @wav[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(mono)" sine "@wav[mono].wav"))))
(rule
  (target @mp3[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(mono)" sine "@mp3[mono].mp3"))))
(rule
  (target @mp3[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(stereo)" sine "@mp3[stereo].mp3"))))
(rule
  (target @ogg[@vorbis[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(mono))" sine "@ogg[@vorbis[mono]].ogg"))))
(rule
  (target @ogg[@vorbis[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(stereo))" sine "@ogg[@vorbis[stereo]].ogg"))))
(rule
  (target @ogg[@flac[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(mono))" sine "@ogg[@flac[mono]].ogg"))))
(rule
  (target @ogg[@flac[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(stereo))" sine "@ogg[@flac[stereo]].ogg"))))
(rule
  (target @ogg[@opus[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(mono))" sine "@ogg[@opus[mono]].ogg"))))
(rule
  (target @ogg[@opus[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(stereo))" sine "@ogg[@opus[stereo]].ogg"))))
(rule
  (target @ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"),%video.none)" sine "@ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4"))))
(rule
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"))))
(rule
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4"))))
(rule
 (target @flac[stereo].flac)
 (deps
  (:encoder @flac[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%flac(stereo)")))
(rule
 (target @flac[mono].flac)
 (deps
  (:encoder @flac[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%flac(mono)")))
(rule
 (target @wav[stereo].wav)
 (deps
  (:encoder @wav[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%wav(stereo)")))
(rule
 (target @wav[mono].wav)
 (deps
  (:encoder @wav[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%wav(mono)")))
(rule
 (target @mp3[mono].mp3)
 (deps
  (:encoder @mp3[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%mp3(mono)")))
(rule
 (target @mp3[stereo].mp3)
 (deps
  (:encoder @mp3[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%mp3(stereo)")))
(rule
 (target @ogg[@vorbis[mono]].ogg)
 (deps
  (:encoder @ogg[@vorbis[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%vorbis(mono))")))
(rule
 (target @ogg[@vorbis[stereo]].ogg)
 (deps
  (:encoder @ogg[@vorbis[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%vorbis(stereo))")))
(rule
 (target @ogg[@flac[mono]].ogg)
 (deps
  (:encoder @ogg[@flac[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%flac(mono))")))
(rule
 (target @ogg[@flac[stereo]].ogg)
 (deps
  (:encoder @ogg[@flac[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%flac(stereo))")))
(rule
 (target @ogg[@opus[mono]].ogg)
 (deps
  (:encoder @ogg[@opus[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%opus(mono))")))
(rule
 (target @ogg[@opus[stereo]].ogg)
 (deps
  (:encoder @ogg[@opus[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ogg(%opus(stereo))")))
(rule
 (target @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"),%video.none)")))
(rule
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))")))
(rule
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))")))
(rule
  (target all_media_files)
  (deps
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
@ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4)
  (action (run touch %{target})))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run echo TODO)))