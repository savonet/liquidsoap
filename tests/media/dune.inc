
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
 (alias runtest)
 (package liquidsoap)
 (deps
  @flac[stereo]_encoder.liq
@flac[mono]_encoder.liq
@wav[stereo]_encoder.liq
@wav[mono]_encoder.liq
@mp3[mono]_encoder.liq
@mp3[stereo]_encoder.liq
@ogg[@vorbis[mono]]_encoder.liq
@ogg[@vorbis[stereo]]_encoder.liq
@ogg[@flac[mono]]_encoder.liq
@ogg[@flac[stereo]]_encoder.liq
@ogg[@opus[mono]]_encoder.liq
@ogg[@opus[stereo]]_encoder.liq
@ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq
@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@flac[stereo]_encoder.liq" "%flac(stereo).flac encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@flac[mono]_encoder.liq" "%flac(mono).flac encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@wav[stereo]_encoder.liq" "%wav(stereo).wav encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@wav[mono]_encoder.liq" "%wav(mono).wav encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@mp3[mono]_encoder.liq" "%mp3(mono).mp3 encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@mp3[stereo]_encoder.liq" "%mp3(stereo).mp3 encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@vorbis[mono]]_encoder.liq" "%ogg(%vorbis(mono)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@vorbis[stereo]]_encoder.liq" "%ogg(%vorbis(stereo)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@flac[mono]]_encoder.liq" "%ogg(%flac(mono)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@flac[stereo]]_encoder.liq" "%ogg(%flac(stereo)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@opus[mono]]_encoder.liq" "%ogg(%opus(mono)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ogg[@opus[stereo]]_encoder.liq" "%ogg(%opus(stereo)).ogg encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq" "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"),%video.none).mp4 encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq" "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\")).mp4 encoder")
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq" "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\")).mp4 encoder"))))
  