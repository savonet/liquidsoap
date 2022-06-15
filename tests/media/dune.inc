
(rule
  (alias  runtest)
  (target @flac[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(stereo)" sine "@flac[stereo].flac"))))
(rule
  (alias  runtest)
  (target @flac[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(mono)" sine "@flac[mono].flac"))))
(rule
  (alias  runtest)
  (target @wav[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(stereo)" sine "@wav[stereo].wav"))))
(rule
  (alias  runtest)
  (target @wav[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(mono)" sine "@wav[mono].wav"))))
(rule
  (alias  runtest)
  (target @mp3[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(mono)" sine "@mp3[mono].mp3"))))
(rule
  (alias  runtest)
  (target @mp3[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(stereo)" sine "@mp3[stereo].mp3"))))
(rule
  (alias  runtest)
  (target @ogg[@vorbis[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(mono))" sine "@ogg[@vorbis[mono]].ogg"))))
(rule
  (alias  runtest)
  (target @ogg[@vorbis[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(stereo))" sine "@ogg[@vorbis[stereo]].ogg"))))
(rule
  (alias  runtest)
  (target @ogg[@flac[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(mono))" sine "@ogg[@flac[mono]].ogg"))))
(rule
  (alias  runtest)
  (target @ogg[@flac[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(stereo))" sine "@ogg[@flac[stereo]].ogg"))))
(rule
  (alias  runtest)
  (target @ogg[@opus[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(mono))" sine "@ogg[@opus[mono]].ogg"))))
(rule
  (alias  runtest)
  (target @ogg[@opus[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(stereo))" sine "@ogg[@opus[stereo]].ogg"))))
(rule
  (alias  runtest)
  (target @ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"),%video.none)" sine "@ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4"))))
(rule
  (alias  runtest)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"))))
(rule
  (alias  runtest)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4"))))
(rule
  (alias  runtest)
  (target @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio.none,%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4"))))
(rule
 (alias  runtest)
 (target @flac[stereo].flac)
 (deps
  (:encoder @flac[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%flac(stereo)")))
(rule
 (alias  runtest)
 (target @flac[mono].flac)
 (deps
  (:encoder @flac[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%flac(mono)")))
(rule
 (alias  runtest)
 (target @wav[stereo].wav)
 (deps
  (:encoder @wav[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%wav(stereo)")))
(rule
 (alias  runtest)
 (target @wav[mono].wav)
 (deps
  (:encoder @wav[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%wav(mono)")))
(rule
 (alias  runtest)
 (target @mp3[mono].mp3)
 (deps
  (:encoder @mp3[mono]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%mp3(mono)")))
(rule
 (alias  runtest)
 (target @mp3[stereo].mp3)
 (deps
  (:encoder @mp3[stereo]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%mp3(stereo)")))
(rule
 (alias  runtest)
 (target @ogg[@vorbis[mono]].ogg)
 (deps
  (:encoder @ogg[@vorbis[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%vorbis(mono))")))
(rule
 (alias  runtest)
 (target @ogg[@vorbis[stereo]].ogg)
 (deps
  (:encoder @ogg[@vorbis[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%vorbis(stereo))")))
(rule
 (alias  runtest)
 (target @ogg[@flac[mono]].ogg)
 (deps
  (:encoder @ogg[@flac[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%flac(mono))")))
(rule
 (alias  runtest)
 (target @ogg[@flac[stereo]].ogg)
 (deps
  (:encoder @ogg[@flac[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%flac(stereo))")))
(rule
 (alias  runtest)
 (target @ogg[@opus[mono]].ogg)
 (deps
  (:encoder @ogg[@opus[mono]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%opus(mono))")))
(rule
 (alias  runtest)
 (target @ogg[@opus[stereo]].ogg)
 (deps
  (:encoder @ogg[@opus[stereo]]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ogg(%opus(stereo))")))
(rule
 (alias  runtest)
 (target @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac'],@video.none]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"),%video.none)")))
(rule
 (alias  runtest)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))")))
(rule
 (alias  runtest)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))")))
(rule
 (alias  runtest)
 (target @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']]_encoder.liq)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %{run_test} "%{liquidsoap} %{test_liq} -" %{encoder} "%ffmpeg(format=\"mp4\",%audio.none,%video(codec=\"libx264\"))")))
(rule
  (alias  runtest)
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
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4)
  (action (run touch %{target})))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[stereo].flac" test_mono.liq "Mono decoding test for @flac[stereo].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[stereo].flac" test_stereo.liq "Stereo decoding test for @flac[stereo].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[stereo].flac" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @flac[stereo].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[mono].flac" test_mono.liq "Mono decoding test for @flac[mono].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[mono].flac" test_stereo.liq "Stereo decoding test for @flac[mono].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @flac[mono].flac" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @flac[mono].flac")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[stereo].wav" test_mono.liq "Mono decoding test for @wav[stereo].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[stereo].wav" test_stereo.liq "Stereo decoding test for @wav[stereo].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[stereo].wav" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @wav[stereo].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[mono].wav" test_mono.liq "Mono decoding test for @wav[mono].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[mono].wav" test_stereo.liq "Stereo decoding test for @wav[mono].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @wav[mono].wav" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @wav[mono].wav")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[mono].mp3" test_mono.liq "Mono decoding test for @mp3[mono].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[mono].mp3" test_stereo.liq "Stereo decoding test for @mp3[mono].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[mono].mp3" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @mp3[mono].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[stereo].mp3" test_mono.liq "Mono decoding test for @mp3[stereo].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[stereo].mp3" test_stereo.liq "Stereo decoding test for @mp3[stereo].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @mp3[stereo].mp3" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @mp3[stereo].mp3")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[mono]].ogg" test_mono.liq "Mono decoding test for @ogg[@vorbis[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[mono]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@vorbis[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[mono]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@vorbis[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[stereo]].ogg" test_mono.liq "Mono decoding test for @ogg[@vorbis[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[stereo]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@vorbis[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@vorbis[stereo]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@vorbis[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[mono]].ogg" test_mono.liq "Mono decoding test for @ogg[@flac[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[mono]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@flac[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[mono]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@flac[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[stereo]].ogg" test_mono.liq "Mono decoding test for @ogg[@flac[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[stereo]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@flac[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@flac[stereo]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@flac[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[mono]].ogg" test_mono.liq "Mono decoding test for @ogg[@opus[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[mono]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@opus[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[mono]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@opus[mono]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[stereo]].ogg" test_mono.liq "Mono decoding test for @ogg[@opus[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[stereo]].ogg" test_stereo.liq "Stereo decoding test for @ogg[@opus[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ogg[@opus[stereo]].ogg" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ogg[@opus[stereo]].ogg")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_mono.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4" test_mono.liq "Mono decoding test for @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_stereo.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4" test_stereo.liq "Stereo decoding test for @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_audio_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4" test_ffmpeg_audio_decoder.liq "FFmpeg audio decoder test for @ffmpeg[format='mp4',@audio[codec='aac'],@video.none].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4" test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4" test_video_size.liq "FFmpeg video size test for @ffmpeg[format='mp4',@audio.none,@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_video_size.liq "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_video_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_video_decoder.liq "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_video_size.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_video_size.liq "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_add_text.liq "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_copy_decoder.liq "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_copy_and_encode_decoder.liq "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_filter.liq "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_raw_decoder.liq "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_raw_and_encode_decoder.liq "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" test_ffmpeg_raw_and_copy_decoder.liq "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_add_text.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_add_text.liq "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_copy_decoder.liq "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_copy_and_encode_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_copy_and_encode_decoder.liq "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_filter.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_filter.liq "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_raw_decoder.liq "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_encode_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_raw_and_encode_decoder.liq "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_and_copy_decoder.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" test_ffmpeg_raw_and_copy_decoder.liq "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_ffmpeg_inline_encode_decode.liq "")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode_audio.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_ffmpeg_inline_encode_decode_audio.liq "")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_inline_encode_decode_video.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_ffmpeg_inline_encode_decode_video.liq "")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_distributed_hls.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_ffmpeg_distributed_hls.liq "")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_ffmpeg_raw_hls.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_ffmpeg_raw_hls.liq "")))
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  test_taglib.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run %{run_test} "%{liquidsoap} %{test_liq} - -- " test_taglib.liq "")))