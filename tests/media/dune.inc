
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @fdkaac[aot='mpeg4_aac_lc',channels=1]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%fdkaac(aot=\"mpeg4_aac_lc\",channels=1)" sine "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @fdkaac[channels=2]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%fdkaac(channels=2)" sine "@fdkaac[channels=2].aac"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @shine[channels=1]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%shine(channels=1)" sine "@shine[channels=1].mp3"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @shine[channels=2]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%shine(channels=2)" sine "@shine[channels=2].mp3"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @flac[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(stereo)" sine "@flac[stereo].flac"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @flac[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%flac(mono)" sine "@flac[mono].flac"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @wav[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(stereo)" sine "@wav[stereo].wav"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @wav[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%wav(mono)" sine "@wav[mono].wav"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @mp3[mono]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(mono)" sine "@mp3[mono].mp3"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @mp3[stereo]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%mp3(stereo)" sine "@mp3[stereo].mp3"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@vorbis[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(mono))" sine "@ogg[@vorbis[mono]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@vorbis[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%vorbis(stereo))" sine "@ogg[@vorbis[stereo]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@flac[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(mono))" sine "@ogg[@flac[mono]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@flac[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%flac(stereo))" sine "@ogg[@flac[stereo]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@opus[mono]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(mono))" sine "@ogg[@opus[mono]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ogg[@opus[stereo]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ogg(%opus(stereo))" sine "@ogg[@opus[stereo]].ogg"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"))" sine "@ffmpeg[format='mp4',@audio[codec='aac']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(pcm_s16,codec=\"aac\"))" sine "@ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(pcm_f32,codec=\"aac\"))" sine "@ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\",r=12))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%audio_2(codec=\"aac\",channels=1),%video(codec=\"libx264\"),%video_2(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%gno(audio_content,codec=\"aac\",channels=2),%gni(audio_content,codec=\"aac\",channels=1),%bar(video_content,codec=\"libx264\"),%foo(video_content,codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4"))))
(rule
  (alias mediatest)
  (package liquidsoap)
  (target @ffmpeg[format='mp4',@video[codec='libx264']]_encoder.liq)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder.liq.in))
  (action
    (with-stdout-to %{target}
      (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%video(codec=\"libx264\"))" noise "@ffmpeg[format='mp4',@video[codec='libx264']].mp4"))))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @fdkaac[aot='mpeg4_aac_lc',channels=1].aac)
 (deps
  (:encoder @fdkaac[aot='mpeg4_aac_lc',channels=1]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%fdkaac(aot=\"mpeg4_aac_lc\",channels=1)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @fdkaac[channels=2].aac)
 (deps
  (:encoder @fdkaac[channels=2]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%fdkaac(channels=2)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @shine[channels=1].mp3)
 (deps
  (:encoder @shine[channels=1]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%shine(channels=1)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @shine[channels=2].mp3)
 (deps
  (:encoder @shine[channels=2]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%shine(channels=2)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @flac[stereo].flac)
 (deps
  (:encoder @flac[stereo]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%flac(stereo)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @flac[mono].flac)
 (deps
  (:encoder @flac[mono]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%flac(mono)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @wav[stereo].wav)
 (deps
  (:encoder @wav[stereo]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%wav(stereo)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @wav[mono].wav)
 (deps
  (:encoder @wav[mono]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%wav(mono)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @mp3[mono].mp3)
 (deps
  (:encoder @mp3[mono]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%mp3(mono)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @mp3[stereo].mp3)
 (deps
  (:encoder @mp3[stereo]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%mp3(stereo)")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@vorbis[mono]].ogg)
 (deps
  (:encoder @ogg[@vorbis[mono]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%vorbis(mono))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@vorbis[stereo]].ogg)
 (deps
  (:encoder @ogg[@vorbis[stereo]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%vorbis(stereo))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@flac[mono]].ogg)
 (deps
  (:encoder @ogg[@flac[mono]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%flac(mono))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@flac[stereo]].ogg)
 (deps
  (:encoder @ogg[@flac[stereo]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%flac(stereo))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@opus[mono]].ogg)
 (deps
  (:encoder @ogg[@opus[mono]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%opus(mono))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ogg[@opus[stereo]].ogg)
 (deps
  (:encoder @ogg[@opus[stereo]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ogg(%opus(stereo))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(pcm_s16,codec=\"aac\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(pcm_f32,codec=\"aac\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\",r=12))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%audio_2(codec=\"aac\",channels=1),%video(codec=\"libx264\"),%video_2(codec=\"libx264\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%gno(audio_content,codec=\"aac\",channels=2),%gni(audio_content,codec=\"aac\",channels=1),%bar(video_content,codec=\"libx264\"),%foo(video_content,codec=\"libx264\"))")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (target @ffmpeg[format='mp4',@video[codec='libx264']].mp4)
 (deps
  (:encoder @ffmpeg[format='mp4',@video[codec='libx264']]_encoder.liq)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %{run_test} %{encoder} liquidsoap %{test_liq} %{encoder} -- "%ffmpeg(format=\"mp4\",%video(codec=\"libx264\"))")))
(rule
  (alias mediatest)
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
@ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4
@ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4
@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4
@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4
@ffmpeg[format='mp4',@video[codec='libx264']].mp4)
  (action (run touch %{target})))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} mono.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} stereo.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @fdkaac[aot='mpeg4_aac_lc',channels=1].aac" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@fdkaac[aot='mpeg4_aac_lc',channels=1].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} mono.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} stereo.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @fdkaac[channels=2].aac" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@fdkaac[channels=2].aac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @shine[channels=1].mp3" liquidsoap %{test_liq} mono.liq -- "@shine[channels=1].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @shine[channels=1].mp3" liquidsoap %{test_liq} stereo.liq -- "@shine[channels=1].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @shine[channels=1].mp3" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@shine[channels=1].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @shine[channels=2].mp3" liquidsoap %{test_liq} mono.liq -- "@shine[channels=2].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @shine[channels=2].mp3" liquidsoap %{test_liq} stereo.liq -- "@shine[channels=2].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @shine[channels=2].mp3" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@shine[channels=2].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @flac[stereo].flac" liquidsoap %{test_liq} mono.liq -- "@flac[stereo].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @flac[stereo].flac" liquidsoap %{test_liq} stereo.liq -- "@flac[stereo].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @flac[stereo].flac" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@flac[stereo].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @flac[mono].flac" liquidsoap %{test_liq} mono.liq -- "@flac[mono].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @flac[mono].flac" liquidsoap %{test_liq} stereo.liq -- "@flac[mono].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @flac[mono].flac" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@flac[mono].flac")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @wav[stereo].wav" liquidsoap %{test_liq} mono.liq -- "@wav[stereo].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @wav[stereo].wav" liquidsoap %{test_liq} stereo.liq -- "@wav[stereo].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @wav[stereo].wav" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@wav[stereo].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @wav[mono].wav" liquidsoap %{test_liq} mono.liq -- "@wav[mono].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @wav[mono].wav" liquidsoap %{test_liq} stereo.liq -- "@wav[mono].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @wav[mono].wav" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@wav[mono].wav")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @mp3[mono].mp3" liquidsoap %{test_liq} mono.liq -- "@mp3[mono].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @mp3[mono].mp3" liquidsoap %{test_liq} stereo.liq -- "@mp3[mono].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @mp3[mono].mp3" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@mp3[mono].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @mp3[stereo].mp3" liquidsoap %{test_liq} mono.liq -- "@mp3[stereo].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @mp3[stereo].mp3" liquidsoap %{test_liq} stereo.liq -- "@mp3[stereo].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @mp3[stereo].mp3" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@mp3[stereo].mp3")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@vorbis[mono]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@vorbis[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@vorbis[stereo]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@vorbis[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@flac[mono]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@flac[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@flac[stereo]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@flac[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@opus[mono]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@opus[mono]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} mono.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} stereo.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ogg[@opus[stereo]].ogg" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ogg[@opus[stereo]].ogg")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} mono.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} stereo.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ffmpeg[format='mp4',@audio[codec='aac']].mp4" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4" liquidsoap %{test_liq} mono.liq -- "@ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4" liquidsoap %{test_liq} stereo.liq -- "@ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ffmpeg[format='mp4',@audio[pcm_s16,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  mono.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Mono decoding test for @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4" liquidsoap %{test_liq} mono.liq -- "@ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  stereo.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "Stereo decoding test for @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4" liquidsoap %{test_liq} stereo.liq -- "@ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_audio_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg audio decoder test for @ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4" liquidsoap %{test_liq} ffmpeg_audio_decoder.liq -- "@ffmpeg[format='mp4',@audio[pcm_f32,codec='aac']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@video[codec='libx264']].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_video_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_video_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  video_size.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg video size test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} video_size.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@video[codec='libx264',r=12]].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_add_text.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg add text filter test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_add_text.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_copy_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg copy+encode decode test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_copy_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_filter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg filter test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_filter.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw decoder test test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_encode_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+encode decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_encode_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_and_copy_decoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "FFmpeg raw+copy decoder test for @ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4" liquidsoap %{test_liq} ffmpeg_raw_and_copy_decoder.liq -- "@ffmpeg[format='mp4',@gno[audio_content,codec='aac',channels=2],@gni[audio_content,codec='aac',channels=1],@bar[video_content,codec='libx264'],@foo[video_content,codec='libx264']].mp4")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  multitrack.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "multitrack.liq" liquidsoap %{test_liq} multitrack.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_inline_encode_decode.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "ffmpeg_inline_encode_decode.liq" liquidsoap %{test_liq} ffmpeg_inline_encode_decode.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_inline_encode_decode_audio.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "ffmpeg_inline_encode_decode_audio.liq" liquidsoap %{test_liq} ffmpeg_inline_encode_decode_audio.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_inline_encode_decode_video.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "ffmpeg_inline_encode_decode_video.liq" liquidsoap %{test_liq} ffmpeg_inline_encode_decode_video.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  ffmpeg_raw_hls.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "ffmpeg_raw_hls.liq" liquidsoap %{test_liq} ffmpeg_raw_hls.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  taglib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "taglib.liq" liquidsoap %{test_liq} taglib.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  pcm_s16_decode.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "pcm_s16_decode.liq" liquidsoap %{test_liq} pcm_s16_decode.liq -- "")))
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  pcm_f32_decode.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} "pcm_f32_decode.liq" liquidsoap %{test_liq} pcm_f32_decode.liq -- "")))
(rule
  (alias fmt)
  (deps ffmpeg_add_text.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_add_text.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_add_text.liq)))
      (diff ffmpeg_add_text.liq ffmpeg_add_text.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_audio_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_audio_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_audio_decoder.liq)))
      (diff ffmpeg_audio_decoder.liq ffmpeg_audio_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_complex_filter.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_complex_filter.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_complex_filter.liq)))
      (diff ffmpeg_complex_filter.liq ffmpeg_complex_filter.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_copy_and_encode_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_copy_and_encode_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_copy_and_encode_decoder.liq)))
      (diff ffmpeg_copy_and_encode_decoder.liq ffmpeg_copy_and_encode_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_copy_concat.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_copy_concat.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_copy_concat.liq)))
      (diff ffmpeg_copy_concat.liq ffmpeg_copy_concat.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_copy_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_copy_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_copy_decoder.liq)))
      (diff ffmpeg_copy_decoder.liq ffmpeg_copy_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_distributed_hls.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_distributed_hls.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_distributed_hls.liq)))
      (diff ffmpeg_distributed_hls.liq ffmpeg_distributed_hls.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_filter.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_filter.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_filter.liq)))
      (diff ffmpeg_filter.liq ffmpeg_filter.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_filter_changing_rate.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_filter_changing_rate.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_filter_changing_rate.liq)))
      (diff ffmpeg_filter_changing_rate.liq ffmpeg_filter_changing_rate.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_inline_encode_decode.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_inline_encode_decode.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_inline_encode_decode.liq)))
      (diff ffmpeg_inline_encode_decode.liq ffmpeg_inline_encode_decode.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_inline_encode_decode_audio.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_inline_encode_decode_audio.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_inline_encode_decode_audio.liq)))
      (diff ffmpeg_inline_encode_decode_audio.liq ffmpeg_inline_encode_decode_audio.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_inline_encode_decode_video.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_inline_encode_decode_video.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_inline_encode_decode_video.liq)))
      (diff ffmpeg_inline_encode_decode_video.liq ffmpeg_inline_encode_decode_video.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_raw_and_copy_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_raw_and_copy_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_raw_and_copy_decoder.liq)))
      (diff ffmpeg_raw_and_copy_decoder.liq ffmpeg_raw_and_copy_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_raw_and_encode_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_raw_and_encode_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_raw_and_encode_decoder.liq)))
      (diff ffmpeg_raw_and_encode_decoder.liq ffmpeg_raw_and_encode_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_raw_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_raw_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_raw_decoder.liq)))
      (diff ffmpeg_raw_decoder.liq ffmpeg_raw_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_raw_hls.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_raw_hls.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_raw_hls.liq)))
      (diff ffmpeg_raw_hls.liq ffmpeg_raw_hls.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_transparency_filter.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_transparency_filter.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_transparency_filter.liq)))
      (diff ffmpeg_transparency_filter.liq ffmpeg_transparency_filter.liq.prettier))))

(rule
  (alias fmt)
  (deps ffmpeg_video_decoder.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ffmpeg_video_decoder.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/ffmpeg_video_decoder.liq)))
      (diff ffmpeg_video_decoder.liq ffmpeg_video_decoder.liq.prettier))))

(rule
  (alias fmt)
  (deps harbor_audio.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to harbor_audio.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/harbor_audio.liq)))
      (diff harbor_audio.liq harbor_audio.liq.prettier))))

(rule
  (alias fmt)
  (deps mono.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to mono.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/mono.liq)))
      (diff mono.liq mono.liq.prettier))))

(rule
  (alias fmt)
  (deps multitrack.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to multitrack.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/multitrack.liq)))
      (diff multitrack.liq multitrack.liq.prettier))))

(rule
  (alias fmt)
  (deps pcm_f32_decode.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to pcm_f32_decode.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/pcm_f32_decode.liq)))
      (diff pcm_f32_decode.liq pcm_f32_decode.liq.prettier))))

(rule
  (alias fmt)
  (deps pcm_s16_decode.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to pcm_s16_decode.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/pcm_s16_decode.liq)))
      (diff pcm_s16_decode.liq pcm_s16_decode.liq.prettier))))

(rule
  (alias fmt)
  (deps stereo.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to stereo.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/stereo.liq)))
      (diff stereo.liq stereo.liq.prettier))))

(rule
  (alias fmt)
  (deps taglib.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to taglib.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/taglib.liq)))
      (diff taglib.liq taglib.liq.prettier))))

(rule
  (alias fmt)
  (deps video_size.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to video_size.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/media/video_size.liq)))
      (diff video_size.liq video_size.liq.prettier))))
