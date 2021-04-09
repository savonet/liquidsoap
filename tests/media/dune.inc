
(rule
 (target encoder-stereo.flac.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo.flac.liq
    (run %{mk_encoder_test} "%flac(stereo)" sine))))

(rule
 (target encoder-mono.flac.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono.flac.liq
    (run %{mk_encoder_test} "%flac(mono)" sine))))

(rule
 (target encoder-stereo.wav.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo.wav.liq
    (run %{mk_encoder_test} "%wav(stereo)" sine))))

(rule
 (target encoder-mono.wav.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono.wav.liq
    (run %{mk_encoder_test} "%wav(mono)" sine))))

(rule
 (target encoder-stereo.mp3.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo.mp3.liq
    (run %{mk_encoder_test} "%mp3(stereo)" sine))))

(rule
 (target encoder-mono.mp3.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono.mp3.liq
    (run %{mk_encoder_test} "%mp3(mono)" sine))))

(rule
 (target encoder-stereo_vorbis.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo_vorbis.ogg.liq
    (run %{mk_encoder_test} "%vorbis(stereo)" sine))))

(rule
 (target encoder-mono_vorbis.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono_vorbis.ogg.liq
    (run %{mk_encoder_test} "%vorbis(mono)" sine))))

(rule
 (target encoder-stereo_flac.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo_flac.ogg.liq
    (run %{mk_encoder_test} "%ogg(%flac(stereo))" sine))))

(rule
 (target encoder-mono_flac.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono_flac.ogg.liq
    (run %{mk_encoder_test} "%ogg(%flac(mono))" sine))))

(rule
 (target encoder-stereo_opus.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo_opus.ogg.liq
    (run %{mk_encoder_test} "%opus(stereo)" sine))))

(rule
 (target encoder-mono_opus.ogg.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono_opus.ogg.liq
    (run %{mk_encoder_test} "%opus(mono)" sine))))

(rule
 (target encoder-mono.m4a.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono.m4a.liq
    (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video.none)" sine))))

(rule
 (target encoder-stereo.m4a.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo.m4a.liq
    (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video.none)" sine))))

(rule
 (target encoder-mono_video.mp4.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-mono_video.mp4.liq
    (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))" noise))))

(rule
 (target encoder-stereo_video.mp4.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-stereo_video.mp4.liq
    (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))" noise))))

(rule
 (target encoder-video_only.mp4.liq)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to encoder-video_only.mp4.liq
    (run %{mk_encoder_test} "%ffmpeg(format=\"mp4\",%audio.none,%video(codec=\"libx264\"))" noise))))

(rule
 (targets stereo.flac)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo.flac.liq)
  (action
    (progn
      (echo "Generating stereo.flac..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo.flac.liq -- stereo.flac)))))

(rule
 (targets mono.flac)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono.flac.liq)
  (action
    (progn
      (echo "Generating mono.flac..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono.flac.liq -- mono.flac)))))

(rule
 (targets stereo.wav)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo.wav.liq)
  (action
    (progn
      (echo "Generating stereo.wav..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo.wav.liq -- stereo.wav)))))

(rule
 (targets mono.wav)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono.wav.liq)
  (action
    (progn
      (echo "Generating mono.wav..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono.wav.liq -- mono.wav)))))

(rule
 (targets stereo.mp3)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo.mp3.liq)
  (action
    (progn
      (echo "Generating stereo.mp3..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo.mp3.liq -- stereo.mp3)))))

(rule
 (targets mono.mp3)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono.mp3.liq)
  (action
    (progn
      (echo "Generating mono.mp3..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono.mp3.liq -- mono.mp3)))))

(rule
 (targets stereo_vorbis.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo_vorbis.ogg.liq)
  (action
    (progn
      (echo "Generating stereo_vorbis.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo_vorbis.ogg.liq -- stereo_vorbis.ogg)))))

(rule
 (targets mono_vorbis.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono_vorbis.ogg.liq)
  (action
    (progn
      (echo "Generating mono_vorbis.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono_vorbis.ogg.liq -- mono_vorbis.ogg)))))

(rule
 (targets stereo_flac.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo_flac.ogg.liq)
  (action
    (progn
      (echo "Generating stereo_flac.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo_flac.ogg.liq -- stereo_flac.ogg)))))

(rule
 (targets mono_flac.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono_flac.ogg.liq)
  (action
    (progn
      (echo "Generating mono_flac.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono_flac.ogg.liq -- mono_flac.ogg)))))

(rule
 (targets stereo_opus.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo_opus.ogg.liq)
  (action
    (progn
      (echo "Generating stereo_opus.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo_opus.ogg.liq -- stereo_opus.ogg)))))

(rule
 (targets mono_opus.ogg)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono_opus.ogg.liq)
  (action
    (progn
      (echo "Generating mono_opus.ogg..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono_opus.ogg.liq -- mono_opus.ogg)))))

(rule
 (targets mono.m4a)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono.m4a.liq)
  (action
    (progn
      (echo "Generating mono.m4a..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono.m4a.liq -- mono.m4a)))))

(rule
 (targets stereo.m4a)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo.m4a.liq)
  (action
    (progn
      (echo "Generating stereo.m4a..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo.m4a.liq -- stereo.m4a)))))

(rule
 (targets mono_video.mp4)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-mono_video.mp4.liq)
  (action
    (progn
      (echo "Generating mono_video.mp4..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-mono_video.mp4.liq -- mono_video.mp4)))))

(rule
 (targets stereo_video.mp4)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-stereo_video.mp4.liq)
  (action
    (progn
      (echo "Generating stereo_video.mp4..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-stereo_video.mp4.liq -- stereo_video.mp4)))))

(rule
 (targets video_only.mp4)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   encoder-video_only.mp4.liq)
  (action
    (progn
      (echo "Generating video_only.mp4..\n")
      (ignore-outputs
        (run %{liquidsoap} --no-libs %{main_lib} encoder-video_only.mp4.liq -- video_only.mp4)))))

(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:main_lib ../../libs/main.liq)
  (:run_test ../run_test.sh)
  stereo.flac
mono.flac
stereo.wav
mono.wav
stereo.mp3
mono.mp3
stereo_vorbis.ogg
mono_vorbis.ogg
stereo_flac.ogg
mono_flac.ogg
stereo_opus.ogg
mono_opus.ogg
mono.m4a
stereo.m4a
mono_video.mp4
stereo_video.mp4
video_only.mp4
  ../test.liq
  (source_tree ../../libs)
  (source_tree .))
 (action
  (progn
    (run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for flac stereo" "" stereo.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for flac stereo" "" stereo.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for flac stereo" "" stereo.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for flac mono" "" mono.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for flac mono" "" mono.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for flac mono" "" mono.flac)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for wav mono" "" stereo.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for wav mono" "" stereo.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for wav mono" "" stereo.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for wav mono" "" mono.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for wav mono" "" mono.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for wav mono" "" mono.wav)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for mp3 stereo" "" stereo.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for mp3 stereo" "" stereo.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for mp3 stereo" "" stereo.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for mp3 mono" "" mono.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for mp3 mono" "" mono.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for mp3 mono" "" mono.mp3)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for vorbis stereo" "" stereo_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for vorbis stereo" "" stereo_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for vorbis stereo" "" stereo_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for vorbis mono" "" mono_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for vorbis mono" "" mono_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for vorbis mono" "" mono_vorbis.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for ogg/flac stereo" "" stereo_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for ogg/flac stereo" "" stereo_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for ogg/flac stereo" "" stereo_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for ogg/flac mono" "" mono_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for ogg/flac mono" "" mono_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for ogg/flac mono" "" mono_flac.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for opus stereo" "" stereo_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for opus stereo" "" stereo_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for opus stereo" "" stereo_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for opus mono" "" mono_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for opus mono" "" mono_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for opus mono" "" mono_opus.ogg)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for mpeg4 stereo audio only" "" mono.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for mpeg4 stereo audio only" "" mono.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for mpeg4 stereo audio only" "" mono.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_mono.liq "mono decoding test for mpeg4 mono audio only" "" stereo.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_stereo.liq "stereo decoding test for mpeg4 mono audio only" "" stereo.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_audio_decoder.liq "ffmpeg audio decoder test for mpeg4 mono audio only" "" stereo.m4a)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_copy_decoder.liq "ffmpeg copy decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_copy_and_encode_decoder.liq "ffmpeg copy+encode decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_decoder.liq "ffmpeg raw decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_and_encode_decoder.liq "ffmpeg raw+encode decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_and_copy_decoder.liq "ffmpeg raw+copy decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_video_decoder.liq "ffmpeg video decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_gstreamer_video_decoder.liq "gstreamer video decoder test for mpeg4 mono audio & video" "" mono_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_copy_decoder.liq "ffmpeg copy decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_copy_and_encode_decoder.liq "ffmpeg copy+encode decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_decoder.liq "ffmpeg raw decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_and_encode_decoder.liq "ffmpeg raw+encode decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_and_copy_decoder.liq "ffmpeg raw+copy decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_video_decoder.liq "ffmpeg video decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_gstreamer_video_decoder.liq "gstreamer video decoder test for mpeg4 stereo audio & video" "" stereo_video.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_video_decoder.liq "ffmpeg video decoder test for mpeg4 video only" "" video_only.mp4)
(run %{run_test} %{liquidsoap} %{main_lib} test_gstreamer_video_decoder.liq "gstreamer video decoder test for mpeg4 video only" "" video_only.mp4)
    (run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_inline_encode_decode.liq "ffmpeg inline encode+decode")
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_inline_encode_decode_audio.liq "ffmpeg inline encode+decode audio only")
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_inline_encode_decode_video.liq "ffmpeg inline encode+decode video only")
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_distributed_hls.liq "ffmpeg distributed HLS")
(run %{run_test} %{liquidsoap} %{main_lib} test_ffmpeg_raw_hls.liq "ffmpeg raw HLS")
  )))
