(rule
 (alias test_195)
 (package liquidsoap)
 (deps
  test_195.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_195.liq liquidsoap %{test_liq} test_195.liq)))

(rule
 (alias test_add_all_metadata)
 (package liquidsoap)
 (deps
  test_add_all_metadata.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_add_all_metadata.liq
   liquidsoap
   %{test_liq}
   test_add_all_metadata.liq)))

(rule
 (alias test_autostart)
 (package liquidsoap)
 (deps
  test_autostart.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_autostart.liq
   liquidsoap
   %{test_liq}
   test_autostart.liq)))

(rule
 (alias test_cross-override)
 (package liquidsoap)
 (deps
  test_cross-override.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_cross-override.liq
   liquidsoap
   %{test_liq}
   test_cross-override.liq)))

(rule
 (alias test_cross-persist-override)
 (package liquidsoap)
 (deps
  test_cross-persist-override.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_cross-persist-override.liq
   liquidsoap
   %{test_liq}
   test_cross-persist-override.liq)))

(rule
 (alias test_cross)
 (package liquidsoap)
 (deps
  test_cross.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_cross.liq liquidsoap %{test_liq} test_cross.liq)))

(rule
 (alias test_crossfade)
 (package liquidsoap)
 (deps
  test_crossfade.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_crossfade.liq
   liquidsoap
   %{test_liq}
   test_crossfade.liq)))

(rule
 (alias test_crossfade2)
 (package liquidsoap)
 (deps
  test_crossfade2.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_crossfade2.liq
   liquidsoap
   %{test_liq}
   test_crossfade2.liq)))

(rule
 (alias test_ctype1)
 (package liquidsoap)
 (deps
  test_ctype1.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_ctype1.liq liquidsoap %{test_liq} test_ctype1.liq)))

(rule
 (alias test_ctype2)
 (package liquidsoap)
 (deps
  test_ctype2.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_ctype2.liq liquidsoap %{test_liq} test_ctype2.liq)))

(rule
 (alias test_cue-cut)
 (package liquidsoap)
 (deps
  test_cue-cut.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_cue-cut.liq liquidsoap %{test_liq} test_cue-cut.liq)))

(rule
 (alias test_dtmf)
 (package liquidsoap)
 (deps
  test_dtmf.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_dtmf.liq liquidsoap %{test_liq} test_dtmf.liq)))

(rule
 (alias test_dtmf_pcm_s16)
 (package liquidsoap)
 (deps
  test_dtmf_pcm_s16.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_dtmf_pcm_s16.liq
   liquidsoap
   %{test_liq}
   test_dtmf_pcm_s16.liq)))

(rule
 (alias test_fades-overrides)
 (package liquidsoap)
 (deps
  test_fades-overrides.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_fades-overrides.liq
   liquidsoap
   %{test_liq}
   test_fades-overrides.liq)))

(rule
 (alias test_fades-persistent-override)
 (package liquidsoap)
 (deps
  test_fades-persistent-override.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_fades-persistent-override.liq
   liquidsoap
   %{test_liq}
   test_fades-persistent-override.liq)))

(rule
 (alias test_ffmpeg-errors)
 (package liquidsoap)
 (deps
  test_ffmpeg-errors.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg-errors.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg-errors.liq)))

(rule
 (alias test_ffmpeg-trim)
 (package liquidsoap)
 (deps
  test_ffmpeg-trim.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg-trim.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg-trim.liq)))

(rule
 (alias test_ffmpeg_on_subitles)
 (package liquidsoap)
 (deps
  test_ffmpeg_on_subitles.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg_on_subitles.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg_on_subitles.liq)))

(rule
 (alias test_file_output)
 (package liquidsoap)
 (deps
  test_file_output.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_file_output.liq
   liquidsoap
   %{test_liq}
   test_file_output.liq)))

(rule
 (alias test_harbor_kick_client)
 (package liquidsoap)
 (deps
  test_harbor_kick_client.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_harbor_kick_client.liq
   liquidsoap
   %{test_liq}
   test_harbor_kick_client.liq)))

(rule
 (alias test_harbor_metadata)
 (package liquidsoap)
 (deps
  test_harbor_metadata.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_harbor_metadata.liq
   liquidsoap
   %{test_liq}
   test_harbor_metadata.liq)))

(rule
 (alias test_harbor_metadata_2)
 (package liquidsoap)
 (deps
  test_harbor_metadata_2.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_harbor_metadata_2.liq
   liquidsoap
   %{test_liq}
   test_harbor_metadata_2.liq)))

(rule
 (alias test_harbor_metadata_3)
 (package liquidsoap)
 (deps
  test_harbor_metadata_3.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_harbor_metadata_3.liq
   liquidsoap
   %{test_liq}
   test_harbor_metadata_3.liq)))

(rule
 (alias test_hls_custom_tags)
 (package liquidsoap)
 (deps
  test_hls_custom_tags.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_custom_tags.liq
   liquidsoap
   %{test_liq}
   test_hls_custom_tags.liq)))

(rule
 (alias test_hls_discontinuity)
 (package liquidsoap)
 (deps
  test_hls_discontinuity.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_discontinuity.liq
   liquidsoap
   %{test_liq}
   test_hls_discontinuity.liq)))

(rule
 (alias test_hls_global_headers)
 (package liquidsoap)
 (deps
  test_hls_global_headers.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_global_headers.liq
   liquidsoap
   %{test_liq}
   test_hls_global_headers.liq)))

(rule
 (alias test_hls_id3v2)
 (package liquidsoap)
 (deps
  test_hls_id3v2.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_id3v2.liq
   liquidsoap
   %{test_liq}
   test_hls_id3v2.liq)))

(rule
 (alias test_hls_large_frame)
 (package liquidsoap)
 (deps
  test_hls_large_frame.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_large_frame.liq
   liquidsoap
   %{test_liq}
   test_hls_large_frame.liq)))

(rule
 (alias test_hls_main_playlist)
 (package liquidsoap)
 (deps
  test_hls_main_playlist.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_hls_main_playlist.liq
   liquidsoap
   %{test_liq}
   test_hls_main_playlist.liq)))

(rule
 (alias test_huge-playlist)
 (package liquidsoap)
 (deps
  test_huge-playlist.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_huge-playlist.liq
   liquidsoap
   %{test_liq}
   test_huge-playlist.liq)))

(rule
 (alias test_icecast_last_meta)
 (package liquidsoap)
 (deps
  test_icecast_last_meta.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_icecast_last_meta.liq
   liquidsoap
   %{test_liq}
   test_icecast_last_meta.liq)))

(rule
 (alias test_icecast_no_restart)
 (package liquidsoap)
 (deps
  test_icecast_no_restart.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_icecast_no_restart.liq
   liquidsoap
   %{test_liq}
   test_icecast_no_restart.liq)))

(rule
 (alias test_image)
 (package liquidsoap)
 (deps
  test_image.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_image.liq liquidsoap %{test_liq} test_image.liq)))

(rule
 (alias test_many-playlists)
 (package liquidsoap)
 (deps
  test_many-playlists.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_many-playlists.liq
   liquidsoap
   %{test_liq}
   test_many-playlists.liq)))

(rule
 (alias test_merge_metadata)
 (package liquidsoap)
 (deps
  test_merge_metadata.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_merge_metadata.liq
   liquidsoap
   %{test_liq}
   test_merge_metadata.liq)))

(rule
 (alias test_metadata-override)
 (package liquidsoap)
 (deps
  test_metadata-override.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_metadata-override.liq
   liquidsoap
   %{test_liq}
   test_metadata-override.liq)))

(rule
 (alias test_never)
 (package liquidsoap)
 (deps
  test_never.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_never.liq liquidsoap %{test_liq} test_never.liq)))

(rule
 (alias test_nfo_sidecar_metadata)
 (package liquidsoap)
 (deps
  test_nfo_sidecar_metadata.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_nfo_sidecar_metadata.liq
   liquidsoap
   %{test_liq}
   test_nfo_sidecar_metadata.liq)))

(rule
 (alias test_no-cue-cut)
 (package liquidsoap)
 (deps
  test_no-cue-cut.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_no-cue-cut.liq
   liquidsoap
   %{test_liq}
   test_no-cue-cut.liq)))

(rule
 (alias test_non_repeating)
 (package liquidsoap)
 (deps
  test_non_repeating.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_non_repeating.liq
   liquidsoap
   %{test_liq}
   test_non_repeating.liq)))

(rule
 (alias test_on_end)
 (package liquidsoap)
 (deps
  test_on_end.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_on_end.liq liquidsoap %{test_liq} test_on_end.liq)))

(rule
 (alias test_on_end_partial)
 (package liquidsoap)
 (deps
  test_on_end_partial.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_end_partial.liq
   liquidsoap
   %{test_liq}
   test_on_end_partial.liq)))

(rule
 (alias test_on_frame)
 (package liquidsoap)
 (deps
  test_on_frame.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_frame.liq
   liquidsoap
   %{test_liq}
   test_on_frame.liq)))

(rule
 (alias test_on_metadata)
 (package liquidsoap)
 (deps
  test_on_metadata.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_metadata.liq
   liquidsoap
   %{test_liq}
   test_on_metadata.liq)))

(rule
 (alias test_on_offset)
 (package liquidsoap)
 (deps
  test_on_offset.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_offset.liq
   liquidsoap
   %{test_liq}
   test_on_offset.liq)))

(rule
 (alias test_on_offset_partial)
 (package liquidsoap)
 (deps
  test_on_offset_partial.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_offset_partial.liq
   liquidsoap
   %{test_liq}
   test_on_offset_partial.liq)))

(rule
 (alias test_on_subtitle)
 (package liquidsoap)
 (deps
  test_on_subtitle.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_subtitle.liq
   liquidsoap
   %{test_liq}
   test_on_subtitle.liq)))

(rule
 (alias test_on_track)
 (package liquidsoap)
 (deps
  test_on_track.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_on_track.liq
   liquidsoap
   %{test_liq}
   test_on_track.liq)))

(rule
 (alias test_playlist-watch)
 (package liquidsoap)
 (deps
  test_playlist-watch.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_playlist-watch.liq
   liquidsoap
   %{test_liq}
   test_playlist-watch.liq)))

(rule
 (alias test_radio)
 (package liquidsoap)
 (deps
  test_radio.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_radio.liq liquidsoap %{test_liq} test_radio.liq)))

(rule
 (alias test_radio2)
 (package liquidsoap)
 (deps
  test_radio2.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_radio2.liq liquidsoap %{test_liq} test_radio2.liq)))

(rule
 (alias test_random)
 (package liquidsoap)
 (deps
  test_random.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_random.liq liquidsoap %{test_liq} test_random.liq)))

(rule
 (alias test_replaygain)
 (package liquidsoap)
 (deps
  test_replaygain.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_replaygain.liq
   liquidsoap
   %{test_liq}
   test_replaygain.liq)))

(rule
 (alias test_request)
 (package liquidsoap)
 (deps
  test_request.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_request.liq liquidsoap %{test_liq} test_request.liq)))

(rule
 (alias test_rotate)
 (package liquidsoap)
 (deps
  test_rotate.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_rotate.liq liquidsoap %{test_liq} test_rotate.liq)))

(rule
 (alias test_say)
 (package liquidsoap)
 (deps
  test_say.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_say.liq liquidsoap %{test_liq} test_say.liq)))

(rule
 (alias test_sharing)
 (package liquidsoap)
 (deps
  test_sharing.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_sharing.liq liquidsoap %{test_liq} test_sharing.liq)))

(rule
 (alias test_sine.detect.full_conv)
 (package liquidsoap)
 (deps
  test_sine.detect.full_conv.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_sine.detect.full_conv.liq
   liquidsoap
   %{test_liq}
   test_sine.detect.full_conv.liq)))

(rule
 (alias test_sine.detect)
 (package liquidsoap)
 (deps
  test_sine.detect.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_sine.detect.liq
   liquidsoap
   %{test_liq}
   test_sine.detect.liq)))

(rule
 (alias test_sine.detect.pcm_f32)
 (package liquidsoap)
 (deps
  test_sine.detect.pcm_f32.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_sine.detect.pcm_f32.liq
   liquidsoap
   %{test_liq}
   test_sine.detect.pcm_f32.liq)))

(rule
 (alias test_sine.detect.pcm_s16)
 (package liquidsoap)
 (deps
  test_sine.detect.pcm_s16.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_sine.detect.pcm_s16.liq
   liquidsoap
   %{test_liq}
   test_sine.detect.pcm_s16.liq)))

(rule
 (alias test_soundtouch-tracks)
 (package liquidsoap)
 (deps
  test_soundtouch-tracks.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_soundtouch-tracks.liq
   liquidsoap
   %{test_liq}
   test_soundtouch-tracks.liq)))

(rule
 (alias test_source-cue)
 (package liquidsoap)
 (deps
  test_source-cue.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_source-cue.liq
   liquidsoap
   %{test_liq}
   test_source-cue.liq)))

(rule
 (alias test_srt_listen_callback)
 (package liquidsoap)
 (deps
  test_srt_listen_callback.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_srt_listen_callback.liq
   liquidsoap
   %{test_liq}
   test_srt_listen_callback.liq)))

(rule
 (alias test_srt_multiple_outputs)
 (package liquidsoap)
 (deps
  test_srt_multiple_outputs.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_srt_multiple_outputs.liq
   liquidsoap
   %{test_liq}
   test_srt_multiple_outputs.liq)))

(rule
 (alias test_srt_passphrase)
 (package liquidsoap)
 (deps
  test_srt_passphrase.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_srt_passphrase.liq
   liquidsoap
   %{test_liq}
   test_srt_passphrase.liq)))

(rule
 (alias test_srt_raw_pcm)
 (package liquidsoap)
 (deps
  test_srt_raw_pcm.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_srt_raw_pcm.liq
   liquidsoap
   %{test_liq}
   test_srt_raw_pcm.liq)))

(rule
 (alias test_stretch-clock-propagation)
 (package liquidsoap)
 (deps
  test_stretch-clock-propagation.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_stretch-clock-propagation.liq
   liquidsoap
   %{test_liq}
   test_stretch-clock-propagation.liq)))

(rule
 (alias test_string_binary_flags)
 (package liquidsoap)
 (deps
  test_string_binary_flags.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_string_binary_flags.liq
   liquidsoap
   %{test_liq}
   test_string_binary_flags.liq)))

(rule
 (alias test_subtitles_insert)
 (package liquidsoap)
 (deps
  test_subtitles_insert.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_subtitles_insert.liq
   liquidsoap
   %{test_liq}
   test_subtitles_insert.liq)))

(rule
 (alias test_subtitles_map)
 (package liquidsoap)
 (deps
  test_subtitles_map.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_subtitles_map.liq
   liquidsoap
   %{test_liq}
   test_subtitles_map.liq)))

(rule
 (alias test_subtitles_track_insert)
 (package liquidsoap)
 (deps
  test_subtitles_track_insert.liq
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ../media/test-subtitle.srt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_subtitles_track_insert.liq
   liquidsoap
   %{test_liq}
   test_subtitles_track_insert.liq)))

(alias
 (name citest)
 (deps
  (alias test_195)
  (alias test_add_all_metadata)
  (alias test_autostart)
  (alias test_cross)
  (alias test_cross-override)
  (alias test_cross-persist-override)
  (alias test_crossfade)
  (alias test_crossfade2)
  (alias test_ctype1)
  (alias test_ctype2)
  (alias test_cue-cut)
  (alias test_dtmf)
  (alias test_dtmf_pcm_s16)
  (alias test_fades-overrides)
  (alias test_fades-persistent-override)
  (alias test_ffmpeg-errors)
  (alias test_ffmpeg-trim)
  (alias test_ffmpeg_on_subitles)
  (alias test_file_output)
  (alias test_harbor_kick_client)
  (alias test_harbor_metadata)
  (alias test_harbor_metadata_2)
  (alias test_harbor_metadata_3)
  (alias test_hls_custom_tags)
  (alias test_hls_discontinuity)
  (alias test_hls_global_headers)
  (alias test_hls_id3v2)
  (alias test_hls_large_frame)
  (alias test_hls_main_playlist)
  (alias test_huge-playlist)
  (alias test_icecast_last_meta)
  (alias test_icecast_no_restart)
  (alias test_image)
  (alias test_many-playlists)
  (alias test_merge_metadata)
  (alias test_metadata-override)
  (alias test_never)
  (alias test_nfo_sidecar_metadata)
  (alias test_no-cue-cut)
  (alias test_non_repeating)
  (alias test_on_end)
  (alias test_on_end_partial)
  (alias test_on_frame)
  (alias test_on_metadata)
  (alias test_on_offset)
  (alias test_on_offset_partial)
  (alias test_on_subtitle)
  (alias test_on_track)
  (alias test_playlist-watch)
  (alias test_radio)
  (alias test_radio2)
  (alias test_random)
  (alias test_replaygain)
  (alias test_request)
  (alias test_rotate)
  (alias test_say)
  (alias test_sharing)
  (alias test_sine.detect)
  (alias test_sine.detect.full_conv)
  (alias test_sine.detect.pcm_f32)
  (alias test_sine.detect.pcm_s16)
  (alias test_soundtouch-tracks)
  (alias test_source-cue)
  (alias test_srt_listen_callback)
  (alias test_srt_multiple_outputs)
  (alias test_srt_passphrase)
  (alias test_srt_raw_pcm)
  (alias test_stretch-clock-propagation)
  (alias test_string_binary_flags)
  (alias test_subtitles_insert)
  (alias test_subtitles_map)
  (alias test_subtitles_track_insert)))
