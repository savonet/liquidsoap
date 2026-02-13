(rule
 (alias 195)
 (package liquidsoap)
 (deps
  195.liq
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
  (run %{run_test} 195.liq liquidsoap %{test_liq} 195.liq)))

(rule
 (alias add_all_metadata)
 (package liquidsoap)
 (deps
  add_all_metadata.liq
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
   add_all_metadata.liq
   liquidsoap
   %{test_liq}
   add_all_metadata.liq)))

(rule
 (alias autostart)
 (package liquidsoap)
 (deps
  autostart.liq
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
  (run %{run_test} autostart.liq liquidsoap %{test_liq} autostart.liq)))

(rule
 (alias cross-override)
 (package liquidsoap)
 (deps
  cross-override.liq
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
   cross-override.liq
   liquidsoap
   %{test_liq}
   cross-override.liq)))

(rule
 (alias cross-persist-override)
 (package liquidsoap)
 (deps
  cross-persist-override.liq
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
   cross-persist-override.liq
   liquidsoap
   %{test_liq}
   cross-persist-override.liq)))

(rule
 (alias cross)
 (package liquidsoap)
 (deps
  cross.liq
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
  (run %{run_test} cross.liq liquidsoap %{test_liq} cross.liq)))

(rule
 (alias crossfade)
 (package liquidsoap)
 (deps
  crossfade.liq
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
  (run %{run_test} crossfade.liq liquidsoap %{test_liq} crossfade.liq)))

(rule
 (alias crossfade2)
 (package liquidsoap)
 (deps
  crossfade2.liq
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
  (run %{run_test} crossfade2.liq liquidsoap %{test_liq} crossfade2.liq)))

(rule
 (alias ctype1)
 (package liquidsoap)
 (deps
  ctype1.liq
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
  (run %{run_test} ctype1.liq liquidsoap %{test_liq} ctype1.liq)))

(rule
 (alias ctype2)
 (package liquidsoap)
 (deps
  ctype2.liq
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
  (run %{run_test} ctype2.liq liquidsoap %{test_liq} ctype2.liq)))

(rule
 (alias cue-cut)
 (package liquidsoap)
 (deps
  cue-cut.liq
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
  (run %{run_test} cue-cut.liq liquidsoap %{test_liq} cue-cut.liq)))

(rule
 (alias dtmf)
 (package liquidsoap)
 (deps
  dtmf.liq
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
  (run %{run_test} dtmf.liq liquidsoap %{test_liq} dtmf.liq)))

(rule
 (alias dtmf_pcm_s16)
 (package liquidsoap)
 (deps
  dtmf_pcm_s16.liq
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
  (run %{run_test} dtmf_pcm_s16.liq liquidsoap %{test_liq} dtmf_pcm_s16.liq)))

(rule
 (alias fades-overrides)
 (package liquidsoap)
 (deps
  fades-overrides.liq
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
   fades-overrides.liq
   liquidsoap
   %{test_liq}
   fades-overrides.liq)))

(rule
 (alias fades-persistent-override)
 (package liquidsoap)
 (deps
  fades-persistent-override.liq
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
   fades-persistent-override.liq
   liquidsoap
   %{test_liq}
   fades-persistent-override.liq)))

(rule
 (alias ffmpeg-errors)
 (package liquidsoap)
 (deps
  ffmpeg-errors.liq
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
   ffmpeg-errors.liq
   liquidsoap
   %{test_liq}
   ffmpeg-errors.liq)))

(rule
 (alias ffmpeg-trim)
 (package liquidsoap)
 (deps
  ffmpeg-trim.liq
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
  (run %{run_test} ffmpeg-trim.liq liquidsoap %{test_liq} ffmpeg-trim.liq)))

(rule
 (alias file_output)
 (package liquidsoap)
 (deps
  file_output.liq
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
  (run %{run_test} file_output.liq liquidsoap %{test_liq} file_output.liq)))

(rule
 (alias harbor_kick_client)
 (package liquidsoap)
 (deps
  harbor_kick_client.liq
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
   harbor_kick_client.liq
   liquidsoap
   %{test_liq}
   harbor_kick_client.liq)))

(rule
 (alias harbor_metadata)
 (package liquidsoap)
 (deps
  harbor_metadata.liq
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
   harbor_metadata.liq
   liquidsoap
   %{test_liq}
   harbor_metadata.liq)))

(rule
 (alias harbor_metadata_2)
 (package liquidsoap)
 (deps
  harbor_metadata_2.liq
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
   harbor_metadata_2.liq
   liquidsoap
   %{test_liq}
   harbor_metadata_2.liq)))

(rule
 (alias harbor_metadata_3)
 (package liquidsoap)
 (deps
  harbor_metadata_3.liq
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
   harbor_metadata_3.liq
   liquidsoap
   %{test_liq}
   harbor_metadata_3.liq)))

(rule
 (alias hls_custom_tags)
 (package liquidsoap)
 (deps
  hls_custom_tags.liq
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
   hls_custom_tags.liq
   liquidsoap
   %{test_liq}
   hls_custom_tags.liq)))

(rule
 (alias hls_discontinuity)
 (package liquidsoap)
 (deps
  hls_discontinuity.liq
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
   hls_discontinuity.liq
   liquidsoap
   %{test_liq}
   hls_discontinuity.liq)))

(rule
 (alias hls_global_headers)
 (package liquidsoap)
 (deps
  hls_global_headers.liq
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
   hls_global_headers.liq
   liquidsoap
   %{test_liq}
   hls_global_headers.liq)))

(rule
 (alias hls_id3v2)
 (package liquidsoap)
 (deps
  hls_id3v2.liq
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
  (run %{run_test} hls_id3v2.liq liquidsoap %{test_liq} hls_id3v2.liq)))

(rule
 (alias hls_large_frame)
 (package liquidsoap)
 (deps
  hls_large_frame.liq
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
   hls_large_frame.liq
   liquidsoap
   %{test_liq}
   hls_large_frame.liq)))

(rule
 (alias hls_main_playlist)
 (package liquidsoap)
 (deps
  hls_main_playlist.liq
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
   hls_main_playlist.liq
   liquidsoap
   %{test_liq}
   hls_main_playlist.liq)))

(rule
 (alias huge-playlist)
 (package liquidsoap)
 (deps
  huge-playlist.liq
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
   huge-playlist.liq
   liquidsoap
   %{test_liq}
   huge-playlist.liq)))

(rule
 (alias icecast_last_meta)
 (package liquidsoap)
 (deps
  icecast_last_meta.liq
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
   icecast_last_meta.liq
   liquidsoap
   %{test_liq}
   icecast_last_meta.liq)))

(rule
 (alias icecast_no_restart)
 (package liquidsoap)
 (deps
  icecast_no_restart.liq
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
   icecast_no_restart.liq
   liquidsoap
   %{test_liq}
   icecast_no_restart.liq)))

(rule
 (alias image)
 (package liquidsoap)
 (deps
  image.liq
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
  (run %{run_test} image.liq liquidsoap %{test_liq} image.liq)))

(rule
 (alias many-playlists)
 (package liquidsoap)
 (deps
  many-playlists.liq
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
   many-playlists.liq
   liquidsoap
   %{test_liq}
   many-playlists.liq)))

(rule
 (alias merge_metadata)
 (package liquidsoap)
 (deps
  merge_metadata.liq
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
   merge_metadata.liq
   liquidsoap
   %{test_liq}
   merge_metadata.liq)))

(rule
 (alias metadata-override)
 (package liquidsoap)
 (deps
  metadata-override.liq
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
   metadata-override.liq
   liquidsoap
   %{test_liq}
   metadata-override.liq)))

(rule
 (alias never)
 (package liquidsoap)
 (deps
  never.liq
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
  (run %{run_test} never.liq liquidsoap %{test_liq} never.liq)))

(rule
 (alias no-cue-cut)
 (package liquidsoap)
 (deps
  no-cue-cut.liq
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
  (run %{run_test} no-cue-cut.liq liquidsoap %{test_liq} no-cue-cut.liq)))

(rule
 (alias non_repeating)
 (package liquidsoap)
 (deps
  non_repeating.liq
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
   non_repeating.liq
   liquidsoap
   %{test_liq}
   non_repeating.liq)))

(rule
 (alias on_end)
 (package liquidsoap)
 (deps
  on_end.liq
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
  (run %{run_test} on_end.liq liquidsoap %{test_liq} on_end.liq)))

(rule
 (alias on_end_partial)
 (package liquidsoap)
 (deps
  on_end_partial.liq
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
   on_end_partial.liq
   liquidsoap
   %{test_liq}
   on_end_partial.liq)))

(rule
 (alias on_frame)
 (package liquidsoap)
 (deps
  on_frame.liq
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
  (run %{run_test} on_frame.liq liquidsoap %{test_liq} on_frame.liq)))

(rule
 (alias on_metadata)
 (package liquidsoap)
 (deps
  on_metadata.liq
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
  (run %{run_test} on_metadata.liq liquidsoap %{test_liq} on_metadata.liq)))

(rule
 (alias on_offset)
 (package liquidsoap)
 (deps
  on_offset.liq
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
  (run %{run_test} on_offset.liq liquidsoap %{test_liq} on_offset.liq)))

(rule
 (alias on_offset_partial)
 (package liquidsoap)
 (deps
  on_offset_partial.liq
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
   on_offset_partial.liq
   liquidsoap
   %{test_liq}
   on_offset_partial.liq)))

(rule
 (alias on_track)
 (package liquidsoap)
 (deps
  on_track.liq
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
  (run %{run_test} on_track.liq liquidsoap %{test_liq} on_track.liq)))

(rule
 (alias playlist-watch)
 (package liquidsoap)
 (deps
  playlist-watch.liq
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
   playlist-watch.liq
   liquidsoap
   %{test_liq}
   playlist-watch.liq)))

(rule
 (alias radio)
 (package liquidsoap)
 (deps
  radio.liq
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
  (run %{run_test} radio.liq liquidsoap %{test_liq} radio.liq)))

(rule
 (alias radio2)
 (package liquidsoap)
 (deps
  radio2.liq
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
  (run %{run_test} radio2.liq liquidsoap %{test_liq} radio2.liq)))

(rule
 (alias random)
 (package liquidsoap)
 (deps
  random.liq
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
  (run %{run_test} random.liq liquidsoap %{test_liq} random.liq)))

(rule
 (alias replaygain)
 (package liquidsoap)
 (deps
  replaygain.liq
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
  (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))

(rule
 (alias request)
 (package liquidsoap)
 (deps
  request.liq
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
  (run %{run_test} request.liq liquidsoap %{test_liq} request.liq)))

(rule
 (alias rotate)
 (package liquidsoap)
 (deps
  rotate.liq
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
  (run %{run_test} rotate.liq liquidsoap %{test_liq} rotate.liq)))

(rule
 (alias say)
 (package liquidsoap)
 (deps
  say.liq
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
  (run %{run_test} say.liq liquidsoap %{test_liq} say.liq)))

(rule
 (alias sharing)
 (package liquidsoap)
 (deps
  sharing.liq
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
  (run %{run_test} sharing.liq liquidsoap %{test_liq} sharing.liq)))

(rule
 (alias sine.detect.full_conv)
 (package liquidsoap)
 (deps
  sine.detect.full_conv.liq
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
   sine.detect.full_conv.liq
   liquidsoap
   %{test_liq}
   sine.detect.full_conv.liq)))

(rule
 (alias sine.detect)
 (package liquidsoap)
 (deps
  sine.detect.liq
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
  (run %{run_test} sine.detect.liq liquidsoap %{test_liq} sine.detect.liq)))

(rule
 (alias sine.detect.pcm_f32)
 (package liquidsoap)
 (deps
  sine.detect.pcm_f32.liq
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
   sine.detect.pcm_f32.liq
   liquidsoap
   %{test_liq}
   sine.detect.pcm_f32.liq)))

(rule
 (alias sine.detect.pcm_s16)
 (package liquidsoap)
 (deps
  sine.detect.pcm_s16.liq
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
   sine.detect.pcm_s16.liq
   liquidsoap
   %{test_liq}
   sine.detect.pcm_s16.liq)))

(rule
 (alias soundtouch-tracks)
 (package liquidsoap)
 (deps
  soundtouch-tracks.liq
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
   soundtouch-tracks.liq
   liquidsoap
   %{test_liq}
   soundtouch-tracks.liq)))

(rule
 (alias source-cue)
 (package liquidsoap)
 (deps
  source-cue.liq
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
  (run %{run_test} source-cue.liq liquidsoap %{test_liq} source-cue.liq)))

(rule
 (alias srt_listen_callback)
 (package liquidsoap)
 (deps
  srt_listen_callback.liq
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
   srt_listen_callback.liq
   liquidsoap
   %{test_liq}
   srt_listen_callback.liq)))

(rule
 (alias srt_multiple_outputs)
 (package liquidsoap)
 (deps
  srt_multiple_outputs.liq
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
   srt_multiple_outputs.liq
   liquidsoap
   %{test_liq}
   srt_multiple_outputs.liq)))

(rule
 (alias srt_passphrase)
 (package liquidsoap)
 (deps
  srt_passphrase.liq
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
   srt_passphrase.liq
   liquidsoap
   %{test_liq}
   srt_passphrase.liq)))

(rule
 (alias srt_raw_pcm)
 (package liquidsoap)
 (deps
  srt_raw_pcm.liq
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
  (run %{run_test} srt_raw_pcm.liq liquidsoap %{test_liq} srt_raw_pcm.liq)))

(rule
 (alias stretch-clock-propagation)
 (package liquidsoap)
 (deps
  stretch-clock-propagation.liq
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
   stretch-clock-propagation.liq
   liquidsoap
   %{test_liq}
   stretch-clock-propagation.liq)))

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

(rule
 (alias video_size_autodetect)
 (package liquidsoap)
 (deps
  video_size_autodetect.liq
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
   video_size_autodetect.liq
   liquidsoap
   %{test_liq}
   video_size_autodetect.liq)))

(alias
 (name citest)
 (deps
  (alias 195)
  (alias add_all_metadata)
  (alias autostart)
  (alias cross)
  (alias cross-override)
  (alias cross-persist-override)
  (alias crossfade)
  (alias crossfade2)
  (alias ctype1)
  (alias ctype2)
  (alias cue-cut)
  (alias dtmf)
  (alias dtmf_pcm_s16)
  (alias fades-overrides)
  (alias fades-persistent-override)
  (alias ffmpeg-errors)
  (alias ffmpeg-trim)
  (alias file_output)
  (alias harbor_kick_client)
  (alias harbor_metadata)
  (alias harbor_metadata_2)
  (alias harbor_metadata_3)
  (alias hls_custom_tags)
  (alias hls_discontinuity)
  (alias hls_global_headers)
  (alias hls_id3v2)
  (alias hls_large_frame)
  (alias hls_main_playlist)
  (alias huge-playlist)
  (alias icecast_last_meta)
  (alias icecast_no_restart)
  (alias image)
  (alias many-playlists)
  (alias merge_metadata)
  (alias metadata-override)
  (alias never)
  (alias no-cue-cut)
  (alias non_repeating)
  (alias on_end)
  (alias on_end_partial)
  (alias on_frame)
  (alias on_metadata)
  (alias on_offset)
  (alias on_offset_partial)
  (alias on_track)
  (alias playlist-watch)
  (alias radio)
  (alias radio2)
  (alias random)
  (alias replaygain)
  (alias request)
  (alias rotate)
  (alias say)
  (alias sharing)
  (alias sine.detect)
  (alias sine.detect.full_conv)
  (alias sine.detect.pcm_f32)
  (alias sine.detect.pcm_s16)
  (alias soundtouch-tracks)
  (alias source-cue)
  (alias srt_listen_callback)
  (alias srt_multiple_outputs)
  (alias srt_passphrase)
  (alias srt_raw_pcm)
  (alias stretch-clock-propagation)
  (alias test_ffmpeg_on_subitles)
  (alias test_on_subtitle)
  (alias test_subtitles_insert)
  (alias test_subtitles_map)
  (alias test_subtitles_track_insert)
  (alias video_size_autodetect)))
