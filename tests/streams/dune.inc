
(rule
  (alias tree-sitter-parse)
  (deps
   195.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/195.liq)))))

(rule
  (alias fmt)
  (deps 195.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to 195.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/195.liq)))
      (diff 195.liq 195.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 195.liq liquidsoap %{test_liq} 195.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   197.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/197.liq)))))

(rule
  (alias fmt)
  (deps 197.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to 197.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/197.liq)))
      (diff 197.liq 197.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  197.liq
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 197.liq liquidsoap %{test_liq} 197.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   autostart.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/autostart.liq)))))

(rule
  (alias fmt)
  (deps autostart.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to autostart.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/autostart.liq)))
      (diff autostart.liq autostart.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} autostart.liq liquidsoap %{test_liq} autostart.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   cross-override.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/cross-override.liq)))))

(rule
  (alias fmt)
  (deps cross-override.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to cross-override.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/cross-override.liq)))
      (diff cross-override.liq cross-override.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} cross-override.liq liquidsoap %{test_liq} cross-override.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   cross-persist-override.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/cross-persist-override.liq)))))

(rule
  (alias fmt)
  (deps cross-persist-override.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to cross-persist-override.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/cross-persist-override.liq)))
      (diff cross-persist-override.liq cross-persist-override.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} cross-persist-override.liq liquidsoap %{test_liq} cross-persist-override.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   cross.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/cross.liq)))))

(rule
  (alias fmt)
  (deps cross.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to cross.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/cross.liq)))
      (diff cross.liq cross.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} cross.liq liquidsoap %{test_liq} cross.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   crossfade.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/crossfade.liq)))))

(rule
  (alias fmt)
  (deps crossfade.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to crossfade.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/crossfade.liq)))
      (diff crossfade.liq crossfade.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} crossfade.liq liquidsoap %{test_liq} crossfade.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ctype1.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/ctype1.liq)))))

(rule
  (alias fmt)
  (deps ctype1.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ctype1.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/ctype1.liq)))
      (diff ctype1.liq ctype1.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ctype1.liq liquidsoap %{test_liq} ctype1.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ctype2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/ctype2.liq)))))

(rule
  (alias fmt)
  (deps ctype2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ctype2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/ctype2.liq)))
      (diff ctype2.liq ctype2.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ctype2.liq liquidsoap %{test_liq} ctype2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   dtmf.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/dtmf.liq)))))

(rule
  (alias fmt)
  (deps dtmf.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to dtmf.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/dtmf.liq)))
      (diff dtmf.liq dtmf.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} dtmf.liq liquidsoap %{test_liq} dtmf.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   dtmf_pcm_s16.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/dtmf_pcm_s16.liq)))))

(rule
  (alias fmt)
  (deps dtmf_pcm_s16.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to dtmf_pcm_s16.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/dtmf_pcm_s16.liq)))
      (diff dtmf_pcm_s16.liq dtmf_pcm_s16.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} dtmf_pcm_s16.liq liquidsoap %{test_liq} dtmf_pcm_s16.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   fades-overrides.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/fades-overrides.liq)))))

(rule
  (alias fmt)
  (deps fades-overrides.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to fades-overrides.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/fades-overrides.liq)))
      (diff fades-overrides.liq fades-overrides.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} fades-overrides.liq liquidsoap %{test_liq} fades-overrides.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   fades-persistent-override.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/fades-persistent-override.liq)))))

(rule
  (alias fmt)
  (deps fades-persistent-override.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to fades-persistent-override.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/fades-persistent-override.liq)))
      (diff fades-persistent-override.liq fades-persistent-override.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} fades-persistent-override.liq liquidsoap %{test_liq} fades-persistent-override.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   ffmpeg-errors.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/ffmpeg-errors.liq)))))

(rule
  (alias fmt)
  (deps ffmpeg-errors.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to ffmpeg-errors.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/ffmpeg-errors.liq)))
      (diff ffmpeg-errors.liq ffmpeg-errors.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-errors.liq liquidsoap %{test_liq} ffmpeg-errors.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   file_output.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/file_output.liq)))))

(rule
  (alias fmt)
  (deps file_output.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to file_output.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/file_output.liq)))
      (diff file_output.liq file_output.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file_output.liq liquidsoap %{test_liq} file_output.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   harbor_kick_client.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/harbor_kick_client.liq)))))

(rule
  (alias fmt)
  (deps harbor_kick_client.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to harbor_kick_client.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/harbor_kick_client.liq)))
      (diff harbor_kick_client.liq harbor_kick_client.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} harbor_kick_client.liq liquidsoap %{test_liq} harbor_kick_client.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   harbor_metadata.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/harbor_metadata.liq)))))

(rule
  (alias fmt)
  (deps harbor_metadata.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to harbor_metadata.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/harbor_metadata.liq)))
      (diff harbor_metadata.liq harbor_metadata.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} harbor_metadata.liq liquidsoap %{test_liq} harbor_metadata.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   hls_custom_tags.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/hls_custom_tags.liq)))))

(rule
  (alias fmt)
  (deps hls_custom_tags.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to hls_custom_tags.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/hls_custom_tags.liq)))
      (diff hls_custom_tags.liq hls_custom_tags.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} hls_custom_tags.liq liquidsoap %{test_liq} hls_custom_tags.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   hls_id3v2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/hls_id3v2.liq)))))

(rule
  (alias fmt)
  (deps hls_id3v2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to hls_id3v2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/hls_id3v2.liq)))
      (diff hls_id3v2.liq hls_id3v2.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} hls_id3v2.liq liquidsoap %{test_liq} hls_id3v2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   huge-playlist.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/huge-playlist.liq)))))

(rule
  (alias fmt)
  (deps huge-playlist.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to huge-playlist.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/huge-playlist.liq)))
      (diff huge-playlist.liq huge-playlist.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} huge-playlist.liq liquidsoap %{test_liq} huge-playlist.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   image.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/image.liq)))))

(rule
  (alias fmt)
  (deps image.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to image.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/image.liq)))
      (diff image.liq image.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} image.liq liquidsoap %{test_liq} image.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   many-playlists.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/many-playlists.liq)))))

(rule
  (alias fmt)
  (deps many-playlists.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to many-playlists.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/many-playlists.liq)))
      (diff many-playlists.liq many-playlists.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} many-playlists.liq liquidsoap %{test_liq} many-playlists.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   merge_metadata.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/merge_metadata.liq)))))

(rule
  (alias fmt)
  (deps merge_metadata.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to merge_metadata.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/merge_metadata.liq)))
      (diff merge_metadata.liq merge_metadata.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} merge_metadata.liq liquidsoap %{test_liq} merge_metadata.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   never.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/never.liq)))))

(rule
  (alias fmt)
  (deps never.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to never.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/never.liq)))
      (diff never.liq never.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} never.liq liquidsoap %{test_liq} never.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   on_frame.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/on_frame.liq)))))

(rule
  (alias fmt)
  (deps on_frame.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to on_frame.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/on_frame.liq)))
      (diff on_frame.liq on_frame.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} on_frame.liq liquidsoap %{test_liq} on_frame.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   on_metadata.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/on_metadata.liq)))))

(rule
  (alias fmt)
  (deps on_metadata.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to on_metadata.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/on_metadata.liq)))
      (diff on_metadata.liq on_metadata.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} on_metadata.liq liquidsoap %{test_liq} on_metadata.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   on_track.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/on_track.liq)))))

(rule
  (alias fmt)
  (deps on_track.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to on_track.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/on_track.liq)))
      (diff on_track.liq on_track.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} on_track.liq liquidsoap %{test_liq} on_track.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   playlist-watch.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/playlist-watch.liq)))))

(rule
  (alias fmt)
  (deps playlist-watch.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to playlist-watch.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/playlist-watch.liq)))
      (diff playlist-watch.liq playlist-watch.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} playlist-watch.liq liquidsoap %{test_liq} playlist-watch.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   radio.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/radio.liq)))))

(rule
  (alias fmt)
  (deps radio.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to radio.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/radio.liq)))
      (diff radio.liq radio.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} radio.liq liquidsoap %{test_liq} radio.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   radio2.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/radio2.liq)))))

(rule
  (alias fmt)
  (deps radio2.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to radio2.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/radio2.liq)))
      (diff radio2.liq radio2.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} radio2.liq liquidsoap %{test_liq} radio2.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   random.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/random.liq)))))

(rule
  (alias fmt)
  (deps random.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to random.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/random.liq)))
      (diff random.liq random.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} random.liq liquidsoap %{test_liq} random.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   replaygain.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/replaygain.liq)))))

(rule
  (alias fmt)
  (deps replaygain.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to replaygain.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/replaygain.liq)))
      (diff replaygain.liq replaygain.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   request.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/request.liq)))))

(rule
  (alias fmt)
  (deps request.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to request.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/request.liq)))
      (diff request.liq request.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} request.liq liquidsoap %{test_liq} request.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   rotate.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/rotate.liq)))))

(rule
  (alias fmt)
  (deps rotate.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to rotate.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/rotate.liq)))
      (diff rotate.liq rotate.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} rotate.liq liquidsoap %{test_liq} rotate.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   say.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/say.liq)))))

(rule
  (alias fmt)
  (deps say.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to say.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/say.liq)))
      (diff say.liq say.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} say.liq liquidsoap %{test_liq} say.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   seek_track_map.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/seek_track_map.liq)))))

(rule
  (alias fmt)
  (deps seek_track_map.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to seek_track_map.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/seek_track_map.liq)))
      (diff seek_track_map.liq seek_track_map.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  seek_track_map.liq
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} seek_track_map.liq liquidsoap %{test_liq} seek_track_map.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   sharing.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/sharing.liq)))))

(rule
  (alias fmt)
  (deps sharing.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to sharing.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/sharing.liq)))
      (diff sharing.liq sharing.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sharing.liq liquidsoap %{test_liq} sharing.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   sine.detect.full_conv.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/sine.detect.full_conv.liq)))))

(rule
  (alias fmt)
  (deps sine.detect.full_conv.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to sine.detect.full_conv.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/sine.detect.full_conv.liq)))
      (diff sine.detect.full_conv.liq sine.detect.full_conv.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sine.detect.full_conv.liq liquidsoap %{test_liq} sine.detect.full_conv.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   sine.detect.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/sine.detect.liq)))))

(rule
  (alias fmt)
  (deps sine.detect.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to sine.detect.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/sine.detect.liq)))
      (diff sine.detect.liq sine.detect.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sine.detect.liq liquidsoap %{test_liq} sine.detect.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   sine.detect.pcm_f32.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/sine.detect.pcm_f32.liq)))))

(rule
  (alias fmt)
  (deps sine.detect.pcm_f32.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to sine.detect.pcm_f32.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/sine.detect.pcm_f32.liq)))
      (diff sine.detect.pcm_f32.liq sine.detect.pcm_f32.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sine.detect.pcm_f32.liq liquidsoap %{test_liq} sine.detect.pcm_f32.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   sine.detect.pcm_s16.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/sine.detect.pcm_s16.liq)))))

(rule
  (alias fmt)
  (deps sine.detect.pcm_s16.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to sine.detect.pcm_s16.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/sine.detect.pcm_s16.liq)))
      (diff sine.detect.pcm_s16.liq sine.detect.pcm_s16.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sine.detect.pcm_s16.liq liquidsoap %{test_liq} sine.detect.pcm_s16.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   smart-crossfade.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/smart-crossfade.liq)))))

(rule
  (alias fmt)
  (deps smart-crossfade.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to smart-crossfade.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/smart-crossfade.liq)))
      (diff smart-crossfade.liq smart-crossfade.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  smart-crossfade.liq
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} smart-crossfade.liq liquidsoap %{test_liq} smart-crossfade.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   srt_listen_callback.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/srt_listen_callback.liq)))))

(rule
  (alias fmt)
  (deps srt_listen_callback.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to srt_listen_callback.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/srt_listen_callback.liq)))
      (diff srt_listen_callback.liq srt_listen_callback.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} srt_listen_callback.liq liquidsoap %{test_liq} srt_listen_callback.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   srt_multiple_outputs.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/srt_multiple_outputs.liq)))))

(rule
  (alias fmt)
  (deps srt_multiple_outputs.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to srt_multiple_outputs.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/srt_multiple_outputs.liq)))
      (diff srt_multiple_outputs.liq srt_multiple_outputs.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} srt_multiple_outputs.liq liquidsoap %{test_liq} srt_multiple_outputs.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   srt_passphrase.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/srt_passphrase.liq)))))

(rule
  (alias fmt)
  (deps srt_passphrase.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to srt_passphrase.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/srt_passphrase.liq)))
      (diff srt_passphrase.liq srt_passphrase.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} srt_passphrase.liq liquidsoap %{test_liq} srt_passphrase.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   srt_raw_pcm.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/srt_raw_pcm.liq)))))

(rule
  (alias fmt)
  (deps srt_raw_pcm.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to srt_raw_pcm.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/srt_raw_pcm.liq)))
      (diff srt_raw_pcm.liq srt_raw_pcm.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} srt_raw_pcm.liq liquidsoap %{test_liq} srt_raw_pcm.liq)))

(rule
  (alias tree-sitter-parse)
  (deps
   stretch-clock-propagation.liq
   ../../src/tooling/test-tree-sitter
 )
  (action
    (chdir ../../src/tooling/test-tree-sitter/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../../tests/streams/stretch-clock-propagation.liq)))))

(rule
  (alias fmt)
  (deps stretch-clock-propagation.liq ../../src/tooling/test-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to stretch-clock-propagation.liq.prettier
       (chdir ../../src/tooling/test-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/stretch-clock-propagation.liq)))
      (diff stretch-clock-propagation.liq stretch-clock-propagation.liq.prettier))))

(rule
 (alias citest)
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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} stretch-clock-propagation.liq liquidsoap %{test_liq} stretch-clock-propagation.liq)))
