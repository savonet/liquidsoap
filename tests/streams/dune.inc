
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  197.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" 197.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  huge-playlist.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" huge-playlist.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  playlist-watch.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" playlist-watch.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  on_frame.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" on_frame.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  195.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" 195.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  image.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" image.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  ctype2.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" ctype2.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  say.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" say.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  ctype1.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" ctype1.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  random.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" random.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  smart-crossfade.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" smart-crossfade.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  request.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" request.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  radio2.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" radio2.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  cross.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" cross.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  stretch-clock-propagation.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" stretch-clock-propagation.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  rotate.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" rotate.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  sine.detect.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" sine.detect.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  on_metadata.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" on_metadata.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  dtmf.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" dtmf.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  on_track.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" on_track.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  crossfade.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" crossfade.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  radio.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" radio.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  autostart.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" autostart.liq)))
  