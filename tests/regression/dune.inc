
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1159.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1159.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  LS460.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" LS460.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  LS503.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" LS503.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  115-1.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" 115-1.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  115-2.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" 115-2.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  123.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" 123.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  LS354-2.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" LS354-2.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  init-error.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" init-error.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  LS354-1.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" LS354-1.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1129.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1129.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  metadata_cache.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" metadata_cache.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1279.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1279.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1327.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1327.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  BUG403.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" BUG403.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  input_rtmp.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" input_rtmp.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  infallible-shutdown.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" infallible-shutdown.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH-action-919422659.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH-action-919422659.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1151.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1151.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  playlist-id.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" playlist-id.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1146.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" GH1146.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  AC5109.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" AC5109.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  LS268.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" LS268.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH2573.output
  (:check_output ../check_output.exe)
  (:run_test ../run_test.sh))
 (action (run %{run_test} %{check_output} GH2573.output)))
  