(rule
 (alias test_115-1)
 (package liquidsoap)
 (deps
  test_115-1.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_115-1.liq liquidsoap %{test_liq} test_115-1.liq)))

(rule
 (alias test_115-2)
 (package liquidsoap)
 (deps
  test_115-2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_115-2.liq liquidsoap %{test_liq} test_115-2.liq)))

(rule
 (alias test_3553)
 (package liquidsoap)
 (deps
  test_3553.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_3553.liq liquidsoap %{test_liq} test_3553.liq)))

(rule
 (alias test_AC5109)
 (package liquidsoap)
 (deps
  test_AC5109.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_AC5109.liq liquidsoap %{test_liq} test_AC5109.liq)))

(rule
 (alias test_BUG403)
 (package liquidsoap)
 (deps
  test_BUG403.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_BUG403.liq liquidsoap %{test_liq} test_BUG403.liq)))

(rule
 (alias test_GH-action-919422659)
 (package liquidsoap)
 (deps
  test_GH-action-919422659.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH-action-919422659.liq
   liquidsoap
   %{test_liq}
   test_GH-action-919422659.liq)))

(rule
 (alias test_GH1129)
 (package liquidsoap)
 (deps
  test_GH1129.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1129.liq liquidsoap %{test_liq} test_GH1129.liq)))

(rule
 (alias test_GH1146)
 (package liquidsoap)
 (deps
  test_GH1146.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1146.liq liquidsoap %{test_liq} test_GH1146.liq)))

(rule
 (alias test_GH1151)
 (package liquidsoap)
 (deps
  test_GH1151.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1151.liq liquidsoap %{test_liq} test_GH1151.liq)))

(rule
 (alias test_GH1159)
 (package liquidsoap)
 (deps
  test_GH1159.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1159.liq liquidsoap %{test_liq} test_GH1159.liq)))

(rule
 (alias test_GH1279)
 (package liquidsoap)
 (deps
  test_GH1279.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1279.liq liquidsoap %{test_liq} test_GH1279.liq)))

(rule
 (alias test_GH1327)
 (package liquidsoap)
 (deps
  test_GH1327.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH1327.liq liquidsoap %{test_liq} test_GH1327.liq)))

(rule
 (alias test_GH2585)
 (package liquidsoap)
 (deps
  test_GH2585.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2585.liq liquidsoap %{test_liq} test_GH2585.liq)))

(rule
 (alias test_GH2602)
 (package liquidsoap)
 (deps
  test_GH2602.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2602.liq liquidsoap %{test_liq} test_GH2602.liq)))

(rule
 (alias test_GH2756-2)
 (package liquidsoap)
 (deps
  test_GH2756-2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH2756-2.liq
   liquidsoap
   %{test_liq}
   test_GH2756-2.liq)))

(rule
 (alias test_GH2756)
 (package liquidsoap)
 (deps
  test_GH2756.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2756.liq liquidsoap %{test_liq} test_GH2756.liq)))

(rule
 (alias test_GH2758)
 (package liquidsoap)
 (deps
  test_GH2758.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2758.liq liquidsoap %{test_liq} test_GH2758.liq)))

(rule
 (alias test_GH2842)
 (package liquidsoap)
 (deps
  test_GH2842.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2842.liq liquidsoap %{test_liq} test_GH2842.liq)))

(rule
 (alias test_GH2850)
 (package liquidsoap)
 (deps
  test_GH2850.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2850.liq liquidsoap %{test_liq} test_GH2850.liq)))

(rule
 (alias test_GH2867)
 (package liquidsoap)
 (deps
  test_GH2867.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2867.liq liquidsoap %{test_liq} test_GH2867.liq)))

(rule
 (alias test_GH2871)
 (package liquidsoap)
 (deps
  test_GH2871.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2871.liq liquidsoap %{test_liq} test_GH2871.liq)))

(rule
 (alias test_GH2872)
 (package liquidsoap)
 (deps
  test_GH2872.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2872.liq liquidsoap %{test_liq} test_GH2872.liq)))

(rule
 (alias test_GH2897)
 (package liquidsoap)
 (deps
  test_GH2897.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2897.liq liquidsoap %{test_liq} test_GH2897.liq)))

(rule
 (alias test_GH2902)
 (package liquidsoap)
 (deps
  test_GH2902.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2902.liq liquidsoap %{test_liq} test_GH2902.liq)))

(rule
 (alias test_GH2926)
 (package liquidsoap)
 (deps
  test_GH2926.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH2926.liq liquidsoap %{test_liq} test_GH2926.liq)))

(rule
 (alias test_GH3093)
 (package liquidsoap)
 (deps
  test_GH3093.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3093.liq liquidsoap %{test_liq} test_GH3093.liq)))

(rule
 (alias test_GH3132)
 (package liquidsoap)
 (deps
  test_GH3132.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3132.liq liquidsoap %{test_liq} test_GH3132.liq)))

(rule
 (alias test_GH3134)
 (package liquidsoap)
 (deps
  test_GH3134.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3134.liq liquidsoap %{test_liq} test_GH3134.liq)))

(rule
 (alias test_GH3224)
 (package liquidsoap)
 (deps
  test_GH3224.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3224.liq liquidsoap %{test_liq} test_GH3224.liq)))

(rule
 (alias test_GH3239-2)
 (package liquidsoap)
 (deps
  test_GH3239-2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH3239-2.liq
   liquidsoap
   %{test_liq}
   test_GH3239-2.liq)))

(rule
 (alias test_GH3239-bis)
 (package liquidsoap)
 (deps
  test_GH3239-bis.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH3239-bis.liq
   liquidsoap
   %{test_liq}
   test_GH3239-bis.liq)))

(rule
 (alias test_GH3239)
 (package liquidsoap)
 (deps
  test_GH3239.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3239.liq liquidsoap %{test_liq} test_GH3239.liq)))

(rule
 (alias test_GH3276)
 (package liquidsoap)
 (deps
  test_GH3276.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3276.liq liquidsoap %{test_liq} test_GH3276.liq)))

(rule
 (alias test_GH3303)
 (package liquidsoap)
 (deps
  test_GH3303.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3303.liq liquidsoap %{test_liq} test_GH3303.liq)))

(rule
 (alias test_GH3316)
 (package liquidsoap)
 (deps
  test_GH3316.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3316.liq liquidsoap %{test_liq} test_GH3316.liq)))

(rule
 (alias test_GH3645)
 (package liquidsoap)
 (deps
  test_GH3645.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3645.liq liquidsoap %{test_liq} test_GH3645.liq)))

(rule
 (alias test_GH3675)
 (package liquidsoap)
 (deps
  test_GH3675.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3675.liq liquidsoap %{test_liq} test_GH3675.liq)))

(rule
 (alias test_GH3813)
 (package liquidsoap)
 (deps
  test_GH3813.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3813.liq liquidsoap %{test_liq} test_GH3813.liq)))

(rule
 (alias test_GH3840)
 (package liquidsoap)
 (deps
  test_GH3840.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3840.liq liquidsoap %{test_liq} test_GH3840.liq)))

(rule
 (alias test_GH3881)
 (package liquidsoap)
 (deps
  test_GH3881.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3881.liq liquidsoap %{test_liq} test_GH3881.liq)))

(rule
 (alias test_GH3960)
 (package liquidsoap)
 (deps
  test_GH3960.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH3960.liq liquidsoap %{test_liq} test_GH3960.liq)))

(rule
 (alias test_GH4071)
 (package liquidsoap)
 (deps
  test_GH4071.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4071.liq liquidsoap %{test_liq} test_GH4071.liq)))

(rule
 (alias test_GH4090)
 (package liquidsoap)
 (deps
  test_GH4090.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4090.liq liquidsoap %{test_liq} test_GH4090.liq)))

(rule
 (alias test_GH4116)
 (package liquidsoap)
 (deps
  test_GH4116.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4116.liq liquidsoap %{test_liq} test_GH4116.liq)))

(rule
 (alias test_GH4124)
 (package liquidsoap)
 (deps
  test_GH4124.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4124.liq liquidsoap %{test_liq} test_GH4124.liq)))

(rule
 (alias test_GH4140)
 (package liquidsoap)
 (deps
  test_GH4140.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4140.liq liquidsoap %{test_liq} test_GH4140.liq)))

(rule
 (alias test_GH4144)
 (package liquidsoap)
 (deps
  test_GH4144.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4144.liq liquidsoap %{test_liq} test_GH4144.liq)))

(rule
 (alias test_GH4159)
 (package liquidsoap)
 (deps
  test_GH4159.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4159.liq liquidsoap %{test_liq} test_GH4159.liq)))

(rule
 (alias test_GH4163)
 (package liquidsoap)
 (deps
  test_GH4163.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4163.liq liquidsoap %{test_liq} test_GH4163.liq)))

(rule
 (alias test_GH4246)
 (package liquidsoap)
 (deps
  test_GH4246.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4246.liq liquidsoap %{test_liq} test_GH4246.liq)))

(rule
 (alias test_GH4281-2)
 (package liquidsoap)
 (deps
  test_GH4281-2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH4281-2.liq
   liquidsoap
   %{test_liq}
   test_GH4281-2.liq)))

(rule
 (alias test_GH4281)
 (package liquidsoap)
 (deps
  test_GH4281.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4281.liq liquidsoap %{test_liq} test_GH4281.liq)))

(rule
 (alias test_GH4395)
 (package liquidsoap)
 (deps
  test_GH4395.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4395.liq liquidsoap %{test_liq} test_GH4395.liq)))

(rule
 (alias test_GH4499)
 (package liquidsoap)
 (deps
  test_GH4499.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4499.liq liquidsoap %{test_liq} test_GH4499.liq)))

(rule
 (alias test_GH4629)
 (package liquidsoap)
 (deps
  test_GH4629.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4629.liq liquidsoap %{test_liq} test_GH4629.liq)))

(rule
 (alias test_GH4711)
 (package liquidsoap)
 (deps
  test_GH4711.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4711.liq liquidsoap %{test_liq} test_GH4711.liq)))

(rule
 (alias test_GH4745)
 (package liquidsoap)
 (deps
  test_GH4745.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4745.liq liquidsoap %{test_liq} test_GH4745.liq)))

(rule
 (alias test_GH4748)
 (package liquidsoap)
 (deps
  test_GH4748.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4748.liq liquidsoap %{test_liq} test_GH4748.liq)))

(rule
 (alias test_GH4819-bis)
 (package liquidsoap)
 (deps
  test_GH4819-bis.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_GH4819-bis.liq
   liquidsoap
   %{test_liq}
   test_GH4819-bis.liq)))

(rule
 (alias test_GH4819)
 (package liquidsoap)
 (deps
  test_GH4819.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4819.liq liquidsoap %{test_liq} test_GH4819.liq)))

(rule
 (alias test_GH4828)
 (package liquidsoap)
 (deps
  test_GH4828.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4828.liq liquidsoap %{test_liq} test_GH4828.liq)))

(rule
 (alias test_GH4840)
 (package liquidsoap)
 (deps
  test_GH4840.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4840.liq liquidsoap %{test_liq} test_GH4840.liq)))

(rule
 (alias test_GH4882)
 (package liquidsoap)
 (deps
  test_GH4882.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (glob_files ./*.dylib)
  (glob_files ./*.so)
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4882.liq liquidsoap %{test_liq} test_GH4882.liq)))

(rule
 (alias test_GH4922)
 (package liquidsoap)
 (deps
  test_GH4922.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_GH4922.liq liquidsoap %{test_liq} test_GH4922.liq)))

(rule
 (alias test_LS268)
 (package liquidsoap)
 (deps
  test_LS268.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_LS268.liq liquidsoap %{test_liq} test_LS268.liq)))

(rule
 (alias test_LS354-1)
 (package liquidsoap)
 (deps
  test_LS354-1.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_LS354-1.liq liquidsoap %{test_liq} test_LS354-1.liq)))

(rule
 (alias test_LS354-2)
 (package liquidsoap)
 (deps
  test_LS354-2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_LS354-2.liq liquidsoap %{test_liq} test_LS354-2.liq)))

(rule
 (alias test_LS460)
 (package liquidsoap)
 (deps
  test_LS460.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_LS460.liq liquidsoap %{test_liq} test_LS460.liq)))

(rule
 (alias test_LS503)
 (package liquidsoap)
 (deps
  test_LS503.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_LS503.liq liquidsoap %{test_liq} test_LS503.liq)))

(rule
 (alias test_append-merge)
 (package liquidsoap)
 (deps
  test_append-merge.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_append-merge.liq
   liquidsoap
   %{test_liq}
   test_append-merge.liq)))

(rule
 (alias test_append-merge2)
 (package liquidsoap)
 (deps
  test_append-merge2.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_append-merge2.liq
   liquidsoap
   %{test_liq}
   test_append-merge2.liq)))

(rule
 (alias test_append)
 (package liquidsoap)
 (deps
  test_append.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_append.liq liquidsoap %{test_liq} test_append.liq)))

(rule
 (alias test_default_format)
 (package liquidsoap)
 (deps
  test_default_format.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_default_format.liq
   liquidsoap
   %{test_liq}
   test_default_format.liq)))

(rule
 (alias test_double-nullable)
 (package liquidsoap)
 (deps
  test_double-nullable.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_double-nullable.liq
   liquidsoap
   %{test_liq}
   test_double-nullable.liq)))

(rule
 (alias test_eval-check-demeth)
 (package liquidsoap)
 (deps
  test_eval-check-demeth.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_eval-check-demeth.liq
   liquidsoap
   %{test_liq}
   test_eval-check-demeth.liq)))

(rule
 (alias test_external-encoder)
 (package liquidsoap)
 (deps
  test_external-encoder.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_external-encoder.liq
   liquidsoap
   %{test_liq}
   test_external-encoder.liq)))

(rule
 (alias test_fallible_ogg)
 (package liquidsoap)
 (deps
  test_fallible_ogg.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_fallible_ogg.liq
   liquidsoap
   %{test_liq}
   test_fallible_ogg.liq)))

(rule
 (alias test_ffmpeg-copy-encode)
 (package liquidsoap)
 (deps
  test_ffmpeg-copy-encode.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg-copy-encode.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg-copy-encode.liq)))

(rule
 (alias test_ffmpeg-copy-input-http)
 (package liquidsoap)
 (deps
  test_ffmpeg-copy-input-http.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg-copy-input-http.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg-copy-input-http.liq)))

(rule
 (alias test_ffmpeg-naming-convention)
 (package liquidsoap)
 (deps
  test_ffmpeg-naming-convention.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_ffmpeg-naming-convention.liq
   liquidsoap
   %{test_liq}
   test_ffmpeg-naming-convention.liq)))

(rule
 (alias test_infallible-shutdown)
 (package liquidsoap)
 (deps
  test_infallible-shutdown.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_infallible-shutdown.liq
   liquidsoap
   %{test_liq}
   test_infallible-shutdown.liq)))

(rule
 (alias test_init-error)
 (package liquidsoap)
 (deps
  test_init-error.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_init-error.liq
   liquidsoap
   %{test_liq}
   test_init-error.liq)))

(rule
 (alias test_initial_request_queue)
 (package liquidsoap)
 (deps
  test_initial_request_queue.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_initial_request_queue.liq
   liquidsoap
   %{test_liq}
   test_initial_request_queue.liq)))

(rule
 (alias test_input_rtmp)
 (package liquidsoap)
 (deps
  test_input_rtmp.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_input_rtmp.liq
   liquidsoap
   %{test_liq}
   test_input_rtmp.liq)))

(rule
 (alias test_metadata-resolvers-metadata)
 (package liquidsoap)
 (deps
  test_metadata-resolvers-metadata.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_metadata-resolvers-metadata.liq
   liquidsoap
   %{test_liq}
   test_metadata-resolvers-metadata.liq)))

(rule
 (alias test_metadata_cache)
 (package liquidsoap)
 (deps
  test_metadata_cache.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_metadata_cache.liq
   liquidsoap
   %{test_liq}
   test_metadata_cache.liq)))

(rule
 (alias test_output_on_metadata)
 (package liquidsoap)
 (deps
  test_output_on_metadata.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_output_on_metadata.liq
   liquidsoap
   %{test_liq}
   test_output_on_metadata.liq)))

(rule
 (alias test_output_on_track)
 (package liquidsoap)
 (deps
  test_output_on_track.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_output_on_track.liq
   liquidsoap
   %{test_liq}
   test_output_on_track.liq)))

(rule
 (alias test_playlist-id)
 (package liquidsoap)
 (deps
  test_playlist-id.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_playlist-id.liq
   liquidsoap
   %{test_liq}
   test_playlist-id.liq)))

(rule
 (alias test_replaygain)
 (package liquidsoap)
 (deps
  test_replaygain.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
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
 (alias test_seek_track_map)
 (package liquidsoap)
 (deps
  test_seek_track_map.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_seek_track_map.liq
   liquidsoap
   %{test_liq}
   test_seek_track_map.liq)))

(rule
 (alias test_shoutcast-args)
 (package liquidsoap)
 (deps
  test_shoutcast-args.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_shoutcast-args.liq
   liquidsoap
   %{test_liq}
   test_shoutcast-args.liq)))

(rule
 (alias test_source_cleanup)
 (package liquidsoap)
 (deps
  test_source_cleanup.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_source_cleanup.liq
   liquidsoap
   %{test_liq}
   test_source_cleanup.liq)))

(rule
 (alias test_source_dynamic)
 (package liquidsoap)
 (deps
  test_source_dynamic.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_source_dynamic.liq
   liquidsoap
   %{test_liq}
   test_source_dynamic.liq)))

(rule
 (alias test_track_sensitive)
 (package liquidsoap)
 (deps
  test_track_sensitive.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_track_sensitive.liq
   liquidsoap
   %{test_liq}
   test_track_sensitive.liq)))

(rule
 (alias test_unified-pcm-types)
 (package liquidsoap)
 (deps
  test_unified-pcm-types.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_unified-pcm-types.liq
   liquidsoap
   %{test_liq}
   test_unified-pcm-types.liq)))

(rule
 (alias test_video-only)
 (package liquidsoap)
 (deps
  test_video-only.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_video-only.liq
   liquidsoap
   %{test_liq}
   test_video-only.liq)))

(alias
 (name citest)
 (deps
  (alias test_115-1)
  (alias test_115-2)
  (alias test_3553)
  (alias test_AC5109)
  (alias test_BUG403)
  (alias test_GH-action-919422659)
  (alias test_GH1129)
  (alias test_GH1146)
  (alias test_GH1151)
  (alias test_GH1159)
  (alias test_GH1279)
  (alias test_GH1327)
  (alias test_GH2585)
  (alias test_GH2602)
  (alias test_GH2756)
  (alias test_GH2756-2)
  (alias test_GH2758)
  (alias test_GH2842)
  (alias test_GH2850)
  (alias test_GH2867)
  (alias test_GH2871)
  (alias test_GH2872)
  (alias test_GH2897)
  (alias test_GH2902)
  (alias test_GH2926)
  (alias test_GH3093)
  (alias test_GH3132)
  (alias test_GH3134)
  (alias test_GH3224)
  (alias test_GH3239)
  (alias test_GH3239-2)
  (alias test_GH3239-bis)
  (alias test_GH3276)
  (alias test_GH3303)
  (alias test_GH3316)
  (alias test_GH3645)
  (alias test_GH3675)
  (alias test_GH3813)
  (alias test_GH3840)
  (alias test_GH3881)
  (alias test_GH3960)
  (alias test_GH4071)
  (alias test_GH4090)
  (alias test_GH4116)
  (alias test_GH4124)
  (alias test_GH4140)
  (alias test_GH4144)
  (alias test_GH4159)
  (alias test_GH4163)
  (alias test_GH4246)
  (alias test_GH4281)
  (alias test_GH4281-2)
  (alias test_GH4395)
  (alias test_GH4499)
  (alias test_GH4629)
  (alias test_GH4711)
  (alias test_GH4745)
  (alias test_GH4748)
  (alias test_GH4819)
  (alias test_GH4819-bis)
  (alias test_GH4828)
  (alias test_GH4840)
  (alias test_GH4882)
  (alias test_GH4922)
  (alias test_LS268)
  (alias test_LS354-1)
  (alias test_LS354-2)
  (alias test_LS460)
  (alias test_LS503)
  (alias test_append)
  (alias test_append-merge)
  (alias test_append-merge2)
  (alias test_default_format)
  (alias test_double-nullable)
  (alias test_eval-check-demeth)
  (alias test_external-encoder)
  (alias test_fallible_ogg)
  (alias test_ffmpeg-copy-encode)
  (alias test_ffmpeg-copy-input-http)
  (alias test_ffmpeg-naming-convention)
  (alias test_infallible-shutdown)
  (alias test_init-error)
  (alias test_initial_request_queue)
  (alias test_input_rtmp)
  (alias test_metadata-resolvers-metadata)
  (alias test_metadata_cache)
  (alias test_output_on_metadata)
  (alias test_output_on_track)
  (alias test_playlist-id)
  (alias test_replaygain)
  (alias test_seek_track_map)
  (alias test_shoutcast-args)
  (alias test_source_cleanup)
  (alias test_source_dynamic)
  (alias test_track_sensitive)
  (alias test_unified-pcm-types)
  (alias test_video-only)))
