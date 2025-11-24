
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-1.liq liquidsoap %{test_liq} 115-1.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-2.liq liquidsoap %{test_liq} 115-2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  3553.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 3553.liq liquidsoap %{test_liq} 3553.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  AC5109.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} AC5109.liq liquidsoap %{test_liq} AC5109.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  BUG403.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} BUG403.liq liquidsoap %{test_liq} BUG403.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH-action-919422659.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH-action-919422659.liq liquidsoap %{test_liq} GH-action-919422659.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1129.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1129.liq liquidsoap %{test_liq} GH1129.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1146.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1146.liq liquidsoap %{test_liq} GH1146.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1151.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1151.liq liquidsoap %{test_liq} GH1151.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1159.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1159.liq liquidsoap %{test_liq} GH1159.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1279.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1279.liq liquidsoap %{test_liq} GH1279.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1327.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1327.liq liquidsoap %{test_liq} GH1327.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2585.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2585.liq liquidsoap %{test_liq} GH2585.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2602.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2602.liq liquidsoap %{test_liq} GH2602.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2756-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2756-2.liq liquidsoap %{test_liq} GH2756-2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2756.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2756.liq liquidsoap %{test_liq} GH2756.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2758.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2758.liq liquidsoap %{test_liq} GH2758.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2842.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2842.liq liquidsoap %{test_liq} GH2842.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2850.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2850.liq liquidsoap %{test_liq} GH2850.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2867.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2867.liq liquidsoap %{test_liq} GH2867.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2871.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2871.liq liquidsoap %{test_liq} GH2871.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2872.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2872.liq liquidsoap %{test_liq} GH2872.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2897.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2897.liq liquidsoap %{test_liq} GH2897.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2902.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2902.liq liquidsoap %{test_liq} GH2902.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2926.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2926.liq liquidsoap %{test_liq} GH2926.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3093.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3093.liq liquidsoap %{test_liq} GH3093.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3132.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3132.liq liquidsoap %{test_liq} GH3132.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3134.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3134.liq liquidsoap %{test_liq} GH3134.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3224.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3224.liq liquidsoap %{test_liq} GH3224.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3239-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3239-2.liq liquidsoap %{test_liq} GH3239-2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3239.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3239.liq liquidsoap %{test_liq} GH3239.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3276.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3276.liq liquidsoap %{test_liq} GH3276.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3303.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3303.liq liquidsoap %{test_liq} GH3303.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3316.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3316.liq liquidsoap %{test_liq} GH3316.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3645.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3645.liq liquidsoap %{test_liq} GH3645.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3675.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3675.liq liquidsoap %{test_liq} GH3675.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3813.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3813.liq liquidsoap %{test_liq} GH3813.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3840.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3840.liq liquidsoap %{test_liq} GH3840.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3881.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3881.liq liquidsoap %{test_liq} GH3881.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH3960.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3960.liq liquidsoap %{test_liq} GH3960.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4071.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4071.liq liquidsoap %{test_liq} GH4071.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4090.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4090.liq liquidsoap %{test_liq} GH4090.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4116.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4116.liq liquidsoap %{test_liq} GH4116.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4124.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4124.liq liquidsoap %{test_liq} GH4124.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4140.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4140.liq liquidsoap %{test_liq} GH4140.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4144.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4144.liq liquidsoap %{test_liq} GH4144.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4159.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4159.liq liquidsoap %{test_liq} GH4159.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4163.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4163.liq liquidsoap %{test_liq} GH4163.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4246.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4246.liq liquidsoap %{test_liq} GH4246.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4281-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4281-2.liq liquidsoap %{test_liq} GH4281-2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4281.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4281.liq liquidsoap %{test_liq} GH4281.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4395.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4395.liq liquidsoap %{test_liq} GH4395.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4499.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4499.liq liquidsoap %{test_liq} GH4499.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4629.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4629.liq liquidsoap %{test_liq} GH4629.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4711.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4711.liq liquidsoap %{test_liq} GH4711.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4745.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4745.liq liquidsoap %{test_liq} GH4745.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH4748.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4748.liq liquidsoap %{test_liq} GH4748.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS268.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS268.liq liquidsoap %{test_liq} LS268.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-1.liq liquidsoap %{test_liq} LS354-1.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-2.liq liquidsoap %{test_liq} LS354-2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS460.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS460.liq liquidsoap %{test_liq} LS460.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS503.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS503.liq liquidsoap %{test_liq} LS503.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  append-merge.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} append-merge.liq liquidsoap %{test_liq} append-merge.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  append-merge2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} append-merge2.liq liquidsoap %{test_liq} append-merge2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  append.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} append.liq liquidsoap %{test_liq} append.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  default_format.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} default_format.liq liquidsoap %{test_liq} default_format.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  external-encoder.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} external-encoder.liq liquidsoap %{test_liq} external-encoder.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  fallible_ogg.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} fallible_ogg.liq liquidsoap %{test_liq} fallible_ogg.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-copy-encode.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-copy-encode.liq liquidsoap %{test_liq} ffmpeg-copy-encode.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-copy-input-http.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-copy-input-http.liq liquidsoap %{test_liq} ffmpeg-copy-input-http.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-naming-convention.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-naming-convention.liq liquidsoap %{test_liq} ffmpeg-naming-convention.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  infallible-shutdown.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} infallible-shutdown.liq liquidsoap %{test_liq} infallible-shutdown.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  init-error.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} init-error.liq liquidsoap %{test_liq} init-error.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  initial_request_queue.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} initial_request_queue.liq liquidsoap %{test_liq} initial_request_queue.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  input_rtmp.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} input_rtmp.liq liquidsoap %{test_liq} input_rtmp.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata-resolvers-metadata.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata-resolvers-metadata.liq liquidsoap %{test_liq} metadata-resolvers-metadata.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata_cache.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata_cache.liq liquidsoap %{test_liq} metadata_cache.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  output_on_metadata.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} output_on_metadata.liq liquidsoap %{test_liq} output_on_metadata.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  output_on_track.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} output_on_track.liq liquidsoap %{test_liq} output_on_track.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  playlist-id.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} playlist-id.liq liquidsoap %{test_liq} playlist-id.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  seek_track_map.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} seek_track_map.liq liquidsoap %{test_liq} seek_track_map.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  shoutcast-args.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} shoutcast-args.liq liquidsoap %{test_liq} shoutcast-args.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  source_dynamic.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} source_dynamic.liq liquidsoap %{test_liq} source_dynamic.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  track_sensitive.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} track_sensitive.liq liquidsoap %{test_liq} track_sensitive.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  unified-pcm-types.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} unified-pcm-types.liq liquidsoap %{test_liq} unified-pcm-types.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  video-only.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} video-only.liq liquidsoap %{test_liq} video-only.liq)))
  