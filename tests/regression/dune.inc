
(rule
 (alias 115-1)
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
 (alias 115-2)
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
 (alias 3553)
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
 (alias AC5109)
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
 (alias BUG403)
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
 (alias GH-action-919422659)
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
 (alias GH1129)
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
 (alias GH1146)
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
 (alias GH1151)
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
 (alias GH1159)
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
 (alias GH1279)
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
 (alias GH1327)
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
 (alias GH2585)
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
 (alias GH2602)
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
 (alias GH2756-2)
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
 (alias GH2756)
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
 (alias GH2758)
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
 (alias GH2842)
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
 (alias GH2850)
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
 (alias GH2867)
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
 (alias GH2871)
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
 (alias GH2872)
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
 (alias GH2897)
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
 (alias GH2902)
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
 (alias GH2926)
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
 (alias GH3093)
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
 (alias GH3132)
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
 (alias GH3134)
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
 (alias GH3224)
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
 (alias GH3239-2)
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
 (alias GH3239-bis)
 (package liquidsoap)
 (deps
  GH3239-bis.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH3239-bis.liq liquidsoap %{test_liq} GH3239-bis.liq)))
  
(rule
 (alias GH3239)
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
 (alias GH3276)
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
 (alias GH3303)
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
 (alias GH3316)
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
 (alias GH3645)
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
 (alias GH3675)
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
 (alias GH3813)
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
 (alias GH3840)
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
 (alias GH3881)
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
 (alias GH3960)
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
 (alias GH4071)
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
 (alias GH4090)
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
 (alias GH4116)
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
 (alias GH4124)
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
 (alias GH4140)
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
 (alias GH4144)
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
 (alias GH4159)
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
 (alias GH4163)
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
 (alias GH4246)
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
 (alias GH4281-2)
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
 (alias GH4281)
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
 (alias GH4395)
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
 (alias GH4499)
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
 (alias GH4629)
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
 (alias GH4711)
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
 (alias GH4745)
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
 (alias GH4748)
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
 (alias GH4819)
 (package liquidsoap)
 (deps
  GH4819.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH4819.liq liquidsoap %{test_liq} GH4819.liq)))
  
(rule
 (alias LS268)
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
 (alias LS354-1)
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
 (alias LS354-2)
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
 (alias LS460)
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
 (alias LS503)
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
 (alias append-merge)
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
 (alias append-merge2)
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
 (alias append)
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
 (alias default_format)
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
 (alias eval-check-demeth)
 (package liquidsoap)
 (deps
  eval-check-demeth.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} eval-check-demeth.liq liquidsoap %{test_liq} eval-check-demeth.liq)))
  
(rule
 (alias external-encoder)
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
 (alias fallible_ogg)
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
 (alias ffmpeg-copy-encode)
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
 (alias ffmpeg-copy-input-http)
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
 (alias ffmpeg-naming-convention)
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
 (alias infallible-shutdown)
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
 (alias init-error)
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
 (alias initial_request_queue)
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
 (alias input_rtmp)
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
 (alias metadata-resolvers-metadata)
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
 (alias metadata_cache)
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
 (alias output_on_metadata)
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
 (alias output_on_track)
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
 (alias playlist-id)
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
 (alias replaygain)
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
 (alias seek_track_map)
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
 (alias shoutcast-args)
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
 (alias source_cleanup)
 (package liquidsoap)
 (deps
  source_cleanup.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} source_cleanup.liq liquidsoap %{test_liq} source_cleanup.liq)))
  
(rule
 (alias source_dynamic)
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
 (alias track_sensitive)
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
 (alias unified-pcm-types)
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
 (alias video-only)
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
  (alias
  (name citest)
  (deps
    (alias 115-1)
    (alias 115-2)
    (alias 3553)
    (alias AC5109)
    (alias BUG403)
    (alias GH-action-919422659)
    (alias GH1129)
    (alias GH1146)
    (alias GH1151)
    (alias GH1159)
    (alias GH1279)
    (alias GH1327)
    (alias GH2585)
    (alias GH2602)
    (alias GH2756)
    (alias GH2756-2)
    (alias GH2758)
    (alias GH2842)
    (alias GH2850)
    (alias GH2867)
    (alias GH2871)
    (alias GH2872)
    (alias GH2897)
    (alias GH2902)
    (alias GH2926)
    (alias GH3093)
    (alias GH3132)
    (alias GH3134)
    (alias GH3224)
    (alias GH3239)
    (alias GH3239-2)
    (alias GH3239-bis)
    (alias GH3276)
    (alias GH3303)
    (alias GH3316)
    (alias GH3645)
    (alias GH3675)
    (alias GH3813)
    (alias GH3840)
    (alias GH3881)
    (alias GH3960)
    (alias GH4071)
    (alias GH4090)
    (alias GH4116)
    (alias GH4124)
    (alias GH4140)
    (alias GH4144)
    (alias GH4159)
    (alias GH4163)
    (alias GH4246)
    (alias GH4281)
    (alias GH4281-2)
    (alias GH4395)
    (alias GH4499)
    (alias GH4629)
    (alias GH4711)
    (alias GH4745)
    (alias GH4748)
    (alias GH4819)
    (alias LS268)
    (alias LS354-1)
    (alias LS354-2)
    (alias LS460)
    (alias LS503)
    (alias append)
    (alias append-merge)
    (alias append-merge2)
    (alias default_format)
    (alias eval-check-demeth)
    (alias external-encoder)
    (alias fallible_ogg)
    (alias ffmpeg-copy-encode)
    (alias ffmpeg-copy-input-http)
    (alias ffmpeg-naming-convention)
    (alias infallible-shutdown)
    (alias init-error)
    (alias initial_request_queue)
    (alias input_rtmp)
    (alias metadata-resolvers-metadata)
    (alias metadata_cache)
    (alias output_on_metadata)
    (alias output_on_track)
    (alias playlist-id)
    (alias replaygain)
    (alias seek_track_map)
    (alias shoutcast-args)
    (alias source_cleanup)
    (alias source_dynamic)
    (alias track_sensitive)
    (alias unified-pcm-types)
    (alias video-only)))
