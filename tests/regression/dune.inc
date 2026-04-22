(rule
 (alias test_115-1)
 (package liquidsoap)
 (deps
  115-1.liq
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
  (run %{run_test} 115-1 liquidsoap %{test_liq} 115-1.liq)))

(rule
 (alias test_115-2)
 (package liquidsoap)
 (deps
  115-2.liq
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
  (run %{run_test} 115-2 liquidsoap %{test_liq} 115-2.liq)))

(rule
 (alias test_3553)
 (package liquidsoap)
 (deps
  3553.liq
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
  (run %{run_test} 3553 liquidsoap %{test_liq} 3553.liq)))

(rule
 (alias test_AC5109)
 (package liquidsoap)
 (deps
  AC5109.liq
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
  (run %{run_test} AC5109 liquidsoap %{test_liq} AC5109.liq)))

(rule
 (alias test_BUG403)
 (package liquidsoap)
 (deps
  BUG403.liq
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
  (run %{run_test} BUG403 liquidsoap %{test_liq} BUG403.liq)))

(rule
 (alias test_GH-action-919422659)
 (package liquidsoap)
 (deps
  GH-action-919422659.liq
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
   GH-action-919422659
   liquidsoap
   %{test_liq}
   GH-action-919422659.liq)))

(rule
 (alias test_GH1129)
 (package liquidsoap)
 (deps
  GH1129.liq
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
  (run %{run_test} GH1129 liquidsoap %{test_liq} GH1129.liq)))

(rule
 (alias test_GH1146)
 (package liquidsoap)
 (deps
  GH1146.liq
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
  (run %{run_test} GH1146 liquidsoap %{test_liq} GH1146.liq)))

(rule
 (alias test_GH1151)
 (package liquidsoap)
 (deps
  GH1151.liq
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
  (run %{run_test} GH1151 liquidsoap %{test_liq} GH1151.liq)))

(rule
 (alias test_GH1159)
 (package liquidsoap)
 (deps
  GH1159.liq
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
  (run %{run_test} GH1159 liquidsoap %{test_liq} GH1159.liq)))

(rule
 (alias test_GH1279)
 (package liquidsoap)
 (deps
  GH1279.liq
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
  (run %{run_test} GH1279 liquidsoap %{test_liq} GH1279.liq)))

(rule
 (alias test_GH1327)
 (package liquidsoap)
 (deps
  GH1327.liq
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
  (run %{run_test} GH1327 liquidsoap %{test_liq} GH1327.liq)))

(rule
 (alias test_GH2585)
 (package liquidsoap)
 (deps
  GH2585.liq
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
  (run %{run_test} GH2585 liquidsoap %{test_liq} GH2585.liq)))

(rule
 (alias test_GH2602)
 (package liquidsoap)
 (deps
  GH2602.liq
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
  (run %{run_test} GH2602 liquidsoap %{test_liq} GH2602.liq)))

(rule
 (alias test_GH2756-2)
 (package liquidsoap)
 (deps
  GH2756-2.liq
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
  (run %{run_test} GH2756-2 liquidsoap %{test_liq} GH2756-2.liq)))

(rule
 (alias test_GH2756)
 (package liquidsoap)
 (deps
  GH2756.liq
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
  (run %{run_test} GH2756 liquidsoap %{test_liq} GH2756.liq)))

(rule
 (alias test_GH2758)
 (package liquidsoap)
 (deps
  GH2758.liq
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
  (run %{run_test} GH2758 liquidsoap %{test_liq} GH2758.liq)))

(rule
 (alias test_GH2842)
 (package liquidsoap)
 (deps
  GH2842.liq
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
  (run %{run_test} GH2842 liquidsoap %{test_liq} GH2842.liq)))

(rule
 (alias test_GH2850)
 (package liquidsoap)
 (deps
  GH2850.liq
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
  (run %{run_test} GH2850 liquidsoap %{test_liq} GH2850.liq)))

(rule
 (alias test_GH2867)
 (package liquidsoap)
 (deps
  GH2867.liq
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
  (run %{run_test} GH2867 liquidsoap %{test_liq} GH2867.liq)))

(rule
 (alias test_GH2871)
 (package liquidsoap)
 (deps
  GH2871.liq
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
  (run %{run_test} GH2871 liquidsoap %{test_liq} GH2871.liq)))

(rule
 (alias test_GH2872)
 (package liquidsoap)
 (deps
  GH2872.liq
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
  (run %{run_test} GH2872 liquidsoap %{test_liq} GH2872.liq)))

(rule
 (alias test_GH2897)
 (package liquidsoap)
 (deps
  GH2897.liq
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
  (run %{run_test} GH2897 liquidsoap %{test_liq} GH2897.liq)))

(rule
 (alias test_GH2902)
 (package liquidsoap)
 (deps
  GH2902.liq
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
  (run %{run_test} GH2902 liquidsoap %{test_liq} GH2902.liq)))

(rule
 (alias test_GH2926)
 (package liquidsoap)
 (deps
  GH2926.liq
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
  (run %{run_test} GH2926 liquidsoap %{test_liq} GH2926.liq)))

(rule
 (alias test_GH3093)
 (package liquidsoap)
 (deps
  GH3093.liq
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
  (run %{run_test} GH3093 liquidsoap %{test_liq} GH3093.liq)))

(rule
 (alias test_GH3132)
 (package liquidsoap)
 (deps
  GH3132.liq
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
  (run %{run_test} GH3132 liquidsoap %{test_liq} GH3132.liq)))

(rule
 (alias test_GH3134)
 (package liquidsoap)
 (deps
  GH3134.liq
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
  (run %{run_test} GH3134 liquidsoap %{test_liq} GH3134.liq)))

(rule
 (alias test_GH3224)
 (package liquidsoap)
 (deps
  GH3224.liq
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
  (run %{run_test} GH3224 liquidsoap %{test_liq} GH3224.liq)))

(rule
 (alias test_GH3239-2)
 (package liquidsoap)
 (deps
  GH3239-2.liq
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
  (run %{run_test} GH3239-2 liquidsoap %{test_liq} GH3239-2.liq)))

(rule
 (alias test_GH3239-bis)
 (package liquidsoap)
 (deps
  GH3239-bis.liq
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
  (run %{run_test} GH3239-bis liquidsoap %{test_liq} GH3239-bis.liq)))

(rule
 (alias test_GH3239)
 (package liquidsoap)
 (deps
  GH3239.liq
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
  (run %{run_test} GH3239 liquidsoap %{test_liq} GH3239.liq)))

(rule
 (alias test_GH3276)
 (package liquidsoap)
 (deps
  GH3276.liq
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
  (run %{run_test} GH3276 liquidsoap %{test_liq} GH3276.liq)))

(rule
 (alias test_GH3303)
 (package liquidsoap)
 (deps
  GH3303.liq
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
  (run %{run_test} GH3303 liquidsoap %{test_liq} GH3303.liq)))

(rule
 (alias test_GH3316)
 (package liquidsoap)
 (deps
  GH3316.liq
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
  (run %{run_test} GH3316 liquidsoap %{test_liq} GH3316.liq)))

(rule
 (alias test_GH3645)
 (package liquidsoap)
 (deps
  GH3645.liq
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
  (run %{run_test} GH3645 liquidsoap %{test_liq} GH3645.liq)))

(rule
 (alias test_GH3675)
 (package liquidsoap)
 (deps
  GH3675.liq
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
  (run %{run_test} GH3675 liquidsoap %{test_liq} GH3675.liq)))

(rule
 (alias test_GH3813)
 (package liquidsoap)
 (deps
  GH3813.liq
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
  (run %{run_test} GH3813 liquidsoap %{test_liq} GH3813.liq)))

(rule
 (alias test_GH3840)
 (package liquidsoap)
 (deps
  GH3840.liq
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
  (run %{run_test} GH3840 liquidsoap %{test_liq} GH3840.liq)))

(rule
 (alias test_GH3881)
 (package liquidsoap)
 (deps
  GH3881.liq
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
  (run %{run_test} GH3881 liquidsoap %{test_liq} GH3881.liq)))

(rule
 (alias test_GH3960)
 (package liquidsoap)
 (deps
  GH3960.liq
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
  (run %{run_test} GH3960 liquidsoap %{test_liq} GH3960.liq)))

(rule
 (alias test_GH4071)
 (package liquidsoap)
 (deps
  GH4071.liq
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
  (run %{run_test} GH4071 liquidsoap %{test_liq} GH4071.liq)))

(rule
 (alias test_GH4090)
 (package liquidsoap)
 (deps
  GH4090.liq
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
  (run %{run_test} GH4090 liquidsoap %{test_liq} GH4090.liq)))

(rule
 (alias test_GH4116)
 (package liquidsoap)
 (deps
  GH4116.liq
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
  (run %{run_test} GH4116 liquidsoap %{test_liq} GH4116.liq)))

(rule
 (alias test_GH4124)
 (package liquidsoap)
 (deps
  GH4124.liq
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
  (run %{run_test} GH4124 liquidsoap %{test_liq} GH4124.liq)))

(rule
 (alias test_GH4140)
 (package liquidsoap)
 (deps
  GH4140.liq
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
  (run %{run_test} GH4140 liquidsoap %{test_liq} GH4140.liq)))

(rule
 (alias test_GH4144)
 (package liquidsoap)
 (deps
  GH4144.liq
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
  (run %{run_test} GH4144 liquidsoap %{test_liq} GH4144.liq)))

(rule
 (alias test_GH4159)
 (package liquidsoap)
 (deps
  GH4159.liq
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
  (run %{run_test} GH4159 liquidsoap %{test_liq} GH4159.liq)))

(rule
 (alias test_GH4163)
 (package liquidsoap)
 (deps
  GH4163.liq
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
  (run %{run_test} GH4163 liquidsoap %{test_liq} GH4163.liq)))

(rule
 (alias test_GH4246)
 (package liquidsoap)
 (deps
  GH4246.liq
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
  (run %{run_test} GH4246 liquidsoap %{test_liq} GH4246.liq)))

(rule
 (alias test_GH4281-2)
 (package liquidsoap)
 (deps
  GH4281-2.liq
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
  (run %{run_test} GH4281-2 liquidsoap %{test_liq} GH4281-2.liq)))

(rule
 (alias test_GH4281)
 (package liquidsoap)
 (deps
  GH4281.liq
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
  (run %{run_test} GH4281 liquidsoap %{test_liq} GH4281.liq)))

(rule
 (alias test_GH4395)
 (package liquidsoap)
 (deps
  GH4395.liq
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
  (run %{run_test} GH4395 liquidsoap %{test_liq} GH4395.liq)))

(rule
 (alias test_GH4499)
 (package liquidsoap)
 (deps
  GH4499.liq
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
  (run %{run_test} GH4499 liquidsoap %{test_liq} GH4499.liq)))

(rule
 (alias test_GH4629)
 (package liquidsoap)
 (deps
  GH4629.liq
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
  (run %{run_test} GH4629 liquidsoap %{test_liq} GH4629.liq)))

(rule
 (alias test_GH4711)
 (package liquidsoap)
 (deps
  GH4711.liq
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
  (run %{run_test} GH4711 liquidsoap %{test_liq} GH4711.liq)))

(rule
 (alias test_GH4745)
 (package liquidsoap)
 (deps
  GH4745.liq
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
  (run %{run_test} GH4745 liquidsoap %{test_liq} GH4745.liq)))

(rule
 (alias test_GH4748)
 (package liquidsoap)
 (deps
  GH4748.liq
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
  (run %{run_test} GH4748 liquidsoap %{test_liq} GH4748.liq)))

(rule
 (alias test_GH4819-bis)
 (package liquidsoap)
 (deps
  GH4819-bis.liq
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
  (run %{run_test} GH4819-bis liquidsoap %{test_liq} GH4819-bis.liq)))

(rule
 (alias test_GH4819)
 (package liquidsoap)
 (deps
  GH4819.liq
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
  (run %{run_test} GH4819 liquidsoap %{test_liq} GH4819.liq)))

(rule
 (alias test_GH4828)
 (package liquidsoap)
 (deps
  GH4828.liq
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
  (run %{run_test} GH4828 liquidsoap %{test_liq} GH4828.liq)))

(rule
 (alias test_GH4840)
 (package liquidsoap)
 (deps
  GH4840.liq
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
  (run %{run_test} GH4840 liquidsoap %{test_liq} GH4840.liq)))

(rule
 (alias test_GH4882)
 (package liquidsoap)
 (deps
  GH4882.liq
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
  (run %{run_test} GH4882 liquidsoap %{test_liq} GH4882.liq)))

(rule
 (alias test_GH4922)
 (package liquidsoap)
 (deps
  GH4922.liq
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
  (run %{run_test} GH4922 liquidsoap %{test_liq} GH4922.liq)))

(rule
 (alias test_GH4995)
 (package liquidsoap)
 (deps
  GH4995.liq
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4
  ../streams/playlist
  ../streams/file2.mp3
  ../streams/file3.mp3
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} GH4995 liquidsoap %{test_liq} GH4995.liq)))

(rule
 (alias test_GH5019)
 (package liquidsoap)
 (targets GH5019.wav)
 (deps
  GH5019.liq
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
  (run %{run_test} GH5019 liquidsoap %{test_liq} GH5019.liq)))

(rule
 (alias test_GH5110)
 (package liquidsoap)
 (deps
  GH5110.liq
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
  (run %{run_test} GH5110 liquidsoap %{test_liq} GH5110.liq)))

(rule
 (alias test_LS268)
 (package liquidsoap)
 (deps
  LS268.liq
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
  (run %{run_test} LS268 liquidsoap %{test_liq} LS268.liq)))

(rule
 (alias test_LS354-1)
 (package liquidsoap)
 (deps
  LS354-1.liq
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
  (run %{run_test} LS354-1 liquidsoap %{test_liq} LS354-1.liq)))

(rule
 (alias test_LS354-2)
 (package liquidsoap)
 (deps
  LS354-2.liq
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
  (run %{run_test} LS354-2 liquidsoap %{test_liq} LS354-2.liq)))

(rule
 (alias test_LS460)
 (package liquidsoap)
 (deps
  LS460.liq
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
  (run %{run_test} LS460 liquidsoap %{test_liq} LS460.liq)))

(rule
 (alias test_LS503)
 (package liquidsoap)
 (deps
  LS503.liq
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
  (run %{run_test} LS503 liquidsoap %{test_liq} LS503.liq)))

(rule
 (alias test_append-merge)
 (package liquidsoap)
 (deps
  append-merge.liq
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
  (run %{run_test} append-merge liquidsoap %{test_liq} append-merge.liq)))

(rule
 (alias test_append-merge2)
 (package liquidsoap)
 (deps
  append-merge2.liq
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
  (run %{run_test} append-merge2 liquidsoap %{test_liq} append-merge2.liq)))

(rule
 (alias test_append)
 (package liquidsoap)
 (deps
  append.liq
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
  (run %{run_test} append liquidsoap %{test_liq} append.liq)))

(rule
 (alias test_default_format)
 (package liquidsoap)
 (deps
  default_format.liq
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
  (run %{run_test} default_format liquidsoap %{test_liq} default_format.liq)))

(rule
 (alias test_double-nullable)
 (package liquidsoap)
 (deps
  double-nullable.liq
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
   double-nullable
   liquidsoap
   %{test_liq}
   double-nullable.liq)))

(rule
 (alias test_eval-check-demeth)
 (package liquidsoap)
 (deps
  eval-check-demeth.liq
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
   eval-check-demeth
   liquidsoap
   %{test_liq}
   eval-check-demeth.liq)))

(rule
 (alias test_external-encoder)
 (package liquidsoap)
 (deps
  external-encoder.liq
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
   external-encoder
   liquidsoap
   %{test_liq}
   external-encoder.liq)))

(rule
 (alias test_fallible_ogg)
 (package liquidsoap)
 (deps
  fallible_ogg.liq
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
  (run %{run_test} fallible_ogg liquidsoap %{test_liq} fallible_ogg.liq)))

(rule
 (alias test_ffmpeg-copy-encode)
 (package liquidsoap)
 (deps
  ffmpeg-copy-encode.liq
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
   ffmpeg-copy-encode
   liquidsoap
   %{test_liq}
   ffmpeg-copy-encode.liq)))

(rule
 (alias test_ffmpeg-copy-input-http)
 (package liquidsoap)
 (deps
  ffmpeg-copy-input-http.liq
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
   ffmpeg-copy-input-http
   liquidsoap
   %{test_liq}
   ffmpeg-copy-input-http.liq)))

(rule
 (alias test_ffmpeg-naming-convention)
 (package liquidsoap)
 (deps
  ffmpeg-naming-convention.liq
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
   ffmpeg-naming-convention
   liquidsoap
   %{test_liq}
   ffmpeg-naming-convention.liq)))

(rule
 (alias test_infallible-shutdown)
 (package liquidsoap)
 (deps
  infallible-shutdown.liq
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
   infallible-shutdown
   liquidsoap
   %{test_liq}
   infallible-shutdown.liq)))

(rule
 (alias test_init-error)
 (package liquidsoap)
 (deps
  init-error.liq
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
  (run %{run_test} init-error liquidsoap %{test_liq} init-error.liq)))

(rule
 (alias test_initial_request_queue)
 (package liquidsoap)
 (deps
  initial_request_queue.liq
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
   initial_request_queue
   liquidsoap
   %{test_liq}
   initial_request_queue.liq)))

(rule
 (alias test_input_rtmp)
 (package liquidsoap)
 (deps
  input_rtmp.liq
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
  (run %{run_test} input_rtmp liquidsoap %{test_liq} input_rtmp.liq)))

(rule
 (alias test_metadata-resolvers-metadata)
 (package liquidsoap)
 (deps
  metadata-resolvers-metadata.liq
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
   metadata-resolvers-metadata
   liquidsoap
   %{test_liq}
   metadata-resolvers-metadata.liq)))

(rule
 (alias test_metadata_cache)
 (package liquidsoap)
 (deps
  metadata_cache.liq
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
  (run %{run_test} metadata_cache liquidsoap %{test_liq} metadata_cache.liq)))

(rule
 (alias test_output_on_metadata)
 (package liquidsoap)
 (deps
  output_on_metadata.liq
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
   output_on_metadata
   liquidsoap
   %{test_liq}
   output_on_metadata.liq)))

(rule
 (alias test_output_on_track)
 (package liquidsoap)
 (deps
  output_on_track.liq
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
   output_on_track
   liquidsoap
   %{test_liq}
   output_on_track.liq)))

(rule
 (alias test_playlist-id)
 (package liquidsoap)
 (deps
  playlist-id.liq
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
  (run %{run_test} playlist-id liquidsoap %{test_liq} playlist-id.liq)))

(rule
 (alias test_replaygain)
 (package liquidsoap)
 (deps
  replaygain.liq
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
  (run %{run_test} replaygain liquidsoap %{test_liq} replaygain.liq)))

(rule
 (alias test_seek_track_map)
 (package liquidsoap)
 (deps
  seek_track_map.liq
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
  (run %{run_test} seek_track_map liquidsoap %{test_liq} seek_track_map.liq)))

(rule
 (alias test_shoutcast-args)
 (package liquidsoap)
 (deps
  shoutcast-args.liq
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
  (run %{run_test} shoutcast-args liquidsoap %{test_liq} shoutcast-args.liq)))

(rule
 (alias test_source_cleanup)
 (package liquidsoap)
 (deps
  source_cleanup.liq
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
  (run %{run_test} source_cleanup liquidsoap %{test_liq} source_cleanup.liq)))

(rule
 (alias test_source_dynamic)
 (package liquidsoap)
 (deps
  source_dynamic.liq
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
  (run %{run_test} source_dynamic liquidsoap %{test_liq} source_dynamic.liq)))

(rule
 (alias test_switch_per_source_no_reselect_during_fade)
 (package liquidsoap)
 (deps
  switch_per_source_no_reselect_during_fade.liq
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
   switch_per_source_no_reselect_during_fade
   liquidsoap
   %{test_liq}
   switch_per_source_no_reselect_during_fade.liq)))

(rule
 (alias test_switch_per_source_on_leave)
 (package liquidsoap)
 (deps
  switch_per_source_on_leave.liq
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
   switch_per_source_on_leave
   liquidsoap
   %{test_liq}
   switch_per_source_on_leave.liq)))

(rule
 (alias test_switch_per_source_on_select)
 (package liquidsoap)
 (deps
  switch_per_source_on_select.liq
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
   switch_per_source_on_select
   liquidsoap
   %{test_liq}
   switch_per_source_on_select.liq)))

(rule
 (alias test_switch_per_source_single)
 (package liquidsoap)
 (deps
  switch_per_source_single.liq
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
   switch_per_source_single
   liquidsoap
   %{test_liq}
   switch_per_source_single.liq)))

(rule
 (alias test_track_sensitive)
 (package liquidsoap)
 (deps
  track_sensitive.liq
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
   track_sensitive
   liquidsoap
   %{test_liq}
   track_sensitive.liq)))

(rule
 (alias test_unified-pcm-types)
 (package liquidsoap)
 (deps
  unified-pcm-types.liq
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
   unified-pcm-types
   liquidsoap
   %{test_liq}
   unified-pcm-types.liq)))

(rule
 (alias test_video-only)
 (package liquidsoap)
 (deps
  video-only.liq
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
  (run %{run_test} video-only liquidsoap %{test_liq} video-only.liq)))

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
  (alias test_GH4995)
  (alias test_GH5019)
  (alias test_GH5110)
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
  (alias test_switch_per_source_no_reselect_during_fade)
  (alias test_switch_per_source_on_leave)
  (alias test_switch_per_source_on_select)
  (alias test_switch_per_source_single)
  (alias test_track_sensitive)
  (alias test_unified-pcm-types)
  (alias test_video-only)))
