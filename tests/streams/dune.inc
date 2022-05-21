
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  197.liq
huge-playlist.liq
playlist-watch.liq
on_frame.liq
195.liq
image.liq
ctype2.liq
say.liq
ctype1.liq
random.liq
smart-crossfade.liq
request.liq
radio2.liq
cross.liq
stretch-clock-propagation.liq
rotate.liq
sine.detect.liq
on_metadata.liq
dtmf.liq
on_track.liq
crossfade.liq
radio.liq
autostart.liq
  (:file1_mp3 ./file1.mp3)
  (:file2_mp3 ./file2.mp3)
  (:file3_mp3 ./file3.mp3)
  (:jingle1_mp3 ./jingle1.mp3)
  (:jingle2_mp3 ./jingle2.mp3)
  (:jingle3_mp3 ./jingle3.mp3)
  (:file1_png ./file1.png)
  (:file2_png ./file2.png)
  (:jingles ./jingles)
  (:playlist ./playlist)
  (:huge_playlist ./huge_playlist)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" 197.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" huge-playlist.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" playlist-watch.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" on_frame.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" 195.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" image.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" ctype2.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" say.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" ctype1.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" random.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" smart-crossfade.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" request.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" radio2.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" cross.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" stretch-clock-propagation.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" rotate.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" sine.detect.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" on_metadata.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" dtmf.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" on_track.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" crossfade.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" radio.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" autostart.liq))))
  