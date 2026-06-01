let stanza name libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name name
    (String.concat " " libraries)

let () =
  if Has_ffmpeg.available then begin
    stanza "test_resample" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "test_info" ["ffmpeg-av"; "ffmpeg-swresample"];
    stanza "test_subtitle_read" ["ffmpeg-av"];
    stanza "test_unhandled_packet" ["ffmpeg-av"];
    print_string
      {|
(rule
 (targets test_with_subs.mkv)
 (deps fixtures/sample.srt)
 (action
  (run
   ffmpeg
   -y
   -f
   lavfi
   -i
   "color=c=blue:s=320x240:d=25"
   -f
   lavfi
   -i
   "anullsrc=r=44100:cl=stereo:d=25"
   -i
   %{deps}
   -c:v
   mpeg4
   -c:a
   aac
   -c:s
   srt
   -shortest
   %{targets})))

(rule
 (targets raw_remuxed_subs.srt)
 (deps
  (:exe ../examples/subtitle_remux.exe)
  test_with_subs.mkv)
 (action
  (run %{exe} test_with_subs.mkv %{targets} subrip)))

(rule
 (targets remuxed_subs.srt)
 (deps
  (:exe normalize_line_endings.exe)
  (:input raw_remuxed_subs.srt))
 (action
  (with-stdout-to
   %{targets}
   (run %{exe} %{input}))))

(rule
 (alias test_subtitle_remux)
 (package ffmpeg)
 (deps remuxed_subs.srt fixtures/sample.srt)
 (action
  (diff fixtures/sample.srt remuxed_subs.srt)))
|}
  end
