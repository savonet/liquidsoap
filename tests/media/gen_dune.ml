let audio_formats =
  [
    "%flac(stereo).flac";
    "%flac(mono).flac";
    "%wav(stereo).wav";
    "%wav(mono).wav";
    "%mp3(mono).mp3";
    "%mp3(stereo).mp3";
    "%ogg(%vorbis(mono)).ogg";
    "%ogg(%vorbis(stereo)).ogg";
    "%ogg(%flac(mono)).ogg";
    "%ogg(%flac(stereo)).ogg";
    "%ogg(%opus(mono)).ogg";
    "%ogg(%opus(stereo)).ogg";
    {|%ffmpeg(format="mp4",%audio(codec="aac"),%video.none).mp4|};
  ]

let video_formats =
  [
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=1),%video(codec="libx264")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%video(codec="libx264")).mp4|};
  ]

let formats = audio_formats @ video_formats

let encoder_format format =
  match List.rev (String.split_on_char '.' format) with
    | _ :: l -> String.concat "." (List.rev l)
    | _ -> assert false

let escaped_format =
  String.map (function
    | '%' -> '@'
    | '"' -> '\''
    | '(' -> '['
    | ')' -> ']'
    | c -> c)

let encoder_script format =
  Printf.sprintf "%s_encoder.liq" (escaped_format (encoder_format format))

let mk_encoder source format =
  Printf.printf
    {|
(rule
  (target %s)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:test_encoder_in ./test_encoder.liq.in))
  (action
    (with-stdout-to %%{target}
      (run %%{mk_encoder_test} %S %s %S))))|}
    (encoder_script format) (encoder_format format) source
    (escaped_format format)

let mk_encoded_file format =
  Printf.printf
    {|
(rule
 (target %s)
 (deps
  (:encoder %s)
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
   (run %%{run_test} "%%{liquidsoap} --no-stdlib %%{stdlib} %%{test_liq} -" %%{encoder} %S)))|}
    (escaped_format format) (encoder_script format) (encoder_format format)

let () =
  List.iter (mk_encoder "sine") audio_formats;
  List.iter (mk_encoder "noise") video_formats;
  List.iter mk_encoded_file formats;
  Printf.printf
    {|
(rule
  (target all_media_files)
  (deps
    %s)
  (action (run touch %%{target})))|}
    (String.concat "\n" (List.map escaped_format formats))

let () =
  Printf.printf
    {|
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (run echo TODO)))|}
