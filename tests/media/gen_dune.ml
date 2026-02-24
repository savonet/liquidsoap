let audio_decoding_tests =
  [
    ("Mono decoding", "mono.liq");
    ("Stereo decoding", "stereo.liq");
    ("FFmpeg audio decoder", "ffmpeg_audio_decoder.liq");
  ]

let video_decoding_tests =
  [
    ("FFmpeg video decoder", "ffmpeg_video_decoder.liq");
    ("FFmpeg video size", "video_size.liq");
  ]

let audio_video_decoding_tests =
  [
    ("FFmpeg add text filter", "ffmpeg_add_text.liq");
    ("FFmpeg copy decoder", "ffmpeg_copy_decoder.liq");
    ("FFmpeg copy+encode decode", "ffmpeg_copy_and_encode_decoder.liq");
    ("FFmpeg filter", "ffmpeg_filter.liq");
    ("FFmpeg filter parse", "ffmpeg_filter_parse.liq");
    ("FFmpeg bitstream filter", "ffmpeg_bitstream_filter.liq");
    ("FFmpeg raw decoder", "ffmpeg_raw_decoder.liq");
    ("FFmpeg raw+encode decoder", "ffmpeg_raw_and_encode_decoder.liq");
    ("FFmpeg raw+copy decoder", "ffmpeg_raw_and_copy_decoder.liq");
  ]

let standalone_tests =
  [
    ("multitrack.liq", []);
    ("ffmpeg_inline_encode_decode.liq", []);
    ("ffmpeg_inline_encode_decode_audio.liq", []);
    ("ffmpeg_inline_encode_decode_video.liq", []);
    ("ffmpeg_raw_hls.liq", []);
    ("pcm_s16_decode.liq", []);
    ("pcm_f32_decode.liq", []);
    ("subtitle_concat.liq", ["test-subtitle.srt"]);
    ("subtitle_concat_copy.liq", ["test-subtitle.srt"]);
    ("subtitle_multi.liq", ["test-subtitle.srt"]);
    ("subtitle_copy_encode.liq", ["test-subtitle.srt"]);
    ("subtitle_reencode.liq", ["test-subtitle.srt"]);
  ]

let audio_formats = Test_media_formats.audio_formats
let video_formats = Test_media_formats.video_formats
let audio_video_formats = Test_media_formats.audio_video_formats
let multitrack_formats = Test_media_formats.multitrack_formats
let audio_subtitle_formats = Test_media_formats.audio_subtitle_formats
let video_subtitle_formats = Test_media_formats.video_subtitle_formats

let audio_video_subtitle_formats =
  Test_media_formats.audio_video_subtitle_formats

let formats = Test_media_formats.all
let audio_decoding_test_formats = Test_media_formats.audio_decoding_test_formats
let video_decoding_test_formats = Test_media_formats.video_decoding_test_formats

let audio_video_decoding_test_formats =
  Test_media_formats.audio_video_decoding_test_formats

let encoder_format format =
  match List.rev (String.split_on_char '.' format) with
    | _ :: l -> String.concat "." (List.rev l)
    | _ -> assert false

let escaped_format = Test_media_formats.escaped_format
let filename = Test_media_formats.filename

let encoder_script format =
  Printf.sprintf "%s_encoder.liq" (escaped_format (encoder_format format))

let mediatests = ref []

let mediatest (type a) : (a, unit, string, string) format4 -> a =
  Printf.ksprintf (fun s ->
      mediatests := Printf.sprintf "(alias %s)" s :: !mediatests;
      s)

let mk_encoder ?(deps = []) source format =
  let extra_deps =
    match deps with [] -> "" | l -> "\n    " ^ String.concat "\n    " l
  in
  Printf.printf
    {|
(rule
  (alias %s)
  (package liquidsoap)
  (target %s)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder_%s.liq.in)%s)
  (action
    (with-stdout-to %%{target}
      (run %%{mk_encoder_test} %S %s %S))))

|}
    (mediatest "encoder_%s" source)
    (encoder_script format) source extra_deps (encoder_format format) source
    (escaped_format format)

let mk_encoded_file format =
  Printf.printf
    {|
(rule
 (alias mediatest)
 (package liquidsoap)
 (target %s)
 (deps
  (:encoder %s)
  (package liquidsoap)
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
   (run %%{run_test} %%{encoder} liquidsoap %%{test_liq} %%{encoder} -- %S)))

|}
    (filename format) (encoder_script format) (encoder_format format)

let () =
  List.iter (mk_encoder "audio_only") audio_formats;
  List.iter (mk_encoder "video_only") video_formats;
  List.iter (mk_encoder "audio_video") audio_video_formats;
  List.iter (mk_encoder "multitrack") multitrack_formats;
  let subtitle_deps = ["test-subtitle.srt"] in
  List.iter
    (mk_encoder ~deps:subtitle_deps "audio_subtitle")
    audio_subtitle_formats;
  List.iter
    (mk_encoder ~deps:subtitle_deps "video_subtitle")
    video_subtitle_formats;
  List.iter
    (mk_encoder ~deps:subtitle_deps "audio_video_subtitle")
    audio_video_subtitle_formats;
  List.iter mk_encoded_file formats;
  Printf.printf
    {|
(alias
  (name all_media_files)
  (deps
    %s))
|}
    (String.concat "\n" (List.map filename formats))

let file_test ?(deps = []) ~label ~test fname =
  let extra_deps =
    match deps with [] -> "" | l -> "\n  " ^ String.concat "\n  " l
  in
  Printf.printf
    {|
(rule
 (alias %s)
 (package liquidsoap)
 (deps
  (alias all_media_files)
  %s%s
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %%{run_test} %S liquidsoap %%{test_liq} %s -- %S)))

|}
    (mediatest "%s" (Filename.remove_extension test))
    test extra_deps label test fname

let () =
  List.iter
    (fun format ->
      let fname = filename format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        audio_decoding_tests)
    audio_decoding_test_formats

let () =
  List.iter
    (fun format ->
      let fname = filename format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        video_decoding_tests)
    video_decoding_test_formats

let () =
  List.iter
    (fun format ->
      let fname = filename format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        audio_video_decoding_tests)
    audio_video_decoding_test_formats

let () =
  List.iter
    (fun (test, deps) -> file_test ~deps ~label:test ~test "")
    standalone_tests

let () =
  Printf.printf
    {|(alias
  (name mediatest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !mediatests))
