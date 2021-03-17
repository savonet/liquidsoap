let formats =
  [
    (`Audio, "%flac(stereo)", "stereo.flac", "flac stereo");
    (`Audio, "%flac(mono)", "mono.flac", "flac mono");
    (`Audio, "%wav(stereo)", "stereo.wav", "wav mono");
    (`Audio, "%wav(mono)", "mono.wav", "wav mono");
    (`Audio, "%mp3(stereo)", "stereo.mp3", "mp3 stereo");
    (`Audio, "%mp3(mono)", "mono.mp3", "mp3 mono");
    (`Audio, "%vorbis(stereo)", "stereo_vorbis.ogg", "vorbis stereo");
    (`Audio, "%vorbis(mono)", "mono_vorbis.ogg", "vorbis mono");
    (`Audio, "%ogg(%flac(stereo))", "stereo_flac.ogg", "ogg/flac stereo");
    (`Audio, "%ogg(%flac(mono))", "mono_flac.ogg", "ogg/flac mono");
    (`Audio, "%opus(stereo)", "stereo_opus.ogg", "opus stereo");
    (`Audio, "%opus(mono)", "mono_opus.ogg", "opus mono");
    ( `Audio,
      "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video.none)",
      "mono.m4a",
      "mpeg4 stereo audio only" );
    ( `Audio,
      "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video.none)",
      "stereo.m4a",
      "mpeg4 mono audio only" );
    ( `Both,
      "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=1),%video(codec=\"libx264\"))",
      "mono_video.mp4",
      "mpeg4 mono audio & video" );
    ( `Both,
      "%ffmpeg(format=\"mp4\",%audio(codec=\"aac\",channels=2),%video(codec=\"libx264\"))",
      "stereo_video.mp4",
      "mpeg4 stereo audio & video" );
    ( `Video,
      "%ffmpeg(format=\"mp4\",%audio.none,%video(codec=\"libx264\"))",
      "video_only.mp4",
      "mpeg4 video only" );
  ]

let audio_tests =
  [
    ("mono decoding", "test_mono.liq");
    ("stereo decoding", "test_stereo.liq");
    ("ffmpeg audio decoder", "test_ffmpeg_audio_decoder.liq");
  ]

let audio_video_tests =
  [
    ("ffmpeg copy decoder", "test_ffmpeg_copy_decoder.liq");
    ("ffmpeg copy+encode decoder", "test_ffmpeg_copy_and_encode_decoder.liq");
    ("ffmpeg raw decoder", "test_ffmpeg_raw_decoder.liq");
    ("ffmpeg raw+encode decoder", "test_ffmpeg_raw_and_encode_decoder.liq");
    ("ffmpeg raw+copy decoder", "test_ffmpeg_raw_and_copy_decoder.liq");
  ]

let video_tests =
  [
    ("ffmpeg video decoder", "test_ffmpeg_video_decoder.liq");
    ("gstreamer video decoder", "test_gstreamer_video_decoder.liq");
  ]

let ffmpeg_tests =
  [
    ("ffmpeg inline encode+decode", "test_ffmpeg_inline_encode_decode.liq");
    ( "ffmpeg inline encode+decode audio only",
      "test_ffmpeg_inline_encode_decode_audio.liq" );
    ( "ffmpeg inline encode+decode video only",
      "test_ffmpeg_inline_encode_decode_video.liq" );
    ("ffmpeg distributed HLS", "test_ffmpeg_distributed_hls.liq");
    ("ffmpeg raw HLS", "test_ffmpeg_raw_hls.liq");
  ]

let encoder_file = Printf.sprintf "encoder-%s.liq"

let generate_encoder_template (mode, format, file, _) =
  let source = match mode with `Audio -> "sine" | _ -> "noise" in
  let encoder = encoder_file file in
  Printf.printf
    {|
(rule
 (target %s)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:mk_encoder_test ./mk_encoder_test.sh)
   (source_tree .))
 (action
   (with-stdout-to %s
    (run %%{mk_encoder_test} %S %s))))
|}
    encoder encoder format source

let genenerate_template (_, _, file, _) =
  let encoder = encoder_file file in
  Printf.printf
    {|
(rule
 (targets %s)
 (alias runtest)
 (package liquidsoap)
 (deps
   (:liquidsoap ../../src/bin/liquidsoap.exe)
   (:main_lib ../../libs/main.liq)
   (source_tree ../../libs)
  (source_tree .)
   %s)
  (action
    (progn
      (echo "Generating %s..\n")
      (ignore-outputs
        (run %%{liquidsoap} --no-libs %%{main_lib} %s -- %s)))))
|}
    file encoder file encoder file

let gen_rule_template formats =
  List.iter generate_encoder_template formats;
  List.iter genenerate_template formats

let test_action_template (mode, _, audio_file, audio_description) =
  let tests =
    match mode with
      | `Audio -> audio_tests
      | `Video -> video_tests
      | `Both -> audio_video_tests @ video_tests
  in
  String.concat "\n"
    (List.map
       (fun (test_name, test_file) ->
         Printf.sprintf
           {|(run %%{run_test} %%{liquidsoap} %%{main_lib} %s "%s test for %s" "" %s)|}
           test_file test_name audio_description audio_file)
       tests)

let ffmpeg_action_template (name, file) =
  Printf.sprintf {|(run %%{run_test} %%{liquidsoap} %%{main_lib} %s %S)|} file
    name

let test_rule_template formats =
  Printf.printf
    {|
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:main_lib ../../libs/main.liq)
  (:run_test ../run_test.sh)
  %s
  ../test.liq
  (source_tree ../../libs)
  (source_tree .))
 (action
  (progn
    %s
    %s
  )))
|}
    (String.concat "\n" (List.map (fun (_, _, file, _) -> file) formats))
    (String.concat "\n" (List.map test_action_template formats))
    (String.concat "\n" (List.map ffmpeg_action_template ffmpeg_tests))

let () =
  gen_rule_template formats;
  test_rule_template formats
