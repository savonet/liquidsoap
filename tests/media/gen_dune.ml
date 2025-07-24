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
    ("FFmpeg bitstream filter", "ffmpeg_bitstream_filter.liq");
    ("FFmpeg raw decoder", "ffmpeg_raw_decoder.liq");
    ("FFmpeg raw+encode decoder", "ffmpeg_raw_and_encode_decoder.liq");
    ("FFmpeg raw+copy decoder", "ffmpeg_raw_and_copy_decoder.liq");
  ]

let standalone_tests =
  [
    "multitrack.liq";
    "ffmpeg_inline_encode_decode.liq";
    "ffmpeg_inline_encode_decode_audio.liq";
    "ffmpeg_inline_encode_decode_video.liq";
    "ffmpeg_raw_hls.liq";
    "pcm_s16_decode.liq";
    "pcm_f32_decode.liq";
  ]

let audio_formats =
  [
    {|%fdkaac(aot="mpeg4_aac_lc",channels=1).aac|};
    "%fdkaac(channels=2).aac";
    "%shine(channels=1).mp3";
    "%shine(channels=2).mp3";
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
    {|%ffmpeg(format="mp4",%audio(codec="aac",samplerate="48k")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac")).mp4|};
    {|%ffmpeg(format="mp4",%audio(pcm_s16,codec="aac")).mp4|};
    {|%ffmpeg(format="mp4",%audio(pcm_f32,codec="aac")).mp4|};
  ]

let video_formats = [{|%ffmpeg(format="mp4",%video(codec="libx264")).mp4|}]

let audio_video_formats =
  [
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=1),%video(codec="libx264")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%video(codec="libx264")).mp4|};
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%video(codec="libx264",r=12)).mp4|};
  ]

let multitrack_formats =
  [
    {|%ffmpeg(format="mp4",%audio(codec="aac",channels=2),%audio_2(codec="aac",channels=1),%video(codec="libx264"),%video_2(codec="libx264")).mp4|};
  ]

let formats =
  audio_formats @ audio_video_formats @ video_formats @ multitrack_formats

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
  (alias mediatest)
  (package liquidsoap)
  (target %s)
  (deps
    (:mk_encoder_test ./mk_encoder_test.sh)
    (:encoder_in ./encoder_%s.liq.in))
  (action
    (with-stdout-to %%{target}
      (run %%{mk_encoder_test} %S %s %S))))|}
    (encoder_script format) source (encoder_format format) source
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
   (run %%{run_test} %%{encoder} liquidsoap %%{test_liq} %%{encoder} -- %S)))|}
    (escaped_format format) (encoder_script format) (encoder_format format)

let () =
  List.iter (mk_encoder "audio_only") audio_formats;
  List.iter (mk_encoder "video_only") video_formats;
  List.iter (mk_encoder "audio_video") audio_video_formats;
  List.iter (mk_encoder "multitrack") multitrack_formats;
  List.iter mk_encoded_file formats;
  Printf.printf
    {|
(rule
  (alias mediatest)
  (package liquidsoap)
  (target all_media_files)
  (deps
    %s)
  (action (run touch %%{target})))|}
    (String.concat "\n" (List.map escaped_format formats))

let file_test ~label ~test fname =
  Printf.printf
    {|
(rule
 (alias mediatest)
 (package liquidsoap)
 (deps
  all_media_files
  %s
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %%{run_test} %S liquidsoap %%{test_liq} %s -- %S)))|}
    test label test fname

let () =
  List.iter
    (fun format ->
      let fname = escaped_format format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        audio_decoding_tests)
    audio_formats

let () =
  List.iter
    (fun format ->
      let fname = escaped_format format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        video_decoding_tests)
    (video_formats @ audio_video_formats)

let () =
  List.iter
    (fun format ->
      let fname = escaped_format format in
      List.iter
        (fun (name, test) ->
          file_test ~label:(name ^ " test for " ^ fname) ~test fname)
        audio_video_decoding_tests)
    audio_video_formats

let () = List.iter (fun test -> file_test ~label:test ~test "") standalone_tests
