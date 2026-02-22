let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

(* Create fresh formats for each use to avoid mutation issues *)
let audio_decode () = Content.(default_format Audio.kind)
let audio_copy () = Content.(default_format Ffmpeg_copy_content.kind)
let video_decode () = Content.(default_format Video.kind)
let video_copy () = Content.(default_format Ffmpeg_copy_content.kind)
let subtitle_decode () = Subtitle_content.format
let subtitle_copy () = Content.(default_format Ffmpeg_copy_content.kind)
let is_audio_decode f = Content.Audio.is_format f
let is_audio_copy f = Ffmpeg_copy_content.is_format f
let is_video_decode f = Content.Video.is_format f
let is_video_copy f = Ffmpeg_copy_content.is_format f
let is_subtitle_decode f = Subtitle_content.is_format f
let is_subtitle_copy f = Ffmpeg_copy_content.is_format f
let test_failed = ref false

let check name condition =
  if condition then Printf.printf "PASS: %s\n%!" name
  else (
    Printf.printf "FAIL: %s\n%!" name;
    test_failed := true)

let with_container ?format filepath f =
  let av_container = Av.open_input filepath in
  Fun.protect
    ~finally:(fun () -> Av.close av_container)
    (fun () ->
      let container =
        Ffmpeg_stream_description.container ?format ~url:filepath av_container
      in
      f container)

let test_audio_video_file filepath =
  Printf.printf "\n=== Testing audio/video file: %s ===\n%!" filepath;

  (* Test: request decoded audio only -> get decoded audio only *)
  with_container filepath (fun container ->
      let ctype = Frame.Fields.make ~audio:(audio_decode ()) () in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded audio only"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_decode (Frame.Fields.find Frame.Fields.audio result)
        && not (Frame.Fields.mem Frame.Fields.video result)));

  (* Test: request decoded video only -> get decoded video only *)
  with_container filepath (fun container ->
      let ctype = Frame.Fields.make ~video:(video_decode ()) () in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded video only"
        (Frame.Fields.mem Frame.Fields.video result
        && is_video_decode (Frame.Fields.find Frame.Fields.video result)
        && not (Frame.Fields.mem Frame.Fields.audio result)));

  (* Test: request decoded audio + decoded video -> get both decoded *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ~video:(video_decode ()) ()
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded audio + decoded video"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_decode (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.video result
        && is_video_decode (Frame.Fields.find Frame.Fields.video result)));

  (* Test: request copy audio + decoded video -> get copy audio + decoded video *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_copy ()) ~video:(video_decode ()) ()
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "copy audio + decoded video"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_copy (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.video result
        && is_video_decode (Frame.Fields.find Frame.Fields.video result)));

  (* Test: request decoded audio + copy video -> get decoded audio + copy video *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ~video:(video_copy ()) ()
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded audio + copy video"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_decode (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.video result
        && is_video_copy (Frame.Fields.find Frame.Fields.video result)));

  (* Test: request copy audio + copy video -> get both copy *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_copy ()) ~video:(video_copy ()) ()
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "copy audio + copy video"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_copy (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.video result
        && is_video_copy (Frame.Fields.find Frame.Fields.video result)));

  (* Test: request subtitle on audio/video file -> no subtitle in result *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ~video:(video_decode ()) ()
        |> Frame.Fields.add Frame.Fields.subtitles (subtitle_decode ())
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "subtitle request on audio/video file returns no subtitle"
        (not (Frame.Fields.mem Frame.Fields.subtitles result)))

let test_audio_subtitle_file filepath =
  Printf.printf "\n=== Testing audio/subtitle file: %s ===\n%!" filepath;

  (* Test: request decoded audio + decoded subtitle *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ()
        |> Frame.Fields.add Frame.Fields.subtitles (subtitle_decode ())
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded audio + decoded subtitle"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_decode (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.subtitles result
        && is_subtitle_decode (Frame.Fields.find Frame.Fields.subtitles result)
        ));

  (* Test: request copy audio + decoded subtitle *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_copy ()) ()
        |> Frame.Fields.add Frame.Fields.subtitles (subtitle_decode ())
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "copy audio + decoded subtitle"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_copy (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.subtitles result
        && is_subtitle_decode (Frame.Fields.find Frame.Fields.subtitles result)
        ));

  (* Test: request decoded audio + copy subtitle *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ()
        |> Frame.Fields.add Frame.Fields.subtitles (subtitle_copy ())
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "decoded audio + copy subtitle"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_decode (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.subtitles result
        && is_subtitle_copy (Frame.Fields.find Frame.Fields.subtitles result)));

  (* Test: request copy audio + copy subtitle *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_copy ()) ()
        |> Frame.Fields.add Frame.Fields.subtitles (subtitle_copy ())
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "copy audio + copy subtitle"
        (Frame.Fields.mem Frame.Fields.audio result
        && is_audio_copy (Frame.Fields.find Frame.Fields.audio result)
        && Frame.Fields.mem Frame.Fields.subtitles result
        && is_subtitle_copy (Frame.Fields.find Frame.Fields.subtitles result)));

  (* Test: request video on audio/subtitle file -> no video in result *)
  with_container filepath (fun container ->
      let ctype =
        Frame.Fields.make ~audio:(audio_decode ()) ~video:(video_decode ()) ()
      in
      let result = Ffmpeg_stream_description.get_type ~ctype container in
      check "video request on audio/subtitle file returns no video"
        (not (Frame.Fields.mem Frame.Fields.video result)))

let test_format_option filepath =
  Printf.printf "\n=== Testing format option ===\n%!";

  match Av.Format.find_input_format "mp4" with
    | Some format ->
        with_container ~format filepath (fun container_with ->
            with_container filepath (fun container_without ->
                check "format option produces same format detection"
                  (container_with.format = container_without.format)))
    | None -> check "mp4 format found" false

let () =
  let base_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in

  let audio_video_format = List.hd Test_media_formats.audio_video_formats in
  let audio_video_file =
    Filename.concat base_dir (Test_media_formats.filename audio_video_format)
  in

  let audio_subtitle_format =
    List.hd Test_media_formats.audio_subtitle_formats
  in
  let audio_subtitle_file =
    Filename.concat base_dir (Test_media_formats.filename audio_subtitle_format)
  in

  if Sys.file_exists audio_video_file then
    test_audio_video_file audio_video_file
  else Printf.printf "SKIP: audio/video file not found: %s\n%!" audio_video_file;

  if Sys.file_exists audio_subtitle_file then
    test_audio_subtitle_file audio_subtitle_file
  else
    Printf.printf "SKIP: audio/subtitle file not found: %s\n%!"
      audio_subtitle_file;

  if Sys.file_exists audio_video_file then test_format_option audio_video_file;

  Printf.printf "\n";
  if !test_failed then (
    Printf.printf "Some tests FAILED!\n";
    exit 1)
  else Printf.printf "All tests PASSED!\n"
