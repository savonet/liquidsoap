let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

type mode = Decode | Audio_copy | Video_copy | Subtitle_copy

let string_of_mode = function
  | Decode -> "decode"
  | Audio_copy -> "audio_copy"
  | Video_copy -> "video_copy"
  | Subtitle_copy -> "subtitle_copy"

let ctype_of_mode = function
  | Decode ->
      Frame.Fields.make
        ~audio:Content.(default_format Audio.kind)
        ~video:Content.(default_format Video.kind)
        ()
      |> Frame.Fields.add Frame.Fields.subtitles Subtitle_content.format
  | Audio_copy ->
      Frame.Fields.make
        ~audio:Content.(default_format Ffmpeg_copy_content.kind)
        ~video:Content.(default_format Video.kind)
        ()
  | Video_copy ->
      Frame.Fields.make
        ~audio:Content.(default_format Audio.kind)
        ~video:Content.(default_format Ffmpeg_copy_content.kind)
        ()
  | Subtitle_copy ->
      Frame.Fields.make
        ~audio:Content.(default_format Audio.kind)
        ~video:Content.(default_format Video.kind)
        ()
      |> Frame.Fields.add Frame.Fields.subtitles
           Content.(default_format Ffmpeg_copy_content.kind)

let get_description ~mode filename =
  try
    let container = Av.open_input filename in
    Fun.protect
      ~finally:(fun () -> Av.close container)
      (fun () ->
        let ctype = ctype_of_mode mode in
        let result =
          Ffmpeg_stream_description.get_type ~ctype ~url:filename container
        in
        Ffmpeg_stream_description.json_of_container result.container)
  with exn -> `Assoc [("error", `String (Printexc.to_string exn))]

let () =
  let base_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let files =
    List.map
      (fun fmt -> Filename.concat base_dir (Test_media_formats.filename fmt))
      Test_media_formats.all
  in
  let modes = [Decode; Audio_copy; Video_copy; Subtitle_copy] in
  let descriptions =
    List.map
      (fun filepath ->
        let filename = Filename.basename filepath in
        if Sys.file_exists filepath then (
          let mode_results =
            List.map
              (fun mode ->
                (string_of_mode mode, get_description ~mode filepath))
              modes
          in
          (filename, `Assoc mode_results))
        else (filename, `Assoc [("error", `String "file not found")]))
      files
  in
  let json : Json.t = `Assoc descriptions in
  print_endline (Json.to_string ~compact:false json)
