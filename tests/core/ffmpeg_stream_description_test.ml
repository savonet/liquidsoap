let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

let get_description filename =
  let container = Av.open_input filename in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () ->
      let ctype =
        Frame.Fields.make
          ~audio:Content.(default_format Audio.kind)
          ~video:Content.(default_format Video.kind)
          ()
      in
      let result =
        Ffmpeg_stream_description.get_type ~ctype ~url:filename container
      in
      Ffmpeg_stream_description.json_of_container result.container)

let () =
  let base_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let files =
    List.map
      (fun fmt -> Filename.concat base_dir (Test_media_formats.filename fmt))
      Test_media_formats.all
  in
  let descriptions =
    List.filter_map
      (fun filepath ->
        let filename = Filename.basename filepath in
        if Sys.file_exists filepath then
          Some (filename, get_description filepath)
        else (
          Printf.eprintf "Warning: file %s not found, skipping\n%!" filepath;
          None))
      files
  in
  let json : Json.t = `Assoc (List.map (fun (f, d) -> (f, d)) descriptions) in
  print_endline (Json.to_string ~compact:false json)
