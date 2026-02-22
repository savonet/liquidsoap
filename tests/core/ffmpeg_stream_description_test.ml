let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

let rational_to_json (r : Avutil.rational) : Json.t =
  `Assoc [("num", `Int r.Avutil.num); ("den", `Int r.Avutil.den)]

let audio_params_to_json (p : Ffmpeg_stream_description.audio_params) : Json.t =
  `Assoc
    [
      ("type", `String "audio");
      ("codec", `String p.codec_name);
      ("time_base", rational_to_json p.time_base);
      ("samplerate", `Int p.samplerate);
      ("channels", `Int p.channels);
      ("channel_layout", `String p.channel_layout);
    ]

let video_params_to_json (p : Ffmpeg_stream_description.video_params) : Json.t =
  let frame_rate =
    match p.frame_rate with None -> `Null | Some r -> rational_to_json r
  in
  `Assoc
    [
      ("type", `String "video");
      ("codec", `String p.codec_name);
      ("time_base", rational_to_json p.time_base);
      ("width", `Int p.width);
      ("height", `Int p.height);
      ("pixel_format", `String p.pixel_format);
      ("frame_rate", frame_rate);
    ]

let subtitle_params_to_json (p : Ffmpeg_stream_description.subtitle_params) :
    Json.t =
  `Assoc
    [
      ("type", `String "subtitle");
      ("codec", `String p.codec_name);
      ("time_base", rational_to_json p.time_base);
    ]

let data_params_to_json (p : Ffmpeg_stream_description.data_params) : Json.t =
  `Assoc [("type", `String "data"); ("codec", `String p.codec_name)]

let stream_params_to_json = function
  | `Audio p -> audio_params_to_json p
  | `Video p -> video_params_to_json p
  | `Subtitle p -> subtitle_params_to_json p
  | `Data p -> data_params_to_json p

let stream_to_json (s : Ffmpeg_stream_description.stream) : Json.t =
  `Assoc
    [
      ("field", `String (Frame.Fields.string_of_field s.field));
      ("params", stream_params_to_json s.params);
    ]

let json_of_container (c : Ffmpeg_stream_description.container) : Json.t =
  let format = match c.format with None -> `Null | Some f -> `String f in
  `Assoc
    [
      ("format", format); ("streams", `Tuple (List.map stream_to_json c.streams));
    ]

let get_description filename =
  try
    let container = Av.open_input filename in
    Fun.protect
      ~finally:(fun () -> Av.close container)
      (fun () ->
        let c = Ffmpeg_stream_description.container ~url:filename container in
        json_of_container c)
  with exn -> `Assoc [("error", `String (Printexc.to_string exn))]

let () =
  let base_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let files =
    List.map
      (fun fmt -> Filename.concat base_dir (Test_media_formats.filename fmt))
      Test_media_formats.all
  in
  let descriptions =
    List.map
      (fun filepath ->
        let filename = Filename.basename filepath in
        if Sys.file_exists filepath then (filename, get_description filepath)
        else (filename, `Assoc [("error", `String "file not found")]))
      files
  in
  let json : Json.t = `Assoc descriptions in
  print_endline (Json.to_string ~compact:false json)
