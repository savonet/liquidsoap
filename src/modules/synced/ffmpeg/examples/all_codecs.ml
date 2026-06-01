type known_properties =
  [ `Intra_only
  | `Lossy
  | `Fields
  | `Lossless
  | `Reorder
  | `Bitmap_sub
  | `Enhancement
  | `Text_sub ]

let string_of_known_properties = function
  | `Intra_only -> "Intra only"
  | `Lossy -> "Lossy"
  | `Fields -> "Fields"
  | `Lossless -> "Lossless"
  | `Reorder -> "Reorder"
  | `Bitmap_sub -> "Bitmap sub"
  | `Enhancement -> "enhancements"
  | `Text_sub -> "Text sub"

let string_of_properties = function
  | #known_properties as p -> string_of_known_properties p
  | _ -> "unknown property"

type known_media_type =
  [ `Unknown | `Video | `Audio | `Data | `Subtitle | `Attachment ]

let string_of_known_media_type = function
  | `Unknown -> "unknown"
  | `Video -> "video"
  | `Audio -> "audio"
  | `Data -> "data"
  | `Subtitle -> "subtitle"
  | `Attachment -> "attachment"

let string_of_media_type = function
  | #known_media_type as p -> string_of_known_media_type p
  | _ -> "unknown media type"

let print_descriptor = function
  | None -> "(none)\n"
  | Some
      { Avcodec.media_type; name; long_name; mime_types; properties; profiles }
    ->
      Printf.sprintf
        "\n\
        \  Name: %s\n\
        \  Media type: %s\n\
        \  Long name: %s\n\
        \  Mime types: %s\n\
        \  Properties: %s\n\
        \  Profiles:%s\n"
        name
        (string_of_media_type (media_type :> known_media_type))
        (Option.value ~default:"(none)" long_name)
        (String.concat ", " mime_types)
        (String.concat ", "
           (List.map string_of_properties (properties :> known_properties list)))
        (String.concat ""
           (List.map
              (fun { Avcodec.id; profile_name } ->
                Printf.sprintf "\n    ID: %i, name: %s" id profile_name)
              profiles))

let () =
  Printf.printf "====== Audio ======\n%!";
  List.iter
    (fun id ->
      Printf.printf "Available audio codec: %s\nDescriptor:%s\n"
        (Avcodec.Audio.string_of_id id)
        (print_descriptor (Avcodec.Audio.descriptor id)))
    Avcodec.Audio.codec_ids;

  List.iter
    (fun c ->
      Printf.printf "Available audio encoder: %s - %s\n%!"
        (Avcodec.Audio.get_name c)
        (Avcodec.Audio.get_description c))
    Avcodec.Audio.encoders;

  List.iter
    (fun c ->
      Printf.printf "Available audio decoder: %s - %s\n%!"
        (Avcodec.Audio.get_name c)
        (Avcodec.Audio.get_description c))
    Avcodec.Audio.decoders;

  Printf.printf "\n\n";

  Printf.printf "====== Video ======\n%!";

  List.iter
    (fun id ->
      Printf.printf "Available video codec: %s\nDescriptor:%s\n"
        (Avcodec.Video.string_of_id id)
        (print_descriptor (Avcodec.Video.descriptor id)))
    Avcodec.Video.codec_ids;

  let video_codec_descr c =
    let supported_pixel_formats = Avcodec.Video.get_supported_pixel_formats c in
    let supported_color_spaces = Avcodec.Video.get_supported_color_spaces c in
    Printf.sprintf
      "  Supported pixel_formats: %s\n  Supported color spaces: %s\n"
      (String.concat ", "
         (List.map
            (fun f -> Avutil.Pixel_format.((descriptor f).name))
            supported_pixel_formats))
      (String.concat ", "
         (List.map Avutil.Color_space.name supported_color_spaces))
  in

  List.iter
    (fun c ->
      Printf.printf "Available video encoder: %s - %s\n%s%!"
        (Avcodec.Video.get_name c)
        (Avcodec.Video.get_description c)
        (video_codec_descr c))
    Avcodec.Video.encoders;

  List.iter
    (fun c ->
      Printf.printf "Available video decoder: %s - %s\n%s%!"
        (Avcodec.Video.get_name c)
        (Avcodec.Video.get_description c)
        (video_codec_descr c))
    Avcodec.Video.decoders;

  Printf.printf "\n\n";

  Printf.printf "====== Subtitle ======\n%!";

  List.iter
    (fun id ->
      Printf.printf "Available subtitle codec: %s\nDescriptor:%s\n"
        (Avcodec.Subtitle.string_of_id id)
        (print_descriptor (Avcodec.Subtitle.descriptor id)))
    Avcodec.Subtitle.codec_ids;

  List.iter
    (fun c ->
      Printf.printf "Available subtitle encoder: %s - %s\n%!"
        (Avcodec.Subtitle.get_name c)
        (Avcodec.Subtitle.get_description c))
    Avcodec.Subtitle.encoders;

  List.iter
    (fun c ->
      Printf.printf "Available subtitle decoder: %s - %s\n%!"
        (Avcodec.Subtitle.get_name c)
        (Avcodec.Subtitle.get_description c))
    Avcodec.Subtitle.decoders
