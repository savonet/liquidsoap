external polymorphic_variant_string_to_c_value : string -> int64
  = "polymorphic_variant_string_to_c_value"

let c_flags =
  let len = Array.length Sys.argv in
  if len > 4 then
    Array.to_list (Array.sub Sys.argv 4 (Array.length Sys.argv - 4))
  else []

let include_paths =
  List.fold_left
    (fun cur flag ->
      let len = String.length flag in
      if len > 2 && String.sub flag 0 2 = "-I" then (
        let path = String.sub flag 2 (len - 2) in
        if not (List.mem path cur) then path :: cur else cur)
      else cur)
    Config.paths c_flags

let if_d d fn = match d with None -> () | Some d -> fn d

let print_define_polymorphic_variant_value oc pv =
  let value = Int64.to_string (polymorphic_variant_string_to_c_value pv) in
  output_string oc ("#define PVV_" ^ pv ^ " (" ^ value ^ ")\n")

let rec find_start_line lines line_re =
  match lines with
    | line :: lines when Str.string_match line_re line 0 -> (true, lines)
    | _ :: lines -> find_start_line lines line_re
    | [] -> (false, [])

exception Found of string

let get_path filenames =
  try
    List.iter
      (fun filename ->
        List.iter
          (fun path ->
            let p = path ^ filename in
            if Sys.file_exists p then raise (Found p))
          include_paths)
      filenames;
    None
  with Found p -> Some p

let rec id_to_pv_value id values =
  let id = if id.[0] >= '0' && id.[0] <= '9' then "_" ^ id else id in
  let id =
    String.(
      uppercase_ascii (sub id 0 1) ^ lowercase_ascii (sub id 1 (length id - 1)))
  in
  let value = polymorphic_variant_string_to_c_value id in

  if List.mem value values then id_to_pv_value (id ^ "_") values else (id, value)

let translate_enum_lines ?h_oc ?ml_oc lines labels =
  let ( start_pat,
        pat,
        end_pat,
        enum_prefix,
        c_type_name,
        c_fun_radix,
        ml_type_name,
        extra_entries ) =
    labels
  in

  let start_re = Str.regexp start_pat in
  let re = Str.regexp pat in
  let end_re = Str.regexp end_pat in

  let print_c words =
    if_d h_oc (fun oc -> output_string oc (String.concat "" words ^ "\n"))
  in

  let print_ml words =
    let line = String.concat "" words ^ "\n" in
    if_d ml_oc (fun oc -> output_string oc line)
  in

  let print_entry ~values id =
    let pv, value = id_to_pv_value id values in

    print_c ["  {("; Int64.to_string value; "), "; enum_prefix; id; "},"];
    print_ml ["  | `"; pv];
    (pv, value)
  in

  let rec loop lines values pvs =
    match lines with
      | line :: _ when end_pat <> "" && Str.string_match end_re line 0 ->
          (values, pvs)
      | line :: lines when Str.string_match re line 0 ->
          let id = Str.matched_group 1 line in
          let pv, value = print_entry ~values id in
          loop lines (value :: values) (pv :: pvs)
      | _ :: lines -> loop lines values pvs
      | [] -> (values, pvs)
  in

  let has_start_line, lines = find_start_line lines start_re in

  if start_pat = "" || has_start_line then (
    let tab_name = enum_prefix ^ String.uppercase_ascii ml_type_name ^ "_TAB" in
    let tab_len = tab_name ^ "_LEN" in

    print_c ["static const int64_t "; tab_name; "[][2] = {"];

    print_ml ["type "; ml_type_name; " = ["];

    let pvs, values =
      List.fold_left
        (fun (pvs, values) extra ->
          let pv, value = print_entry ~values extra in
          (pv :: pvs, value :: values))
        ([], []) extra_entries
    in
    let values, pvs = loop lines values pvs in

    print_ml ["]\n"];

    print_ml ["let "; ml_type_name; ": "; ml_type_name; " list  = ["];

    List.iter (fun pv -> print_ml ["`"; pv; ";"]) pvs;

    print_ml ["]\n"];

    print_c ["};\n\n#define "; tab_len; " "; string_of_int (List.length values)];

    print_c
      [
        c_type_name;
        " ";
        c_fun_radix;
        "_val(value v){\nint i;\nfor(i=0;i<";
        tab_len;
        ";i++){\nif(v==";
        tab_name;
        "[i][0])return ";
        tab_name;
        "[i][1];\n}\nFail(\"Could not find C value for %\" PRIu64 \" in "
        ^ tab_name
        ^ ". Do you need to recompile the ffmpeg binding?\", (uint64_t)v);\n\
           return -1;\n\
           }";
      ];

    print_c
      [
        c_type_name;
        " ";
        c_fun_radix;
        "_val_no_raise(value v){\nint i;\nfor(i=0;i<";
        tab_len;
        ";i++){\nif(v==";
        tab_name;
        "[i][0])return ";
        tab_name;
        "[i][1];\n}\nreturn VALUE_NOT_FOUND;\n}";
      ];

    print_c
      [
        "value Val_";
        c_fun_radix;
        "(";
        c_type_name;
        " t){\nint i;\nfor(i=0;i<";
        tab_len;
        "; i++){\nif(t==";
        tab_name;
        "[i][1])return ";
        tab_name;
        "[i][0];\n}\nFail(\"Could not find OCaml value for %\" PRIu64 \" in "
        ^ tab_name
        ^ ". Do you need to recompile the ffmpeg binding?\", (uint64_t)t);\n\
           return -1;\n\
           }";
      ])

let translate_c_values_opt ?h_oc ?ml_oc ~pre_process in_names enums_labels =
  match get_path in_names with
    | None ->
        Printf.eprintf "WARNING : None of the header files [%s] where found\n"
          (String.concat "; " (List.map (Printf.sprintf "%S") in_names))
    | Some path ->
        let lines =
          let rec read lines ic =
            try read (input_line ic :: lines) ic
            with End_of_file -> List.rev lines
          in
          let read ic close =
            let lines = read [] ic in
            close ic;
            lines
          in
          let cat path =
            let ic = open_in path in
            read ic close_in
          in
          if pre_process then (
            try
              let path = Filename.quote path in
              let c_flags = String.concat " " c_flags in
              let cc = Sys.argv.(1) in
              let cmd = Printf.sprintf "%s -E %s %s" cc c_flags path in
              let ic = Unix.open_process_in cmd in
              let close ic =
                assert (Unix.close_process_in ic = Unix.WEXITED 0)
              in
              read ic close
            with _ -> cat path)
          else cat path
        in

        if_d h_oc (fun oc ->
            output_string oc "#include \"avutil_stubs.h\"\n";
            output_string oc "#include <inttypes.h>\n\n";
            output_string oc "#define VALUE_NOT_FOUND 0xFFFFFFF\n\n");

        List.iter
          (fun labels -> translate_enum_lines lines labels ?h_oc ?ml_oc)
          enums_labels

let translate_c_values ~pre_process in_names out_name enums_labels = function
  | "ml" ->
      let ml_oc = open_out (out_name ^ ".ml") in
      translate_c_values_opt ~ml_oc ~pre_process in_names enums_labels;
      close_out ml_oc
  | "h" ->
      let h_oc = open_out (out_name ^ "_stubs.h") in
      translate_c_values_opt ~h_oc ~pre_process in_names enums_labels;
      close_out h_oc
  | _ -> assert false

let gen_polymorphic_variant_h () =
  let pvv_oc_h = open_out "polymorphic_variant_values_stubs.h" in

  List.iter
    (print_define_polymorphic_variant_value pvv_oc_h)
    [
      "Audio";
      "Audio_frame";
      "Audio_packet";
      "Video";
      "Video_frame";
      "Video_packet";
      "Subtitle";
      "Subtitle_frame";
      "Subtitle_packet";
      "Data";
      "Data_packet";
      "Attachment";
      "Nb";
      "Packet";
      "Frame";
      "Ok";
      "Again";
      "Second";
      "Millisecond";
      "Microsecond";
      "Nanosecond";
      "Buffer";
      "Link";
      "Sink";
      (* Avfilter flags *)
      "Dynamic_inputs";
      "Dynamic_outputs";
      "Slice_threads";
      "Support_timeline_generic";
      "Support_timeline_internal";
      (* Avpacket flags. *)
      "Keyframe";
      "Corrupt";
      "Discard";
      "Trusted";
      "Disposable";
      (* Avpacket sidedata types. *)
      "Replaygain";
      "Strings_metadata";
      "Metadata_update";
      (* Options *)
      "Constant";
      "Flags";
      "Int";
      "Int64";
      "Float";
      "Double";
      "String";
      "Rational";
      "Binary";
      "Dict";
      "UInt64";
      "Image_size";
      "Pixel_fmt";
      "Sample_fmt";
      "Video_rate";
      "Duration";
      "Color";
      "Channel_layout";
      "Bool";
      "Array";
      "Encoding_param";
      "Decoding_param";
      "Audio_param";
      "Video_param";
      "Subtitle_param";
      "Export";
      "Readonly";
      "Bsf_param";
      "Runtime_param";
      "Filtering_param";
      "Deprecated";
      "Child_consts";
      (* Subtitle flags *)
      "Forced";
      (* Errors *)
      "Bsf_not_found";
      "Decoder_not_found";
      "Demuxer_not_found";
      "Encoder_not_found";
      "Eof";
      "Exit";
      "Filter_not_found";
      "Invalid_data";
      "Muxer_not_found";
      "Option_not_found";
      "Patch_welcome";
      "Protocol_not_found";
      "Stream_not_found";
      "Bug";
      "Eagain";
      "Unknown";
      "Experimental";
      "Other";
      "Failure";
    ];

  close_out pvv_oc_h

let gen_polymorphic_variant = function
  | "h" -> gen_polymorphic_variant_h ()
  | _ -> assert false

let gen_codec_id mode =
  (* translate_c_values parameters : *)
  (* in_name out_name title (start_pat, pat, end_pat, enum_prefix, c_type_name, c_fun_radix, ml_type_name) *)
  translate_c_values ~pre_process:true
    ["/libavcodec/codec_id.h"; "/libavcodec/avcodec.h"]
    "codec_id"
    [
      ( "[ \t]*AV_CODEC_ID_NONE",
        "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_CODEC_ID_FIRST_AUDIO",
        "AV_CODEC_ID_",
        "enum AVCodecID",
        "VideoCodecID",
        "video",
        ["WRAPPED_AVFRAME"; "NONE"] );
      ( "[ \t]*AV_CODEC_ID_FIRST_AUDIO",
        "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_CODEC_ID_FIRST_SUBTITLE",
        "AV_CODEC_ID_",
        "enum AVCodecID",
        "AudioCodecID",
        "audio",
        ["WRAPPED_AVFRAME"; "NONE"] );
      ( "[ \t]*AV_CODEC_ID_FIRST_SUBTITLE",
        "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_CODEC_ID_FIRST_UNKNOWN",
        "AV_CODEC_ID_",
        "enum AVCodecID",
        "SubtitleCodecID",
        "subtitle",
        ["NONE"] );
      ( "[ \t]*AV_CODEC_ID_FIRST_UNKNOWN",
        "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)",
        "",
        "AV_CODEC_ID_",
        "enum AVCodecID",
        "UnknownCodecID",
        "unknown",
        ["NONE"] );
      ( "[ \t]*AV_CODEC_ID_NONE",
        "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)",
        "",
        "AV_CODEC_ID_",
        "enum AVCodecID",
        "CodecID",
        "codec_id",
        ["NONE"] );
    ]
    mode

let gen_pixel_format mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "pixel_format"
    [
      ( "enum AVPixelFormat",
        "[ \t]*AV_PIX_FMT_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_PIX_FMT_NB",
        "AV_PIX_FMT_",
        "enum AVPixelFormat",
        "PixelFormat",
        "t",
        [] );
    ]
    mode

let gen_color_space mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "color_space"
    [
      ( "enum AVColorSpace",
        "[ \t]*AVCOL_SPC_\\([A-Z0-9_]+\\)",
        "[ \t]*AVCOL_SPC_NB",
        "AVCOL_SPC_",
        "enum AVColorSpace",
        "ColorSpace",
        "t",
        [] );
    ]
    mode

let gen_color_range mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "color_range"
    [
      ( "enum AVColorRange",
        "[ \t]*AVCOL_RANGE_\\([A-Z0-9_]+\\)",
        "[ \t]*AVCOL_RANGE_NB",
        "AVCOL_RANGE_",
        "enum AVColorRange",
        "ColorRange",
        "t",
        [] );
    ]
    mode

let gen_color_primaries mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "color_primaries"
    [
      ( "enum AVColorPrimaries",
        "[ \t]*AVCOL_PRI_\\([A-Z0-9_]+\\)",
        "[ \t]*AVCOL_PRI_NB",
        "AVCOL_PRI_",
        "enum AVColorPrimaries",
        "ColorPrimaries",
        "t",
        [] );
    ]
    mode

let gen_color_trc mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "color_trc"
    [
      ( "enum AVColorTransferCharacteristic",
        "[ \t]*AVCOL_TRC_\\([A-Z0-9_]+\\)",
        "[ \t]*AVCOL_TRC_NB",
        "AVCOL_TRC_",
        "enum AVColorTransferCharacteristic",
        "ColorTrc",
        "t",
        [] );
    ]
    mode

let gen_chroma_location mode =
  translate_c_values ~pre_process:true ["/libavutil/pixfmt.h"] "chroma_location"
    [
      ( "enum AVChromaLocation",
        "[ \t]*AVCHROMA_LOC_\\([A-Z0-9_]+\\)",
        "[ \t]*AVCHROMA_LOC_NB",
        "AVCHROMA_LOC_",
        "enum AVChromaLocation",
        "ChromaLocation",
        "t",
        [] );
    ]
    mode

let gen_pixel_format_flag mode =
  translate_c_values ~pre_process:false ["/libavutil/pixdesc.h"]
    "pixel_format_flag"
    [
      ( "",
        "#define AV_PIX_FMT_FLAG_\\([A-Z0-9_]+\\)",
        "",
        "AV_PIX_FMT_FLAG_",
        "uint64_t",
        "PixelFormatFlag",
        "t",
        [] );
    ]
    mode

let gen_hw_config_method mode =
  translate_c_values ~pre_process:true ["/libavcodec/avcodec.h"]
    "hw_config_method"
    [
      ( "",
        "[ \t]*AV_CODEC_HW_CONFIG_METHOD_\\([A-Z0-9_]+\\)",
        "",
        "AV_CODEC_HW_CONFIG_METHOD_",
        "uint64_t",
        "HwConfigMethod",
        "t",
        [] );
    ]
    mode

let gen_hw_device_type mode =
  translate_c_values ~pre_process:true ["/libavutil/hwcontext.h"]
    "hw_device_type"
    [
      ( "enum AVHWDeviceType",
        "[ \t]*AV_HWDEVICE_TYPE_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_HWDEVICE_TYPE_NONE ",
        "AV_HWDEVICE_TYPE_",
        "enum AVHWDeviceType",
        "HwDeviceType",
        "t",
        [] );
    ]
    mode

let gen_channel_layout mode =
  translate_c_values ~pre_process:false
    ["/libavutil/channel_layout.h"]
    "channel_layout"
    [
      ( "",
        "#define AV_CH_LAYOUT_\\([A-Z0-9_]+\\)",
        "",
        "AV_CH_LAYOUT_",
        "uint64_t",
        "ChannelLayout",
        "t",
        [] );
    ]
    mode

let gen_codec_capabilities mode =
  translate_c_values ~pre_process:false
    ["/libavcodec/codec.h"; "/libavcodec/avcodec.h"]
    "codec_capabilities"
    [
      ( "",
        "#define AV_CODEC_CAP_\\([A-Z0-9_]+\\)",
        "",
        "AV_CODEC_CAP_",
        "uint64_t",
        "CodecCapabilities",
        "t",
        [] );
    ]
    mode

let gen_codec_properties mode =
  translate_c_values ~pre_process:false
    ["/libavcodec/codec_desc.h"; "/libavcodec/avcodec.h"]
    "codec_properties"
    [
      ( "",
        "#define AV_CODEC_PROP_\\([A-Z0-9_]+\\)",
        "",
        "AV_CODEC_PROP_",
        "uint64_t",
        "CodecProperties",
        "t",
        [] );
    ]
    mode

let gen_media_types mode =
  translate_c_values ~pre_process:false
    ["/libavutil/avutil.h"; "/libavutil/avutil.h"]
    "media_types"
    [
      ( "enum AVMediaType",
        "[ \t]*AVMEDIA_TYPE_\\([A-Z0-9_]+\\)",
        "[ \t]*AVMEDIA_TYPE_NB",
        "AVMEDIA_TYPE_",
        "uint64_t",
        "MediaTypes",
        "t",
        [] );
    ]
    mode

let gen_sample_format mode =
  translate_c_values ~pre_process:true ["/libavutil/samplefmt.h"]
    "sample_format"
    [
      ( "enum AVSampleFormat",
        "[ \t]*AV_SAMPLE_FMT_\\([A-Z0-9_]+\\)",
        "[ \t]*AV_SAMPLE_FMT_NB",
        "AV_SAMPLE_FMT_",
        "enum AVSampleFormat",
        "SampleFormat",
        "t",
        [] );
    ]
    mode

let gen_subtitle_type mode =
  translate_c_values ~pre_process:true ["/libavcodec/avcodec.h"] "subtitle_type"
    [
      ( "enum AVSubtitleType",
        "[ \t]*SUBTITLE_\\([A-Z0-9_]+\\)",
        "\\};",
        "SUBTITLE_",
        "enum AVSubtitleType",
        "SubtitleType",
        "t",
        [] );
    ]
    mode

let gen_subtitle_flag mode =
  translate_c_values ~pre_process:false ["/libavcodec/avcodec.h"]
    "subtitle_flag"
    [
      ( "",
        "#define AV_SUBTITLE_FLAG_\\([A-Z0-9_]+\\)",
        "",
        "AV_SUBTITLE_FLAG_",
        "int",
        "SubtitleFlag",
        "t",
        [] );
    ]
    mode

let gen_swresample_options mode =
  translate_c_values ~pre_process:true
    ["/libswresample/swresample.h"]
    "swresample_options"
    [
      ( "[ \t]*SWR_DITHER_NONE",
        "[ \t]*SWR_\\([A-Z0-9_]+\\)",
        "[ \t]*SWR_DITHER_NS",
        "SWR_",
        "enum SwrDitherType",
        "DitherType",
        "dither_type",
        [] );
      ( "enum SwrEngine",
        "[ \t]*SWR_\\([A-Z0-9_]+\\)",
        "[ \t]*SWR_ENGINE_NB",
        "SWR_",
        "enum SwrEngine",
        "Engine",
        "engine",
        [] );
      ( "enum SwrFilterType",
        "[ \t]*SWR_\\([A-Z0-9_]+\\)",
        "\\};",
        "SWR_",
        "enum SwrFilterType",
        "FilterType",
        "filter_type",
        [] );
    ]
    mode

let () =
  let mode = Sys.argv.(3) in
  match Sys.argv.(2) with
    | "polymorphic_variant" -> gen_polymorphic_variant mode
    | "codec_id" -> gen_codec_id mode
    | "color_space" -> gen_color_space mode
    | "color_range" -> gen_color_range mode
    | "color_primaries" -> gen_color_primaries mode
    | "color_trc" -> gen_color_trc mode
    | "chroma_location" -> gen_chroma_location mode
    | "pixel_format" -> gen_pixel_format mode
    | "pixel_format_flag" -> gen_pixel_format_flag mode
    | "hw_config_method" -> gen_hw_config_method mode
    | "hw_device_type" -> gen_hw_device_type mode
    | "channel_layout" -> gen_channel_layout mode
    | "sample_format" -> gen_sample_format mode
    | "swresample_options" -> gen_swresample_options mode
    | "codec_capabilities" -> gen_codec_capabilities mode
    | "codec_properties" -> gen_codec_properties mode
    | "media_types" -> gen_media_types mode
    | "subtitle_type" -> gen_subtitle_type mode
    | "subtitle_flag" -> gen_subtitle_flag mode
    | _ -> assert false
