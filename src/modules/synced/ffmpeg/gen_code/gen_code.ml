external polymorphic_variant_string_to_c_value : string -> int64
  = "polymorphic_variant_string_to_c_value"

let c_flags =
  let len = Array.length Sys.argv in
  if len > 4 then
    Array.to_list (Array.sub Sys.argv 4 (Array.length Sys.argv - 4))
  else []

let include_paths =
  let paths =
    List.fold_left
      (fun cur flag ->
        let len = String.length flag in
        if len > 2 && String.sub flag 0 2 = "-I" then (
          let path = String.sub flag 2 (len - 2) in
          if not (List.mem path cur) then path :: cur else cur)
        else cur)
      Config.paths c_flags
  in
  paths

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

(* One block of constants scanned out of one header. *)
type enum_spec = {
  start_pat : string; (* where to start scanning; "" means the top *)
  pat : string; (* matches one member, capturing its name *)
  end_pat : string; (* where to stop; "" means the end *)
  enum_prefix : string; (* C member prefix, e.g. "AVCOL_SPC_" *)
  c_type_name : string; (* type of the C value *)
  c_fun_radix : string; (* Xxx in Xxx_val / Val_Xxx *)
  ml_type_name : string;
  extra_entries : string list; (* members to inject before scanning *)
}

let translate_enum_lines ?h_oc ?ml_oc lines labels =
  let {
    start_pat;
    pat;
    end_pat;
    enum_prefix;
    c_type_name;
    c_fun_radix;
    ml_type_name;
    extra_entries;
  } =
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
        (* The table is int64_t; c_type_name may be unsigned. *)
        "; i++){\nif((int64_t)t==";
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

(* One generator = one name passed as argv(2) = one pair of output files. *)
type generator = {
  name : string;
  headers : string list;
  pre_process : bool;
  enums : enum_spec list;
}

let enum ?(start = "") ?(stop = "") ?(ml_name = "t") ?(extra = []) ~prefix
    ~c_type ~radix () =
  {
    start_pat = start;
    pat = "[ \t]*" ^ prefix ^ "\\([A-Z0-9_]+\\)";
    end_pat = stop;
    enum_prefix = prefix;
    c_type_name = c_type;
    c_fun_radix = radix;
    ml_type_name = ml_name;
    extra_entries = extra;
  }

(* A family of #define'd flags rather than a C enum: no delimiters, and the
   member pattern is anchored on the #define. *)
let flags ?(ml_name = "t") ~prefix ~c_type ~radix () =
  {
    start_pat = "";
    pat = "#define " ^ prefix ^ "\\([A-Z0-9_]+\\)";
    end_pat = "";
    enum_prefix = prefix;
    c_type_name = c_type;
    c_fun_radix = radix;
    ml_type_name = ml_name;
    extra_entries = [];
  }

let pixfmt = ["/libavutil/pixfmt.h"]
let avcodec_h = ["/libavcodec/avcodec.h"]

(* Every codec id range shares a header, prefix and C type; they differ only
   in which slice of the enum they cover. *)
let codec_id_range ~start ~stop ~radix ~ml_name ~extra =
  enum ~start ~stop ~prefix:"AV_CODEC_ID_" ~c_type:"enum AVCodecID" ~radix
    ~ml_name ~extra ()

let generators =
  [
    {
      name = "codec_id";
      headers = ["/libavcodec/codec_id.h"; "/libavcodec/avcodec.h"];
      pre_process = true;
      enums =
        [
          codec_id_range ~start:"[ \t]*AV_CODEC_ID_NONE"
            ~stop:"[ \t]*AV_CODEC_ID_FIRST_AUDIO" ~radix:"VideoCodecID"
            ~ml_name:"video"
            ~extra:["WRAPPED_AVFRAME"; "NONE"];
          codec_id_range ~start:"[ \t]*AV_CODEC_ID_FIRST_AUDIO"
            ~stop:"[ \t]*AV_CODEC_ID_FIRST_SUBTITLE" ~radix:"AudioCodecID"
            ~ml_name:"audio"
            ~extra:["WRAPPED_AVFRAME"; "NONE"];
          codec_id_range ~start:"[ \t]*AV_CODEC_ID_FIRST_SUBTITLE"
            ~stop:"[ \t]*AV_CODEC_ID_FIRST_UNKNOWN" ~radix:"SubtitleCodecID"
            ~ml_name:"subtitle" ~extra:["NONE"];
          codec_id_range ~start:"[ \t]*AV_CODEC_ID_FIRST_UNKNOWN" ~stop:""
            ~radix:"UnknownCodecID" ~ml_name:"unknown" ~extra:["NONE"];
          codec_id_range ~start:"[ \t]*AV_CODEC_ID_NONE" ~stop:""
            ~radix:"CodecID" ~ml_name:"codec_id" ~extra:["NONE"];
        ];
    };
    {
      name = "pixel_format";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVPixelFormat" ~stop:"[ \t]*AV_PIX_FMT_NB"
            ~prefix:"AV_PIX_FMT_" ~c_type:"enum AVPixelFormat"
            ~radix:"PixelFormat" ();
        ];
    };
    {
      name = "color_space";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVColorSpace" ~stop:"[ \t]*AVCOL_SPC_NB"
            ~prefix:"AVCOL_SPC_" ~c_type:"enum AVColorSpace" ~radix:"ColorSpace"
            ();
        ];
    };
    {
      name = "color_range";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVColorRange" ~stop:"[ \t]*AVCOL_RANGE_NB"
            ~prefix:"AVCOL_RANGE_" ~c_type:"enum AVColorRange"
            ~radix:"ColorRange" ();
        ];
    };
    {
      name = "color_primaries";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVColorPrimaries" ~stop:"[ \t]*AVCOL_PRI_NB"
            ~prefix:"AVCOL_PRI_" ~c_type:"enum AVColorPrimaries"
            ~radix:"ColorPrimaries" ();
        ];
    };
    {
      name = "color_trc";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVColorTransferCharacteristic"
            ~stop:"[ \t]*AVCOL_TRC_NB" ~prefix:"AVCOL_TRC_"
            ~c_type:"enum AVColorTransferCharacteristic" ~radix:"ColorTrc" ();
        ];
    };
    {
      name = "chroma_location";
      headers = pixfmt;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVChromaLocation" ~stop:"[ \t]*AVCHROMA_LOC_NB"
            ~prefix:"AVCHROMA_LOC_" ~c_type:"enum AVChromaLocation"
            ~radix:"ChromaLocation" ();
        ];
    };
    {
      name = "hw_device_type";
      headers = ["/libavutil/hwcontext.h"];
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVHWDeviceType" ~stop:"[ \t]*AV_HWDEVICE_TYPE_NONE "
            ~prefix:"AV_HWDEVICE_TYPE_" ~c_type:"enum AVHWDeviceType"
            ~radix:"HwDeviceType" ();
        ];
    };
    {
      name = "sample_format";
      headers = ["/libavutil/samplefmt.h"];
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVSampleFormat" ~stop:"[ \t]*AV_SAMPLE_FMT_NB"
            ~prefix:"AV_SAMPLE_FMT_" ~c_type:"enum AVSampleFormat"
            ~radix:"SampleFormat" ();
        ];
    };
    {
      name = "subtitle_type";
      headers = avcodec_h;
      pre_process = true;
      enums =
        [
          enum ~start:"enum AVSubtitleType" ~stop:"\\};" ~prefix:"SUBTITLE_"
            ~c_type:"enum AVSubtitleType" ~radix:"SubtitleType" ();
        ];
    };
    {
      name = "media_types";
      headers = ["/libavutil/avutil.h"; "/libavutil/avutil.h"];
      pre_process = false;
      enums =
        [
          enum ~start:"enum AVMediaType" ~stop:"[ \t]*AVMEDIA_TYPE_NB"
            ~prefix:"AVMEDIA_TYPE_" ~c_type:"uint64_t" ~radix:"MediaTypes" ();
        ];
    };
    {
      name = "hw_config_method";
      headers = avcodec_h;
      pre_process = true;
      enums =
        [
          enum ~prefix:"AV_CODEC_HW_CONFIG_METHOD_" ~c_type:"uint64_t"
            ~radix:"HwConfigMethod" ();
        ];
    };
    {
      name = "pixel_format_flag";
      headers = ["/libavutil/pixdesc.h"];
      pre_process = false;
      enums =
        [
          flags ~prefix:"AV_PIX_FMT_FLAG_" ~c_type:"uint64_t"
            ~radix:"PixelFormatFlag" ();
        ];
    };
    {
      name = "channel_layout";
      headers = ["/libavutil/channel_layout.h"];
      pre_process = false;
      enums =
        [
          flags ~prefix:"AV_CH_LAYOUT_" ~c_type:"uint64_t"
            ~radix:"ChannelLayout" ();
        ];
    };
    {
      name = "codec_capabilities";
      headers = ["/libavcodec/codec.h"; "/libavcodec/avcodec.h"];
      pre_process = false;
      enums =
        [
          flags ~prefix:"AV_CODEC_CAP_" ~c_type:"uint64_t"
            ~radix:"CodecCapabilities" ();
        ];
    };
    {
      name = "codec_properties";
      headers = ["/libavcodec/codec_desc.h"; "/libavcodec/avcodec.h"];
      pre_process = false;
      enums =
        [
          flags ~prefix:"AV_CODEC_PROP_" ~c_type:"uint64_t"
            ~radix:"CodecProperties" ();
        ];
    };
    {
      name = "subtitle_flag";
      headers = avcodec_h;
      pre_process = false;
      enums =
        [
          flags ~prefix:"AV_SUBTITLE_FLAG_" ~c_type:"int" ~radix:"SubtitleFlag"
            ();
        ];
    };
    {
      name = "swresample_options";
      headers = ["/libswresample/swresample.h"];
      pre_process = true;
      enums =
        [
          enum ~start:"[ \t]*SWR_DITHER_NONE" ~stop:"[ \t]*SWR_DITHER_NS"
            ~prefix:"SWR_" ~c_type:"enum SwrDitherType" ~radix:"DitherType"
            ~ml_name:"dither_type" ();
          enum ~start:"enum SwrEngine" ~stop:"[ \t]*SWR_ENGINE_NB"
            ~prefix:"SWR_" ~c_type:"enum SwrEngine" ~radix:"Engine"
            ~ml_name:"engine" ();
          enum ~start:"enum SwrFilterType" ~stop:"\\};" ~prefix:"SWR_"
            ~c_type:"enum SwrFilterType" ~radix:"FilterType"
            ~ml_name:"filter_type" ();
        ];
    };
  ]

let () =
  let mode = Sys.argv.(3) in
  match Sys.argv.(2) with
    | "polymorphic_variant" -> gen_polymorphic_variant mode
    | name -> (
        match List.find_opt (fun g -> g.name = name) generators with
          | Some g ->
              translate_c_values ~pre_process:g.pre_process g.headers g.name
                g.enums mode
          | None -> failwith ("gen_code: unknown generator " ^ name))
