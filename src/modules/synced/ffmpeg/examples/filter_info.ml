let string_of_spec to_string { Avutil.Options.default; min; max; values } =
  let opt_str = function None -> "none" | Some v -> to_string v in
  Printf.sprintf "{default: %s, min: %s, max: %s, values: %s}" (opt_str default)
    (opt_str min) (opt_str max)
    (Printf.sprintf "[%s]"
       (String.concat ", "
          (List.map
             (fun (name, v) -> Printf.sprintf "%s: %s" name (to_string v))
             values)))

let string_of_flags flags =
  let string_of_flag = function
    | `Encoding_param -> "encoding param"
    | `Decoding_param -> "decoding param"
    | `Audio_param -> "audio param"
    | `Video_param -> "video param"
    | `Subtitle_param -> "subtitle param"
    | `Export -> "export"
    | `Readonly -> "readonly"
    | `Bsf_param -> "bsf param"
    | `Runtime_param -> "runtime param"
    | `Filtering_param -> "filtering param"
    | `Deprecated -> "deprecated"
    | `Child_consts -> "child constants"
  in
  String.concat ", " (List.map string_of_flag flags)

let string_of_ground : Avutil.Options.ground -> string * string = function
  | `Flags entry -> ("flags", string_of_spec Int64.to_string entry)
  | `Int entry -> ("int", string_of_spec string_of_int entry)
  | `Int64 entry -> ("int64", string_of_spec Int64.to_string entry)
  | `Float entry -> ("float", string_of_spec string_of_float entry)
  | `Double entry -> ("double", string_of_spec string_of_float entry)
  | `String entry -> ("string", string_of_spec (fun v -> v) entry)
  | `Rational entry ->
      ( "rational",
        string_of_spec
          (fun { Avutil.num; den } -> Printf.sprintf "%d/%d" num den)
          entry )
  | `Binary entry -> ("binary", string_of_spec (fun v -> v) entry)
  | `Dict entry -> ("dict", string_of_spec (fun v -> v) entry)
  | `UInt64 entry -> ("uint64", string_of_spec Int64.to_string entry)
  | `Image_size entry -> ("image_size", string_of_spec (fun v -> v) entry)
  | `Pixel_fmt entry ->
      ( "pixel_fmt",
        string_of_spec
          (fun p ->
            match Avutil.Pixel_format.to_string p with
              | None -> "none"
              | Some f -> f)
          entry )
  | `Sample_fmt entry ->
      ( "sample_fmt",
        string_of_spec
          (fun f ->
            match Avutil.Sample_format.get_name f with
              | None -> "none"
              | Some f -> f)
          entry )
  | `Video_rate entry -> ("video_rate", string_of_spec (fun v -> v) entry)
  | `Duration entry -> ("duration", string_of_spec Int64.to_string entry)
  | `Color entry -> ("color", string_of_spec (fun v -> v) entry)
  | `Channel_layout entry ->
      ( "channel_layout",
        string_of_spec Avutil.Channel_layout.get_description entry )
  | `Bool entry -> ("bool", string_of_spec string_of_bool entry)

let string_of_option { Avutil.Options.name; help; flags; spec } =
  let _type, spec =
    match spec with
      | #Avutil.Options.ground as g -> string_of_ground g
      | `Array g ->
          let inner_type, inner_spec = string_of_ground g in
          (Printf.sprintf "array<%s>" inner_type, inner_spec)
  in

  Printf.sprintf
    "- %s:\n    type: %s\n    help: %s\n    flags: %s\n    spec: %s" name _type
    (match help with None -> "none" | Some v -> v)
    (string_of_flags flags) spec

let string_of_filter_flag = function
  | `Dynamic_inputs -> "dynamic inputs"
  | `Dynamic_outputs -> "dynamic outputs"
  | `Slice_threads -> "slice threads"
  | `Support_timeline_generic -> "support timeline generic"
  | `Support_timeline_internal -> "support timeline internal"

let string_of_pads pads =
  let string_of_pad pad = Printf.sprintf "%s" (Avfilter.pad_name pad) in
  String.concat ", " (List.map string_of_pad pads)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <filter_name>\n" Sys.argv.(0);
    exit 1);

  let filter_name = Sys.argv.(1) in

  try
    let filter = Avfilter.find filter_name in
    Printf.printf "====== Filter: %s ======\n" filter.name;
    Printf.printf "Description: %s\n" filter.description;
    Printf.printf "Flags: %s\n"
      (String.concat ", " (List.map string_of_filter_flag filter.flags));
    Printf.printf "Audio inputs: %s\n" (string_of_pads filter.io.inputs.audio);
    Printf.printf "Video inputs: %s\n" (string_of_pads filter.io.inputs.video);
    Printf.printf "Audio outputs: %s\n" (string_of_pads filter.io.outputs.audio);
    Printf.printf "Video outputs: %s\n" (string_of_pads filter.io.outputs.video);
    Printf.printf "Options:\n%s\n"
      (String.concat "\n"
         (List.map string_of_option (Avutil.Options.opts filter.options)))
  with Not_found ->
    Printf.eprintf "Error: Filter '%s' not found\n" filter_name;
    exit 1
