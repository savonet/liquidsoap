(* Enumeration table generator, spec/build.md §3.

   [gen_code "<cc command>" <library> <c flags...>] runs the target's
   preprocessor once on the headers of every table of [library] and writes,
   for each table, [<table>.ml] and [<table>_stubs.h] in the current
   directory. [gen_code variants] writes the variant constants that
   hand-written stubs use.

   The C value of a member is never read: tables name the C constants. *)

let fail fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline ("gen_code: " ^ msg);
      exit 1)
    fmt

(* [Enumerators] takes the enumerators of any enumeration that carry the
   table's prefix. *)
type source = Enum of string | Macros | Enumerators

type variant_type = {
  type_name : string;
  source : source;
  select : string list -> string list;
      (** picks the members among all the names of the source, markers included,
          in declaration order *)
}

type table = {
  library : string;
  name : string;
  header : string;
  prefix : string;
  excluded : string list;
  types : variant_type list;
}

let simple ?(excluded = []) library name header source prefix =
  {
    library;
    name;
    header;
    prefix;
    excluded;
    types = [{ type_name = "t"; source; select = Fun.id }];
  }

let rec drop_until marker = function
  | [] -> []
  | name :: rest -> if name = marker then rest else drop_until marker rest

let rec take_until marker = function
  | [] -> []
  | name :: rest -> if name = marker then [] else name :: take_until marker rest

let codec_id =
  let id name = "AV_CODEC_ID_" ^ name in
  let enum = Enum "AVCodecID" in
  let range ?after ?before names =
    let names =
      Option.fold ~none:names ~some:(fun m -> drop_until (id m) names) after
    in
    Option.fold ~none:names ~some:(fun m -> take_until (id m) names) before
  in
  let family type_name ~first select =
    {
      type_name;
      source = enum;
      select = (fun names -> List.map id first @ select names);
    }
  in
  {
    library = "avcodec";
    name = "codec_id";
    header = "libavcodec/codec_id.h";
    prefix = "AV_CODEC_ID_";
    excluded = List.map id ["FIRST_AUDIO"; "FIRST_SUBTITLE"; "FIRST_UNKNOWN"];
    types =
      [
        family "video"
          ~first:["WRAPPED_AVFRAME"; "NONE"]
          (range ~after:"NONE" ~before:"FIRST_AUDIO");
        family "audio"
          ~first:["WRAPPED_AVFRAME"; "NONE"]
          (range ~after:"FIRST_AUDIO" ~before:"FIRST_SUBTITLE");
        family "subtitle" ~first:["NONE"]
          (range ~after:"FIRST_SUBTITLE" ~before:"FIRST_UNKNOWN");
        family "unknown" ~first:["NONE"] (range ~after:"FIRST_UNKNOWN");
        { type_name = "codec_id"; source = enum; select = Fun.id };
      ];
  }

let swresample_options =
  let enum type_name tag = { type_name; source = Enum tag; select = Fun.id } in
  {
    library = "swresample";
    name = "swresample_options";
    header = "libswresample/swresample.h";
    prefix = "SWR_";
    excluded = ["SWR_DITHER_NS"];
    types =
      [
        enum "dither_type" "SwrDitherType";
        enum "engine" "SwrEngine";
        enum "filter_type" "SwrFilterType";
      ];
  }

let tables =
  let pixfmt = "libavutil/pixfmt.h" and avcodec = "libavcodec/avcodec.h" in
  let avutil ?excluded = simple ?excluded "avutil" in
  let avcodec_table = simple "avcodec" in
  [
    avutil "pixel_format" pixfmt (Enum "AVPixelFormat") "AV_PIX_FMT_";
    avutil "pixel_format_flag" "libavutil/pixdesc.h" Macros "AV_PIX_FMT_FLAG_";
    avutil "color_space" pixfmt (Enum "AVColorSpace") "AVCOL_SPC_";
    avutil "color_range" pixfmt (Enum "AVColorRange") "AVCOL_RANGE_";
    avutil "color_primaries" pixfmt (Enum "AVColorPrimaries") "AVCOL_PRI_"
      ~excluded:["AVCOL_PRI_EXT_BASE"];
    avutil "color_trc" pixfmt (Enum "AVColorTransferCharacteristic")
      "AVCOL_TRC_" ~excluded:["AVCOL_TRC_EXT_BASE"];
    avutil "chroma_location" pixfmt (Enum "AVChromaLocation") "AVCHROMA_LOC_";
    avutil "sample_format" "libavutil/samplefmt.h" (Enum "AVSampleFormat")
      "AV_SAMPLE_FMT_";
    avutil "channel_layout" "libavutil/channel_layout.h" Macros "AV_CH_LAYOUT_";
    avutil "hw_device_type" "libavutil/hwcontext.h" (Enum "AVHWDeviceType")
      "AV_HWDEVICE_TYPE_";
    avutil "media_types" "libavutil/avutil.h" (Enum "AVMediaType")
      "AVMEDIA_TYPE_";
    avutil "subtitle_type" avcodec (Enum "AVSubtitleType") "SUBTITLE_";
    avutil "subtitle_flag" avcodec Macros "AV_SUBTITLE_FLAG_";
    avcodec_table "codec_capabilities" "libavcodec/codec.h" Macros
      "AV_CODEC_CAP_";
    avcodec_table "codec_properties" "libavcodec/codec_desc.h" Macros
      "AV_CODEC_PROP_";
    avcodec_table "hw_config_method" "libavcodec/codec.h" Enumerators
      "AV_CODEC_HW_CONFIG_METHOD_";
    codec_id;
    swresample_options;
  ]

(* Constructors of the hand-written tables of the stubs, all libraries. *)
let hand_written_variants =
  [
    "Audio";
    "Video";
    "Subtitle";
    "Data";
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
    "Quiet";
    "Panic";
    "Fatal";
    "Error";
    "Warning";
    "Info";
    "Verbose";
    "Debug";
    "Trace";
    "Second";
    "Millisecond";
    "Microsecond";
    "Nanosecond";
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
    "Flags";
    "Int";
    "Int64";
    "UInt64";
    "Duration";
    "Double";
    "Float";
    "Rational";
    "String";
    "Binary";
    "Dict";
    "Image_size";
    "Video_rate";
    "Color";
    "Pixel_fmt";
    "Sample_fmt";
    "Channel_layout";
    "Bool";
    "Const";
    "Unsupported";
    "Device_context";
    "Frame_context";
    "Keyframe";
    "Corrupt";
    "Discard";
    "Trusted";
    "Disposable";
    "Replaygain";
    "Strings_metadata";
    "Metadata_update";
    "Dynamic_inputs";
    "Dynamic_outputs";
    "Slice_threads";
    "Support_timeline_generic";
    "Support_timeline_internal";
    "Fast";
  ]

(* spec/build.md §3.6. *)
let variant_constant name =
  let hash =
    String.fold_left
      (fun h c -> ((223 * h) + Char.code c) land 0x7FFFFFFF)
      0 name
  in
  let constant = ((2 * hash) + 1) land 0xFFFFFFFF in
  if constant >= 0x80000000 then constant - 0x100000000 else constant

let () =
  List.iter
    (fun (name, constant) -> assert (variant_constant name = constant))
    [
      ("Jpeg", 1652440465);
      ("Unspecified", -789514449);
      ("Audio", 1968951661);
      ("Video", -1806497609);
      ("None", 1741061553);
      ("_0rgb", -1505465991);
    ]

type token = Word of string | Symbol of char

let is_word_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit c = c >= '0' && c <= '9'

(* String and character literals are skipped: a brace or a comma inside one
   is not syntax. *)
let tokenize text =
  let length = String.length text in
  let rec skip_literal quote i =
    if i >= length then i
    else if text.[i] = '\\' then skip_literal quote (i + 2)
    else if text.[i] = quote then i + 1
    else skip_literal quote (i + 1)
  in
  let rec scan i tokens =
    if i >= length then List.rev tokens
    else (
      match text.[i] with
        | ' ' | '\t' | '\n' | '\r' -> scan (i + 1) tokens
        | ('"' | '\'') as quote -> scan (skip_literal quote (i + 1)) tokens
        | c when is_word_char c ->
            let j = ref i in
            while !j < length && is_word_char text.[!j] do
              incr j
            done;
            scan !j (Word (String.sub text i (!j - i)) :: tokens)
        | c -> scan (i + 1) (Symbol c :: tokens))
  in
  scan 0 []

(* [alias] is set when the initialiser is the bare name of another member, or
   the initialiser of an earlier one: the member shares a value. *)
type enumerator = {
  enumerator : string;
  initialiser : token list;
  alias : bool;
}

type enumeration = { tag : string option; enumerators : enumerator list }

let rec parse_initialiser depth tokens = function
  | Symbol (',' | '}') :: _ as rest when depth = 0 -> (List.rev tokens, rest)
  | (Symbol '(' as t) :: rest ->
      parse_initialiser (depth + 1) (t :: tokens) rest
  | (Symbol ')' as t) :: rest ->
      parse_initialiser (depth - 1) (t :: tokens) rest
  | t :: rest -> parse_initialiser depth (t :: tokens) rest
  | [] -> (List.rev tokens, [])

let rec parse_enumerators acc = function
  | Symbol '}' :: rest -> (List.rev acc, rest)
  | Word enumerator :: rest ->
      let initialiser, rest = parse_initialiser 0 [] rest in
      let alias =
        match initialiser with
          | [Symbol '='; Word w] -> not (is_digit w.[0])
          | _ -> false
      in
      let repeated =
        initialiser <> []
        && List.exists (fun e -> e.initialiser = initialiser) acc
      in
      parse_enumerators
        ({ enumerator; initialiser; alias = alias || repeated } :: acc)
        rest
  | _ :: rest -> parse_enumerators acc rest
  | [] -> (List.rev acc, [])

let rec parse_enumerations acc = function
  | Word "enum" :: Word tag :: Symbol '{' :: rest ->
      let enumerators, rest = parse_enumerators [] rest in
      parse_enumerations ({ tag = Some tag; enumerators } :: acc) rest
  | Word "enum" :: Symbol '{' :: rest ->
      let enumerators, rest = parse_enumerators [] rest in
      parse_enumerations ({ tag = None; enumerators } :: acc) rest
  | _ :: rest -> parse_enumerations acc rest
  | [] -> List.rev acc

type preprocessed = { enumerations : enumeration list; macros : string list }

(* The name an object-like [#define] or an [#undef] line gives. *)
let directive_name ~keyword line =
  let prefix = "#" ^ keyword ^ " " in
  if not (String.starts_with ~prefix line) then None
  else (
    let start = String.length prefix in
    let stop = ref start in
    while !stop < String.length line && is_word_char line.[!stop] do
      incr stop
    done;
    if !stop < String.length line && line.[!stop] = '(' then None
    else Some (String.sub line start (!stop - start)))

(* [-dD] keeps the [#define] and [#undef] directives in place, so macros come
   in declaration order. *)
let parse_preprocessed text =
  let code = Buffer.create (String.length text) in
  let add_line macros line =
    if not (String.starts_with ~prefix:"#" line) then (
      Buffer.add_string code line;
      Buffer.add_char code '\n';
      macros)
    else (
      match
        ( directive_name ~keyword:"define" line,
          directive_name ~keyword:"undef" line )
      with
        | Some name, _ ->
            if List.mem name macros then macros else name :: macros
        | None, Some name -> List.filter (( <> ) name) macros
        | None, None -> macros)
  in
  let macros = List.fold_left add_line [] (String.split_on_char '\n' text) in
  {
    enumerations = parse_enumerations [] (tokenize (Buffer.contents code));
    macros = List.rev macros;
  }

let preprocess ~cc ~c_flags headers =
  let describe = String.concat ", " headers in
  let command = String.split_on_char ' ' cc |> List.filter (( <> ) "") in
  let argv =
    Array.of_list (command @ c_flags @ ["-E"; "-dD"; "-x"; "c"; "-"])
  in
  let source =
    String.concat "" (List.map (Printf.sprintf "#include <%s>\n") headers)
  in
  let source_read, source_write = Unix.pipe ~cloexec:true () in
  let output_read, output_write = Unix.pipe ~cloexec:true () in
  let pid =
    try Unix.create_process argv.(0) argv source_read output_write Unix.stderr
    with Unix.Unix_error (error, _, _) ->
      fail "cannot run the preprocessor %S on %s: %s" cc describe
        (Unix.error_message error)
  in
  Unix.close source_read;
  Unix.close output_write;
  let oc = Unix.out_channel_of_descr source_write in
  output_string oc source;
  close_out oc;
  let ic = Unix.in_channel_of_descr output_read in
  let text = In_channel.input_all ic in
  close_in ic;
  match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 -> text
    | _ -> fail "the preprocessor %S failed on %s" cc describe

(* spec/build.md §3.4. *)
let constructors ~prefix members =
  let strip name =
    let start = String.length prefix in
    String.sub name start (String.length name - start)
  in
  let rec derive seen name =
    let name = if is_digit name.[0] then "_" ^ name else name in
    let name = String.capitalize_ascii (String.lowercase_ascii name) in
    if List.mem (variant_constant name) seen then derive seen (name ^ "_")
    else name
  in
  let add (seen, acc) member =
    let constructor = derive seen (strip member) in
    (variant_constant constructor :: seen, (constructor, member) :: acc)
  in
  List.rev (snd (List.fold_left add ([], []) members))

let find_enumeration preprocessed table tag =
  match List.find_opt (fun e -> e.tag = Some tag) preprocessed.enumerations with
    | Some enumeration -> enumeration
    | None -> fail "enum %s not found in %s" tag table.header

let source_names preprocessed table source =
  let names enumeration =
    List.map (fun m -> m.enumerator) enumeration.enumerators
  in
  let prefixed = List.filter (String.starts_with ~prefix:table.prefix) in
  match source with
    | Macros -> prefixed preprocessed.macros
    | Enumerators -> prefixed (List.concat_map names preprocessed.enumerations)
    | Enum tag -> names (find_enumeration preprocessed table tag)

let members preprocessed table variant_type =
  let names =
    variant_type.select (source_names preprocessed table variant_type.source)
  in
  let kept name =
    String.starts_with ~prefix:table.prefix name
    && (not (String.ends_with ~suffix:"_NB" name))
    && not (List.mem name table.excluded)
  in
  match constructors ~prefix:table.prefix (List.filter kept names) with
    | [] ->
        fail "table %s.%s of %s has no member" table.name variant_type.type_name
          table.header
    | members -> members

let c_name table variant_type =
  if variant_type.type_name = "t" then table.name
  else table.name ^ "_" ^ variant_type.type_name

let write path print = Out_channel.with_open_bin path print

let write_ml table types =
  write (table.name ^ ".ml") (fun oc ->
      let pr fmt = Printf.fprintf oc fmt in
      List.iter
        (fun (variant_type, members) ->
          let name = variant_type.type_name in
          pr "type %s = [\n" name;
          List.iter (fun (constructor, _) -> pr "  | `%s\n" constructor) members;
          pr "]\n\nlet %s : %s list = [\n" name name;
          List.iter (fun (constructor, _) -> pr "  `%s;\n" constructor) members;
          pr "]\n\n")
        types)

(* The switch makes the compiler compare the table with the enumeration it
   sees: a member the generator missed fails the build. *)
let write_check oc preprocessed table variant_type =
  let pr fmt = Printf.fprintf oc fmt in
  match variant_type.source with
    | Macros | Enumerators -> ()
    | Enum tag ->
        let enumeration = find_enumeration preprocessed table tag in
        pr "#ifdef OCAML_FFMPEG_CHECK_TABLES\n";
        pr "#pragma GCC diagnostic push\n";
        pr "#pragma GCC diagnostic error \"-Wswitch\"\n";
        pr
          "static inline void %s_check(enum %s member) {\n  switch (member) {\n"
          (c_name table variant_type)
          tag;
        List.iter
          (fun m -> if not m.alias then pr "  case %s:\n" m.enumerator)
          enumeration.enumerators;
        pr "    break;\n  }\n}\n#pragma GCC diagnostic pop\n#endif\n\n"

let write_header preprocessed table types =
  let guard =
    "OCAML_FFMPEG_" ^ String.uppercase_ascii table.name ^ "_STUBS_H"
  in
  write (table.name ^ "_stubs.h") (fun oc ->
      let pr fmt = Printf.fprintf oc fmt in
      pr "#ifndef %s\n#define %s\n\n" guard guard;
      pr "#include <stdint.h>\n#include <caml/mlvalues.h>\n#include <%s>\n"
        table.header;
      pr "#include \"polymorphic_variant_values_stubs.h\"\n\n";
      List.iter
        (fun (variant_type, members) ->
          pr
            "static inline const ocaml_ffmpeg_variant_table *%s_table(void) {\n"
            (c_name table variant_type);
          pr "  static const ocaml_ffmpeg_variant_entry entries[] = {\n";
          List.iter
            (fun (constructor, member) ->
              pr "    {(value)(%d), (int64_t)(%s)},\n"
                (variant_constant constructor)
                member)
            members;
          pr "  };\n";
          pr "  static const ocaml_ffmpeg_variant_table table = {\n";
          pr "      entries, %d, \"%s.%s\"};\n" (List.length members)
            (String.capitalize_ascii table.name)
            variant_type.type_name;
          pr "  return &table;\n}\n\n")
        types;
      List.sort_uniq compare (List.map (fun (vt, _) -> vt.source) types)
      |> List.iter (fun source ->
          let checked (vt, _) = vt.source = source in
          write_check oc preprocessed table (fst (List.find checked types)));
      pr "#endif\n")

let write_variants () =
  write "polymorphic_variant_values_stubs.h" (fun oc ->
      let pr fmt = Printf.fprintf oc fmt in
      pr "#ifndef OCAML_FFMPEG_POLYMORPHIC_VARIANT_VALUES_STUBS_H\n";
      pr "#define OCAML_FFMPEG_POLYMORPHIC_VARIANT_VALUES_STUBS_H\n\n";
      pr
        "#include <stddef.h>\n\
         #include <stdint.h>\n\
         #include <caml/mlvalues.h>\n\n";
      pr "/* One member of a variant type: its OCaml and its C constant. */\n";
      pr "typedef struct {\n  value variant;\n  int64_t constant;\n";
      pr "} ocaml_ffmpeg_variant_entry;\n\n";
      pr "/* Entries are in declaration order; name is the OCaml type. */\n";
      pr "typedef struct {\n  const ocaml_ffmpeg_variant_entry *entries;\n";
      pr
        "  size_t length;\n\
        \  const char *name;\n\
         } ocaml_ffmpeg_variant_table;\n\n";
      List.iter
        (fun name ->
          pr "#define PVV_%s ((value)(%d))\n" name (variant_constant name))
        hand_written_variants;
      pr "\n#endif\n")

let () =
  match List.tl (Array.to_list Sys.argv) with
    | ["variants"] -> write_variants ()
    | cc :: library :: c_flags ->
        let tables = List.filter (fun t -> t.library = library) tables in
        if tables = [] then fail "no table for library %s" library;
        let headers =
          List.sort_uniq compare (List.map (fun t -> t.header) tables)
        in
        let preprocessed =
          parse_preprocessed (preprocess ~cc ~c_flags headers)
        in
        List.iter
          (fun table ->
            let types =
              List.map
                (fun vt -> (vt, members preprocessed table vt))
                table.types
            in
            write_ml table types;
            write_header preprocessed table types)
          tables
    | _ -> fail "usage: gen_code (variants | <cc> <library> <c flags...>)"
