(* What a dump leaves of a content type: each content's own encoding of its
   parameters, what a reader makes of it, and whether types that shared a
   format still share one. *)

open Liquidsoap_lang

let () =
  Frame_settings.conf_duration#set 0.04;
  Frame_settings.lazy_config_eval := true

let roundtrip format =
  let dumped = Content.serialize_format format in
  Printf.printf "%-24s %-20s %s\n"
    (Content.string_of_format format)
    dumped
    (match Content.parse_format dumped with
      | Some format -> Content.string_of_format format
      | None -> "<none>")

let () =
  List.iter roundtrip
    [
      Content.Audio.format_of_channels 1;
      Content.Audio.format_of_channels 2;
      Content.Audio.format_of_channels 6;
      Content.default_format Content_pcm_s16.kind;
      Content.default_format Content_pcm_f32.kind;
      Content.(default_format Video.kind);
      Content.(default_format Midi.kind);
      Content.Metadata.format;
      Content.Track_marks.format;
      Content.default_format Ffmpeg_copy_content.kind;
      Content.default_format Ffmpeg_raw_content.Audio.kind;
      Content.default_format Ffmpeg_raw_content.Video.kind;
    ]

let () = print_newline ()

let dump env =
  Liquidsoap_lang_types.Jsoo_safe_env.restore
    (Liquidsoap_lang_types.Jsoo_safe_env.strip env)

let scheme descr = ([], Type.make descr)

let () =
  let descr =
    Format_type.descr (`Format (Content.Audio.format_of_channels 1))
  in
  match dump [("x", scheme descr); ("y", scheme descr)] with
    | [("x", (_, x)); ("y", (_, y))] ->
        Printf.printf "format:   %s\n"
          (Content.string_of_format (Format_type.content_type x));
        Printf.printf "shared:   %b\n"
          (Format_type.content_type x == Format_type.content_type y)
    | _ -> print_endline "format:   <unexpected environment>"

let () =
  let descr = Format_type.descr (`Kind Content.Audio.kind) in
  match dump [("x", scheme descr)] with
    | [("x", (_, x))] -> Printf.printf "kind:     %s\n" (Type.to_string x)
    | _ -> print_endline "kind:     <unexpected environment>"

(* A payload's own types belong to the graph around it: the variable inside
   [pcm('a)] is the one the second argument names. *)
let () =
  let inner = Type.var () in
  let kind =
    Type.make
      (Type.Custom (Format_type.kind_handler (Content.Audio.kind, inner)))
  in
  let arrow =
    Type.make
      (Type.Arrow ([(false, "", kind); (false, "", inner)], Type.make Type.Int))
  in
  match dump [("f", ([], arrow))] with
    | [("f", (_, t))] -> Printf.printf "inner:    %s\n" (Type.to_string t)
    | _ -> print_endline "inner:    <unexpected environment>"
