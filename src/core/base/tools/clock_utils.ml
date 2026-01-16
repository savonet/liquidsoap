(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

type source_entry = { id : string; activations : string list }

type clock_entry = {
  name : string;
  outputs : source_entry list;
  active : source_entry list;
  passive : source_entry list;
  sub_clocks : clock_entry list;
}

let utf8_len s = Liquidsoap_lang.Lang_string.length ~encoding:`Utf8 s

let format_source { id; activations } =
  Printf.sprintf "%s [%s]" id (String.concat ", " activations)

let format_clock ?(max_width = 100) entry =
  let format_list label items ~prefix ~bar_prefix ~max_width =
    match items with
      | [] -> None
      | _ ->
          let items = List.sort (fun e e' -> Stdlib.compare e.id e'.id) items in
          let items = List.map format_source items in
          let label_str = Printf.sprintf "%s%s = [" prefix label in
          let label_part = Printf.sprintf "%s = [" label in
          let label_visual_len = utf8_len label_part in
          let cont_prefix = bar_prefix ^ String.make label_visual_len ' ' in
          let rec build_lines acc current_line = function
            | [] ->
                let final = current_line ^ "]" in
                List.rev (final :: acc)
            | item :: rest ->
                let sep = if current_line = label_str then "" else ", " in
                let candidate = current_line ^ sep ^ item in
                if String.length candidate + 1 <= max_width then
                  build_lines acc candidate rest
                else (
                  let continued = cont_prefix ^ item in
                  build_lines ((current_line ^ ",") :: acc) continued rest)
          in
          Some (String.concat "\n" (build_lines [] label_str items))
  in
  let rec format_entry ~prefix ~bar ~is_root entry =
    let header = Printf.sprintf "%s%s:" prefix entry.name in
    let has_subs = entry.sub_clocks <> [] in
    let format_field ~is_last label items =
      if is_root then (
        let marker = if is_last && not has_subs then "└── " else "├── " in
        let bar_cont = if is_last && not has_subs then "    " else "│   " in
        format_list label items ~prefix:(bar ^ marker)
          ~bar_prefix:(bar ^ bar_cont) ~max_width)
      else
        format_list label items ~prefix:(bar ^ "│ ") ~bar_prefix:(bar ^ "│ ")
          ~max_width
    in
    let all_fields =
      [
        ("outputs", entry.outputs);
        ("active ", entry.active);
        ("passive", entry.passive);
      ]
    in
    let non_empty_fields =
      List.filter (fun (_, items) -> items <> []) all_fields
    in
    let num_fields = List.length non_empty_fields in
    let fields =
      List.mapi
        (fun i (label, items) ->
          format_field ~is_last:(i = num_fields - 1) label items)
        non_empty_fields
    in
    let fields = List.filter_map Fun.id fields in
    let sub_clock_count = List.length entry.sub_clocks in
    let sub_clocks_str =
      List.mapi
        (fun i sub ->
          let is_last = i = sub_clock_count - 1 in
          let connector = if is_last then "└─ " else "├─ " in
          let sub_bar = bar ^ if is_last then "   " else "│  " in
          format_entry ~prefix:(bar ^ connector) ~bar:sub_bar ~is_root:false sub)
        entry.sub_clocks
    in
    String.concat "\n" ((header :: fields) @ sub_clocks_str)
  in
  format_entry ~prefix:"" ~bar:"" ~is_root:true entry

type dump_entry = {
  clock_name : string;
  ticks : int;
  time : float;
  self_sync : bool;
  outputs : source_entry list;
  active : source_entry list;
  passive : source_entry list;
  sub_clocks : dump_entry list;
}

let format_dump ?(max_width = 100) entries =
  let format_sources ~prefix ~bar_prefix ~label sources ~max_width =
    match sources with
      | [] -> Printf.sprintf "%s%s:" prefix label
      | _ ->
          let items = List.map format_source sources in
          let label_str = Printf.sprintf "%s%s: " prefix label in
          let label_part = Printf.sprintf "%s: " label in
          let label_visual_len = utf8_len label_part in
          let cont_prefix = bar_prefix ^ String.make label_visual_len ' ' in
          let rec build_lines acc current_line = function
            | [] -> List.rev (current_line :: acc)
            | item :: rest ->
                let sep = if current_line = label_str then "" else ", " in
                let candidate = current_line ^ sep ^ item in
                if String.length candidate <= max_width then
                  build_lines acc candidate rest
                else (
                  let continued = cont_prefix ^ item in
                  build_lines ((current_line ^ ",") :: acc) continued rest)
          in
          String.concat "\n" (build_lines [] label_str items)
  in
  let rec format_entry ~prefix ~is_last entry =
    let connector = if is_last then "└── " else "├── " in
    let child_prefix = prefix ^ if is_last then "    " else "│   " in
    let format_field ~is_last label sources =
      let marker = if is_last then "└── " else "├── " in
      let bar = if is_last then "    " else "│   " in
      format_sources ~prefix:(child_prefix ^ marker)
        ~bar_prefix:(child_prefix ^ bar) ~label sources ~max_width
    in
    let subs = entry.sub_clocks in
    let has_subs = subs <> [] in
    let len = List.length subs in
    let sub_clocks_str =
      if has_subs then
        "\n"
        ^ String.concat "\n"
            (List.mapi
               (fun i sub ->
                 format_entry ~prefix:child_prefix ~is_last:(i = len - 1) sub)
               subs)
      else ""
    in
    Printf.sprintf
      "%s%s%s (ticks: %d, time: %.02fs, self_sync: %b)\n%s\n%s\n%s%s" prefix
      connector entry.clock_name entry.ticks entry.time entry.self_sync
      (format_field ~is_last:false "outputs" entry.outputs)
      (format_field ~is_last:false "active sources" entry.active)
      (format_field ~is_last:(not has_subs) "passive sources" entry.passive)
      sub_clocks_str
  in
  let len = List.length entries in
  let descriptions =
    List.mapi
      (fun i e -> format_entry ~prefix:"" ~is_last:(i = len - 1) e)
      entries
  in
  String.concat "\n" descriptions

type source_kind = [ `Output | `Active | `Passive ]

type graph_source = {
  source_name : string;
  source_kind : source_kind;
  source_activations : string list;
}

let format_source_graph sources =
  let string_of_kind = function
    | `Output -> "output"
    | `Active -> "active"
    | `Passive -> "passive"
  in
  (* Build lookup table by name *)
  let by_name = Hashtbl.create (List.length sources) in
  List.iter (fun s -> Hashtbl.add by_name s.source_name s) sources;
  (* Build inverted index: for each source, find sources it activates (children)
     If source S has activations [A1, A2], it means A1 and A2 activate S,
     so S is a child of A1 and A2. We invert this to get children of each source. *)
  let children_of = Hashtbl.create (List.length sources) in
  let dangling = ref [] in
  List.iter
    (fun s ->
      List.iter
        (fun parent_name ->
          if Hashtbl.mem by_name parent_name then (
            let current =
              Option.value ~default:[]
                (Hashtbl.find_opt children_of parent_name)
            in
            Hashtbl.replace children_of parent_name (s.source_name :: current))
          else dangling := (s.source_name, parent_name) :: !dangling)
        s.source_activations)
    sources;
  (* Track seen sources *)
  let seen = Hashtbl.create 16 in
  let rec format_source ~prefix ~is_last name =
    let connector = if is_last then "└── " else "├── " in
    let child_prefix = prefix ^ if is_last then "    " else "│   " in
    match Hashtbl.find_opt by_name name with
      | None -> Printf.sprintf "%s%s%s [not found]" prefix connector name
      | Some source ->
          let label =
            Printf.sprintf "%s [%s]" source.source_name
              (string_of_kind source.source_kind)
          in
          if Hashtbl.mem seen name then
            Printf.sprintf "%s%s%s (*)" prefix connector label
          else (
            Hashtbl.add seen name ();
            let source_children =
              Option.value ~default:[] (Hashtbl.find_opt children_of name)
              |> List.rev
            in
            let len = List.length source_children in
            let children_str =
              if len = 0 then ""
              else
                "\n"
                ^ String.concat "\n"
                    (List.mapi
                       (fun i child_name ->
                         format_source ~prefix:child_prefix
                           ~is_last:(i = len - 1)
                           child_name)
                       source_children)
            in
            Printf.sprintf "%s%s%s%s" prefix connector label children_str)
  in
  let format_section title items format_fn =
    if items = [] then None
    else (
      let len = List.length items in
      let content =
        String.concat "\n"
          (List.mapi
             (fun i item -> format_fn ~is_last:(i = len - 1) item)
             items)
      in
      Some (Printf.sprintf "%s:\n%s" title content))
  in
  (* Find outputs and print them *)
  let outputs =
    List.filter (fun s -> s.source_kind = `Output) sources
    |> List.map (fun s -> s.source_name)
  in
  let outputs_section =
    format_section "Outputs" outputs (fun ~is_last name ->
        format_source ~prefix:"" ~is_last name)
  in
  (* Find singletons (not visited after printing outputs) *)
  let singletons =
    List.filter (fun s -> not (Hashtbl.mem seen s.source_name)) sources
  in
  let singletons_section =
    format_section "Singletons" singletons (fun ~is_last source ->
        let connector = if is_last then "└── " else "├── " in
        Printf.sprintf "%s%s [%s]" connector source.source_name
          (string_of_kind source.source_kind))
  in
  (* External activations: sources activated by something outside this clock *)
  let external_section =
    let unique_external = List.sort_uniq compare !dangling in
    format_section "External activations" unique_external
      (fun ~is_last (source, external_activator) ->
        let connector = if is_last then "└── " else "├── " in
        Printf.sprintf "%s%s <- %s" connector source external_activator)
  in
  String.concat "\n\n"
    (List.filter_map Fun.id
       [outputs_section; singletons_section; external_section])
