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
  let rec format_entry ~prefix ~bar entry =
    let header = Printf.sprintf "%s%s:" prefix entry.name in
    let content_prefix = bar ^ "│ " in
    let format_field label items =
      format_list label items ~prefix:content_prefix ~bar_prefix:content_prefix
        ~max_width
    in
    let fields =
      List.filter_map Fun.id
        [
          format_field "outputs" entry.outputs;
          format_field "active " entry.active;
          format_field "passive" entry.passive;
        ]
    in
    let sub_clock_count = List.length entry.sub_clocks in
    let sub_clocks_str =
      List.mapi
        (fun i sub ->
          let is_last = i = sub_clock_count - 1 in
          let connector = if is_last then "└─ " else "├─ " in
          let sub_bar = bar ^ if is_last then "   " else "│  " in
          format_entry ~prefix:(bar ^ connector) ~bar:sub_bar sub)
        entry.sub_clocks
    in
    String.concat "\n" ((header :: fields) @ sub_clocks_str)
  in
  format_entry ~prefix:"" ~bar:"" entry

type dump_entry = {
  clock_name : string;
  ticks : int;
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
    let len = List.length subs in
    let sub_clocks_str =
      if subs <> [] then
        "\n"
        ^ String.concat "\n"
            (List.mapi
               (fun i sub ->
                 format_entry ~prefix:child_prefix ~is_last:(i = len - 1) sub)
               subs)
      else ""
    in
    Printf.sprintf "%s%s%s (ticks: %d, self_sync: %b)\n%s\n%s\n%s%s" prefix
      connector entry.clock_name entry.ticks entry.self_sync
      (format_field ~is_last:false "outputs" entry.outputs)
      (format_field ~is_last:false "active sources" entry.active)
      (format_field ~is_last:true "passive sources" entry.passive)
      sub_clocks_str
  in
  let len = List.length entries in
  let descriptions =
    List.mapi
      (fun i e -> format_entry ~prefix:"" ~is_last:(i = len - 1) e)
      entries
  in
  String.concat "\n" descriptions
