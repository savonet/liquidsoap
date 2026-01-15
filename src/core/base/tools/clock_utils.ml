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

type clock_entry = {
  name : string;
  outputs : string list;
  active : string list;
  passive : string list;
  sub_clocks : clock_entry list;
}

let format_clock ?(max_width = 100) entry =
  let utf8_len s = Liquidsoap_lang.Lang_string.length ~encoding:`Utf8 s in
  let format_list label items ~prefix ~bar_prefix ~max_width =
    match items with
      | [] -> None
      | _ ->
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
