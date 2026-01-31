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

type subtitle = {
  start_time : int;  (** Start time relative to position, in main ticks *)
  end_time : int;  (** End time relative to position, in main ticks *)
  text : string;
  format : [ `Ass | `Text ];
  forced : bool;
}

let compare (x : int) (y : int) = x - y [@@inline always]

module Specs = struct
  type kind = [ `Subtitle ]
  type params = unit
  type 'a content = { length : int; mutable data : (int * 'a) list }
  type data = subtitle content

  let name = "subtitle"
  let kind = `Subtitle
  let is_sparse = true
  let string_of_kind _ = "subtitles"
  let kind_of_string = function "subtitles" -> Some `Subtitle | _ -> None
  let string_of_params () = ""
  let compatible () () = true
  let default_params _ = ()
  let parse_param _ _ = None
  let merge () () = ()
  let make ?(length = 0) _ = { length; data = [] }
  let length { length } = length

  let sort data =
    List.sort_uniq (fun (p, _) (p', _) -> compare p' p) (List.rev data)

  let sub c ofs length =
    {
      length;
      data =
        List.filter_map
          (fun (pos, x) ->
            if ofs <= pos && pos < ofs + length then Some (pos - ofs, x)
            else None)
          c.data;
    }

  let blit src src_pos dst dst_pos len =
    let head = (sub dst 0 dst_pos).data in
    let middle =
      List.map (fun (pos, x) -> (dst_pos + pos, x)) (sub src src_pos len).data
    in
    let tail_length = dst.length - len - dst_pos in
    let tail = (sub dst (dst_pos + len) tail_length).data in
    dst.data <- sort (head @ middle @ tail)

  let copy d = { d with data = List.map (fun (pos, x) -> (pos, x)) d.data }
  let params _ = ()

  let checksum d =
    let entries =
      List.map
        (fun (pos, sub) ->
          Printf.sprintf "%d:%d:%d:%s:%s:%b" pos sub.start_time sub.end_time
            sub.text
            (match sub.format with `Ass -> "ass" | `Text -> "text")
            sub.forced)
        d.data
    in
    Digest.string (String.concat "|" entries) |> Digest.to_hex
end

include Content_base.MkContentBase (Specs)

let format = lift_params ()

let lift_data ?length s =
  let length =
    match length with
      | Some l -> l
      | None ->
          (* Default to max position + 1 to ensure content covers all positions *)
          List.fold_left (fun l (p, _) -> max l (p + 1)) 0 s
  in
  lift_data { Specs.length; data = s }

let set_data d s =
  let d = get_data d in
  d.Specs.data <- s

let get_data d =
  let { Specs.length; data } = get_data d in
  List.filter
    (fun (p, _) -> 0 <= p && p < length)
    (List.stable_sort (fun (p, _) (p', _) -> compare p p') data)
