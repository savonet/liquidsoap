(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let compare (x : int) (y : int) = x - y [@@inline always]

type length = Finite of int | Infinite

(** Generic content spec for timed list contents. *)
module Specs = struct
  type params = unit
  type 'a content = { length : length; mutable data : (int * 'a) list }

  let make ?length () =
    let length =
      match length with None -> Infinite | Some len -> Finite len
    in
    { length; data = [] }

  let length { length } =
    match length with Finite len -> len | Infinite -> max_int

  let clear d = d.data <- []
  let free = clear
  let is_empty { data } = data = []

  let sort : 'a. (int * 'a) list -> (int * 'a) list =
   fun data -> List.stable_sort (fun (p, _) (p', _) -> compare p p') data

  let sub c ofs length =
    let len = match length with Infinite -> max_int | Finite len -> len in
    {
      length;
      data =
        List.filter_map
          (fun (pos, x) ->
            if ofs <= pos && pos < ofs + len then Some (pos - ofs, x) else None)
          c.data;
    }

  let blit : 'a. 'a content -> int -> 'a content -> int -> int -> unit =
   fun src src_pos dst dst_pos len ->
    let head = (sub dst 0 (Finite dst_pos)).data in
    let middle =
      List.map
        (fun (pos, x) -> (dst_pos + pos, x))
        (sub src src_pos (Finite len)).data
    in
    let tail_length =
      match dst.length with
        | Infinite -> Infinite
        | Finite dst_len -> Finite (dst_len - len - dst_pos)
    in
    let tail = (sub dst (dst_pos + len) tail_length).data in
    dst.data <- sort (head @ middle @ tail)

  let copy ~copy d =
    { d with data = List.map (fun (pos, x) -> (pos, copy x)) d.data }

  let params _ = ()
end

module Metadata_specs = struct
  include Specs

  type kind = [ `Metadata ]
  type params = unit
  type data = Metadata_base.t content

  let name = "metadata"
  let internal_content_type = None
  let kind = `Metadata
  let string_of_kind _ = "metadata"
  let kind_of_string = function "metadata" -> Some `Metadata | _ -> None
  let string_of_params () = ""
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let copy = copy ~copy:(fun x -> x)
end

module Metadata = struct
  include Content_base.MkContentBase (Metadata_specs)

  let format = lift_params ()

  let lift_data m =
    lift_data
      {
        Specs.length = Finite (List.fold_left (fun l (p, _) -> max l p) 0 m);
        data = m;
      }

  let set_data d m =
    let d = get_data d in
    d.Specs.data <- m

  let get_data d =
    let { Specs.length; data } = get_data d in
    let length = match length with Infinite -> max_int | Finite len -> len in
    List.filter
      (fun (p, _) -> 0 <= p && p < length)
      (List.stable_sort (fun (p, _) (p', _) -> compare p p') data)
end

module Track_marks_specs = struct
  include Specs

  type kind = [ `Track_marks ]
  type data = unit content

  let name = "track_marks"
  let internal_content_type = None
  let kind = `Track_marks
  let string_of_kind _ = "track_marks"
  let kind_of_string = function "track_mark" -> Some `Track_marks | _ -> None
  let string_of_params () = ""
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let copy = copy ~copy:(fun () -> ())
end

module Track_marks = struct
  include Content_base.MkContentBase (Track_marks_specs)

  let format = lift_params ()

  let lift_data p =
    lift_data
      {
        Specs.length = Finite (List.fold_left (fun l p -> max l p) 0 p);
        data = List.map (fun p -> (p, ())) p;
      }

  let set_data d b =
    let d = get_data d in
    d.Specs.data <- List.map (fun pos -> (pos, ())) b

  let get_data d =
    let { Specs.length; data } = get_data d in
    let length = match length with Infinite -> max_int | Finite len -> len in
    List.filter
      (fun p -> 0 <= p && p < length)
      (List.stable_sort compare (List.map fst data))
end
