(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** Generic content spec for timed list contents. *)
module Specs = struct
  type ('a, 'b) content = {
    mutable params : 'a;
    mutable size : int;
    mutable data : (int * 'b) list;
  }

  let make ~size params = { params; size; data = [] }
  let clear d = d.data <- []
  let is_empty { data } = data = []

  let sort : 'a. (int * 'a) list -> (int * 'a) list =
   fun data -> List.sort (fun (p, _) (p', _) -> Stdlib.compare p p') data

  let sub c ofs len =
    {
      c with
      size = len;
      data =
        List.filter_map
          (fun (pos, x) ->
            if ofs <= pos && pos < ofs + len then Some (pos - ofs, x) else None)
          c.data;
    }

  let blit :
        'a 'b.
        copy:('b -> 'b) ->
        ('a, 'b) content ->
        int ->
        ('a, 'b) content ->
        int ->
        int ->
        unit =
   fun ~copy src src_pos dst dst_pos len ->
    (* No compatibility check here, it's
       assumed to have been done beforehand. *)
    dst.params <- src.params;
    let head = (sub dst 0 dst_pos).data in
    let middle =
      List.map
        (fun (pos, x) -> (dst_pos + pos, copy x))
        (sub src src_pos len).data
    in
    let tail = (sub dst (dst_pos + len) (dst.size - len - dst_pos)).data in
    dst.data <- sort (head @ middle @ tail)

  let fill :
        'a 'b. ('a, 'b) content -> int -> ('a, 'b) content -> int -> int -> unit
      =
   fun src src_pos dst dst_pos len ->
    blit ~copy:(fun x -> x) src src_pos dst dst_pos len

  let copy ~copy d =
    { d with data = List.map (fun (pos, x) -> (pos, copy x)) d.data }

  let params { params } = params
  let length { size } = size
end

module MetadataSpecs = struct
  include Specs

  type kind = [ `Metadata ]
  type params = unit
  type data = (params, Frame_base.metadata) content

  let kind = `Metadata
  let string_of_kind _ = "metadata"
  let kind_of_string = function "metadata" -> Some `Metadata | _ -> None
  let string_of_params () = "metadata"
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let blit = blit ~copy:Hashtbl.copy
  let copy = copy ~copy:Hashtbl.copy
end

module Metadata = struct
  include Content_base.MkContent (MetadataSpecs)

  let format = lift_params ()
  let lift_data ~size data = lift_data { Specs.params = (); size; data }

  let set_data d m =
    let d = get_data d in
    d.Specs.data <- m

  let get_data d =
    List.sort
      (fun (p, _) (p', _) -> Stdlib.compare p p')
      (get_data d).Specs.data
end

module BreaksSpecs = struct
  include Specs

  type kind = [ `Breaks ]
  type params = unit
  type data = (params, unit) content

  let kind = `Breaks
  let string_of_kind _ = "breaks"
  let kind_of_string = function "breaks" -> Some `Breaks | _ -> None
  let string_of_params () = "breaks"
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let blit = blit ~copy:(fun () -> ())
  let copy = copy ~copy:(fun () -> ())
end

module Breaks = struct
  include Content_base.MkContent (BreaksSpecs)

  let format = lift_params ()

  let lift_data ~size data =
    lift_data
      { Specs.params = (); size; data = List.map (fun p -> (p, ())) data }

  let set_data d b =
    let d = get_data d in
    d.Specs.data <- List.map (fun pos -> (pos, ())) b

  let get_data d =
    List.sort Stdlib.compare (List.map fst (get_data d).Specs.data)
end
