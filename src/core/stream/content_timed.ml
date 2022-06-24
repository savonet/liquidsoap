(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
  type params = unit
  type 'a content = { mutable length : int; mutable data : (int * 'a) list }

  let make ~length () = { length; data = [] }
  let clear d = d.data <- []
  let is_empty { data } = data = []

  let sort : 'a. (int * 'a) list -> (int * 'a) list =
   fun data -> List.sort (fun (p, _) (p', _) -> Stdlib.compare p p') data

  let sub c ofs len =
    {
      c with
      data =
        List.filter_map
          (fun (pos, x) ->
            if ofs <= pos && pos < ofs + len then Some (pos - ofs, x) else None)
          c.data;
    }

  let blit : 'a. 'a content -> int -> 'a content -> int -> int -> unit =
   fun src src_pos dst dst_pos len ->
    let head = (sub dst 0 dst_pos).data in
    let middle =
      List.map (fun (pos, x) -> (dst_pos + pos, x)) (sub src src_pos len).data
    in
    let tail = (sub dst (dst_pos + len) (dst.length - len - dst_pos)).data in
    dst.data <- sort (head @ middle @ tail)

  let copy ~copy d =
    { d with data = List.map (fun (pos, x) -> (pos, copy x)) d.data }

  let params _ = ()
end

module MetadataSpecs = struct
  include Specs

  type kind = [ `Metadata ]
  type params = unit
  type data = Frame_base.metadata content

  let internal_content_type = None
  let kind = `Metadata
  let string_of_kind _ = "metadata"
  let kind_of_string = function "metadata" -> Some `Metadata | _ -> None
  let string_of_params () = "metadata"
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let copy = copy ~copy:Hashtbl.copy
end

module Metadata = struct
  include Liquidsoap_lang.Content.MkContent (MetadataSpecs)

  let format = lift_params ()

  let set_data d m =
    let d = get_data d in
    d.Specs.data <- m

  let get_data d =
    let { Specs.length; data } = get_data d in
    List.filter
      (fun (p, _) -> p < length)
      (List.sort (fun (p, _) (p', _) -> Stdlib.compare p p') data)
end

module TrackMarkSpecs = struct
  include Specs

  type kind = [ `TrackMark ]
  type data = unit content

  let internal_content_type = None
  let kind = `TrackMark
  let string_of_kind _ = "track_mark"
  let kind_of_string = function "track_mark" -> Some `TrackMark | _ -> None
  let string_of_params () = "track_mark"
  let compatible _ _ = true
  let default_params _ = ()
  let parse_param _ _ = Some ()
  let merge _ _ = ()
  let copy = copy ~copy:(fun () -> ())
end

module TrackMark = struct
  include Liquidsoap_lang.Content.MkContent (TrackMarkSpecs)

  let format = lift_params ()

  let set_data d b =
    let d = get_data d in
    d.Specs.data <- List.map (fun pos -> (pos, ())) b

  let get_data d =
    let { Specs.length; data } = get_data d in
    List.filter
      (fun p -> p < length)
      (List.sort Stdlib.compare (List.map fst data))
end
