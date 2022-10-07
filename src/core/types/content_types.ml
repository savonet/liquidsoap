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

(** A frame kind type is a purely abstract type representing a
    frame kind. *)
let frame_t ?pos audio video midi =
  Type.make ?pos
    (Type.Constr
       {
         Type.constructor = "stream_kind";
         Type.params =
           [(`Covariant, audio); (`Covariant, video); (`Covariant, midi)];
       })

let kind_t ?pos kind =
  let evar ?(constraints = []) () = Type.var ~constraints ?pos () in
  let mk_format f = Type.make ?pos (Format_type.descr f) in
  match kind with
    | `Any -> evar ()
    | `Internal -> evar ~constraints:[Format_type.internal_media] ()
    | `Kind k ->
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content.string_of_kind k;
               Type.params = [(`Covariant, evar ())];
             })
    | `Format f ->
        let k = Content.kind f in
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content.string_of_kind k;
               Type.params = [(`Covariant, mk_format f)];
             })

let of_frame_t t =
  let t = Type.deref t in
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          Type.params = [(_, audio); (_, video); (_, midi)];
        } ->
        Frame.mk_fields ~audio ~video ~midi ()
    | Type.Var ({ contents = Type.Free _ } as var) ->
        let audio = kind_t `Any in
        let video = kind_t `Any in
        let midi = kind_t `Any in
        var := Type.Link (`Invariant, frame_t audio video midi);
        Frame.mk_fields ~audio ~video ~midi ()
    | _ -> assert false

(** Type of audio formats that can encode frame of a given kind. *)
let format_t ?pos k =
  Type.make ?pos
    (Type.Constr
       { Type.constructor = "format"; Type.params = [(`Covariant, k)] })

(** Type of sources carrying frames of a given kind. *)
let source_t ?pos k =
  Type.make ?pos
    (Type.Constr
       { Type.constructor = "source"; Type.params = [(`Invariant, k)] })

(* Filled in later to avoid dependency cycles. *)
let source_methods_t = ref (fun () : Type.t -> assert false)

let of_source_t t =
  match (Type.deref t).Type.descr with
    | Type.Constr { Type.constructor = "source"; Type.params = [(_, t)] } -> t
    | _ -> assert false
