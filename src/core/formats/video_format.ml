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

open Content_base

let ( !! ) = Lazy.Mutexed.force

(* The frame's dimensions: what the settings default to, and what a process
   that has no settings answers. *)
let default_width = 1280
let default_height = 720

let dimensions =
  ref (fun () ->
      (Lazy.Mutexed.from_val default_width, Lazy.Mutexed.from_val default_height))

module Specs = struct
  type kind = [ `Canvas ]

  (* [alpha] is refined after typechecking: decoders probe the actual codec
     pixel format and [Unifier.set] the result, which propagates to every
     format this one has been merged with. A unifier (rather than a plain
     value) is required so that refinement still propagates across formats
     unified by [merge]. *)
  type params = {
    width : int Lazy.Mutexed.t option;
    height : int Lazy.Mutexed.t option;
    alpha : bool option Unifier.t;
  }

  let name = "yuv420p"
  let string_of_kind = function `Canvas -> "yuv420p"

  let string_of_params { width; height; alpha } =
    print_optional
      [
        ("width", Option.map (fun x -> string_of_int !!x) width);
        ("height", Option.map (fun x -> string_of_int !!x) height);
        ("alpha", Option.map string_of_bool (Unifier.deref alpha));
      ]

  let serialize_params { width; height; alpha } =
    let field to_string = function Some v -> to_string v | None -> "" in
    String.concat ","
      [
        field (fun w -> string_of_int !!w) width;
        field (fun h -> string_of_int !!h) height;
        field string_of_bool (Unifier.deref alpha);
      ]

  let parse_params s =
    let dimension = function
      | "" -> Some None
      | s ->
          Option.map
            (fun v -> Some (Lazy.Mutexed.from_val v))
            (int_of_string_opt s)
    in
    let alpha = function
      | "" -> Some None
      | s -> Option.map Option.some (bool_of_string_opt s)
    in
    match String.split_on_char ',' s with
      | [width; height; a] -> (
          match (dimension width, dimension height, alpha a) with
            | Some width, Some height, Some alpha ->
                Some { width; height; alpha = Unifier.make alpha }
            | _ -> None)
      | _ -> None

  let parse_param label value =
    match label with
      | "width" ->
          Some
            {
              width = Some (Lazy.Mutexed.from_val (int_of_string value));
              height = None;
              alpha = Unifier.make None;
            }
      | "height" ->
          Some
            {
              width = None;
              height = Some (Lazy.Mutexed.from_val (int_of_string value));
              alpha = Unifier.make None;
            }
      | "alpha" ->
          Some
            {
              width = None;
              height = None;
              alpha = Unifier.make (Some (bool_of_string value));
            }
      | _ -> None

  let merge p p' =
    let alpha =
      merge_param ~name:"alpha" (Unifier.deref p.alpha, Unifier.deref p'.alpha)
    in
    Unifier.set p'.alpha alpha;
    Unifier.(p.alpha <-- p'.alpha);
    {
      width =
        Option.map Lazy.Mutexed.from_val
          (merge_param ~name:"width"
             ( Option.map Lazy.Mutexed.force p.width,
               Option.map Lazy.Mutexed.force p'.width ));
      height =
        Option.map Lazy.Mutexed.from_val
          (merge_param ~name:"height"
             ( Option.map Lazy.Mutexed.force p.height,
               Option.map Lazy.Mutexed.force p'.height ));
      alpha = p.alpha;
    }

  let compatible p p' =
    let compare = function
      | None, None -> true
      | Some _, None | None, Some _ -> true
      | Some x, Some y -> !!x = !!y
    in
    let compare_bool = function
      | None, None -> true
      | Some _, None | None, Some _ -> true
      | Some x, Some y -> x = y
    in
    compare (p.width, p'.width)
    && compare (p.height, p'.height)
    && compare_bool (Unifier.deref p.alpha, Unifier.deref p'.alpha)

  let kind = `Canvas

  let default_params _ =
    { width = None; height = None; alpha = Unifier.make None }

  let kind_of_string = function "yuv420p" -> Some `Canvas | _ -> None

  let content_lang_typ =
    let open Liquidsoap_lang in
    Lang_core.record_t
      [
        ("width", Type.make Type.Int);
        ("height", Type.make Type.Int);
        ("alpha", Type.make (Type.Nullable (Type.make Type.Bool)));
      ]

  let params_to_value { width; height; alpha } =
    let open Liquidsoap_lang in
    let default_width, default_height = !dimensions () in
    let width =
      Lazy.Mutexed.force (Option.value ~default:default_width width)
    in
    let height =
      Lazy.Mutexed.force (Option.value ~default:default_height height)
    in
    Lang_core.record
      [
        ("width", Lang_core.mk (`Int width));
        ("height", Lang_core.mk (`Int height));
        ( "alpha",
          match Unifier.deref alpha with
            | None -> Lang_core.mk `Null
            | Some b -> Lang_core.mk (`Bool b) );
      ]
end

module Format = MkFormatBase (Specs)
include Format

let kind = lift_kind `Canvas
