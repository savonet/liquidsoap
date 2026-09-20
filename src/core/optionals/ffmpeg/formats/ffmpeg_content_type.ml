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

(** What a process knows about the parameters of an ffmpeg stream. Libav reads
    them off a codec; a process that only typechecks scripts knows their names
    and no more. *)
module type Params = sig
  type t

  (** Who knows this much about them, for the error a second one gets. *)
  val implementation : string

  val default : t
  val to_string : t -> string
  val parse : string -> string -> t option
  val merge : t -> t -> t
  val compatible : t -> t -> bool
end

module type Names = sig
  (** What the content registry calls it, and what a script writes. *)
  type kind

  val kind : kind
  val name : string
  val kind_name : string
end

(* One implementation of the ffmpeg content types per process: two would
   register the same kinds, and the second to run would win a race nobody
   meant to hold. *)
let implementation = ref None

let implement name =
  match !implementation with
    | Some other when other <> name ->
        failwith
          (Printf.sprintf
             "FFmpeg content types are implemented by %s here: %s cannot be \
              linked with it!"
             other name)
    | _ -> implementation := Some name

module Make (P : Params) (N : Names) = struct
  let () = implement P.implementation

  module Specs = struct
    type kind = N.kind
    type params = P.t

    let name = N.name
    let kind = N.kind
    let string_of_kind _ = N.kind_name
    let kind_of_string s = if s = N.kind_name then Some N.kind else None
    let string_of_params = P.to_string
    let parse_param = P.parse
    let merge = P.merge
    let compatible = P.compatible
    let default_params _ = P.default

    (* A dump carries the stream type; what libav read off a codec does not
       travel, and a reader that has no libav could not rebuild it. *)
    let serialize_params _ = ""
    let parse_params = function "" -> Some P.default | _ -> None
    let content_lang_typ = Liquidsoap_lang.Lang_core.string_t
    let params_to_value p = Liquidsoap_lang.Lang_core.string (P.to_string p)
  end

  module Format = MkFormatBase (Specs)
  include Format

  let kind = lift_kind N.kind

  let () =
    Type.register_type N.kind_name (fun () ->
        Type.make
          (Type.Custom
             (Format_type.kind_handler (kind, Liquidsoap_lang.Lang.univ_t ()))))
end
