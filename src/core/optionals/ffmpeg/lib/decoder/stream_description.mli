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

(** The description of an opened input container (stream-description.md): its
    streams, the field each one gets, its specific format name, the line logged
    for it, and the content type it yields for a requested one.

    The probe and the decoder derive their fields from one description, so a
    field of the content type {!get_type} returns names the stream the decoder
    fills under it. *)

type media =
  [ `Audio of Avutil.audio Avcodec.params
  | `Video of Avutil.video Avcodec.params * Avutil.rational option
  | `Subtitle of Avutil.subtitle Avcodec.params
  | `Data of [ `Data ] Avcodec.params ]

type stream = {
  field : Frame.field;
  index : int;
  time_base : Avutil.rational;
  media : media;
}

type t = { format : string option; streams : stream list }

(** Describes the container; streams whose parameters cannot be read are left
    out with a warning, keeping their rank.

    @raise Failure when no stream is described. *)
val container :
  ?format:(Avutil.input, _) Avutil.format ->
  url:string ->
  Avutil.input Avutil.container ->
  t

(** The specific container format name of §4. *)
val specific_format : metadata:(string * string) list -> string -> string

val describe : t -> string

(** The copy parameters of a stream (content.md §3.1). *)
val copy_params : stream -> Copy_params.t option

(** [get_type ~ctype container] maps the container onto the consuming source's
    own content type, merging copy parameters into its formats.

    @raise Failure naming the differing property when a copy merge fails. *)
val get_type : ctype:Frame.content_type -> t -> Frame.content_type

val log : Log.t
