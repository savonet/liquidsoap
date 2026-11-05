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

(** The properties of a copied stream a source can differ in (content.md §3.1),
    named as [Copy_params] names them. *)
type property =
  [ `Media
  | `Codec
  | `Bitstream_form
  | `Sample_rate
  | `Channel_layout
  | `Sample_format
  | `Width
  | `Height
  | `Pixel_format
  | `Extradata
  | `Profile
  | `Level
  | `Block_align ]

(** The form a codec's configuration travels in: inside the stream ([`Annex_b]
    H.264 or HEVC, [`Adts] AAC) or in the codec parameters alone
    ([`Length_prefixed], [`Raw_aac]); [`Other] for codecs with a single form. *)
type form = [ `Annex_b | `Length_prefixed | `Adts | `Raw_aac | `Other ]

(** The name copy parameters print for a form. *)
val form_name : form -> string

(** A muxer name, or [`Unknown] for a [%ffmpeg] format without a literal
    [format]. *)
type container = [ `Named of string | `Unknown ]

(** The containers the encoders of a copied track write (content.md §3.1.1). The
    encoder typing sets them from a [%ffmpeg] format ({!of_container}), a merge
    takes the union, and a source may change only what every one of them allows
    ({!allowed}). An empty list is a track no encoder constrains: nothing may
    change. *)
type t = container list

val default : t
val merge : t -> t -> t
val of_container : string option -> t

(** [containers=mpegts+matroska], [?] for an unknown container; empty for no
    container. *)
val to_string : t -> string

(** The [containers] value of an [ffmpeg.copy] type, as {!to_string} prints it.
*)
val value : t -> string

val parse_param : string -> string -> t option
val parse_serialized : string -> t option

(** What [container] does when a source of this [codec] and bitstream [form]
    changes [property]: carries the change in the stream ([`In_band]), follows
    it with a new container ([`Restart], Ogg chaining, which the encoder does
    for stream and file outputs only), or cannot ([`No]). A user-visible
    property or a codec header changes only where the table lists it, as its
    conformance tests decode; block align, which users do not see, changes
    anywhere and FFmpeg judges it (content.md §3.1.1). *)
val rule :
  container:container ->
  codec:string ->
  form:form ->
  property ->
  [ `In_band | `Restart | `No ]

(** Every container of [t] follows a change of [property]. *)
val allowed : t -> codec:string -> form:form -> property -> bool
