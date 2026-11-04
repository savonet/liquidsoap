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

(** What the encoders of a copied track require of its sources' header details
    (extradata, profile, level; content.md §3.1.1). [`In_band]: every encoder
    follows a change, so a source may bring other ones. [`Stored]: an encoder
    cannot, so they must match the stream's. [`Unspecified]: no encoder said,
    which the merge at open treats as [`Stored]. The encoder typing sets the
    requirement of each [%ffmpeg] copy track from its container
    ({!of_container}). *)
type t = [ `Unspecified | `In_band | `Stored ]

val default : t

(** The stricter of the two: [`Stored], else [`In_band], else [`Unspecified]. *)
val merge : t -> t -> t

(** Whether a merge under this requirement compares header details. *)
val headers : t -> [ `Compare | `Ignore ]

(** [header=in_band] or [header=stored]; empty for [`Unspecified]. *)
val to_string : t -> string

(** The [header] annotation of an [ffmpeg.copy] type. *)
val parse_param : string -> string -> t option

val parse_serialized : string -> t option

(** Whether the muxer carries codec headers inside the stream (MPEG-TS, ADTS,
    Annex B elementary streams, MP3). *)
val carries_header_in_band : string -> bool

(** [`In_band] for a container that carries header details in band or chains to
    follow them (Ogg); [`Stored] otherwise, and when the container is not named.
*)
val of_container : string option -> t
