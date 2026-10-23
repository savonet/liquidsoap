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

(** Known [ffmpeg.copy] parameters (content.md §3.1): the codec parameters of
    one coded stream, plus its average frame rate (video) or time base
    (subtitles), and the properties a merge fills in.

    The type merge compares type properties ({!incompatibility}); the encoder
    compares header details ({!header_compatible}) and reports descriptive
    differences ({!descriptive_differences}) at a source switch. Both read
    [settings.ffmpeg.content.copy.relaxed_compatibility_check] at each call. *)

type media =
  [ `Audio of Avutil.audio Avcodec.params
  | `Video of Avutil.video Avcodec.params
  | `Subtitle of Avutil.subtitle Avcodec.params ]

type t = private {
  media : media;
  frame_rate : Avutil.rational option;
  time_base : Avutil.rational option;
  pixel_format : Avutil.Pixel_format.t option;
  sample_format : Avutil.Sample_format.t option;
  profile : int option;
  level : int option;
}

val make :
  ?frame_rate:Avutil.rational -> ?time_base:Avutil.rational -> media -> t

val media_name : t -> string
val codec_name : t -> string

(** Whether the codec's descriptor says intra-only; a codec without a descriptor
    and a subtitle codec count as intra-only. *)
val intra_only : t -> bool

(** Whether the stream declares a reorder delay (B-frames). *)
val has_reordering : t -> bool

val bitstream_form : t -> string

(** The first type property that differs, in table order, with both values;
    [None] when the two are compatible. Under the relaxed setting only media and
    codec are compared. *)
val incompatibility : t -> t -> string option

val compatible : t -> t -> bool

(** The left side, with the properties it leaves unset taken from the right.

    @raise Failure naming the first incompatible property. *)
val merge : t -> t -> t

(** Header details (extradata, profile, level, block align, bits per coded
    sample) all compatible; [true] under the relaxed setting. *)
val header_compatible : t -> t -> bool

(** [(property, old value, new value)] for each descriptive property that
    differs. *)
val descriptive_differences : t -> t -> (string * string * string) list

val to_string : t -> string

(** Same value, or the same printed parameters and extradata. *)
val equal : t -> t -> bool
