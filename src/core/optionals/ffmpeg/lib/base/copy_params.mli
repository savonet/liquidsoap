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

    Every comparison lists the {!differences} of two parameter sets and keeps
    those whose property its caller does not allow to change: the type merge
    with what the track's containers follow, or {!relaxed_allowed} under
    [settings.ffmpeg.content.copy.relaxed_compatibility_check]; the encoder, at
    a source switch, with what its own container carries in band. Descriptive
    differences ({!descriptive_differences}) are only reported. *)

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

(** The same parameters with that coded side data, a new value. *)
val with_side_data : t -> Avcodec.Packet_side_data.raw list -> t

val side_data : t -> Avcodec.Packet_side_data.raw list
val media_name : t -> string
val codec_name : t -> string

(** Whether the codec's descriptor says intra-only; a codec without a descriptor
    and a subtitle codec count as intra-only. *)
val intra_only : t -> bool

(** Whether the stream declares a reorder delay (B-frames). *)
val has_reordering : t -> bool

val bitstream_form : t -> Liquidsoap_ffmpeg_formats.Copy_changes.form

(** [settings.ffmpeg.content.copy.relaxed_compatibility_check]. *)
val relaxed : unit -> bool

(** [(property, old value, new value)] for each compared property that differs,
    in table order: media, codec, bitstream form, the media's geometry, then the
    header details. Frame rate is not compared. *)
val differences :
  t ->
  t ->
  (Liquidsoap_ffmpeg_formats.Copy_changes.property * string * string) list

(** The properties the relaxed setting lets change: all but media and codec. *)
val relaxed_allowed : Liquidsoap_ffmpeg_formats.Copy_changes.property -> bool

(** The first difference whose property is not [allowed], named with both
    values; [None] when there is none. *)
val incompatibility :
  allowed:(Liquidsoap_ffmpeg_formats.Copy_changes.property -> bool) ->
  t ->
  t ->
  string option

(** Only header details differ, or under the relaxed setting media and codec
    agree: what lets one decoder go on across the two (the inline decoder). *)
val compatible : t -> t -> bool

(** The left side, with the properties it leaves unset taken from the right. The
    caller checks {!incompatibility} first. *)
val merge : t -> t -> t

(** [(property, old value, new value)] for each descriptive property that
    differs. *)
val descriptive_differences : t -> t -> (string * string * string) list

val to_string : t -> string

(** Same value, or the same printed parameters and extradata. *)
val equal : t -> t -> bool
