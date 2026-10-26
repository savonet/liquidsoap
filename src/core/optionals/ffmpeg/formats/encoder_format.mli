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

(** The [%ffmpeg(...)] format record (encoder-format.md §1), and what follows
    from it without libav: track names (§2), media types (§2.1), the channel
    rule (§5), the content type (§10), rendering (§11), extension and MIME
    (§12).

    The type checker ({!Encoder_typing}) and the runtime parser (libav side)
    both reduce a format to a list of {!shape}s and type it with {!fields_type},
    so the type a script is checked against is the type the encoder is given.
    What needs libav reaches this module through {!lookups}, which the libav
    side fills and a process without it leaves at its fallback. *)

type value =
  [ `String of string | `Int of int | `Int64 of Int64.t | `Float of float ]

(** Options in written order, a later key replacing an earlier one. *)
type opts = (string * value) list

type media = [ `Audio | `Video | `Subtitle ]
type pcm_kind = [ `Pcm | `Pcm_s16 | `Pcm_f32 ]

(** An audio track. [written] names the parameters the format sets itself: on a
    raw track the others come from the first frame. *)
type audio = {
  pcm_kind : pcm_kind;
  channels : int;
  channel_layout : string option;
  samplerate : int Lazy.Mutexed.t;
  sample_format : string option;
  written : [ `Channels | `Samplerate ] list;
}

type hwaccel = [ `Auto | `None | `Internal | `Device | `Frame ]

type video = {
  framerate : int Lazy.Mutexed.t;
  width : int Lazy.Mutexed.t;
  height : int Lazy.Mutexed.t;
  pixel_format : string option;
  alpha : bool;
  hwaccel : hwaccel;
  hwaccel_device : string option;
  hwaccel_pixel_format : string option;
}

(** [text_to_ass]: [None] is the default [(i, text)] dialogue builder. *)
type subtitle = { text_to_ass : Value.t option }

type options = [ `Audio of audio | `Video of video | `Subtitle of subtitle ]

type encoded = {
  mode : [ `Internal | `Raw ];
  codec : string option;
  options : options;
  opts : opts;
}

type keyframe = [ `Wait_for_keyframe | `Ignore_keyframe ]
type track = [ `Drop | `Copy of keyframe | `Encode of encoded ]

type t = {
  format : string option;
  mime_type : string option;
  output : [ `Stream | `Url of string ];
  streams : (string * track) list;
  interleaved : [ `Default | `True | `False ];
  metadata : (string * string) list;
  opts : opts;
}

(** [set opts key v] replaces [key] in place, or appends it. *)
val set : opts -> string -> value -> opts

(** [authoritative] is [true] once libav fills the record: an unknown layout
    name is then an error rather than an unconstrained track. *)
type lookups = {
  authoritative : bool;
  channels_of_layout : string -> int option;
  pixel_format_has_alpha : string -> bool option;
  codec_media : string -> media option;
}

(** Fallback: the layout table of §10.1, no alpha, no codec media. *)
val lookups : lookups ref

(** [split_track_name name] is the field and mode of [%name] (§2).

    @raise Failure naming the written suffix. *)
val split_track_name : string -> string * [ `Internal | `Raw | `Copy | `Drop ]

(** §2.1, first match wins: a marker among [markers] (in precedence order audio,
    video, subtitle), then the field name, then [codec] when it is a literal. *)
val media_type :
  markers:media list -> field:string -> codec:string option -> media option

val media_of_marker : string -> media option

(** Whether the field name alone gives the media type (§2.1 rule 2). *)
val media_of_field : string -> media option

type shape =
  [ `Untyped
  | `Copy
  | `Raw_audio
  | `Raw_video
  | `Audio of pcm_kind * int
  | `Video of bool
  | `Subtitle ]

val shape_of_track : track -> shape

(** §10. A field written twice raises [Failure] naming it. *)
val fields_type : (string * shape) list -> Type.t Fields.t

val type_of_format : t -> Type.t Fields.t
val to_string : t -> string
val extension : t -> string option
val mime : t -> string

(** The width and height of the single internal or raw video track. *)
val video_size : t -> (int * int) option

val default_text_to_ass : int -> string -> string
