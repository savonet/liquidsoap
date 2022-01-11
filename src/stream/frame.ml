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

include Frame_base
open Content

(** Data types *)

(** High-level description of the content. *)
type kind =
  [ `Any | `Internal | `Kind of Content.kind | `Format of Content.format ]

let none = `Format None.format
let audio_pcm = `Kind Audio.kind

let audio_n = function
  | 0 -> none
  | c ->
      `Format
        Audio.(
          lift_params
            {
              Contents.channel_layout =
                lazy (Audio_converter.Channel_layout.layout_of_channels c);
            })

let audio_mono =
  `Format Audio.(lift_params { Contents.channel_layout = lazy `Mono })

let audio_stereo =
  `Format Audio.(lift_params { Contents.channel_layout = lazy `Stereo })

let video_yuva420p = `Kind Video.kind
let midi_native = `Kind Midi.kind
let midi_n c = `Format Midi.(lift_params { Contents.channels = c })

type content_kind = kind fields

(** Precise description of the channel types for the current track. *)
type content_type = format fields

(** Compatibilities between content kinds, types and values.
  * [sub a b] if [a] is more permissive than [b]..
  * TODO this is the other way around... it's correct in Lang, phew! *)

let map_fields fn c =
  { audio = fn c.audio; video = fn c.video; midi = fn c.midi }

let string_of_format = string_of_format

let string_of_kind = function
  | `Any -> "any"
  | `Internal -> "internal"
  | `Format f -> string_of_format f
  | `Kind k -> string_of_kind k

let string_of_fields fn { audio; video; midi } =
  Printf.sprintf "{audio=%s,video=%s,midi=%s}" (fn audio) (fn video) (fn midi)

let string_of_content_kind = string_of_fields string_of_kind
let string_of_content_type = string_of_fields string_of_format

let compatible c c' =
  Content.compatible c.audio c'.audio
  && Content.compatible c.video c'.video
  && Content.compatible c.midi c'.midi

(* Frames *)

let metadata_of_list l =
  let m = Hashtbl.create (List.length l) in
  List.iter (fun (k, v) -> Hashtbl.add m k v) l;
  m

type t = {
  (* Presentation time, in multiple of frame size. *)
  mutable pts : int64;
  mutable content : Content.data;
}

(** Create a content chunk. All chunks have the same size. *)
let create_content ctype =
  Content.make ~size:0 (Content.Frame.lift_params ctype)

let create ctype = { pts = 0L; content = create_content ctype }

let dummy () =
  let data = Content.None.format in
  {
    pts = 0L;
    content = create_content { audio = data; video = data; midi = data };
  }

let content { content } = content
let append b content = b.content <- Content.append b.content content
let audio { content } = Content.Frame.get_audio content
let set_audio { content } = Content.Frame.set_audio content
let video { content } = Content.Frame.get_video content
let set_video { content } = Content.Frame.set_video content
let midi { content } = Content.Frame.get_midi content
let set_midi { content } = Content.Frame.set_midi content

let content_type frame =
  map_fields format
    { audio = audio frame; video = video frame; midi = midi frame }

(** Content independent *)

(* TODO: historically, track_marks are ordered with most recent first. *)
let track_marks { content } = List.rev (Content.Frame.get_track_marks content)
let position { content } = Content.length content
let is_partial b = position b < !!size
let set_track_marks { content } = Content.Frame.set_track_marks content
let add_track_mark { content } = Content.Frame.add_track_mark content
let clear (b : t) = b.content <- Content.sub b.content 0 0

(* Same as clear but leaves the last metadata at position -1. *)
let advance b = b.pts <- Int64.succ b.pts

(** Presentation time stuff. *)

let pts { pts } = pts
let set_pts frame pts = frame.pts <- pts

(** Metadata stuff *)

let get_all_metadata { content } = Content.Frame.get_all_metadata content
let set_all_metadata { content } = Content.Frame.set_all_metadata content
let set_metadata { content } = Content.Frame.set_metadata content
let get_metadata { content } = Content.Frame.get_metadata content
let free_metadata { content } = Content.Frame.free_metadata content
let free_all_metadata { content } = Content.Frame.free_all_metadata content
