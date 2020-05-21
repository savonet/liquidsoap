(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type bitrate_control = Quality of int | Vbr of int | Abr of int
type mode = Narrowband | Wideband | Ultra_wideband

type t = {
  bitrate_control : bitrate_control;
  samplerate : int Lazy.t;
  stereo : bool;
  mode : mode;
  frames_per_packet : int;
  complexity : int option;
  fill : int option;
  dtx : bool;
  vad : bool;
}

let string_of_br_ctl x =
  match x with
    | Vbr x -> Printf.sprintf "vbr,quality=%d" x
    | Abr x -> Printf.sprintf "abr,bitrate=%d" x
    | Quality x -> Printf.sprintf "quality=%d" x

let string_of_mode x =
  match x with
    | Narrowband -> "narrowband"
    | Wideband -> "widebande"
    | Ultra_wideband -> "ultra-wideband"

let string_of_complexity x =
  match x with None -> "" | Some x -> Printf.sprintf ",complexity=%d" x

let to_string m =
  Printf.sprintf
    "%%speex(%s,%s,samplerate=%d,mode=%s,frames_per_packet=%d%s,dtx=%B,vad=%B)"
    (Encoder_formats.string_of_stereo m.stereo)
    (string_of_br_ctl m.bitrate_control)
    (Lazy.force m.samplerate) (string_of_mode m.mode) m.frames_per_packet
    (string_of_complexity m.complexity)
    m.dtx m.vad
