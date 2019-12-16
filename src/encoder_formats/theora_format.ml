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

type bitrate_control = Quality of int | Bitrate of int

type t = {
  (* TODO: framerate ! *)
  bitrate_control : bitrate_control;
  width : int Lazy.t;
  height : int Lazy.t;
  picture_width : int Lazy.t;
  picture_height : int Lazy.t;
  picture_x : int;
  picture_y : int;
  aspect_numerator : int;
  aspect_denominator : int;
  keyframe_frequency : int;
  vp3_compatible : bool option;
  soft_target : bool;
  buffer_delay : int option;
  speed : int option;
  fill : int option;
}

let bit_ctl_to_string bit_ctl =
  match bit_ctl with
    | Quality x -> Printf.sprintf "quality=%d" x
    | Bitrate x -> Printf.sprintf "bitrate=%d" x

let print_some_bool v x =
  match x with None -> "" | Some x -> Printf.sprintf "%s=%b" v x

let print_some_int v x =
  match x with None -> "" | Some x -> Printf.sprintf "%s=%i" v x

let to_string th =
  let f = Lazy.force in
  Printf.sprintf
    "%%theora(%s,width=%d,height=%d,picture_width=%d,picture_height=%d,picture_x=%d,picture_y=%d,aspect_numerator=%d,aspect_denominator=%d,keyframe_frequence=%d,%s,soft_target=%b,%s,%s)"
    (bit_ctl_to_string th.bitrate_control)
    (f th.width) (f th.height) (f th.picture_width) (f th.picture_height)
    th.picture_x th.picture_y th.aspect_numerator th.aspect_denominator
    th.keyframe_frequency
    (print_some_bool "vp3_compatible" th.vp3_compatible)
    th.soft_target
    (print_some_int "buffer_delay" th.buffer_delay)
    (print_some_int "speed" th.speed)
