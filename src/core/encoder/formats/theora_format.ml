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

type bitrate_control = Quality of int | Bitrate of int

type t = {
  (* TODO: framerate ! *)
  bitrate_control : bitrate_control;
  width : Deferred.t;
  height : Deferred.t;
  picture_width : Deferred.t;
  picture_height : Deferred.t;
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

(** The first picture constraint of the Theora header [th] breaks, as
    [(parameter, message)]. [known] gives a deferred value, or [None] when it is
    unknown yet: a constraint that needs one is skipped. The constraints are
    libtheora's: [pic_x <= min(width - pic_width, 255)],
    [pic_y <= height - pic_height] and [height - pic_height - pic_y <= 255]. *)
let picture_error ~known th =
  (* Equal deferred values cancel without being known. *)
  let diff a b =
    if a = b then Some 0
    else (
      match (known a, known b) with Some a, Some b -> Some (a - b) | _ -> None)
  in
  let breaks constr = function Some v -> not (constr v) | None -> false in
  let margin_x = diff th.width th.picture_width in
  let margin_y = diff th.height th.picture_height in
  if breaks (fun m -> m >= 0) margin_x then
    Some ("picture_width", "picture width must not be larger than width")
  else if breaks (fun m -> m >= 0) margin_y then
    Some ("picture_height", "picture height must not be larger than height")
  else if breaks (fun m -> th.picture_x <= min m 255) margin_x then
    Some
      ( "picture_x",
        "picture x must not be larger than width - picture width or 255, \
         whichever is smaller" )
  else if breaks (fun m -> th.picture_y <= m) margin_y then
    Some
      ("picture_y", "picture y must not be larger than height - picture height")
  else if breaks (fun m -> m - th.picture_y <= 255) margin_y then
    Some
      ( "picture_y",
        "height - picture height - picture y must not be larger than 255" )
  else None

let bit_ctl_to_string bit_ctl =
  match bit_ctl with
    | Quality x -> Printf.sprintf "quality=%d" x
    | Bitrate x -> Printf.sprintf "bitrate=%d" x

let print_some_bool v x =
  match x with None -> "" | Some x -> Printf.sprintf "%s=%b" v x

let print_some_int v x =
  match x with None -> "" | Some x -> Printf.sprintf "%s=%i" v x

let to_string th =
  let f = Deferred.peek in
  Printf.sprintf "%%theora(%s)"
    (String.concat ","
       (List.filter
          (fun s -> s <> "")
          [
            bit_ctl_to_string th.bitrate_control;
            Printf.sprintf "width=%d" (f th.width);
            Printf.sprintf "height=%d" (f th.height);
            Printf.sprintf "picture_width=%d" (f th.picture_width);
            Printf.sprintf "picture_height=%d" (f th.picture_height);
            Printf.sprintf "picture_x=%d" th.picture_x;
            Printf.sprintf "picture_y=%d" th.picture_y;
            Printf.sprintf "aspect_numerator=%d" th.aspect_numerator;
            Printf.sprintf "aspect_denominator=%d" th.aspect_denominator;
            Printf.sprintf "keyframe_frequency=%d" th.keyframe_frequency;
            print_some_bool "vp3_compatible" th.vp3_compatible;
            Printf.sprintf "soft_target=%b" th.soft_target;
            print_some_int "buffer_delay" th.buffer_delay;
            print_some_int "speed" th.speed;
          ]))
