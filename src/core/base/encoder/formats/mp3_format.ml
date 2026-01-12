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

type stereo_mode = Default | Stereo | Joint_stereo

type bitrate_constraints = {
  quality : int option;
  min_bitrate : int option;
  mean_bitrate : int option;
  max_bitrate : int option;
  hard_min : bool option;
}

let string_of_bitrate_constraints
    { quality; min_bitrate; mean_bitrate; max_bitrate; hard_min } =
  let hard_min =
    match hard_min with None -> "" | Some b -> Printf.sprintf "hard_min=%b" b
  in
  let f (v, x) =
    match x with Some x -> Printf.sprintf "%s=%d" v x | None -> ""
  in
  String.concat ","
    (List.filter
       (fun s -> s <> "")
       (List.map f
          [
            ("quality", quality);
            ("bitrate", mean_bitrate);
            ("min_bitrate", min_bitrate);
            ("max_bitrate", max_bitrate);
          ]
       @ [hard_min]))

type bitrate_control =
  | ABR of bitrate_constraints
  | VBR of bitrate_constraints
  | CBR of int

let string_of_bitrate_control = function
  | ABR c | VBR c -> string_of_bitrate_constraints c
  | CBR br -> Printf.sprintf "bitrate=%d" br

type t = {
  stereo : bool;
  stereo_mode : stereo_mode;
  bitrate_control : bitrate_control;
  internal_quality : int;
  samplerate : int Lazy.t;
  id3v2 : int option;
}

let to_string m =
  let name =
    match m.bitrate_control with
      | VBR _ -> "%mp3.vbr"
      | ABR _ -> "%mp3.abr"
      | CBR _ -> "%mp3"
  in
  Printf.sprintf "%s(%s,%s,samplerate=%d,id3v2=%s)" name
    (Encoder_formats.string_of_stereo m.stereo)
    (string_of_bitrate_control m.bitrate_control)
    (Lazy.force m.samplerate)
    (match m.id3v2 with None -> "none" | Some v -> string_of_int v)

let bitrate m =
  match m.bitrate_control with
    | VBR _ -> raise Not_found
    | CBR n -> n * 1000
    | ABR c -> Option.get c.mean_bitrate * 1000
