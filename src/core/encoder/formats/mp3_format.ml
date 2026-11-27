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

let samplerates = [8000; 11025; 12000; 16000; 22050; 24000; 32000; 44100; 48000]
let valid_samplerate i = List.mem i samplerates

(* MPEG-1 Layer III, then MPEG-2 and 2.5, whose rates are a half and a
   quarter of MPEG-1's. *)
let bitrates_of_samplerate samplerate =
  if List.mem samplerate [32000; 44100; 48000] then
    [32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320]
  else [8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160]

(* The first bit rate the rate's MPEG version does not have, with the list it
   has. *)
let bitrate_error ~samplerate bitrates =
  let allowed = bitrates_of_samplerate samplerate in
  Option.map
    (fun bitrate ->
      Printf.sprintf
        "bitrate %d is not a bit rate of the %d Hz MPEG version: %s" bitrate
        samplerate
        (String.concat ", " (List.map string_of_int allowed)))
    (List.find_opt (fun bitrate -> not (List.mem bitrate allowed)) bitrates)

type t = {
  stereo : bool;
  stereo_mode : stereo_mode;
  bitrate_control : bitrate_control;
  internal_quality : int;
  samplerate : Deferred.t;
  id3v2 : int option Encoder_formats.adaptable;
}

let to_string m =
  let name =
    match m.bitrate_control with
      | VBR _ -> "%mp3.vbr"
      | ABR _ -> "%mp3.abr"
      | CBR _ -> "%mp3"
  in
  Printf.sprintf "%s(%s,%s,samplerate=%d%s)" name
    (Encoder_formats.string_of_stereo m.stereo)
    (string_of_bitrate_control m.bitrate_control)
    (Deferred.peek m.samplerate)
    (Encoder_formats.string_of_id3v2 m.id3v2)

let bitrate m =
  match m.bitrate_control with
    | VBR _ -> raise Not_found
    | CBR n -> n * 1000
    | ABR c -> Option.get c.mean_bitrate * 1000

let bitrates m =
  match m.bitrate_control with
    | CBR bitrate -> [bitrate]
    | ABR c | VBR c ->
        List.filter_map Fun.id [c.min_bitrate; c.mean_bitrate; c.max_bitrate]

(* The checks of the whole configuration: bit rates against the rate's MPEG
   version, when the rate is known, and their order. *)
let check ~samplerate m =
  let order =
    match m.bitrate_control with
      | CBR _ -> None
      | ABR c | VBR c ->
          let ordered =
            List.filter_map Fun.id
              [c.min_bitrate; c.mean_bitrate; c.max_bitrate]
          in
          if List.sort Int.compare ordered = ordered then None
          else Some "min_bitrate <= bitrate <= max_bitrate does not hold"
  in
  match
    Option.bind samplerate (fun samplerate ->
        bitrate_error ~samplerate (bitrates m))
  with
    | Some error -> Some error
    | None -> order
