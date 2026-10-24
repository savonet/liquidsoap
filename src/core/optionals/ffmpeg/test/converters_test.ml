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

open Liquidsoap_ffmpeg
module Converter = Ffmpeg_samplerate_converter

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let channels = 2
let rate () = float (Lazy.Mutexed.force Frame.audio_rate)

let tone n =
  Array.init channels (fun _ -> Array.init n (fun i -> sin (float i /. 10.)))

let converted t ratio n =
  let _, _, length = Converter.convert t ratio (tone n) 0 n in
  length

let flushed t = Array.length (Converter.flush t).(0)

(* Converts [n] samples at each ratio in order, then flushes. *)
let total ratios n =
  let t = Converter.create channels in
  let count =
    List.fold_left (fun count r -> count + converted t r n) 0 ratios
  in
  count + flushed t

(* Per-rate count: the ratio a converter honours is round(in × r) / in. *)
let expected n ratio =
  Float.round (float n *. Float.round (rate () *. ratio) /. rate ())

let close ~tolerance count target =
  Float.abs (float count -. target) <= float tolerance

(* converters.md §2: n samples at r, then flush. *)
let fixed_ratio ratio =
  let n = 10000 in
  let count = total [ratio] n in
  check
    (Printf.sprintf "fixed ratio %g: %d" ratio count)
    (close ~tolerance:1 count (expected n ratio))

(* converters.md §2: a ratio change loses no samples. *)
let ratio_change r1 r2 =
  let n = 10000 in
  let count = total [r1; r2] n in
  check
    (Printf.sprintf "ratio change %g -> %g: %d" r1 r2 count)
    (close ~tolerance:2 count (expected n r1 +. expected n r2))

(* converters.md §2: ratio 1. after another ratio returns the held samples
   first; a twin converter flushed at the same point gives them. *)
let held_first () =
  let n = 4410 in
  let twin = Converter.create channels in
  ignore (converted twin 2. n);
  let held = Converter.flush twin in
  let t = Converter.create channels in
  ignore (converted t 2. n);
  let data, offset, length = Converter.convert t 1. (tone n) 0 n in
  let held_length = Array.length held.(0) in
  check "samples are held" (held_length > 0);
  check "held samples come first"
    (length >= held_length
    && Array.for_all2 (fun h d -> Array.sub d offset held_length = h) held data
    )

let () =
  Frame_settings.lazy_config_eval := true;
  List.iter fixed_ratio [0.5; 2.; 48000. /. 44100.; 1.];
  ratio_change 0.5 2.;
  ratio_change (48000. /. 44100.) 1.;
  held_first ();
  let expected = 8 in
  if !checks <> expected then
    failwith (Printf.sprintf "ran %d checks, expected %d" !checks expected);
  Printf.printf "converters_test: %d checks passed\n" !checks
