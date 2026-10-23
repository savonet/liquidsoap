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

type t = {
  write : Avutil.audio Avutil.frame -> unit;
  reframer : Avfilter.Utils.audio_converter option;
  mutable written : int;
  mutable fed : int;
}

let create ~frame_size ~(params : Avfilter.Utils.audio_params) ~start write =
  let reframer =
    Option.map
      (fun size ->
        Avfilter.Utils.init_audio_converter ~out_frame_size:size
          ~in_time_base:{ Avutil.num = 1; den = params.sample_rate }
          ~in_params:params ())
      frame_size
  in
  { write; reframer; written = start; fed = start }

let write t frame =
  Avutil.Frame.set_pts frame (Some (Int64.of_int t.written));
  t.written <- t.written + Avutil.Audio.frame_nb_samples frame;
  t.write frame

let feed t frame =
  let samples = Avutil.Audio.frame_nb_samples frame in
  if samples > 0 then (
    Avutil.Frame.set_pts frame (Some (Int64.of_int t.fed));
    t.fed <- t.fed + samples;
    match t.reframer with
      | None -> write t frame
      | Some r -> Avfilter.Utils.convert_audio r (write t) (`Frame frame))

(* The partial frame the re-framer holds ends where the jump lands. *)
let jump t target =
  if target > t.fed then (
    t.written <- target - (t.fed - t.written);
    t.fed <- target)

let flush t =
  Option.iter
    (fun r -> Avfilter.Utils.convert_audio r (write t) `Flush)
    t.reframer
