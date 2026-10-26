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

open Mm

module Resampler =
  Swresample.Make (Swresample.PlanarFloatArray) (Swresample.PlanarFloatArray)

let log = Log.make ["ffmpeg"; "samplerate"]

type t = {
  channels : int;
  layout : Avutil.Channel_layout.t;
  in_rate : int;
  faults : Faults.t;
  mutable resampler : (int * Resampler.t) option;
}

let create channels =
  {
    channels;
    layout = Avutil.Channel_layout.get_default channels;
    in_rate = Lazy.Mutexed.force Frame.audio_rate;
    faults = Faults.create log;
    resampler = None;
  }

let empty t = Array.make t.channels [||]
let append = Array.map2 Array.append

let drain t =
  match t.resampler with None -> empty t | Some (_, r) -> Resampler.flush r

let out_rate t ratio =
  max 1 (Float.to_int (Float.round (float t.in_rate *. ratio)))

(* Returns the resampler for [out] and the samples of the one it replaces. *)
let resampler t out =
  match t.resampler with
    | Some (rate, r) when rate = out -> (r, empty t)
    | _ ->
        let drained = drain t in
        let r = Resampler.create t.layout t.in_rate t.layout out in
        t.resampler <- Some (out, r);
        (r, drained)

(* A failed call drops its samples and the resampler; counting the drop is
   the caller's. *)
let guarded t f =
  try f ()
  with Avutil.Error err ->
    t.resampler <- None;
    Faults.uncounted t.faults ~kind:"resample"
      (Printf.sprintf "Samplerate conversion failed, samples dropped: %s"
         (Avutil.string_of_error err));
    empty t

let convert t ratio data offset length =
  let data =
    guarded t (fun () ->
        let r, drained = resampler t (out_rate t ratio) in
        append drained (Resampler.convert ~offset ~length r data))
  in
  (data, 0, Audio.length data)

let flush t = guarded t (fun () -> drain t)

let () =
  Plug.register Audio_converter.Samplerate.converters "ffmpeg"
    ~doc:"Resample audio with FFmpeg's swresample." (fun channels ->
      convert (create channels))
