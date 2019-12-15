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

open Lang_values
open Lang_encoders

let make_cbr params =
  let defaults =
    {
      Vorbis_format.mode = Vorbis_format.CBR 128;
      fill = None;
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *)
      channels = 2;
      samplerate = Frame.audio_rate;
    }
  in
  let vorbis =
    List.fold_left
      (fun f -> function
        | "samplerate", { term = Int i; _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "bitrate", { term = Int i; _ } ->
            { f with Vorbis_format.mode = Vorbis_format.CBR i }
        | "channels", { term = Int i; _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", { term = Int i; _ } ->
            { f with Vorbis_format.fill = Some i }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | _, t -> raise (generic_error t))
      defaults params
  in
  Ogg_format.Vorbis vorbis

let make_abr params =
  let defaults =
    {
      Vorbis_format.mode = Vorbis_format.ABR (None, None, None);
      channels = 2;
      fill = None;
      samplerate = Frame.audio_rate;
    }
  in
  let get_rates x =
    match x.Vorbis_format.mode with
      | Vorbis_format.ABR (x, y, z) -> (x, y, z)
      | _ -> assert false
  in
  let vorbis =
    List.fold_left
      (fun f -> function
        | "samplerate", { term = Int i; _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "bitrate", { term = Int i; _ } ->
            let x, _, y = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (x, Some i, y) }
        | "max_bitrate", { term = Int i; _ } ->
            let x, y, _ = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (x, y, Some i) }
        | "min_bitrate", { term = Int i; _ } ->
            let _, x, y = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (Some i, x, y) }
        | "channels", { term = Int i; _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", { term = Int i; _ } ->
            { f with Vorbis_format.fill = Some i }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | _, t -> raise (generic_error t))
      defaults params
  in
  Ogg_format.Vorbis vorbis

let make params =
  let defaults =
    {
      Vorbis_format.mode = Vorbis_format.VBR 0.3;
      channels = 2;
      fill = None;
      samplerate = Frame.audio_rate;
    }
  in
  let vorbis =
    List.fold_left
      (fun f -> function
        | "samplerate", { term = Int i; _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "quality", ({ term = Float q; _ } as t) ->
            if q < -0.2 || q > 1. then
              raise (Error (t, "quality should be in [(-0.2)..1]"));
            { f with Vorbis_format.mode = Vorbis_format.VBR q }
        | "quality", ({ term = Int i; _ } as t) ->
            if i <> 0 && i <> 1 then
              raise (Error (t, "quality should be in [-(0.2)..1]"));
            let q = float i in
            { f with Vorbis_format.mode = Vorbis_format.VBR q }
        | "channels", { term = Int i; _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", { term = Int i; _ } ->
            { f with Vorbis_format.fill = Some i }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | _, t -> raise (generic_error t))
      defaults params
  in
  Ogg_format.Vorbis vorbis
