(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

open Value
open Ground

let type_of_encoder p = Encoder.audio_type (Lang_encoder.channels_of_params p)

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
        | "samplerate", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "bitrate", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.mode = Vorbis_format.CBR i }
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | "channels", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.fill = Some i }
        | t -> Lang_encoder.raise_generic_error t)
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
        | "samplerate", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "bitrate", `Value { value = Ground (Int i); _ } ->
            let x, _, y = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (x, Some i, y) }
        | "max_bitrate", `Value { value = Ground (Int i); _ } ->
            let x, y, _ = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (x, y, Some i) }
        | "min_bitrate", `Value { value = Ground (Int i); _ } ->
            let _, x, y = get_rates f in
            { f with Vorbis_format.mode = Vorbis_format.ABR (Some i, x, y) }
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | "channels", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.fill = Some i }
        | t -> Lang_encoder.raise_generic_error t)
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
        | "samplerate", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.samplerate = Lazy.from_val i }
        | "quality", `Value { value = Ground (Float q); pos } ->
            if q < -0.2 || q > 1. then
              Lang_encoder.raise_error ~pos "quality should be in [(-0.2)..1]";
            { f with Vorbis_format.mode = Vorbis_format.VBR q }
        | "quality", `Value { value = Ground (Int i); pos } ->
            if i <> 0 && i <> 1 then
              Lang_encoder.raise_error ~pos "quality should be in [-(0.2)..1]";
            let q = float i in
            { f with Vorbis_format.mode = Vorbis_format.VBR q }
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Vorbis_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Vorbis_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Vorbis_format.channels = 2 }
        | "channels", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.channels = i }
        | "bytes_per_page", `Value { value = Ground (Int i); _ } ->
            { f with Vorbis_format.fill = Some i }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Ogg_format.Vorbis vorbis

let () =
  let make p = Encoder.Ogg { Ogg_format.audio = Some (make p); video = None } in
  let make_abr p =
    Encoder.Ogg { Ogg_format.audio = Some (make_abr p); video = None }
  in
  let make_cbr p =
    Encoder.Ogg { Ogg_format.audio = Some (make_cbr p); video = None }
  in
  Lang_encoder.register "vorbis" type_of_encoder make;
  Lang_encoder.register "vorbis.abr" type_of_encoder make_abr;
  Lang_encoder.register "vorbis.cbr" type_of_encoder make_cbr
