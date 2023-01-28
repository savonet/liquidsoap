(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let kind_of_encoder p = Encoder.audio_kind (Lang_encoder.channels_of_params p)

let make params =
  let valid_samplerates =
    [
      8000;
      11025;
      12000;
      16000;
      22050;
      24000;
      32000;
      44100;
      48000;
      64000;
      88200;
      96000;
    ]
  in
  let check_samplerate ~pos i =
    lazy
      (let i = Lazy.force i in
       if not (List.mem i valid_samplerates) then (
         let err =
           Printf.sprintf "invalid samplerate value. Possible values: %s"
             (String.concat ", " (List.map string_of_int valid_samplerates))
         in
         Lang_encoder.raise_error ~pos err);
       i)
  in
  let defaults =
    {
      Fdkaac_format.afterburner = false;
      aot = `Mpeg_4 `HE_AAC_v2;
      bandwidth = `Auto;
      bitrate = 64;
      bitrate_mode = `Constant;
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *)
      channels = 2;
      samplerate = check_samplerate ~pos:None Frame.audio_rate;
      sbr_mode = false;
      transmux = `Adts;
    }
  in
  let valid_vbr = [1; 2; 3; 4; 5] in
  let fdkaac =
    List.fold_left
      (fun f -> function
        | "afterburner", `Value { value = Ground (Bool b); _ } ->
            { f with Fdkaac_format.afterburner = b }
        | "aot", `Value { value = Ground (String s); pos } ->
            let aot =
              try Fdkaac_format.aot_of_string s
              with Not_found ->
                Lang_encoder.raise_error ~pos "invalid aot value"
            in
            { f with Fdkaac_format.aot }
        | "vbr", `Value { value = Ground (Int i); pos } ->
            if not (List.mem i valid_vbr) then (
              let err =
                Printf.sprintf "invalid vbr mode. Possible values: %s"
                  (String.concat ", " (List.map string_of_int valid_vbr))
              in
              Lang_encoder.raise_error ~pos err);
            { f with Fdkaac_format.bitrate_mode = `Variable i }
        | "bandwidth", `Value { value = Ground (Int i); _ } ->
            { f with Fdkaac_format.bandwidth = `Fixed i }
        | "bandwidth", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "auto" ->
            { f with Fdkaac_format.bandwidth = `Auto }
        | "bitrate", `Value { value = Ground (Int i); _ } ->
            { f with Fdkaac_format.bitrate = i }
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Fdkaac_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Fdkaac_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Fdkaac_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Fdkaac_format.channels = 2 }
        | "channels", `Value { value = Ground (Int i); _ } ->
            { f with Fdkaac_format.channels = i }
        | "samplerate", `Value { value = Ground (Int i); pos } ->
            {
              f with
              Fdkaac_format.samplerate = check_samplerate ~pos (Lazy.from_val i);
            }
        | "sbr_mode", `Value { value = Ground (Bool b); _ } ->
            { f with Fdkaac_format.sbr_mode = b }
        | "transmux", `Value { value = Ground (String s); pos } ->
            let transmux =
              try Fdkaac_format.transmux_of_string s
              with Not_found ->
                Lang_encoder.raise_error ~pos "invalid transmux value"
            in
            { f with Fdkaac_format.transmux }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Fdkaac_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Fdkaac_format.channels = 2 }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  let aot = fdkaac.Fdkaac_format.aot in
  if aot = `Mpeg_4 `HE_AAC_v2 || aot = `Mpeg_2 `HE_AAC_v2 then
    if fdkaac.Fdkaac_format.channels <> 2 then
      failwith "HE-AAC v2 is only available with 2 channels.";
  Encoder.FdkAacEnc fdkaac

let () = Lang_encoder.register "fdkaac" kind_of_encoder make
