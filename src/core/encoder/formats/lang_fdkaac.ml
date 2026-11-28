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

open Value

let make params =
  let check_samplerate ~pos i =
    if not (Fdkaac_format.valid_samplerate i) then
      Format_builders.raise_error ~pos
        (Printf.sprintf "invalid samplerate value. Possible values: %s"
           (String.concat ", "
              (List.map string_of_int Fdkaac_format.samplerates)))
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
      samplerate = `Setting `Audio_rate;
      sbr_mode = false;
      transmux = `Adts;
    }
  in
  let valid_vbr = [1; 2; 3; 4; 5] in
  let fdkaac =
    List.fold_left
      (fun f -> function
        | `Labelled ("afterburner", Value.Bool { value = b; _ }) ->
            { f with Fdkaac_format.afterburner = b }
        | `Labelled ("aot", Value.String { value = s; pos }) ->
            let aot =
              try Fdkaac_format.aot_of_string s
              with Not_found ->
                Format_builders.raise_error ~pos "invalid aot value"
            in
            { f with Fdkaac_format.aot }
        | `Labelled ("vbr", Value.Int { value = i; pos }) ->
            if not (List.mem i valid_vbr) then (
              let err =
                Printf.sprintf "invalid vbr mode. Possible values: %s"
                  (String.concat ", " (List.map string_of_int valid_vbr))
              in
              Format_builders.raise_error ~pos err);
            { f with Fdkaac_format.bitrate_mode = `Variable i }
        | `Labelled ("bandwidth", Value.Int { value = i; _ }) ->
            { f with Fdkaac_format.bandwidth = `Fixed i }
        | `Labelled ("bandwidth", String { value = s; _ })
          when String.lowercase_ascii s = "auto" ->
            { f with Fdkaac_format.bandwidth = `Auto }
        | `Labelled ("bitrate", Value.Int { value = i; _ }) ->
            { f with Fdkaac_format.bitrate = i }
        | `Labelled ("stereo", Value.Bool { value = b; _ }) ->
            { f with Fdkaac_format.channels = (if b then 2 else 1) }
        | `Labelled ("mono", Value.Bool { value = b; _ }) ->
            { f with Fdkaac_format.channels = (if b then 1 else 2) }
        | `Labelled ("channels", Value.Int { value = i; _ }) ->
            { f with Fdkaac_format.channels = i }
        | `Labelled ("samplerate", Value.Int { value = i; pos }) ->
            check_samplerate ~pos i;
            { f with Fdkaac_format.samplerate = `Value i }
        | `Labelled ("sbr_mode", Value.Bool { value = b; _ }) ->
            { f with Fdkaac_format.sbr_mode = b }
        | `Labelled ("transmux", Value.String { value = s; pos }) ->
            let transmux =
              try Fdkaac_format.transmux_of_string s
              with Not_found ->
                Format_builders.raise_error ~pos "invalid transmux value"
            in
            { f with Fdkaac_format.transmux }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Fdkaac_format.channels = 1 }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Fdkaac_format.channels = 2 }
        | t -> Format_builders.raise_generic_error t)
      defaults params
  in
  let error = Format_builders.raise_error ~pos:None in
  let given label =
    List.exists (function `Labelled (l, _) -> l = label | _ -> false) params
  in
  if given "bitrate" && given "vbr" then
    error "bitrate and vbr are two rate controls: give one";
  let { Fdkaac_format.aot; channels; transmux; _ } = fdkaac in
  if not (List.mem channels [1; 2; 6]) then
    error "%fdkaac takes 1, 2 or 6 channels";
  if (aot = `Mpeg_4 `HE_AAC_v2 || aot = `Mpeg_2 `HE_AAC_v2) && channels <> 2
  then error "HE-AAC v2 is only available with 2 channels";
  (match transmux with
    | `Raw | `Latm | `Latm_out_of_band ->
        error
          "this transport is not a byte stream an output can carry: use adts, \
           loas or adif, or %ffmpeg"
    | (`Adts | `Adif)
      when given "transmux" && (aot = `Mpeg_4 `AAC_LD || aot = `Mpeg_4 `AAC_ELD)
      ->
        error "low-delay object types are carried by loas"
    | `Loas when match aot with `Mpeg_2 _ -> true | _ -> false ->
        error "MPEG-2 object types are not carried by loas"
    | _ -> ());
  let fdkaac =
    (* Low-delay object types are carried by LOAS by default. *)
    if
      (not (given "transmux"))
      && (aot = `Mpeg_4 `AAC_LD || aot = `Mpeg_4 `AAC_ELD)
    then { fdkaac with Fdkaac_format.transmux = `Loas }
    else fdkaac
  in
  Format_value.FdkAacEnc fdkaac

let () = Format_builders.register "fdkaac" make
