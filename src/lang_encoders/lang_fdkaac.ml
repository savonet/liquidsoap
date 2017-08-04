(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang_values
open Lang_encoders

let make params =
  let defaults =
    { Encoder.FdkAacEnc.
        afterburner    = false;
        aot            = `Mpeg_4 `HE_AAC_v2;
        bitrate        = 64;
        bitrate_mode   = `Constant;
        channels       = 2;
        samplerate     = 44100;
        sbr_mode       = false;
        transmux       = `Adts }
  in
  let valid_samplerates = [
    8000;  11025; 12000; 16000; 22050; 24000; 32000;
    44100; 48000; 64000; 88200; 96000 ]
  in
  let valid_vbr = [1;2;3;4;5] in
  let fdkaac =
    List.fold_left
      (fun f ->
        function
          | ("afterburner",{ term = Bool b; _}) ->
              { f with Encoder.FdkAacEnc.afterburner = b }
          | ("aot",({ term = String s; _} as t)) ->
              let aot = try Encoder.FdkAacEnc.aot_of_string s with
                | Not_found -> raise (Error (t,"invalid aot value"))
              in
              { f with Encoder.FdkAacEnc.aot = aot }
          | ("vbr",({ term = Int i; _} as t)) ->
              if not (List.mem i valid_vbr) then
               begin
                let err =
                  Printf.sprintf "invalid vbr mode. Possible values: %s"
                  (String.concat ", "
                    (List.map string_of_int valid_vbr))
                in
                raise (Error (t,err));
               end;
              { f with Encoder.FdkAacEnc.bitrate_mode = `Variable i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Encoder.FdkAacEnc.bitrate = i }
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.FdkAacEnc.channels = i }
          | ("samplerate",({ term = Int i; _} as t)) ->
              if not (List.mem i valid_samplerates) then
               begin
                let err =
                  Printf.sprintf "invalid samplerate value. Possible values: %s"
                  (String.concat ", "
                    (List.map string_of_int valid_samplerates))
                in
                raise (Error (t,err));
               end;
              { f with Encoder.FdkAacEnc.samplerate = i }
          | ("sbr_mode",{ term = Bool b; _}) ->
              { f with Encoder.FdkAacEnc.sbr_mode = b }
          | ("transmux",({ term = String s; _} as t)) ->
              let transmux = try Encoder.FdkAacEnc.transmux_of_string s with
                | Not_found -> raise (Error (t,"invalid transmux value"))
              in
              { f with Encoder.FdkAacEnc.transmux = transmux }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Encoder.FdkAacEnc.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Encoder.FdkAacEnc.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
  let aot = fdkaac.Encoder.FdkAacEnc.aot in
    if aot = `Mpeg_4 `HE_AAC_v2 || aot = `Mpeg_2 `HE_AAC_v2 then
      if fdkaac.Encoder.FdkAacEnc.channels <> 2 then
        failwith "HE-AAC v2 is only available with 2 channels.";
    Encoder.FdkAacEnc fdkaac
