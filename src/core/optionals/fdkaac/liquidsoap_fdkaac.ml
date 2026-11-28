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

(** FDK-AAC encoder *)

let create_encoder ~pos params =
  let encoder = Fdkaac.Encoder.create params.Fdkaac_format.channels in
  let bandwidth =
    match params.Fdkaac_format.bandwidth with `Auto -> 0 | `Fixed b -> b
  in
  let params =
    [
      `Aot params.Fdkaac_format.aot;
      `Bandwidth bandwidth;
      `Samplerate (Deferred.force params.Fdkaac_format.samplerate);
      `Transmux params.Fdkaac_format.transmux;
      `Afterburner params.Fdkaac_format.afterburner;
    ]
    @ (if params.Fdkaac_format.aot = `Mpeg_4 `AAC_ELD then
         [`Sbr_mode params.Fdkaac_format.sbr_mode]
       else [])
    @
      match params.Fdkaac_format.bitrate_mode with
      | `Variable vbr -> [`Bitrate_mode (`Variable vbr)]
      | `Constant -> [`Bitrate (params.Fdkaac_format.bitrate * 1000)]
  in
  let string_of_param = function
    | `Aot _ -> "aot"
    | `Afterburner _ -> "afterburner"
    | `Bandwidth _ -> "bandwidth"
    | `Bitrate _ -> "bitrate"
    | `Bitrate_mode _ -> "bitrate mode"
    | `Granule_length _ -> "granule length"
    | `Samplerate _ -> "samplerate"
    | `Sbr_mode _ -> "sbr mode"
    | `Transmux _ -> "transmux"
  in
  let set p =
    try Fdkaac.Encoder.set encoder p with
      | Fdkaac.Encoder.Invalid_config ->
          Lang_encoder.raise_error ~pos
            ("Invalid configuration: " ^ string_of_param p)
      | Fdkaac.Encoder.Unsupported_parameter ->
          Lang_encoder.raise_error ~pos
            ("Unsupported parameter: " ^ string_of_param p)
      | e -> raise e
  in
  List.iter set params;
  encoder

let encoder ~pos aac =
  let enc = create_encoder ~pos aac in
  let channels = aac.Fdkaac_format.channels in
  let samplerate = Deferred.force aac.Fdkaac_format.samplerate in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  (* The library takes any number of samples: it holds less than a frame. *)
  (* Encodes samples at the stream's rate: those of a frame, and at the end
     those the rate converter still holds. *)
  let encode_converted b start len =
    if len = 0 then Strings.empty
    else (
      let data = Audio.S16LE.make b start len in
      Strings.of_string (Fdkaac.Encoder.encode enc data 0 (String.length data)))
  in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample samplerate_converter
        (dst_freq /. src_freq) b 0 len
    in
    encode_converted b start len
  in
  let encode_drained () =
    let tail = Audio_converter.Samplerate.drain samplerate_converter in
    encode_converted tail 0 (Mm.Audio.length tail)
  in
  let stop () =
    let tail = encode_drained () in
    Strings.append tail (Strings.of_string (Fdkaac.Encoder.flush enc))
  in
  let bitrate () =
    match aac.Fdkaac_format.bitrate_mode with
      | `Variable _ -> None
      | `Constant -> (
          match Fdkaac.Encoder.get enc `Bitrate with
            | `Bitrate bitrate -> Some bitrate
            | _ -> None)
  in
  {
    Encoder.encode_metadata = (fun _ -> ());
    header = (fun () -> Strings.empty);
    encode;
    stop;
    bitrate;
    access_points = None;
    restart_offset = (fun () -> None);
  }

let () =
  Plug.register Encoder.plug "fdkaac" ~doc:""
    (Encoder.of_encoders (function
      | Encoder.FdkAacEnc m -> Some (fun ~pos _ _ -> encoder ~pos m)
      | _ -> None))
