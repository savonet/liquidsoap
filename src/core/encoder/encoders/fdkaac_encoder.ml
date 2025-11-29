(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
      `Samplerate (Lazy.force params.Fdkaac_format.samplerate);
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
  let samplerate = Lazy.force aac.Fdkaac_format.samplerate in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let n = Utils.pagesize in
  let buf = Strings.Mutable.empty () in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample samplerate_converter
        (dst_freq /. src_freq) b 0 len
    in
    let encoded = Strings.Mutable.empty () in
    Strings.Mutable.add buf (Audio.S16LE.make b start len);
    while Strings.Mutable.length buf >= n do
      let data = Bytes.create n in
      Strings.blit (Strings.sub (Strings.Mutable.to_strings buf) 0 n) data 0;
      let data = Bytes.unsafe_to_string data in
      Strings.Mutable.drop buf n;
      Strings.Mutable.add encoded (Fdkaac.Encoder.encode enc data 0 n)
    done;
    Strings.Mutable.to_strings encoded
  in
  let stop () =
    let rem =
      Strings.Mutable.map
        (fun rem ofs len ->
          let rem = Fdkaac.Encoder.encode enc rem ofs len in
          (rem, 0, String.length rem))
        buf
    in
    Strings.Mutable.add rem (Fdkaac.Encoder.flush enc);
    Strings.Mutable.to_strings rem
  in
  {
    Encoder.encode_metadata = (fun _ -> ());
    header = (fun () -> Strings.empty);
    hls = Encoder_utils.mk_id3_hls ~pos encode;
    encode;
    stop;
  }

let () =
  Plug.register Encoder.plug "fdkaac" ~doc:"" (function
    | Encoder.FdkAacEnc m -> Some (fun ?hls:_ ~pos _ _ -> encoder ~pos m)
    | _ -> None)
