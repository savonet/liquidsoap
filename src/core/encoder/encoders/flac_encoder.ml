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

(** FLAC encoder *)

let encoder flac meta =
  let comments =
    Frame.Metadata.to_list (Frame.Metadata.Export.to_metadata meta)
  in
  let channels = flac.Flac_format.channels in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let samplerate = Lazy.force flac.Flac_format.samplerate in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let p =
    {
      Flac.Encoder.channels;
      bits_per_sample = flac.Flac_format.bits_per_sample;
      sample_rate = samplerate;
      compression_level = Some flac.Flac_format.compression;
      total_samples = None;
    }
  in
  let buf = Strings.Mutable.empty () in
  let write chunk = Strings.Mutable.add_bytes buf chunk in
  let enc = Flac.Encoder.create ~comments ~write p in
  let enc = ref enc in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample samplerate_converter
        (dst_freq /. src_freq) b 0 len
    in
    let b = Audio.sub b start len in
    Flac.Encoder.process !enc b;
    Strings.Mutable.flush buf
  in
  let stop () =
    Flac.Encoder.finish !enc;
    Strings.Mutable.flush buf
  in
  {
    Encoder.encode_metadata = (fun _ -> ());
    (* Flac encoder do not support header
     * for now. It will probably never do.. *)
    header = (fun () -> Strings.empty);
    hls = Encoder.dummy_hls encode;
    encode;
    stop;
  }

let () =
  Plug.register Encoder.plug "flac" ~doc:"Flac encoder." (function
    | Encoder.Flac m -> Some (fun ?hls:_ ~pos:_ _ -> encoder m)
    | _ -> None)
