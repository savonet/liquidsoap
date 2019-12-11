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

(** FLAC encoder *)

let encoder flac meta =
  let comments = Utils.list_of_metadata (Meta_format.to_metadata meta) in
  let channels = flac.Flac_format.channels in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let samplerate = Lazy.force flac.Flac_format.samplerate in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let p =
    {
      Flac.Encoder.channels;
      bits_per_sample= flac.Flac_format.bits_per_sample;
      sample_rate= samplerate;
      compression_level= Some flac.Flac_format.compression;
      total_samples= None;
    }
  in
  let buf = Strings.Mutable.empty () in
  let write chunk = Strings.Mutable.add_bytes buf chunk in
  let cb = Flac.Encoder.get_callbacks write in
  let enc = Flac.Encoder.create ~comments p cb in
  let enc = ref enc in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    let b, start, len =
      if src_freq <> dst_freq then (
        let b =
          Audio_converter.Samplerate.resample samplerate_converter
            (dst_freq /. src_freq) (Audio.sub b start len)
        in
        (b, 0, Audio.length b) )
      else (b, start, len)
    in
    let b = Audio.sub b start len in
    let b = Audio.to_array b in
    Flac.Encoder.process !enc cb b ;
    Strings.Mutable.flush buf
  in
  let stop () =
    Flac.Encoder.finish !enc cb ;
    Strings.Mutable.flush buf
  in
  {
    Encoder.insert_metadata= (fun _ -> ());
    (* Flac encoder do not support header
     * for now. It will probably never do.. *)
    header= Strings.empty;
    encode;
    stop;
  }

let () =
  Encoder.plug#register "FLAC" (function
    | Encoder.Flac m ->
        Some (fun _ -> encoder m)
    | _ ->
        None)
