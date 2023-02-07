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

open Mm

(** WAV encoder *)

open Wav_format

let encoder wav =
  let channels = wav.channels in
  let sample_rate = Multicore.force wav.samplerate in
  let sample_size = wav.samplesize in
  let ratio = float sample_rate /. float (Multicore.force Frame.audio_rate) in
  let converter = Audio_converter.Samplerate.create channels in
  let len =
    match wav.duration with
      | None -> None
      | Some d ->
          Some
            (int_of_float
               (d *. float channels *. float sample_rate *. float sample_size
              /. 8.))
  in
  let header =
    Wav_aiff.wav_header ?len ~channels ~sample_rate ~sample_size ()
  in
  let need_header = ref wav.header in
  let encode frame start len =
    let b = AFrame.pcm frame in
    let start = Frame.audio_of_main start in
    let len = Frame.audio_of_main len in
    (* Resample if needed. *)
    let b, start, len =
      Audio_converter.Samplerate.resample converter ratio b start len
    in
    let s = Bytes.create (sample_size / 8 * len * channels) in
    let of_audio =
      match sample_size with
        | 32 -> fun buf s off -> Audio.S32LE.of_audio buf s off
        | 24 -> fun buf s off -> Audio.S24LE.of_audio buf s off
        | 16 -> Audio.S16LE.of_audio
        | 8 -> fun buf s off -> Audio.U8.of_audio buf s off
        | _ -> failwith "unsupported sample size"
    in
    of_audio b start s 0 len;
    let s = Bytes.unsafe_to_string s in
    if !need_header then (
      need_header := false;
      Strings.of_list [header; s])
    else Strings.of_string s
  in
  let hls =
    {
      Encoder.init_encode = (fun f o l -> (None, encode f o l));
      split_encode = (fun f o l -> `Ok (Strings.empty, encode f o l));
      codec_attrs = (fun () -> None);
      bitrate = (fun () -> None);
      video_size = (fun () -> None);
    }
  in
  {
    Encoder.insert_metadata = (fun _ -> ());
    hls;
    encode;
    header = Strings.of_string header;
    stop = (fun () -> Strings.empty);
  }

let () =
  Plug.register Encoder.plug "wav" ~doc:"Native wav encoder." (function
    | Encoder.WAV w -> Some (fun _ _ -> encoder w)
    | _ -> None)
