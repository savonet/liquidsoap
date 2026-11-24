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

(** Fixed-point MP3 encoder *)

open Shine_format

let create_encoder ~samplerate ~bitrate ~channels =
  Shine.create { Shine.channels; samplerate; bitrate }

(* Samples of delay shine adds before its input. *)
let shine_delay = 528

(* Samples of delay of an mp3 decoder's synthesis filter. *)
let mp3_decoder_delay = 529

let encoder ~pos shine =
  let channels = shine.channels in
  let samplerate = Deferred.force shine.samplerate in
  let enc = create_encoder ~samplerate ~bitrate:shine.bitrate ~channels in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  (* Shine accepts data of a fixed length.. *)
  let samples = Frame.main_of_audio (Shine.samples_per_pass enc) in
  let buf =
    Generator.create
      (Frame.Fields.make ~audio:(Content.Audio.format_of_channels channels) ())
  in
  let encoded = Strings.Mutable.empty () in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample samplerate_converter
        (dst_freq /. src_freq) b 0 len
    in
    let start = Frame.main_of_audio start in
    let len = Frame.main_of_audio len in
    Generator.put buf Frame.Fields.audio
      (Content.sub (Content.Audio.lift_data b) start len);
    while Generator.length buf > samples do
      let pcm =
        Content.Audio.get_data
          (Frame.Fields.find Frame.Fields.audio (Generator.slice buf samples))
      in
      Strings.Mutable.add encoded (Shine.encode_buffer enc pcm)
    done;
    Strings.Mutable.flush encoded
  in
  let stop () =
    let pass = Shine.samples_per_pass enc in
    let remaining = Generator.length buf in
    if remaining > 0 then begin
      let pcm =
        Content.Audio.get_data
          (Frame.Fields.find Frame.Fields.audio (Generator.slice buf remaining))
      in
      let padded =
        Array.map
          (fun channel ->
            let padded = Array.make pass 0. in
            Array.blit channel 0 padded 0 (min pass (Array.length channel));
            padded)
          pcm
      in
      Strings.Mutable.add encoded (Shine.encode_buffer enc padded)
    end;
    (* Workaround: shine's flush leaves its last [shine_delay] samples in the
       encoder, and decoders output [mp3_decoder_delay] samples late: silent
       passes push both out, adding up to a frame of silence. To drop once
       shine's flush returns its samples. *)
    for _ = 1 to (shine_delay + mp3_decoder_delay + pass - 1) / pass do
      Strings.Mutable.add encoded
        (Shine.encode_buffer enc
           (Array.init channels (fun _ -> Array.make pass 0.)))
    done;
    Strings.Mutable.add encoded (Shine.flush enc);
    Strings.Mutable.flush encoded
  in
  {
    Encoder.encode_metadata = (fun _ -> ());
    header = (fun () -> Strings.empty);
    encode;
    stop;
    bitrate = (fun () -> None);
    encode_into = None;
    restart_offset = (fun () -> None);
  }

let () =
  Plug.register Encoder.plug "shine" ~doc:"SHINE fixed-point mp3 encoder."
    (function
    | Encoder.Shine m -> Some (fun ~pos _ _ -> encoder ~pos m)
    | _ -> None)
