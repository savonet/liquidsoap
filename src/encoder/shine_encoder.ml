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

(** Fixed-point MP3 encoder *)

open Shine_format
module G = Generator.Generator

let create_encoder ~samplerate ~bitrate ~channels =
  Shine.create { Shine.channels; samplerate; bitrate }

let encoder shine =
  let channels = shine.channels in
  let samplerate = Lazy.force shine.samplerate in
  let enc = create_encoder ~samplerate ~bitrate:shine.bitrate ~channels in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  (* Shine accepts data of a fixed length.. *)
  let samples = Shine.samples_per_pass enc in
  let data = Audio.create channels samples in
  let buf = G.create () in
  let encoded = Strings.Mutable.empty () in
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
      else (Audio.copy b, start, len)
    in
    G.put buf b start len;
    while G.length buf > samples do
      let l = G.get buf samples in
      let f (b, o, o', l) =
        Audio.blit (Audio.sub b o l) (Audio.sub data o' l)
      in
      List.iter f l;
      Strings.Mutable.add encoded
        (Shine.encode_buffer enc (Audio.to_array data))
    done;
    Strings.Mutable.flush encoded
  in
  let stop () = Strings.of_string (Shine.flush enc) in
  {
    Encoder.insert_metadata = (fun _ -> ());
    header = Strings.empty;
    encode;
    stop;
  }

let () =
  Encoder.plug#register "SHINE" (function
    | Encoder.Shine m -> Some (fun _ _ -> encoder m)
    | _ -> None)
