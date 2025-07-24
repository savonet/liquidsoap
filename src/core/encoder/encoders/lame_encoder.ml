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

(** MP3 encoder *)

open Encoder
open Mp3_format

let () =
  let create_encoder mp3 =
    let enc = Lame.create_encoder () in
    (* Input settings *)
    Lame.set_in_samplerate enc (Lazy.force Frame.audio_rate);
    Lame.set_num_channels enc (if mp3.Mp3_format.stereo then 2 else 1);

    (* Internal quality *)
    Lame.set_quality enc mp3.Mp3_format.internal_quality;

    (* Output settings *)
    if not mp3.Mp3_format.stereo then Lame.set_mode enc Lame.Mono
    else (
      match mp3.Mp3_format.stereo_mode with
        | Mp3_format.Default -> ()
        | Mp3_format.Stereo -> Lame.set_mode enc Lame.Stereo
        | Mp3_format.Joint_stereo -> Lame.set_mode enc Lame.Joint_stereo);
    begin
      let apply_constaints enc
          {
            Mp3_format.quality;
            mean_bitrate;
            min_bitrate;
            max_bitrate;
            hard_min;
          } =
        let f (s, v) = match v with Some v -> s enc v | None -> () in
        f (Lame.set_vbr_hard_min, hard_min);
        List.iter f
          [
            (Lame.set_vbr_quality, quality);
            (Lame.set_vbr_mean_bitrate, mean_bitrate);
            (Lame.set_vbr_min_bitrate, min_bitrate);
            (Lame.set_vbr_max_bitrate, max_bitrate);
          ]
      in
      match mp3.Mp3_format.bitrate_control with
        | Mp3_format.VBR c ->
            Lame.set_vbr_mode enc Lame.Vbr_mtrh;
            apply_constaints enc c
        | Mp3_format.CBR br -> Lame.set_brate enc br
        | Mp3_format.ABR c ->
            Lame.set_vbr_mode enc Lame.Vbr_abr;
            apply_constaints enc c
    end;
    Lame.set_out_samplerate enc (Lazy.force mp3.Mp3_format.samplerate);
    Lame.set_bWriteVbrTag enc false;
    Lame.init_params enc;
    enc
  in
  let mp3_encoder ~pos mp3 metadata =
    let enc = create_encoder mp3 in
    let channels = if mp3.Mp3_format.stereo then 2 else 1 in
    (* Lame accepts data of a fixed length.. *)
    let frame_size = Frame.main_of_audio (Lame.frame_size enc) in
    let pending_data =
      Generator.create
        (Frame.Fields.make
           ~audio:(Content.Audio.format_of_channels channels)
           ())
    in
    let encoded = Strings.Mutable.empty () in
    ignore
      (Option.map
         (fun version ->
           Strings.Mutable.add encoded
             (Utils.id3v2_of_metadata ~version
                (Frame.Metadata.to_list
                   (Frame.Metadata.Export.to_metadata metadata))))
         mp3.id3v2);
    let encode frame =
      Generator.put pending_data Frame.Fields.audio
        (Frame.get frame Frame.Fields.audio);
      while Generator.length pending_data > frame_size do
        let pcm =
          Content.Audio.get_data
            (Frame.Fields.find Frame.Fields.audio
               (Generator.slice pending_data frame_size))
        in
        Strings.Mutable.add encoded
          (if channels = 1 then
             Lame.encode_buffer_float_part enc pcm.(0) pcm.(0) 0
               (Array.length pcm.(0))
           else
             Lame.encode_buffer_float_part enc pcm.(0) pcm.(1) 0
               (Array.length pcm.(0)))
      done;
      Strings.Mutable.flush encoded
    in
    let stop () = Strings.of_string (Lame.encode_flush enc) in
    {
      encode_metadata = (fun _ -> ());
      hls = Encoder_utils.mk_id3_hls ~pos encode;
      encode;
      header = (fun () -> Strings.empty);
      stop;
    }
  in
  Plug.register Encoder.plug "lame" ~doc:"LAME mp3 encoder." (function
    | Encoder.MP3 mp3 ->
        Some (fun ?hls:_ ~pos _ meta -> mp3_encoder ~pos mp3 meta)
    | _ -> None)
