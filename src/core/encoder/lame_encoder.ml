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

(** MP3 encoder *)

open Encoder
open Mp3_format

let bit_at s pos =
  let byte_pos = min (String.length s) (pos / 8) in
  let byte = int_of_char s.[byte_pos] in
  let bit_pos = 7 - (pos mod 8) in
  (byte land (1 lsl bit_pos)) lsr bit_pos == 1

type id3v2 = Waiting | Rendered of Strings.t | Done

(* Notation: XYZ; X: copyright bit, Y: original bit, Z: private bit
 *           !: negation
 *
 * Coding: 1XY
 *         0ZT
 *         1UV
 *         etc..
 *
 * Synchronisation: at end of character:
 *         0XY
 *         1ZT
 *         1!Z!T <- Mark end
 *         0UV
 *
 * Or:     1XY
 *         0ZT
 *         0!Z!T <- Mark end
 *         1UV
 *
 * At beginning, previous bit is assumed to be 010 (Lame's default).
 * Thus, initial synchronisation bit is 001.
 *
 * Note: messages are strings. Hence, length is always even :-) *)

let state = ref false

(* Set s!T!Z, negating s _after_ *)
let sync enc =
  Lame.set_copyright enc !state;
  Lame.set_original enc (not (Lame.get_original enc));
  Lame.set_private enc (not (Lame.get_private enc));
  state := !state

(* Set sXY, negating s _before_ *)
let bset enc x y =
  state := not !state;
  Lame.set_copyright enc !state;
  Lame.set_original enc x;
  Lame.set_private enc y

let () =
  let create_encoder mp3 =
    let enc = Lame.create_encoder () in
    (* Input settings *)
    Lame.set_in_samplerate enc (SyncLazy.force Frame.audio_rate);
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
    Lame.set_out_samplerate enc (SyncLazy.force mp3.Mp3_format.samplerate);
    Lame.init_params enc;
    enc
  in
  let mp3_encoder mp3 metadata =
    let enc = create_encoder mp3 in
    let id3v2 = ref Waiting in
    let has_started = ref false in
    let position = ref 0 in
    let msg_position = ref 0 in
    let msg_interval = Frame.audio_of_seconds mp3.Mp3_format.msg_interval in
    let msg = Printf.sprintf "%s%c" mp3.Mp3_format.msg '\000' in
    let msg_len = String.length msg * 8 in
    let is_sync = ref true in
    sync enc;
    let channels = if mp3.Mp3_format.stereo then 2 else 1 in
    let encode frame start len =
      let b = AFrame.pcm frame in
      let len = Frame.audio_of_main len in
      let start = Frame.audio_of_main start in
      position := !position + len;
      if mp3.Mp3_format.msg <> "" && !position > msg_interval then (
        match !is_sync with
          | false ->
              sync enc;
              is_sync := true
          | true ->
              position := 0;
              bset enc (bit_at msg !msg_position)
                (bit_at msg (!msg_position + 1));
              msg_position := (!msg_position + 2) mod msg_len;
              if !msg_position mod 8 = 0 then is_sync := false);
      let encoded () =
        has_started := true;

        if channels = 1 then
          Lame.encode_buffer_float_part enc b.(0) b.(0) start len
        else Lame.encode_buffer_float_part enc b.(0) b.(1) start len
      in
      match !id3v2 with
        | Rendered s when not !has_started ->
            id3v2 := Done;
            Strings.add s (encoded ())
        | _ -> Strings.of_string (encoded ())
    in
    let stop () = Strings.of_string (Lame.encode_flush enc) in
    let insert_metadata =
      match mp3.id3v2 with
        | Some f -> (
            fun (* Only insert metadata at the beginning.. *)
                  m ->
              match !id3v2 with
                | Waiting ->
                    if not (Meta_format.is_empty m) then
                      id3v2 := Rendered (Strings.of_string (f m))
                | _ -> ())
        | None -> fun _ -> ()
    in
    (* Try to insert initial metadata now.. *)
    insert_metadata metadata;
    let hls =
      {
        Encoder.init_encode = (fun f o l -> (None, encode f o l));
        split_encode = (fun f o l -> `Ok (Strings.empty, encode f o l));
        codec_attrs = (fun () -> None);
        bitrate = (fun () -> None);
        video_size = (fun () -> None);
      }
    in
    { insert_metadata; hls; encode; header = Strings.empty; stop }
  in
  Plug.register Encoder.plug "lame" ~doc:"LAME mp3 encoder." (function
    | Encoder.MP3 mp3 -> Some (fun _ meta -> mp3_encoder mp3 meta)
    | _ -> None)
