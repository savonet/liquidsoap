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

(** Xing/LAME "Info" frame: an empty first frame carrying the encoder delay,
    which decoders trim to play the stream gapless. *)

let bitrates_mpeg1 =
  [| 32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320 |]

let bitrates_mpeg2 =
  [| 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160 |]

(* Version bits, samplerate index. *)
let version_of_samplerate = function
  | 44100 -> (0b11, 0)
  | 48000 -> (0b11, 1)
  | 32000 -> (0b11, 2)
  | 22050 -> (0b10, 0)
  | 24000 -> (0b10, 1)
  | 16000 -> (0b10, 2)
  | 11025 -> (0b00, 0)
  | 12000 -> (0b00, 1)
  | 8000 -> (0b00, 2)
  | sr -> failwith (Printf.sprintf "Invalid mp3 samplerate: %d" sr)

(* The fields after the side info: tag id, flags, then the LAME extension. *)
let tag_length = 4 + 4 + 36

(** [frame ~vbr ~bitrate ~samplerate ~channels ~delay ()] is an Info frame
    announcing an encoder delay of [delay] samples. [bitrate], in kbps, is used
    when the frame fits in it, otherwise the smallest bitrate that fits.

    ponytail: no frame count, so decoders trim the start but not the final
    frame's padding (at most one frame of silence at the end); a count needs the
    file rewritten at close. *)
let frame ~vbr ~bitrate ~samplerate ~channels ~delay () =
  let version, samplerate_index = version_of_samplerate samplerate in
  let mpeg1 = version = 0b11 in
  let bitrates = if mpeg1 then bitrates_mpeg1 else bitrates_mpeg2 in
  let side_info =
    match (mpeg1, channels) with
      | true, 1 -> 17
      | true, _ -> 32
      | false, 1 -> 9
      | false, _ -> 17
  in
  let frame_length index =
    (if mpeg1 then 144000 else 72000) * bitrates.(index) / samplerate
  in
  let fits index = frame_length index >= 4 + side_info + tag_length in
  let bitrate_index =
    match
      List.find_opt
        (fun index -> bitrates.(index) = bitrate && fits index)
        (List.init (Array.length bitrates) Fun.id)
    with
      | Some index -> index
      | None -> List.find fits (List.init (Array.length bitrates) Fun.id)
  in
  let length = frame_length bitrate_index in
  let b = Bytes.make length '\000' in
  let mode = if channels = 1 then 0b11 else 0b00 in
  (* Sync, version, layer III, no CRC; bitrate, samplerate; channel mode. *)
  Bytes.set_uint8 b 0 0xff;
  Bytes.set_uint8 b 1 (0xe0 lor (version lsl 3) lor 0b011);
  Bytes.set_uint8 b 2 (((bitrate_index + 1) lsl 4) lor (samplerate_index lsl 2));
  Bytes.set_uint8 b 3 (mode lsl 6);
  let tag = 4 + side_info in
  Bytes.blit_string (if vbr then "Xing" else "Info") 0 b tag 4;
  (* Flags: 0, no optional field. *)
  let lame = tag + 8 in
  Bytes.blit_string "LAME3.100" 0 b lame 9;
  (* Encoder delay and padding, 12 bits each. *)
  let delays = lame + 21 in
  let delay = min delay 0xfff in
  Bytes.set_uint8 b delays (delay lsr 4);
  Bytes.set_uint8 b (delays + 1) ((delay land 0xf) lsl 4);
  Bytes.unsafe_to_string b
