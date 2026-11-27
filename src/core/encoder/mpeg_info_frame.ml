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

(** The encoder-information frame of an MPEG Layer III file: an audio frame
    without audio that holds the Xing fields (frame and byte counts, table of
    contents) and LAME's extension (encoder delay and padding, checksums). The
    writers whose library does not write one build it here. *)

type version = [ `Mpeg_1 | `Mpeg_2 | `Mpeg_2_5 ]

let version_of_samplerate = function
  | 44100 | 48000 | 32000 -> `Mpeg_1
  | 22050 | 24000 | 16000 -> `Mpeg_2
  | 11025 | 12000 | 8000 -> `Mpeg_2_5
  | samplerate ->
      invalid_arg (Printf.sprintf "Mpeg_info_frame: samplerate %d" samplerate)

let samplerate_index = function
  | 44100 | 22050 | 11025 -> 0
  | 48000 | 24000 | 12000 -> 1
  | _ -> 2

let bitrates = function
  | `Mpeg_1 -> [32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320]
  | `Mpeg_2 | `Mpeg_2_5 ->
      [8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160]

let samples_per_frame = function `Mpeg_1 -> 1152 | `Mpeg_2 | `Mpeg_2_5 -> 576

let frame_length ~version ~samplerate bitrate =
  samples_per_frame version / 8 * bitrate * 1000 / samplerate

let side_info_length ~version ~channels =
  match (version, channels) with
    | `Mpeg_1, 1 -> 17
    | `Mpeg_1, _ -> 32
    | _, 1 -> 9
    | _ -> 17

(* Xing identifier, flags, counts, table of contents, quality, then LAME's
   extension. *)
let tag_length = 4 + 4 + 4 + 4 + 100 + 4 + 36

(* CRC-16 with polynomial 0x8005, reflected, as LAME computes it. *)
let crc16_update crc bytes =
  let crc = ref crc in
  String.iter
    (fun byte ->
      crc := !crc lxor Char.code byte;
      for _ = 1 to 8 do
        crc := if !crc land 1 = 1 then (!crc lsr 1) lxor 0xA001 else !crc lsr 1
      done)
    bytes;
  !crc

type t = {
  version : version;
  samplerate : int;
  channels : int;
  bitrate : int;  (** the frame's own, in kbps *)
  stream_bitrate : int;
  length : int;
}

(** The frame of a constant bit rate stream. It takes the stream's bit rate when
    a frame of that rate holds the tag, the lowest that does otherwise. *)
let create ~samplerate ~channels ~bitrate =
  let version = version_of_samplerate samplerate in
  let needed = 4 + side_info_length ~version ~channels + tag_length in
  let holds bitrate = needed <= frame_length ~version ~samplerate bitrate in
  let frame_bitrate =
    if holds bitrate then bitrate
    else (
      match List.find_opt holds (bitrates version) with
        | Some bitrate -> bitrate
        | None -> invalid_arg "Mpeg_info_frame: no bit rate holds the frame")
  in
  {
    version;
    samplerate;
    channels;
    bitrate = frame_bitrate;
    stream_bitrate = bitrate;
    length = frame_length ~version ~samplerate frame_bitrate;
  }

let length t = t.length
let samples_per_frame t = samples_per_frame t.version

let header t =
  let version_bits =
    match t.version with `Mpeg_1 -> 3 | `Mpeg_2 -> 2 | `Mpeg_2_5 -> 0
  in
  let bitrate_index =
    let rec index i = function
      | bitrate :: _ when bitrate = t.bitrate -> i
      | _ :: rest -> index (i + 1) rest
      | [] -> assert false
    in
    index 1 (bitrates t.version)
  in
  let channel_mode = if t.channels = 1 then 3 else 1 in
  let header = Bytes.create 4 in
  Bytes.set_uint8 header 0 0xff;
  (* Layer III, no CRC. *)
  Bytes.set_uint8 header 1 (0xe0 lor (version_bits lsl 3) lor 0x02 lor 0x01);
  Bytes.set_uint8 header 2
    ((bitrate_index lsl 4) lor (samplerate_index t.samplerate lsl 2));
  Bytes.set_uint8 header 3 (channel_mode lsl 6);
  Bytes.unsafe_to_string header

(** The frame. [frames] and [bytes] count the audio frames that follow it and
    their bytes, [music_crc] is the CRC-16 of those bytes; a placeholder has
    them at 0, which decoders read as absent. [delay] and [padding] are in
    samples. *)
let render t ~frames ~bytes ~music_crc ~delay ~padding =
  let frame = Bytes.make t.length '\000' in
  Bytes.blit_string (header t) 0 frame 0 4;
  let tag = 4 + side_info_length ~version:t.version ~channels:t.channels in
  Bytes.blit_string "Info" 0 frame tag 4;
  Bytes.set_int32_be frame (tag + 4) 0x0fl;
  Bytes.set_int32_be frame (tag + 8) (Int32.of_int frames);
  (* The byte count includes this frame. *)
  let stream_bytes = if frames = 0 then 0 else bytes + t.length in
  Bytes.set_int32_be frame (tag + 12) (Int32.of_int stream_bytes);
  (* A constant bit rate stream is linear in time. *)
  if 0 < frames then
    for i = 0 to 99 do
      Bytes.set_uint8 frame (tag + 16 + i) (i * 256 / 100)
    done;
  let lame = tag + 16 + 100 + 4 in
  (* Decoders read the delay and padding only after an identifier they know. *)
  Bytes.blit_string "LAMEshine" 0 frame lame 9;
  (* Tag revision 0, constant bit rate. *)
  Bytes.set_uint8 frame (lame + 9) 1;
  Bytes.set_uint8 frame (lame + 20) (min 255 t.stream_bitrate);
  let delay = min 4095 (max 0 delay) and padding = min 4095 (max 0 padding) in
  Bytes.set_uint8 frame (lame + 21) (delay lsr 4);
  Bytes.set_uint8 frame (lame + 22)
    (((delay land 0xf) lsl 4) lor (padding lsr 8));
  Bytes.set_uint8 frame (lame + 23) (padding land 0xff);
  Bytes.set_int32_be frame (lame + 28) (Int32.of_int stream_bytes);
  Bytes.set_uint16_be frame (lame + 32) music_crc;
  Bytes.set_uint16_be frame (lame + 34)
    (crc16_update 0 (Bytes.sub_string frame 0 (lame + 34)));
  Bytes.unsafe_to_string frame
