(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

type t =
    {
      ic : in_channel;
      filename : string;

      channels_number : int;  (* 1 = mono ; 2 = stereo *)
      sample_rate : int;      (* in Hz *)
      bytes_per_second : int; 
      bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
      (* or 16 bit Mono, 4=16 bit Stereo *)
      bits_per_sample : int;
      length_of_data_to_follow : int;  (* ?? *)
    }

exception Not_a_wav_file of string

let read_int_num_bytes ic =
  let rec aux = function
    | 0 -> 0
    | n ->
        let b = input_byte ic in
          b + 256*(aux (n-1))
  in
    aux

let read_int ic =
  read_int_num_bytes ic 4

let read_short ic =
  read_int_num_bytes ic 2

(* open file and verify it has the right format *)  

let debug =
  try
    ignore (Sys.getenv "LIQUIDSOAP_DEBUG_WAV") ; true
  with
    | Not_found -> false

let read_header ic file =
  let buff = "riffwaveFMT?" in
    (* verify it has a right header *)
    really_input ic buff 0 4;
    ignore (input_byte ic);   (* size *)
    ignore (input_byte ic);   (*  of  *)
    ignore (input_byte ic);   (* the  *)
    ignore (input_byte ic);   (* file *)
    really_input ic buff 4 8;

    if buff <> "RIFFWAVEfmt " then
      raise
        (
          Not_a_wav_file
            "Bad header : string \"RIFF\", \"WAVE\" or \"fmt \" not found"
        );

    ignore (input_byte ic); (* always 0x10 *)
    ignore (input_byte ic); (* always 0x00 *)
    ignore (input_byte ic); (* always 0x00 *)
    ignore (input_byte ic); (* always 0x00 *)
    ignore (input_byte ic); (* always 0x01 *)
    ignore (input_byte ic); (* always 0x00 *)

    let chan_num = read_short ic in
    let samp_hz = read_int ic in
    let byt_per_sec = read_int ic in
    let byt_per_samp= read_short ic in
    let bit_per_samp= read_short ic in

      really_input ic buff 0 4;

      if buff <> "dataWAVEfmt " then
        (
          if buff = "INFOWAVEfmt " then
            raise (Not_a_wav_file "Valid wav file but unread");
          raise (Not_a_wav_file "Bad header : string \"data\" not found")
        );

      let len_dat = read_int ic in
        {
          ic = ic ;
          filename = file;
          channels_number = chan_num;
          sample_rate = samp_hz;
          bytes_per_second = byt_per_sec;
          bytes_per_sample = byt_per_samp;
          bits_per_sample = bit_per_samp;
          length_of_data_to_follow = len_dat;
        }

let fopen file =
  let ic = open_in_bin file in
    try
      read_header ic file
    with
      | End_of_file ->
          close_in ic ;
          raise (Not_a_wav_file "End of file unexpected")
      | e ->
          close_in ic ;
          raise e

let skip_header c = ignore (read_header c "")

let sample w buf pos len=
  match input w.ic buf 0 len with
    | 0 -> raise End_of_file
    | n -> n


let info w =
  Printf.sprintf
    "    filename = %s
    channels_number = %d
    sample_rate = %d
    bytes_per_second = %d
    bytes_per_sample = %d
    bits_per_sample = %d
    length_of_data_to_follow = %d"
    w.filename
    w.channels_number
    w.sample_rate
    w.bytes_per_second
    w.bytes_per_sample
    w.bits_per_sample
    w.length_of_data_to_follow

let channels w = w.channels_number
let sample_rate w = w.sample_rate
let sample_size w = w.bits_per_sample
let big_endian w = false
let signed w = w.bits_per_sample <> 8

let close w =
  close_in w.ic

let short_string i =
  let up = i/256 in
  let down = i-256*up in
    (String.make 1 (char_of_int down))^
    (String.make 1 (char_of_int up))

let int_string n =
  let s = "abcd" in
    s.[0] <- char_of_int (n land 0xff) ;
    s.[1] <- char_of_int ((n land 0xff00) lsr 8) ;
    s.[2] <- char_of_int ((n land 0xff0000) lsr 16) ;
    s.[3] <- char_of_int ((n land 0x7f000000) lsr 24) ;
    s

let header ~channels ~sample_rate ~sample_size ~big_endian ~signed =
  (* The data lengths are set to their maximum possible values. *)
  "RIFF" ^
  "\255\255\255\255" ^
  "WAVEfmt " ^
  (int_string 16) ^
  (short_string 1) ^
  (short_string channels) ^
  (int_string sample_rate) ^
  (int_string   (* bytes per second *)
     (channels*sample_rate*sample_size/8)) ^
  (short_string (* block size *)
     (channels*sample_size/8)) ^
  (short_string sample_size) ^
  "data" ^
  "\255\255\255\255"
