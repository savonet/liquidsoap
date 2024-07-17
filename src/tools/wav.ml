(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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
open Mixer

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

let fopen file =
  let ic = open_in_bin file in
    try
      let buff = "riffwaveFMT?" in
      (* verify it has a right header *)
      really_input ic buff 0 4;
      ignore (input_byte ic);   (* size *)
      ignore (input_byte ic);   (*  of  *)
      ignore (input_byte ic);   (* the  *)
      ignore (input_byte ic);   (* file *)
      really_input ic buff 4 8;

      if buff <> "RIFFWAVEfmt " then raise (Not_a_wav_file 
        "Bad header : string \"RIFF\", \"WAVE\" or \"fmt \" not found") ;

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
       
       let len_dat = read_int ic
       in
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
    with End_of_file -> close_in ic; raise (Not_a_wav_file "End of file unexpected")
    | e -> close_in ic; raise e
	  

let sample w buf pos len=
  match input w.ic buf 0 len with
  | 0 -> raise End_of_file
  | n -> n
	
	
let info w =
  Printf.sprintf 
"         filename = %s
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

let format w =
  {
    Mixer.channels = w.channels_number ; 
    Mixer.sample_freq = w.sample_rate ;
    Mixer.sample_size = w.bits_per_sample ; 
    Mixer.big_endian = false;
    Mixer.signed = (w.bits_per_sample <> 8)
  } 

let close w = 
  close_in w.ic

let short_string i =
  let up = i/256 in
  let down = i-256*up in
    (String.make 1 (char_of_int down))^
    (String.make 1 (char_of_int up))

let int_string b0123 =
  let b123 = b0123/256 in
  let b23 = b123/256 in
  let b3 = b23/256 in
  let b0 = b0123-256*b123 in
  let b1 = b123-256*b23 in
  let b2 = b23-256*b3 in
    (String.make 1 (char_of_int b0))^
    (String.make 1 (char_of_int b1))^
    (String.make 1 (char_of_int b2))^
    (String.make 1 (char_of_int b3))

let header format =
  (* The data lengths are set to their maximum possible values. *)
  "RIFF" ^
  (int_string max_int) ^ (* file_length-8 *)
  "WAVEfmt " ^
  (int_string 16) ^
  (short_string 1) ^
  (short_string format.channels) ^
  (int_string format.sample_freq) ^
  (int_string   (* bytes per second *)
     (format.channels*format.sample_freq*format.sample_size/8)) ^
  (short_string (* block size *)
     (format.channels*format.sample_size/8)) ^
  (short_string format.sample_size) ^
  "data" ^
  (int_string max_int) (* data_length *)
