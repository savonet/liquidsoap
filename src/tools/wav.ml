(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

type 'a read_ops =
  {
    really_input : 'a -> string -> int -> int -> unit ;
    input_byte   : 'a -> int;
    input        : 'a -> string -> int -> int -> int ;
    close        : 'a -> unit; 
  }

let in_chan_ops = { really_input = really_input ;
                    input_byte = input_byte ;
                    input = input; close = close_in }

type 'a t =
    {
      ic : 'a;
      read_ops : 'a read_ops;

      channels_number : int;  (* 1 = mono ; 2 = stereo *)
      sample_rate : int;      (* in Hz *)
      bytes_per_second : int; 
      bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
      (* or 16 bit Mono, 4=16 bit Stereo *)
      bits_per_sample : int;
      length_of_data_to_follow : int;  (* ?? *)
    }

exception Not_a_wav_file of string

let error_translator =
  function
    | Not_a_wav_file x ->
       raise (Utils.Translation
         (Printf.sprintf "Wave error: %s" x))
    | _ -> ()

let () = Utils.register_error_translator error_translator

(* open file and verify it has the right format *)  

let debug = Utils.getenv_opt "LIQUIDSOAP_DEBUG_WAV" <> None

let read_header read_ops ic =
  let really_input = read_ops.really_input in
  let input_byte = read_ops.input_byte in
  let read_int_num_bytes ic =
    let rec aux = function
      | 0 -> 0
      | n ->
          let b = input_byte ic in
            b + 256*(aux (n-1))
    in
      aux
  in
  let read_int ic =
    read_int_num_bytes ic 4
  in
  let read_short ic =
    read_int_num_bytes ic 2
  in
  let read_string ic n =
    let ans = String.create n in
    really_input ic ans 0 n;
    ans
  in

    if read_string ic 4 <> "RIFF" then
      raise (Not_a_wav_file "Bad header: \"RIFF\" expected");
    ignore (read_int ic); (* size of the file *)
    if read_string ic 4 <> "WAVE" then
      raise (Not_a_wav_file "Bad header: \"WAVE\" expected");
    if read_string ic 4 <> "fmt " then
      raise (Not_a_wav_file "Bad header: \"fmt \" expected");

    let fmt_len = read_int ic in
    if fmt_len <> 0x10 then
      raise (Not_a_wav_file "Bad header: invalid \"fmt \" length");
    if read_short ic <> 1 then
      raise (Not_a_wav_file "Bad header: unhandled codec");

    let chan_num = read_short ic in
    let samp_hz = read_int ic in
    let byt_per_sec = read_int ic in
    let byt_per_samp= read_short ic in
    let bit_per_samp= read_short ic in

    let header = ref (read_string ic 4) in
      (* Skip unhandled chunks. *)
      while !header <> "data" do
        let len = read_int ic in
        ignore (read_string ic len);
        header := read_string ic 4
      done;

      let len_dat = read_int ic in
        {
          ic = ic ;
          read_ops = read_ops;
          channels_number = chan_num;
          sample_rate = samp_hz;
          bytes_per_second = byt_per_sec;
          bytes_per_sample = byt_per_samp;
          bits_per_sample = bit_per_samp;
          length_of_data_to_follow = len_dat;
        }

let in_chan_read_header = read_header in_chan_ops

let fopen file =
  let ic = open_in_bin file in
    try
      in_chan_read_header ic
    with
      | End_of_file ->
          close_in ic ;
          raise (Not_a_wav_file "End of file unexpected")
      | e ->
          close_in ic ;
          raise e

let skip_header f c = read_header f c

let sample w buf pos len=
  match w.read_ops.input w.ic buf 0 len with
    | 0 -> raise End_of_file
    | n -> n


let info w =
  Printf.sprintf
    "channels_number = %d
     sample_rate = %d
     bytes_per_second = %d
     bytes_per_sample = %d
     bits_per_sample = %d
     length_of_data_to_follow = %d"
     w.channels_number
     w.sample_rate
     w.bytes_per_second
     w.bytes_per_sample
     w.bits_per_sample
     w.length_of_data_to_follow

let channels w = w.channels_number
let sample_rate w = w.sample_rate
let sample_size w = w.bits_per_sample
let data_length w = w.length_of_data_to_follow
(** Length of the data in samples. *)
let data_samples w = w.length_of_data_to_follow / ((sample_size w / 8) * channels w)

let close w =
  w.read_ops.close w.ic

let data_len file = 
  let stats = Unix.stat file in
  stats.Unix.st_size - 36

let duration w = 
  (float w.length_of_data_to_follow) /. (float w.bytes_per_second)

let short_string i =
  let up = i/256 in
  let down = i-256*up in
    (String.make 1 (char_of_int down))^
    (String.make 1 (char_of_int up))

let int_string n =
  let s = String.create 4 in
    s.[0] <- char_of_int (n land 0xff) ;
    s.[1] <- char_of_int ((n land 0xff00) lsr 8) ;
    s.[2] <- char_of_int ((n land 0xff0000) lsr 16) ;
    s.[3] <- char_of_int ((n land 0x7f000000) lsr 24) ;
    s

let header ?len ~channels ~sample_rate ~sample_size () =
  (* The data lengths are set to their maximum possible values. *)
  let header_len,data_len = 
    match len with
      | None -> "\255\255\255\239","\219\255\255\239"
      | Some v -> int_string (v+36), int_string v
  in
  "RIFF" ^
  header_len ^
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
  data_len
