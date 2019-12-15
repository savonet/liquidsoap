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

type 'a read_ops = {
  really_input : 'a -> Bytes.t -> int -> int -> unit;
  input_byte : 'a -> int;
  input : 'a -> Bytes.t -> int -> int -> int;
  seek : 'a -> int -> unit;
  close : 'a -> unit;
}

let in_chan_ops =
  {
    really_input;
    input_byte;
    input;
    seek = (fun ic len -> seek_in ic (pos_in ic + len));
    close = close_in;
  }

(* buffer ofs len *)
type callback = Bytes.t -> int -> int -> int

let callback_ops =
  let really_input read buf ofs len =
    let ret = Extralib.read_retry read buf ofs len in
    if ret < len then raise End_of_file
  in
  let input read = read in
  let _read read len =
    let buf = Bytes.create len in
    really_input read buf 0 len;
    buf
  in
  let input_byte read = Char.code (Bytes.get (_read read 1) 0) in
  let seek read n = ignore (_read read n) in
  let close _ = () in
  { really_input; input_byte; input; seek; close }

type format = [ `Aiff | `Wav ]

type 'a t = {
  ic : 'a;
  read_ops : 'a read_ops;
  format : format;
  channels_number : int;
  (* 1 = mono ; 2 = stereo *)
  sample_rate : int;
  (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int;
  (* 1=8 bit Mono, 2=8 bit Stereo *)
  (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample : int;
  length_of_data_to_follow : int; (* ?? *)
}

let format_of_handler x = x.format

exception Not_a_iff_file of string

let error_translator = function
  | Not_a_iff_file x -> Some (Printf.sprintf "IFF File error: %s" x)
  | _ -> None

let () = Printexc.register_printer error_translator

(* open file and verify it has the right format *)

(** Round to the lowest even integer above. *)
let even_ceil n = (n + 1) / 2 * 2

let read_header read_ops ic =
  let really_input = read_ops.really_input in
  let read_string ic n =
    let ans = Bytes.create n in
    really_input ic ans 0 n;
    Bytes.unsafe_to_string ans
  in
  let format =
    match read_string ic 4 with
      | "RIFF" -> `Wav
      | "FORM" -> `Aiff
      | _ -> raise (Not_a_iff_file "Unknown file format.")
  in
  let input_byte = read_ops.input_byte in
  let read_int_num_bytes ic total =
    let rec aux cur = function
      | 0 -> cur
      | n ->
          let b = input_byte ic in
          let cur =
            if format = `Wav then (b lsl ((total - n) * 8)) + cur
            else (cur lsl 8) + b
          in
          aux cur (n - 1)
    in
    aux 0 total
  in
  let read_int ic = read_int_num_bytes ic 4 in
  let read_short ic = read_int_num_bytes ic 2 in
  ignore (read_int ic);
  (* size of the file *)
  begin
    match read_string ic 4 with
    | "WAVE" when format = `Wav -> ()
    | "AIFF" when format = `Aiff -> ()
    | _ -> raise (Not_a_iff_file "Bad header")
  end;
  let format_chunk = if format = `Wav then "fmt " else "COMM" in
  let seek_chunk ic name =
    let rec seek () =
      if read_string ic 4 <> name then (
        let n = even_ceil (read_int ic) in
        read_ops.seek ic n;
        seek () )
    in
    seek ()
  in
  seek_chunk ic format_chunk;
  let fmt_len = read_int ic in
  if format = `Wav then (
    if fmt_len < 0x10 then
      raise (Not_a_iff_file "Bad header: invalid \"fmt \" length");
    if read_short ic <> 1 then
      raise (Not_a_iff_file "Bad header: unhandled codec");
    let chan_num = read_short ic in
    let samp_hz = read_int ic in
    let byt_per_sec = read_int ic in
    let byt_per_samp = read_short ic in
    let bit_per_samp = read_short ic in
    (* The fmt header can be padded *)
    if fmt_len > 0x10 then read_ops.seek ic (fmt_len - 0x10);
    (* Skip unhandled chunks. *)
    seek_chunk ic "data";
    let len_dat = read_int ic in
    {
      ic;
      format;
      read_ops;
      channels_number = chan_num;
      sample_rate = samp_hz;
      bytes_per_second = byt_per_sec;
      bytes_per_sample = byt_per_samp;
      bits_per_sample = bit_per_samp;
      length_of_data_to_follow = len_dat;
    } )
  else if format = `Aiff then (
    if fmt_len < 0x12 then
      raise (Not_a_iff_file "Bad header: invalid \"COMM\" length");
    let chan_num = read_short ic in
    read_ops.seek ic 4;
    let bit_per_samp = read_short ic in
    let byt_per_samp = bit_per_samp / 8 in
    let samp_hz =
      int_of_float (Utils.float_of_extended_float (read_string ic 10))
    in
    let byt_per_sec = byt_per_samp * samp_hz in
    (* Test for AIFC header, reject other than PCM. *)
    if fmt_len > 0x12 then (
      match read_string ic 4 with
        | "NONE" -> read_ops.seek ic (even_ceil (fmt_len - 0x16))
        | _ -> raise (Not_a_iff_file "Compressed AIFC data not supported") );
    (* Skip unhandled chunks. *)
    seek_chunk ic "SSND";
    let len_dat = read_int ic in
    let offset = read_int ic in
    read_ops.seek ic (4 + offset);
    {
      ic;
      format;
      read_ops;
      channels_number = chan_num;
      sample_rate = samp_hz;
      bytes_per_second = byt_per_sec;
      bytes_per_sample = byt_per_samp;
      bits_per_sample = bit_per_samp;
      length_of_data_to_follow = len_dat - (8 + offset);
    } )
  else assert false

let in_chan_read_header = read_header in_chan_ops

let fopen file =
  let ic = open_in_bin file in
  try in_chan_read_header ic with
    | End_of_file ->
        close_in ic;
        raise (Not_a_iff_file "End of file unexpected")
    | e ->
        close_in ic;
        raise e

let info w =
  Printf.sprintf
    "channels_number = %d \n\
     sample_rate = %d \n\
     bytes_per_second = %d \n\
     bytes_per_sample = %d \n\
     bits_per_sample = %d \n\
     length_of_data_to_follow = %d" w.channels_number w.sample_rate
    w.bytes_per_second w.bytes_per_sample w.bits_per_sample
    w.length_of_data_to_follow

let channels w = w.channels_number
let sample_rate w = w.sample_rate
let sample_size w = w.bits_per_sample
let data_length w = w.length_of_data_to_follow
let close w = w.read_ops.close w.ic
let duration w = float w.length_of_data_to_follow /. float w.bytes_per_second

let short_string i =
  let up = i / 256 in
  let down = i - (256 * up) in
  Bytes.unsafe_to_string
    (Bytes.cat
       (Bytes.make 1 (char_of_int down))
       (Bytes.make 1 (char_of_int up)))

let int_string n =
  let s = Bytes.create 4 in
  Bytes.set s 0 (char_of_int (n land 0xff));
  Bytes.set s 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.set s 2 (char_of_int ((n land 0xff0000) lsr 16));
  Bytes.set s 3 (char_of_int ((n land 0x7f000000) lsr 24));
  Bytes.unsafe_to_string s

let wav_header ?len ~channels ~sample_rate ~sample_size () =
  (* The data lengths are set to their maximum possible values. *)
  let header_len, data_len =
    match len with
      | None -> ("\255\255\255\239", "\219\255\255\239")
      | Some v -> (int_string (v + 36), int_string v)
  in
  "RIFF" ^ header_len ^ "WAVEfmt " ^ int_string 16 ^ short_string 1
  ^ short_string channels ^ int_string sample_rate
  ^ int_string (* bytes per second *) (channels * sample_rate * sample_size / 8)
  ^ short_string (* block size *) (channels * sample_size / 8)
  ^ short_string sample_size ^ "data" ^ data_len
