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

(** Decode and read metadatas of mp3 files. *)

open Dtools

let get_samplefreq f =
  let bits_in i pos length =
    ((int_of_char i) lsr pos) mod (1 lsl length)
  in
  let samplefreq_array =
    [|
      [| 11025; 12000; 8000; 0 |];
      [| |];
      [| 22050; 24000; 16000; 0 |];
      [| 44100; 48000; 32000; 0 |];
    |]
  in
  let read n =
    let ans = String.create n in
      assert (n = Unix.read f ans 0 n) ;
      ans
  in
  let read_byte () =
    int_of_char (read 1).[0]
  in
  let read_size () =
    let buf = read 4 in
    let b0 = int_of_char buf.[0] in
    let b1 = int_of_char buf.[1] in
    let b2 = int_of_char buf.[2] in
    let b3 = int_of_char buf.[3] in
      (* TODO* : lsl 7 -> overlapping ?? *)
      (((((b0 lsl 7) lor b1) lsl 7) lor b2) lsl 7) lor b3
  in
    if read 3 = "ID3" then
      (
        ignore (Unix.lseek f 3 Unix.SEEK_CUR);
        ignore (Unix.lseek f (read_size ()) Unix.SEEK_CUR);
      )
    else
      ignore (Unix.lseek f 0 Unix.SEEK_SET);
    ( let b = read_byte () in assert (b = 0xff) ) ;
    let buf = read 3 in
    let version = bits_in buf.[0] 3 2 in
    let samplefreq_i = bits_in buf.[1] 2 2 in
      samplefreq_array.(version).(samplefreq_i)

let get_samplefreq fname =
  let f = Unix.openfile fname [Unix.O_RDONLY] 0o400 in
  try
    let freq = get_samplefreq f in
      Unix.close f ;
      freq
  with
    | e -> Unix.close f ; raise e

let decoder file =

  let format = {
    Mixer.channels = Mad.wav_output_channels ;
    Mixer.sample_freq = get_samplefreq file ;
    Mixer.sample_size = Mad.wav_output_sample_size ;
    Mixer.big_endian = Mad.wav_output_big_endian ;
    Mixer.signed = Mad.wav_output_signed ;
  } in
  let fd = 
    Log.logl ~label:"mp3" 4 (lazy (Log.f "open %S" file)) ; 
    Mad.openfile file
  in
  let abg = Mixer.Generator.create () in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let remaining () =              
    (* Computes an approximative remaining number of wav frames. *)
    if !in_bytes = 0 || !out_bytes = 0
    then -1
    else
      max 0
	((in_size - !in_bytes) /
	 (!in_bytes / (
	    (!out_bytes+(Mixer.Generator.length abg))
	    /Mixer.Buffer.size
	  ))
	)
  in
  let close () =
    Log.logl ~label:"mp3" 4 (lazy (Log.f "close %S" file)) ;
    Mad.close fd
  in
  let fill = 
    fun buf ->

      begin
	try
	  while
	    Mixer.Generator.should_be_feeded abg
	  do
	    let frame = Mad.decode_frame fd in
	      Mixer.Generator.feed abg format frame ;
	  done
	with
	  | _ -> ()
      end ;

      let offset = Mixer.Buffer.position buf in
	Mixer.Buffer.fill buf abg ;
	let added = (Mixer.Buffer.position buf) - offset in
	  in_bytes := Mad.get_current_position fd ;
	  out_bytes := !out_bytes + added ;
          if added = 0 then 0 else remaining ()
  in
    { Decoder.fill = fill ; Decoder.close = close }

let _ = Decoder.formats#register "MP3"
	  (fun name -> try Some (decoder name) with _ -> None)
