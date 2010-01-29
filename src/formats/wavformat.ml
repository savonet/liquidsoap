(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** Decode WAV files. *)

let log = Dtools.Log.make ["decoder";"wav"]

(** {1 Generic decoder} *)

exception End_of_stream

let rec really_input input len =
  let s,i = input len in
    if i=len then s else
      if i=0 then raise End_of_stream else
        String.sub s 0 i ^ really_input input (len-i)

let input_byte input =
  let s,i = input 1 in
    if i=0 then raise End_of_stream ;
    int_of_char s.[0]

let read_int_num_bytes ic =
  let rec aux = function
    | 0 -> 0
    | n ->
        let b = input_byte ic in
          b + 256*(aux (n-1))
  in
    aux

let read_int ic = read_int_num_bytes ic 4

let read_short ic = read_int_num_bytes ic 2

module Make (Generator:Generator.S_Asio) =
struct

(* TODO It might be more efficient to write our code for an input
 * channel and use directly the one we have when decoding files
 * or external processes, if we could wrap the input function used
 * for decoding stream (in http and harbor) as an in_channel. *)
let create input =
  let decoder = ref (fun gen -> assert false) in

  let main_decoder converter gen =
    let bytes_to_get = 1024*64 in
    let data,bytes = input bytes_to_get in
      if bytes=0 then raise End_of_stream ;
      log#f 4 "Read %d bytes of PCM" bytes ;
      let content,length = converter (String.sub data 0 bytes) in
        Generator.set_mode gen `Audio ;
        Generator.put_audio gen content 0 length ;
        log#f 4 "Done (%d)" length
  in

  let read_header () =

    if really_input input 4 <> "RIFF" then
      raise (Wav.Not_a_wav_file "Bad header: \"RIFF\" not found") ;
    (* Ignore the file size *)
    ignore (really_input input 4) ;
    if really_input input 8 <> "WAVEfmt " then
      raise (Wav.Not_a_wav_file "Bad header: \"WAVEfmt \" not found") ;
    (* Now we always have the following uninteresting bytes:
     * 0x10 0x00 0x00 0x00 0x01 0x00 *)
    ignore (really_input input 6) ;

    let channels = read_short input in
    let samplerate (* in Hz *) = read_int input in
    let _ (* byt_per_sec *) = read_int input in
    let _ (* byt_per_samp *) = read_short input in
    let samplesize (* in bits *) = read_short input in

    let signed = samplesize <> 8 in
    let big_endian = false in

    let section = really_input input 4 in
    if section <> "data" then begin
      if section = "INFO" then
        raise (Wav.Not_a_wav_file "Valid wav file but unread");
      raise (Wav.Not_a_wav_file "Bad header : string \"data\" not found")
    end ;

    let _ (* len_dat *) = read_int input in

    let converter =
      Rutils.create_from_s16le
        ~channels ~samplesize ~signed ~big_endian ()
    in

      log#f 4
        "WAV header read (%dHz, %dbits), starting decoding..."
        samplerate samplesize ;
      decoder :=
        main_decoder
          (fun pcm -> converter ~audio_src_rate:(float samplerate) pcm)

  in
    decoder := (fun _ -> read_header ()) ;
    Decoder.Decoder (fun gen -> !decoder gen)

end

module Generator = Generator.From_audio_video
module Buffered = Decoder.Buffered(Generator)

(* File decoding *)

module D = Make(Generator)

let get_type filename =
  let chan = open_in filename in
  let info = Wav.read_header chan filename in
    close_in chan ;
    { Frame. video = 0 ; midi = 0 ; audio = Wav.channels info }

let create_file_decoder filename kind =
  let generator = Generator.create `Audio in
    Buffered.file_decoder filename kind D.create generator

let () =
  Decoder.file_decoders#register "WAV"
    ~sdoc:"Decode as WAV any file with a correct header."
    (fun filename kind ->
       let file_type = get_type filename in
         if Frame.type_has_kind file_type kind then
           Some (fun () -> create_file_decoder filename kind)
         else begin
           log#f 3
             "WAV file %S has content type %s but %s was expected."
             filename
             (Frame.string_of_content_type file_type)
             (Frame.string_of_content_kind kind) ;
           None
         end)
