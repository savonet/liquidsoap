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

let bytes_to_get = 1024*64

type 'a wav_decoder = 
  {
    log     : Dtools.Log.t;
    create  : string -> 'a * Wav.t;
    close   : 'a -> unit
  }

(** Generic wav decoder *)
let decoder wav_decoder =
  { File_decoder.
      log = wav_decoder.log;
      openfile = 
        (fun file ->
           let dec,w = wav_decoder.create file in
           (** Get input format *)
           let channels       = Wav.channels w in
           let audio_src_rate = float (Wav.sample_rate w) in
           let samplesize     = Wav.sample_size w in
           let big_endian     = Wav.big_endian w in
           let signed         = Wav.signed w in
           (* Create converter *)
           let converter =
             Rutils.create_from_s16le
               ~channels ~samplesize ~signed ~big_endian ()
           in
           let tmpbuf = String.create bytes_to_get in
           let position = ref 0 in
           (dec,w,tmpbuf,position,converter,channels,audio_src_rate));
    get_type = 
      (fun (_,_,_,_,_,channels,_) -> 
         { Frame.
             audio = channels;
             video = 0;
             midi  = 0});       
    close = 
      (fun (dec,w,_,_,_,_,_) -> 
                Wav.close w;
                wav_decoder.close dec);
    decode = 
      (fun (_,w,tmpbuf,position,converter,_,audio_src_rate) abg ->
         let l = Wav.sample w tmpbuf 0 bytes_to_get in
         position := !position + l ;
         let content,length = converter ~audio_src_rate (String.sub tmpbuf 0 l) in
         Generator.feed abg content 0 length);
    position = (fun (_,_,_,position,_,_,_) -> !position)
  }

(** Wav file decoder *)
let wav_file_decoder = 
  let wav_decoder = 
    { log     = Dtools.Log.make ["format";"wav"] ; 
      create  = (fun file -> (),Wav.fopen file) ;
      close   = (fun () -> ()) }
  in
  decoder wav_decoder

let () = Decoder.formats#register "WAV" (File_decoder.decode wav_file_decoder)

