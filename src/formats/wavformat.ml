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

module Generator = Generator.From_audio_video

let bytes_to_get = 1024*64

type 'a wav_decoder =
  {
    log     : Dtools.Log.t;
    create  : string -> 'a * Wav.t;
    close   : 'a -> unit
  }

type 'a handle =
  {
    decoder          : 'a ;
    w                : Wav.t ;
    tmpbuf           : String.t ;
    mutable position : int ;
    converter        : Rutils.s16le_converter ;
    channels         : int ;
    audio_src_rate   : float
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
           let position = 0 in
           {
             decoder = dec ; w = w ; tmpbuf = tmpbuf;
             converter = converter ; channels = channels ;
             audio_src_rate = audio_src_rate; position = position
           });
    get_kind =
      (fun handle ->
         { Frame.
             audio = Frame.mul_of_int handle.channels;
             video = Frame.mul_of_int 0;
             midi  = Frame.mul_of_int 0});
    close =
      (fun handle ->
                Wav.close handle.w;
                wav_decoder.close handle.decoder);
    decode =
      (fun handle buffer ->
         let l = Wav.sample handle.w handle.tmpbuf 0 bytes_to_get in
         handle.position <- handle.position + l ;
         let audio_src_rate = handle.audio_src_rate in
         let content,length =
           handle.converter ~audio_src_rate (String.sub handle.tmpbuf 0 l)
         in
         Generator.put_audio buffer content 0 length);
    position = (fun handler -> handler.position)
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

