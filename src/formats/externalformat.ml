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

(** Decode files using external decoders. *)

module Generator = Float_pcm.Generator_from_raw

let log = Dtools.Log.make ["format";"external"]

let bytes_to_get = 1024*64

let decoder process =
  let openfile file = 
    let in_e = Unix.open_process_in (process file) in
    let w =
      try
        Wav.read_header in_e "<stdin>"
      with
        | e -> ignore(Unix.close_process_in in_e); raise e
    in
    (** Generator's input format *)
    let channels   = Wav.channels w in
    let in_freq    = float (Wav.sample_rate w) in
    let samplesize = Wav.sample_size w in
    let big_endian = Wav.big_endian w in
    let signed     = Wav.signed w in
    let out_freq   = float (Fmt.samples_per_second()) in
    let abg =
      Generator.create
        ~channels ~samplesize ~signed ~big_endian ~in_freq ~out_freq
    in
    let tmpbuf = String.create bytes_to_get in
    let position = ref 0 in
    (w,in_e,tmpbuf,position),abg
  in
  let close (w,in_e,_,_) =
    Wav.close w;
    ignore(Unix.close_process_in in_e);
  in
  let decode (w,_,tmpbuf,position) abg = 
    let l = Wav.sample w tmpbuf 0 bytes_to_get in
    position := !position + l ;
    Generator.feed abg (String.sub tmpbuf 0 l)
  in
  let position (_,_,_,position) = !position in
  let decoder =
   {
    File_decoder.Raw.
     log = log;
     openfile = openfile;
     decode = decode;
     position = position;
     close = close
   }
  in
  File_decoder.Raw.decode decoder

let register_external_decoder name process =
  Decoder.formats#register name
           (fun name -> try Some (decoder process name) with _ -> None)

let register_external_metadata_resolver name process =
  Request.mresolvers#register name process
