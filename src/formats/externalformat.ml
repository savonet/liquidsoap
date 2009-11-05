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

let external_process_decoder process =
  let wav_decoder =
    { Wavformat.
        log = Dtools.Log.make ["format";"external"] ;
        create =
         (fun file ->
           let in_e = Unix.open_process_in (process file) in
           let w =
             try
               Wav.read_header in_e "<stdin>"
             with
               | e -> ignore(Unix.close_process_in in_e);
                      raise e
           in
           in_e,w) ;
        close = (fun in_e -> ignore(Unix.close_process_in in_e)) }
  in
  Wavformat.decoder wav_decoder

let register_external_decoder name process =
  Decoder.formats#register name (File_decoder.decode (external_process_decoder process))

let register_external_metadata_resolver name process =
  Request.mresolvers#register name process
