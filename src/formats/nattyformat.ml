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

(** Decode files parsed by ocaml-natty. *)

module Generator = Float_pcm.Generator_from_raw

let log = Dtools.Log.make ["format";"natty"]

let bytes_to_get = 1024*64

let decoder_pcm ~get_samples ~pcm w =
  let openfile name = 
    let file = w name in
    let pcm = pcm file in
    (** Generator's input format *)
    let channels   = pcm.Natty.channels in
    let in_freq    = float (pcm.Natty.sample_rate) in
    let samplesize = pcm.Natty.bits_per_sample in
    let big_endian = pcm.Natty.endianess <> Natty.Little_endian in
    let signed     = true in
    let out_freq   = float (Fmt.samples_per_second()) in
    let abg =
      Generator.create
        ~channels ~samplesize ~signed ~big_endian ~in_freq ~out_freq
    in
    let position = ref 0 in
    (file,position),abg
  in
  let close _ = () in
  let decode (file,position) abg =
    let tmpbuf = get_samples file bytes_to_get in
    let l = String.length tmpbuf in
    position := !position + l ;
    Generator.feed abg tmpbuf
  in
  let position (_,position) = !position in
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

let () =
  let decoder =
    let f = Natty.Au.open_f in
    let pcm x =
      match x.Natty.Au.format with
        | Natty.Pcm x -> x
	| _ -> failwith "unrecognized format"
    in
    let get_samples x n =
      try
        Natty.Au.get_samples x n
      with
        | Not_found ->
            let x = Natty.Au.get_all_samples x in
            if x = "" then
              raise Not_found
            else
              x
    in
    decoder_pcm ~get_samples ~pcm f
  in
  Decoder.formats#register "AU"
    (fun name -> try Some (decoder name) with _ -> None)

let () =
  let decoder =
    let f = Natty.Aiff.open_f in
    let pcm x =
      match x.Natty.Aiff.format with
        | Natty.Pcm x -> x
        | _ -> failwith "unrecognized format"
    in
    let get_samples x n =
      try
        Natty.Aiff.get_samples x n
      with
        | Not_found ->
            let x = Natty.Aiff.get_all_samples x in
            if x = "" then
              raise Not_found
            else
              x
    in
    decoder_pcm ~get_samples ~pcm f
  in
  Decoder.formats#register "AIFF"
    (fun name -> try Some (decoder name) with _ -> None)
