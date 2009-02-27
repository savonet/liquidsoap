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

let bytes_to_get = 1024*64

let decoder_pcm ~get_samples ~pcm w file =
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
  let buffer_length = Decoder.buffer_length () in

  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
  in
  let fill buf =
    assert (not !closed) ;

    begin
      try
        while Generator.length abg < buffer_length do
          let tmpbuf = get_samples w bytes_to_get in
	  let l = String.length tmpbuf in
            in_bytes := !in_bytes + l ;
            Generator.feed abg tmpbuf
        done
      with _ -> () (* End_of_file of bad format raised by feed *)
    end ;

    let offset = AFrame.position buf in
      Generator.fill abg buf ;
      out_samples := !out_samples + AFrame.position buf - offset ;
      (* Compute an estimated number of remaining ticks. *)
      let abglen = Generator.length abg in
        assert (!in_bytes!=0) ;
        let compression =
          (float (!out_samples+abglen)) /. (float !in_bytes)
        in
        let remaining_samples =
          (float (file_size - !in_bytes)) *. compression
          +. (float abglen)
        in
          (* I suspect that in_samples in not accurate, since I don't
           * get an exact countdown after than in_size=in_samples, but there
           * is a stall at the beginning after which the countdown starts. *)
          Fmt.ticks_of_samples (int_of_float remaining_samples)

  in { Decoder.fill = fill ; Decoder.close = close }

let () =
  let decoder name =
    let f = Natty.Au.open_f name in
    let pcm =
      match f.Natty.Au.format with
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
    decoder_pcm ~get_samples ~pcm f name
  in
  Decoder.formats#register "AU"
    (fun name -> try Some (decoder name) with _ -> None)

let () =
  let decoder name =
    let f = Natty.Aiff.open_f name in
    let pcm =
      match f.Natty.Aiff.format with
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
    decoder_pcm ~get_samples ~pcm f name
  in
  Decoder.formats#register "AIFF"
    (fun name -> try Some (decoder name) with _ -> None)
