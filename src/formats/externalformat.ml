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

let bytes_to_get = 1024*64

let decoder process file =
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
  let buffer_length = Decoder.buffer_length () in

  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
  let tmpbuf = String.create bytes_to_get in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    ignore(Unix.close_process_in in_e);
  in
  let fill buf =
    assert (not !closed) ;

    begin
      try
        while Generator.length abg < buffer_length do
          let l = Wav.sample w tmpbuf 0 bytes_to_get in
            in_bytes := !in_bytes + l ;
            Generator.feed abg (String.sub tmpbuf 0 l)
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


let register_external_decoder name process =
  Decoder.formats#register name
           (fun name -> try Some (decoder process name) with _ -> None)

let register_external_metadata_resolver name process =
  Request.mresolvers#register name process
