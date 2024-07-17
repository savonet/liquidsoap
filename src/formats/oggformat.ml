(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(** Decode and read metadatas of ogg/vorbis files. *)

module Generator = Float_pcm.Generator

let buflen = 1024

let decoder file =
  let dec, fd = Vorbis.Decoder.openfile_with_fd file in
  let info = Vorbis.Decoder.info dec (-1) in
  (** Generator's input format *)
  let sample_freq = info.Vorbis.audio_samplerate in
  let abg = Generator.create () in
  let buffer_length = Decoder.buffer_length () in

  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    Vorbis.Decoder.close dec
  in
  let fill buf =
    assert (not !closed) ;

    begin
      try
        while Generator.length abg < buffer_length do
          let buf = Vorbis.Decoder.decode_float_alloc dec buflen in
            Generator.feed abg ~sample_freq buf
        done
      with _ -> () (* TODO: log the error *)
    end ;

    (* TODO: better estimation using Vorbis.Decoder.samples! *)

    let offset = AFrame.position buf in
      AFrame.fill_frame abg buf ;
      in_bytes := Unix.lseek fd 0 Unix.SEEK_CUR ;
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
          (* I suspect that in_bytes in not accurate, since I don't
           * get an exact countdown after than in_size=in_bytes, but there
           * is a stall at the beginning after which the countdown starts. *)
          Fmt.ticks_of_samples (int_of_float remaining_samples)
  in
    { Decoder.fill = fill ; Decoder.close = close }

let () =
  Decoder.formats#register "VORBIS"
    (fun name -> try Some (decoder name) with _ -> None)

let duration file =
  let dec = Vorbis.Decoder.openfile file in
  Vorbis.Decoder.duration dec (-1)

let metadatas ~format file =
  try
    if (format <> "VORBIS") then
      raise Not_found ;

    let dec = Vorbis.Decoder.openfile file in
    let ans =
      try
        snd (Vorbis.Decoder.comments dec (-1))
      with _ -> []
    in
      Vorbis.Decoder.close dec;
      ans
  with _ -> []

let () = Request.mresolvers#register "VORBIS" metadatas ;
         Request.dresolvers#register "VORBIS" duration
