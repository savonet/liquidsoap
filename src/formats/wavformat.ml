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

(** Decode WAV files. *)

let decoder file =
  let (w,format) = let w = Wav.fopen file in (w,(Wav.format w)) in
  let buf_gen = Mixer.Generator.create () in
  let buffer_size = Decoder.buffer_size () in
  let tmpbuf = String.create Mixer.Buffer.size in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    Wav.close w
  in
  let fill buf =
    assert (not !closed) ;

    begin
      try
        while Mixer.Generator.length buf_gen < buffer_size do
          let l = Wav.sample w tmpbuf 0 Mixer.Buffer.size in
            in_bytes := !in_bytes + l ;
            Mixer.Generator.feed buf_gen format
              (String.sub tmpbuf 0 l)
        done
      with End_of_file | Mixer.Generator.Invalid_format -> ()
    end ;

    let offset = Mixer.Buffer.position buf in
      Mixer.Buffer.fill buf buf_gen ;
      let added = (Mixer.Buffer.position buf) - offset in
        out_bytes := !out_bytes + added ;
        (* Compute an estimated number of remaining frames. *)
        let abglen = Mixer.Generator.length buf_gen in
          assert (!in_bytes!=0) ;
          let compression = (float (!out_bytes+abglen)) /. (float !in_bytes) in
          let remaining_bytes =
            (float (in_size - !in_bytes)) *. compression
            +. (float abglen)
          in
          let remaining_frames =
            int_of_float (ceil (remaining_bytes /. (float Mixer.Buffer.size)))
          in
            (* I suspect that in_bytes in not accurate, since I don't
             * get an exact countdown after than in_size=in_bytes, but there
             * is a stall at the beginning after which the countdown starts. *)
            remaining_frames

  in { Decoder.fill = fill ; Decoder.close = close }


let _ = Decoder.formats#register "WAV"
          (fun name -> try Some (decoder name) with _ -> None)
