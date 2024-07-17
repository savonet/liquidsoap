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

exception Internal

let decoder file =
  let (w,format) = 
    try
      let w = Wav.fopen file in (w,(Wav.format w))   
    with
      | Wav.Not_a_wav_file s -> failwith "wavformat.ml internal"
  in
  let buf_gen = Mixer.Generator.create () in
  let tmpbuf = String.create Mixer.Buffer.size in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let remaining () = 
    if !in_bytes = 0 || !out_bytes = 0
    then -1
    else
      max 0
	((in_size - !in_bytes) /
	 (!in_bytes / (
	    (!out_bytes+(Mixer.Generator.length buf_gen))
	    /Mixer.Buffer.size
	  ))
	)
  in
  let closed = ref false in
  let close () = if not !closed then ( closed := true ; Wav.close w ) in
  let fill buf =

    begin
      try
        while Mixer.Generator.should_be_feeded buf_gen 
        do
          let l = 
            Wav.sample w tmpbuf 0 Mixer.Buffer.size 
          in
            in_bytes := !in_bytes + l ;
            try 
              Mixer.Generator.feed buf_gen format
                (String.sub tmpbuf 0 l)
            with
              | Mixer.Generator.Invalid_format -> raise Internal
        done ;
      with End_of_file | Internal -> ()
    end ;

    let offset = Mixer.Buffer.position buf in
      Mixer.Buffer.fill buf buf_gen ;
      let added = (Mixer.Buffer.position buf) - offset in
        out_bytes := !out_bytes + added ;
        if added = 0 
        then (close () ; 0) 
        else remaining ()

  in { Decoder.fill = fill ; Decoder.close = close }


let _ = Decoder.formats#register "WAV"
          (fun name -> try Some (decoder name) with _ -> None)
