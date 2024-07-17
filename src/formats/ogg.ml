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

(** Decode and read metadatas of ogg/vorbis files. *)

let bytes_to_get = 1024*64

let _ = Vorbis.set_charset "UTF-8"

let decoder file = 
  let info = Vorbis.get_info ~duration:false file in
  let format = {
    Vorbis.sample_size = Mixer.Buffer.format.Mixer.sample_size ;
    Vorbis.big_endian = Mixer.Buffer.format.Mixer.big_endian ;
    Vorbis.signed = Mixer.Buffer.format.Mixer.signed ;
  } in
  let mixer_format = {
    Mixer.Buffer.format with
      Mixer.channels = info.Vorbis.audio_channels ;
      Mixer.sample_freq = info.Vorbis.audio_sample_rate 
  } in
  let unix_fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let fd = Vorbis.open_dec_fd unix_fd format in
  let abg = Mixer.Generator.create () in
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
	    (!out_bytes+(Mixer.Generator.length abg))
	    /Mixer.Buffer.size
	  ))
	)
  in
  let tmpbuf = String.create bytes_to_get in
  let close () = Vorbis.close_dec_file fd in
  let fill buf =

    begin
      try
        while Mixer.Generator.should_be_feeded abg do
          let l = Vorbis.decode fd tmpbuf 0 bytes_to_get 
          in
            in_bytes := Unix.lseek unix_fd 0 Unix.SEEK_CUR ;
            Mixer.Generator.feed abg mixer_format
              (String.sub tmpbuf 0 l) ;
        done ;
      with _ -> ()
    end ;

    let offset = Mixer.Buffer.position buf in
      Mixer.Buffer.fill buf abg ;
      let added = (Mixer.Buffer.position buf) - offset in
        out_bytes := !out_bytes + added ;
        if added = 0 then 0 else remaining ()
  in
    { Decoder.fill = fill ; Decoder.close = close }

let _ =
  Decoder.formats#register "OGG"
    (fun name -> try Some (decoder name) with _ -> None)

let metadatas file =
  try
    let (vs,comments) = Vorbis.get_comments file in
      Array.to_list comments
  with
    | _ -> []

let _ = Request.mresolvers#register "OGG" metadatas
