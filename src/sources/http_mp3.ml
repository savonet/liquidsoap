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

open Unix

(** Mp3 stream decoder. *)

let mime_types =
  Dtools.Conf.list ~p:(Http_source.conf_mime_types#plug "mp3")
    ~d:["audio/mpeg"]
    "Mime types associated to MP3 format"

let mp3_stream_log = Dtools.Log.make ["stream";"mp3"]

let decode_mp3 sink =
  let dec =
    Mad.openstream
      (fun len ->
         try
           let b = sink.Http_source.read len in
             b, (String.length b)
         with
           | e -> mp3_stream_log#f 2 "Read error %s" (Printexc.to_string e) ;
                  "", 0)
  in
    while true do
      try
        let frame = Mad.decode_frame_float dec in
        let samplerate, _, _ = Mad.get_output_format dec in
          sink.Http_source.put samplerate frame
      with
        | Mad.End_of_stream as e ->
            sink.Http_source.close () ;
            (* TODO check that mad doesn't close itself,
             * or at least that double-close doesn't hurt. *)
            Mad.close dec ;
            raise e
    done

let () =
  ignore
    (Dtools.Init.at_start
       (fun () ->
          List.iter
            (fun s -> Http_source.stream_decoders#register s decode_mp3)
            mime_types#get))
