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

open Unix

(** Mp3 stream decoder. *)

let decode_mp3 sink =
  let dec =
    Mad.openstream
      (fun len ->
         try
           let b = sink.Http.read len in
             b, (String.length b)
         with
           | e -> Dtools.Log.log ~label:"mp3_stream" 2
                    (Dtools.Log.f "Read error %s"
                       (Printexc.to_string e)) ;
                  "", 0)
  in
    while true do
      try
        let frame = Mad.decode_frame_float dec in
        let samplerate, _, _ = Mad.get_output_format dec in
          sink.Http.put samplerate frame
      with
        | Mad.End_of_stream as e ->
            sink.Http.close () ;
            (* TODO check that mad doesn't close itself,
             * or at least that double-close doesn't hurt. *)
            Mad.close dec ;
            raise e
    done

let () =
  Http.stream_decoders#register "audio/mpeg" decode_mp3
