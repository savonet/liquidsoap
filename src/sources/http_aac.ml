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

exception Frame_not_found

let buflen = 1024

(** AAC stream decoder. *)

let decode_aac sink =
  let dec = Faad.create () in
  let buf = ref "" in
  let feed_buf () =
    try
      buf := !buf ^ (sink.Http.read (buflen - String.length !buf))
    with
      | e -> Dtools.Log.log ~label:"aac_stream" 2
               (Dtools.Log.f "Read error %s"
                  (Printexc.to_string e))
  in
  let samplerate, channels =
    feed_buf ();
    let n =
      try
        Faad.find_frame !buf
      with Not_found ->
        sink.Http.close ();
        Faad.close dec;
        raise Frame_not_found
    in
      buf := String.sub !buf n (String.length !buf - n);
      feed_buf ();
      Faad.init dec !buf 0 (String.length !buf)
  in
    try
      while true do
        feed_buf ();
        let consumed, frame = Faad.decode dec !buf 0 (String.length !buf) in
          buf := String.sub !buf consumed (String.length !buf - consumed);
          sink.Http.put samplerate frame
      done
    with
      | Faad.Error n as e ->
          Dtools.Log.log ~label:"aac_stream" 2
            (Dtools.Log.f "Faad error: %s" (Faad.get_error_message n));
          sink.Http.close ();
          Faad.close dec;
          raise e

let () =
  Http.stream_decoders#register "audio/aac" decode_aac;
  Http.stream_decoders#register "audio/aacp" decode_aac
