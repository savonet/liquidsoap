(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class lastfm ~autostart ~bufferize ~max uri =
let playlist_mode = Http_source.First in
object (self)
  inherit Http_source.http ~playlist_mode ~autostart ~bufferize ~max uri as http

  (* Called when there's no decoding process, in order to create one. *)
  method connect url =
    try
      let tracks = Lastfm.get url in
      let (m,uri) =
        match tracks with
          | (m,uri) :: l -> (m,uri)
          | _ -> raise (Lastfm.Error Lastfm.Empty)
      in
      let metas = Hashtbl.create 2 in
        List.iter (fun (a,b) -> Hashtbl.add metas a b) m;
        http#insert_metadata metas;
        http#connect uri
    with
      | Lastfm.Error e ->
          self#log#f 4
            "Could not get file from lastfm: %s" (Lastfm.string_of_error e)
      | e -> self#log#f 4 "Lastfm connection failed: %s" (Printexc.to_string e)

  val mutable abort_stream = false

  method put sample_freq data =
    if abort_stream then begin
      abort_stream <- false ;
      failwith "streaming was aborted"
    end ;
    http#put sample_freq data

  method abort_track =
    abort_stream <- true

end

let () =
    Lang.add_operator "input.lastfm"
      ~category:Lang.Input
      ~descr:("Forwards the given lastfm stream. The relay can be "^
              "paused/resumed using the start/stop telnet commands.")
      [ "autostart", Lang.bool_t, Some (Lang.bool true),
        Some "Initially start relaying or not." ;
        "buffer", Lang.float_t, Some (Lang.float 2.),
        Some "Duration of the pre-buffered data." ;
        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." ;
        "", Lang.string_t, None,
        Some "URI of a lastfm  stream (e.g. lastfm://user/toots5446/playlist)."
      ]
      (fun p ->
         let uri = Lang.to_string (List.assoc "" p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let max = Lang.to_float (List.assoc "max" p) in
           ((new lastfm ~autostart ~bufferize ~max uri):>Source.source))
