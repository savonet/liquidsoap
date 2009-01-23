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

open Lastfm

class lastfm ~autostart ~poll_delay ~submit ~track_on_meta 
             ~feed_delay ~bufferize ~timeout ~bind_address 
             ~debug ~max ~user_agent uri =
let playlist_mode = Http_source.First in
object (self)
  inherit Http_source.http ~playlist_mode ~poll_delay ~timeout 
                           ~autostart ~bind_address ~bufferize 
                           ~feed_delay ~max ~track_on_meta 
                           ~debug ~user_agent uri as http

  val mutable session = None

  (* Called when there's no decoding process, in order to create one. *)
  method connect url =
    try
      let login,station,options = Lastfm.Radio.parse uri in
      let id = 
        match session with
	  | Some (l,v) when l = login -> v
	  | _ ->  let id = Lastfm.Radio.init login in
                  session <- Some (login,id) ;
                  id
      in
      let tracks = 
        try
          ignore(Lastfm.Radio.adjust id station);
          Lastfm.Radio.tracks id options
	with
	  | Lastfm.Radio.Error _ -> 
              (* Give another try in case of expired session *)
	      Lastfm.Radio.clear id ;
	      let id = Lastfm.Radio.init login in
	      session <- Some (login,id) ;
              ignore(Lastfm.Radio.adjust id station);
	      Lastfm.Radio.tracks id options
      in
      let (m,uri) =
        match tracks with
          | (m,uri) :: l -> (m,uri)
          | _ -> Lastfm.Radio.clear id ; 
	         raise (Lastfm.Radio.Error Lastfm.Radio.Empty)
      in
      let metas = Hashtbl.create 2 in
        List.iter (fun (a,b) -> Hashtbl.add metas a b) m;
        http#insert_metadata metas;
        if submit then 
          begin
            match login with
             | Some s -> Liqfm.submit (s.user,s.password) true Liqfm.Lastfm [metas]
             | None -> 
                 self#log#f 3 
                   "Lastfm Submission failed: not authenficated"
          end ;
        http#connect uri
    with
      | Lastfm.Radio.Error e ->
          session <- None ;
          self#log#f 4
            "Could not get file from lastfm: %s" (Lastfm.Radio.string_of_error e)
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
        "bind_address", Lang.string_t, Some (Lang.string ""),
        Some ("address to bind on the local machine. This option can be useful if " ^
        "your machine is bound to multiple IPs. \"\" means no bind address.") ;
        "timeout", Lang.float_t, Some (Lang.float 10.),
        Some "Timeout for HTTP connections." ;
        "poll_delay", Lang.float_t, (Some (Lang.float 2.)),
        Some "Polling delay." ;
        "submit", Lang.bool_t, Some (Lang.bool false),
        Some "Submit song to Audioscrobbler. \
           Only when the url is not anonymous, e.g. \
           <code>lastfm://user:password@artist/foo</code>." ;
        "new_track_on_metadata", Lang.bool_t, Some (Lang.bool true),
        Some "Treat new metadata as new track." ;
        "debug", Lang.bool_t, Some (Lang.bool false),
        Some "Run in debugging mode by not catching some exceptions." ;
        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." ;
        "feed_delay", Lang.float_t, Some (Lang.float (-1.)),
        Some "Feeding delay to apply when the buffer is full. \
              This setting can lead to disconnections when the \
              source is not pulled. If positive, the feeding thread \
              will wait for the given delay before dropping data when \
              the buffer is full." ;
       "user_agent", Lang.string_t,
        Some (Lang.string
            (Printf.sprintf "liquidsoap/%s (%s; ocaml %s)"
                Configure.version Sys.os_type Sys.ocaml_version)),
        Some "User agent." ;
        "", Lang.string_t, None,
        Some "URI of a lastfm  stream (e.g. lastfm://user/toots5446/playlist)."
      ]
      (fun p ->
         let uri = Lang.to_string (List.assoc "" p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let submit = Lang.to_bool (List.assoc "submit" p) in
         let track_on_meta = Lang.to_bool (List.assoc "new_track_on_metadata" p) in
         let debug = Lang.to_bool (List.assoc "debug" p) in
         let bind_address = Lang.to_string (List.assoc "bind_address" p) in
         let bind_address =
           match bind_address with
             | "" -> None
             | s -> Some s
         in
         let user_agent = Lang.to_string (List.assoc "user_agent" p) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
	 let timeout = Lang.to_float (List.assoc "timeout" p) in
         let poll_delay =  Lang.to_float (List.assoc "poll_delay" p) in
         let feed_delay =  Lang.to_float (List.assoc "feed_delay" p) in
         let max = Lang.to_float (List.assoc "max" p) in
           ((new lastfm ~autostart ~submit ~poll_delay ~bufferize ~track_on_meta 
                        ~feed_delay ~bind_address ~timeout ~max ~debug 
                        ~user_agent uri):>Source.source))
