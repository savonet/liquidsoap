(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

class lastfm ~kind ~autostart ~poll_delay ~submit ~track_on_meta 
             ~bufferize ~timeout ~bind_address ~user ~password 
             ~debug ~max ~user_agent ~audioscrobbler_host uri =
 let playlist_mode = Http_source.First in
 let bufferize_time = Frame.master_of_seconds bufferize in
object (self)
  inherit Http_source.http ~kind ~playlist_mode ~poll_delay ~timeout 
                           ~autostart ~bind_address ~bufferize 
                           ~max ~track_on_meta 
                           ~debug ~user_agent uri as http

  val mutable session = None
  val mutable latest_metadata = None

  (* Called when there's no decoding process, in order to create one. *)
  method connect url =
    (* Do nothing is the buffer is
     * still greater than bufferize.
     * This is not an active wait because 
     * the polling thread is timed. Hence,
     * we do not need to put the thread to sleep. *)
    if self#length > bufferize_time then ()
    else
     try
      (* user/password passed through the URL
       * come first.. *)
      let login,station,options = 
       try
        Lastfm.Radio.parse uri
       with
         | Lastfm.Radio.Error (Lastfm.Radio.Auth _) ->
            let subst x =
              Printf.sprintf "lastfm://%s:%s@" user password
            in
            let uri = Pcre.substitute ~pat:"lastfm://" ~subst uri
            in
            Lastfm.Radio.parse uri
      in
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
        let auth = (login.user,login.password) in
        if submit then 
          Liqfm.submit ~host:audioscrobbler_host 
               auth true Liqfm.Lastfm 
               Liqfm.NowPlaying [metas] ;
        latest_metadata <- Some (auth,metas) ;
        http#connect uri
     with
       | Lastfm.Radio.Error e ->
           session <- None ;
           self#log#f 4
             "Could not get file from lastfm: %s"
             (Lastfm.Radio.string_of_error e)
       | e -> self#log#f 4 "Lastfm connection failed: %s" (Printexc.to_string e)

  (* TODO abort streaming on #abort_track,
   *   setting relaying <- false is too radical, it would completely
   *   stop. *)

  method private get_frame ab = 
    http#get_frame ab ;
    if Frame.is_partial ab then
     begin
      match latest_metadata with
        | Some (auth,metas) -> 
          if submit then
            Liqfm.submit ~host:audioscrobbler_host
                 auth true Liqfm.Lastfm
                 Liqfm.Played [metas] ;
          latest_metadata <- None
        | None -> ()
     end

end

(** Values for max buffer and bufferize are 
  * tricky. Lastfm sends a burst of 10s of
  * data at the beginning of the streaming.
  * Hence, if we immediatly request the next
  * track when the current tracks end, the
  * burst sum-up and we end-up with 10 then
  * 20 then 30... seconds in the buffer.
  * Instead, we wait for the buffer to consume
  * its data up-to bufferize and then request the
  * next song.
  * Good values for max and bufferize are then 25
  * and 10. With these values, the initial buffer is
  * 10 so we start streaming immediatly. When the
  * first song ends, the buffer jumps from 10 to 20
  * with the next burst. When the second song ends, 
  * the buffer is emptied up-to 10, then we request
  * the next song and it jumps again to 20.
  * After the second song, the buffer strategy becomes
  * static, with an average buffer of 20s. *)
let () =
    Lang.add_operator "input.lastfm"
      (* I am not really sure,
       * but it seems that all lastfm tracks
       * are stereo.. *)
      ~kind:Lang.audio_stereo
      ~category:Lang.Input
      ~descr:"Forwards the given lastfm stream. The relay can be \
              paused/resumed using the start/stop telnet commands."
      [ "autostart", Lang.bool_t, Some (Lang.bool true),
        Some "Initially start relaying or not." ;
        "buffer", Lang.float_t, Some (Lang.float 10.),
        Some "Duration of the pre-buffered data." ;
        "bind_address", Lang.string_t, Some (Lang.string ""),
        Some "Address to bind on the local machine. \
              This option can be useful if your machine is bound to \
              multiple IPs. \"\" means no bind address." ;
        "timeout", Lang.float_t, Some (Lang.float 10.),
        Some "Timeout for HTTP connections." ;
        "poll_delay", Lang.float_t, (Some (Lang.float 2.)),
        Some "Polling delay." ;
        "submit", Lang.bool_t, Some (Lang.bool false),
        Some "Submit song to Audioscrobbler.";
        "submit_host", Lang.string_t, 
        Some (Lang.string !(Lastfm.Audioscrobbler.base_host)),
        Some "Host for audioscrobbling submissions.";
        "submit_port", Lang.int_t,
        Some (Lang.int !(Lastfm.Audioscrobbler.base_port)),
        Some "Port for audioscrobbling submissions.";
        "new_track_on_metadata", Lang.bool_t, Some (Lang.bool true),
        Some "Treat new metadata as new track." ;
        "debug", Lang.bool_t, Some (Lang.bool false),
        Some "Run in debugging mode by not catching some exceptions." ;
        "max", Lang.float_t, Some (Lang.float 25.),
        Some "Maximum duration of the buffered data." ;
       "user_agent", Lang.string_t,
        Some (Lang.string
            (Printf.sprintf "liquidsoap/%s (%s; ocaml %s)"
                Configure.version Sys.os_type Sys.ocaml_version)),
        Some "User agent." ;
        "user", Lang.string_t, None, Some "Lastfm user." ;
        "password", Lang.string_t, None, Some "Lastfm password." ;
        "", Lang.string_t, None,
        Some "URI of a lastfm  stream (e.g. lastfm://user/toots5446/playlist)."
      ]
      (fun p kind ->
         let uri = Lang.to_string (List.assoc "" p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let submit = Lang.to_bool (List.assoc "submit" p) in
         let track_on_meta =
           Lang.to_bool (List.assoc "new_track_on_metadata" p)
         in
         let debug = Lang.to_bool (List.assoc "debug" p) in
         let bind_address = Lang.to_string (List.assoc "bind_address" p) in
         let bind_address =
           match bind_address with
             | "" -> None
             | s -> Some s
         in
         let user_agent = Lang.to_string (List.assoc "user_agent" p) in
         let submit_host = Lang.to_string (List.assoc "submit_host" p) in
         let submit_port = Lang.to_int (List.assoc "submit_port" p) in
         let audioscrobbler_host = (submit_host,submit_port) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
	 let timeout = Lang.to_float (List.assoc "timeout" p) in
         let poll_delay =  Lang.to_float (List.assoc "poll_delay" p) in
         let max = Lang.to_float (List.assoc "max" p) in
         let user = Lang.to_string (List.assoc "user" p) in
         let password = Lang.to_string (List.assoc "password" p) in
         if bufferize > max then
           raise (Lang.Invalid_value
                    (List.assoc "max" p,
                     "Maximun buffering inferior to pre-buffered data"));
           ((new lastfm ~kind ~autostart ~submit ~poll_delay ~bufferize
                        ~track_on_meta ~audioscrobbler_host
                        ~bind_address ~timeout ~max ~debug 
                        ~user_agent ~user ~password uri):>Source.source))
