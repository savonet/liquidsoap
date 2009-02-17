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

(** Output to an icecast server. *)

open Dtools
open Shout

type icecast_info = 
  {
    quality     : string option;
    bitrate     : int option;
    samplerate  : int option;
    channels    : int option
  }

let no_multicast = "no_multicast"

let () = Shout.init ()

let proto ~no_mount ~no_name =
  [ "restart", Lang.bool_t, Some (Lang.bool false),
    Some "Restart output after a failure. By default, liquidsoap will stop \
          if the output failed." ;
    "restart_delay", Lang.int_t, Some (Lang.int 3),
    Some "Delay, in seconds, before attempting new connection, if restart \
          is enabled." ;
    "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "protocol", Lang.string_t, (Some (Lang.string "http")),
    Some "Protocol of the streaming server: \
          'http' for Icecast, 'icy' for Shoutcast." ;
    "host", Lang.string_t, Some (Lang.string "localhost"), None ;
    "port", Lang.int_t, Some (Lang.int 8000), None ;
    "user", Lang.string_t, Some (Lang.string "source"),
    Some "User for shout source connection. \
          Useful only in special cases, like with per-mountpoint users." ;
    "password", Lang.string_t, Some (Lang.string "hackme"), None ;
    "genre", Lang.string_t, Some (Lang.string "Misc"), None ;
    "url", Lang.string_t, Some (Lang.string "http://savonet.sf.net"), None ;
    "description", Lang.string_t,
    Some (Lang.string "OCaml Radio!"), None ;
    "public", Lang.bool_t, Some (Lang.bool true), None ;
    "multicast_ip", Lang.string_t, Some (Lang.string no_multicast), None ;
    "sync", Lang.bool_t, Some (Lang.bool false),
    Some "Let shout do the synchronization.";
    "dumpfile", Lang.string_t, Some (Lang.string ""), 
    Some "Dump stream to file, for debugging purpose. Disabled if empty."
  ]

(** Abstract class for sending encoded data to an shout-compatible server,
  * this class directly takes the Lang param list and does straightforward
  * extracting by itself.
  * The 'name' and 'mount' params are not extracted that way because the default
  * value for these depends on the format of the stream (ogg/mp3). *)
class virtual output
  ?(format=Shout.Format_vorbis) ?(protocol=Shout.Protocol_http) 
  ~name ~mount ~source ~icecast_info
  ?(raw=false) p =

    let e f v = f (List.assoc v p) in
    let s v = e Lang.to_string v in

    let restart = e Lang.to_bool "restart" in
    let restart_delay = float_of_int (e Lang.to_int "restart_delay") in
    let host = s "host" in
    let port = e Lang.to_int "port" in
    let user = s "user" in
    let password = s "password" in
    let genre = s "genre" in
    let url = s "url" in
    let dumpfile = 
      match s "dumpfile" with
        | "" -> None
	| s -> Some s
    in
    let description = s "description" in
    let public = e Lang.to_bool "public" in
    let multicast_ip = s "multicast_ip" in
    let sync = e Lang.to_bool "sync" in

object (self)

  val mutable connection = None
  val mutable last_attempt = 0.
  val mutable dump = None

  method virtual reset_encoder : (string,string) Hashtbl.t -> string
  method virtual log : Dtools.Log.t

  initializer
    if sync || raw then
      (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  method send b =
    match connection with
      | None ->
          if Unix.time () > restart_delay +. last_attempt then begin
            ignore(self#reset_encoder (Hashtbl.create 0));
            self#icecast_start
          end
      | Some c ->
          (* TODO think about some limitation of shout restarting *)
          begin try
            if sync then Shout.sync c ;
            if raw then 
              let blen = String.length b in
              let rec send offset =
                if offset < blen then 
                  let ret = 
                    Shout.send_raw c (String.sub b offset (blen - offset))
                  in
                  send (offset+ret)
              in
              send 0
            else
              Shout.send c b;
            match dump with
              | Some s -> output_string s b
              | None -> () 
          with
            | Shout.Socket 
            | Shout.Send_error ->
                self#log#f 2
                  "Shout socket error: timeout, network failure, \
                   server shutdown? Restarting the output in %.f seconds."
                  restart_delay  ;
                (* Ask for a restart after last_attempt. *)
                self#icecast_stop ;
                last_attempt <- Unix.time ()
            end

  method icecast_stop =
    match connection with
      | None -> ()
      | Some c ->
          begin try Shout.close c with _ -> () end ;
          connection <- None ;
          match dump with
            | Some f -> close_out f
            | None -> ()

  method icecast_start =
    assert (connection = None) ;
    let conn = new_shout () in

    begin 
      match dumpfile with
        | Some f -> dump <- Some (open_out_bin f)
        | None -> ()
    end ;

      self#log#f 3 "Connecting mount %s for %s@%s..." mount user host ;

      set_host conn host ;
      set_port conn port ;
      set_user conn user ;
      set_password conn password ;

      set_genre conn genre ;
      set_url conn url ;
      set_description conn description ;

      set_name conn name ;

      set_format conn format ;
      set_public conn public ;

      set_protocol conn protocol ;
      if protocol <> Shout.Protocol_icy then set_mount conn mount ;
      let f x y z = 
        match x with
          | Some q -> set_audio_info conn y (z q)
          | None -> ()
      in
      f icecast_info.bitrate "bitrate" string_of_int;
      f icecast_info.quality "quality" (fun x -> x);
      f icecast_info.samplerate "samplerate" string_of_int;
      f icecast_info.channels "channels" string_of_int;

      if multicast_ip <> no_multicast then
        set_multicast_ip conn multicast_ip ;

      set_agent conn
        (Printf.sprintf
          "liquidsoap %s (%s)"
          Configure.version
          (get_agent conn)) ;

      (* Final *)
      try
        begin try
          open_shout conn
        with
          | No_connect as e ->
              self#log#f 2
                "Unable to connect to icecast server %s:%d!"
                host port ;
              raise e
          | Socket as e ->
              self#log#f 2  "Invalid user or password for icecast login!" ;
              raise e
          | No_login as e ->
              self#log#f 2
                "Icecast mount point already taken, or wrong password!" ;
              raise e
        end ;

        self#log#f 3 "Connection setup was successful." ;
        connection <- Some conn
      with
        (* In restart mode, no_connect and no_login are not fatal.
         * The output will just try to reconnect later. *)
        | No_connect | No_login | Socket when restart -> 
            self#log#f 3
              "Connection failed, will try again in %.f sec."
              restart_delay ;
            self#icecast_stop ;
            last_attempt <- Unix.time ()

end
