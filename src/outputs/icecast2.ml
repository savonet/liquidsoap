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

(** Output to an icecast server. *)

open Dtools
open Shout

let no_multicast = "no_multicast"

let () = Shout.init ()

let proto =
  [ "start", Lang.bool_t, Some (Lang.bool true),
    Some "Start output threads on operator initialization." ;
    "restart", Lang.bool_t, Some (Lang.bool false),
    Some "Restart output after a failure. By default, liquidsoap will stop \
          if the output failed." ;
    "restart_delay", Lang.int_t, Some (Lang.int 3),
    Some "Delay, in seconds, before attempting new connection, if restart \
          is enabled." ;
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
class virtual ['a] output
  ?(format=Shout.Format_vorbis) ?(protocol=Shout.Protocol_http) ?bitrate
  ~name ~mount ~source p =

    let e f v = f (List.assoc v p) in
    let s v = e Lang.to_string v in

    let autostart = e Lang.to_bool "start" in
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
  inherit
    ['a] Output.encoded ~autostart ~name:mount ~kind:"output.icecast" source
    as super

  val mutable connection = None
  val mutable last_attempt = 0.
  val mutable dump = None

  initializer
    if sync then
      (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  method send b =
    match connection with
      | None ->
          if Unix.time () > restart_delay +. last_attempt then begin
            (* TODO The base output class ensures that start is called before
             *      send, and stop after them. This is broken if send calls
             *      directly these methods, like here. On the other hand,
             *      we don't seem to have another option here. Calling directly
             *      #reset_encoder would be catastrophic when inherited by
             *      lame_encoded.to_shout, which reset method assumes that shout
             *      is properly connected. Another downside of the current code
             *      is that the encoder gets restarted every delay seconds,
             *      even when shout doesn't actually get restarted. *)
            self#output_stop ;
            self#output_start
          end
      | Some c ->
          (* TODO think about some limitation of shout restarting *)
          begin try
            if sync then Shout.sync c ;
            Shout.send c b ;
            match dump with
              | Some s -> output_string s b
              | None -> () 
          with
            | Shout.Socket ->
                self#log#f 2
                  "Shout socket error: timeout, network failure, \
                   server shutdown? Restarting the output in %.f seconds."
                  restart_delay  ;
                (* Ask for a restart after last_attempt. *)
                self#output_stop ;
                last_attempt <- Unix.time ()
            end

  method output_stop =
    match connection with
      | None -> ()
      | Some c ->
          begin try Shout.close c with _ -> () end ;
          connection <- None ;
          match dump with
            | Some f -> close_out f
            | None -> ()

  method output_start =
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
      begin match bitrate with
        | Some br -> set_audio_info conn "bitrate" br
        | None -> ()
      end ;

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
            self#output_stop ;
            last_attempt <- Unix.time ()

  method output_reset =
    self#output_stop ;
    self#output_start

end
