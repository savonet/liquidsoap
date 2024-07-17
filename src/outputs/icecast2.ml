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

(** Output to an icecast server. *)

open Dtools
open Shout

let no_multicast = "no_multicast"

let _ = Shout.init ()

let proto =
  [ "start", Lang.bool_t, Some (Lang.bool true),
    Some "Start output threads on operator initialization." ;

    "host", Lang.string_t, Some (Lang.string "localhost"), None ;
    "port", Lang.int_t, Some (Lang.int 8000), None ;
    "user", Lang.string_t, Some (Lang.string "source"), None ;
    "password", Lang.string_t, Some (Lang.string "hackme"), None ;
    "genre", Lang.string_t, Some (Lang.string "Misc"), None ;
    "url", Lang.string_t, Some (Lang.string "http://savonet.sf.net"), None ;
    "description", Lang.string_t,
    Some (Lang.string "OCaml Radio!"), None ;
    "public", Lang.bool_t, Some (Lang.bool true), None ;
    "multicast_ip", Lang.string_t, Some (Lang.string no_multicast), None ]

(** Abstract class for sending encoded data to an shout-compatible server,
  * this class directly takes the Lang param list and does straightforward
  * extracting by itself.
  * The 'name' and 'mount' params are not extracted that way because the default
  * value for these depends on the format of the stream (ogg/mp3). *)
class virtual ['a] output
  ?(format=Shout.Format_vorbis) ?(protocol=Shout.Protocol_http)
  ~name ~mount ~source p =

    let e f v = f (List.assoc v p) in
    let s v = e Lang.to_string v in

    let autostart = e Lang.to_bool "start" in

    let host = s "host" in
    let port = e Lang.to_int "port" in
    let user = s "user" in
    let password = s "password" in
    let genre = s "genre" in
    let url = s "url" in
    let description = s "description" in
    let public = e Lang.to_bool "public" in
    let multicast_ip = s "multicast_ip" in

object (self)
  inherit
    ['a] Output.encoded
      ~autostart ~name:mount ~kind:"output.icecast" source
    as super

  val mutable connection = None

  val log = Log.log ~label:mount
  val logl = Log.logl ~label:mount

  method send b =
    match connection with
      | None -> assert false
      | Some c ->
          (* TODO think about some limitation of shout restarting *)
          begin try
            Shout.send c b
          with
            | Shout.Socket ->
                log 3 ("Shout socket error: timeout, network failure, "^
                       "server shutdown ? Restarting the output...") ;
                stop_output <- true ;
                start_output <- true
          end

  method output_stop =
    match connection with
      | None -> assert false
      | Some c -> Shout.close c

  method output_start =
    let conn = new_shout () in

      logl 3 (lazy (Log.f
                      "Connecting mount %s for %s@%s" 
                      mount user host)) ;

      set_host conn host ;
      set_port conn port ;
      set_user conn user ;
      set_password conn password ;

      set_genre conn genre ;
      set_url conn url ;
      set_description conn description ;

      set_name conn name ;
      set_mount conn mount ;

      set_format conn format ;
      set_protocol conn protocol ;
      set_public conn public ;

      if multicast_ip <> no_multicast then
        set_multicast_ip conn multicast_ip ;

      begin
        try
          open_shout conn ;
        with
          | No_connect ->
              failwith
                (Printf.sprintf
		   "unable to connect to icecast server %s:%d !"
		   host port)
	  | Socket ->
              failwith "invalid user or password for icecast login !"
          | No_login ->
              failwith "icecast mount point already taken !"
      end ;

      log 3 "Connection setup was successful." ;
      connection <- Some conn

end
