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

type icecast_info = 
  {
    quality     : string option;
    bitrate     : int option;
    samplerate  : int option;
    channels    : int option
  }

let no_multicast = "no_multicast"

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
          'http' for Icecast, 'icy' for shoutcast." ;
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
    "headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
    Some (Lang.list []), Some "Additional headers." ;
    "dumpfile", Lang.string_t, Some (Lang.string ""), 
    Some "Dump stream to file, for debugging purpose. Disabled if empty."
  ]

(** Abstract class for sending encoded data to an shout-compatible server,
  * this class directly takes the Lang param list and does straightforward
  * extracting by itself.
  * The 'name' and 'mount' params are not extracted that way because the default
  * value for these depends on the format of the stream (ogg/mp3). *)
class virtual output
  ?(format=Cry.ogg_audio) ?(protocol=Cry.Http) 
  ~name ~mount ~source ~icecast_info p =

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
    let headers = 
      List.map (fun v -> 
                  let f (x,y) = 
                    Lang.to_string x, Lang.to_string y 
                  in
                  f (Lang.to_product v))
               (Lang.to_list (List.assoc "headers" p))
    in
                  

object (self)

  val mutable last_attempt = 0.
  val mutable dump = None

  val connection = Cry.create ()

  method virtual reset_encoder : (string,string) Hashtbl.t -> string
  method virtual log : Dtools.Log.t

  method send b =
    match Cry.get_status connection with
      | Cry.Disconnected ->
          if Unix.time () > restart_delay +. last_attempt then begin
            ignore(self#reset_encoder (Hashtbl.create 0));
            self#icecast_start
          end
      | Cry.Connected _ ->
          (* TODO think about some limitation of shout restarting *)
          begin try
            Cry.send connection b;
            match dump with
              | Some s -> output_string s b
              | None -> () 
          with
            | Cry.Error e ->
                self#log#f 2
                  "Cry socket error: timeout, network failure, \
                   server shutdown? Restarting the output in %.f seconds."
                  restart_delay  ;
                (* Ask for a restart after last_attempt. *)
                self#icecast_stop ;
                last_attempt <- Unix.time ()
            end

  method icecast_stop =
    match Cry.get_status connection with
      | Cry.Disconnected -> ()
      | Cry.Connected _ ->
          Cry.close connection;
          match dump with
            | Some f -> close_out f
            | None -> ()

  method icecast_start =
    assert (Cry.get_status connection = Cry.Disconnected) ;

    begin 
      match dumpfile with
        | Some f -> dump <- Some (open_out_bin f)
        | None -> ()
    end ;

      self#log#f 3 "Connecting mount %s for %s@%s..." mount user host ;
     
      let audio_info = Hashtbl.create 10 in
      let f x y z =
        match x with
          | Some q -> Hashtbl.add audio_info y (z q)
          | None -> ()
      in
      f icecast_info.bitrate "bitrate" string_of_int;
      f icecast_info.quality "quality" (fun x -> x);
      f icecast_info.samplerate "samplerate" string_of_int;
      f icecast_info.channels "channels" string_of_int;
      let user_agent =
        Printf.sprintf
          "liquidsoap %s"
          Configure.version
      in 
      let source = 
        Cry.connection ~host ~port ~user ~password
                       ~genre ~url ~description ~name
                       ~public ~protocol ~mount  
                       ~audio_info ~user_agent ~content_type:format ()
      in
      List.iter (fun (x,y) -> Hashtbl.add source.Cry.headers x y) headers;

      (* Final *)
      try
        begin try
          Cry.connect connection source
        with
          | Cry.Error x as e ->
              self#log#f 2
                "Connection failed: %s"
                (Cry.string_of_error x) ;
              raise e
        end ;

        self#log#f 3 "Connection setup was successful."
      with
        (* In restart mode, no_connect and no_login are not fatal.
         * The output will just try to reconnect later. *)
        | Cry.Error _ when restart -> 
            self#log#f 3
              "Connection failed, will try again in %.f sec."
              restart_delay ;
            self#icecast_stop ;
            last_attempt <- Unix.time ()

end
