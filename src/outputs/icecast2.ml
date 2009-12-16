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

let no_mount = "Use [name] with .ogg extension if relevant"
let no_name = "Use [mount]"

let proto kind =
  Output.proto @
  [ ("restart", Lang.bool_t, Some (Lang.bool false),
     Some "Restart output after a failure. By default, liquidsoap will stop \
           if the output failed.") ;
    ("restart_delay", Lang.int_t, Some (Lang.int 3),
     Some "Delay, in seconds, before attempting new connection, if restart \
           is enabled.") ;
    "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    ("protocol", Lang.string_t, (Some (Lang.string "http")),
     Some "Protocol of the streaming server: \
           'http' for Icecast, 'icy' for shoutcast.") ;
    "host", Lang.string_t, Some (Lang.string "localhost"), None ;
    "port", Lang.int_t, Some (Lang.int 8000), None ;
    ("user", Lang.string_t, Some (Lang.string "source"),
     Some "User for shout source connection. \
           Useful only in special cases, like with per-mountpoint users.") ;
    "password", Lang.string_t, Some (Lang.string "hackme"), None ;
    "genre", Lang.string_t, Some (Lang.string "Misc"), None ;
    "url", Lang.string_t, Some (Lang.string "http://savonet.sf.net"), None ;
    ("description", Lang.string_t,
     Some (Lang.string "OCaml Radio!"), None) ;
    "public", Lang.bool_t, Some (Lang.bool true), None ;
    ("headers", Lang.metadata_t,
     Some (Lang.list (Lang.product_t Lang.string_t Lang.string_t) []),
     Some "Additional headers.") ;
    ("format", Lang.string_t, Some (Lang.string ""),
     Some (Printf.sprintf
             "When empty, the encoder is used to guess.")) ;
    ("dumpfile", Lang.string_t, Some (Lang.string ""), 
     Some "Dump stream to file, for debugging purpose. Disabled if empty.") ;
    "", Lang.format_t kind, None, Some "Encoding format." ;
    "", Lang.source_t kind, None, None ]

(** Sending encoded data to a shout-compatible server.
  * It directly takes the Lang param list and extracts stuff from it. *)
class output ~kind p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let protocol =
    let v = List.assoc "protocol" p in
      match Lang.to_string v with
        | "http" -> Cry.Http
        | "icy" -> Cry.Icy
        | _ ->
            raise (Lang.Invalid_value
                     (v, "Valid values are 'http' (icecast) \
                          and 'icy' (shoutcast)"))
  in

  let source = Lang.assoc "" 2 p in
  let encoder_factory,ogg,icecast_info =
    let v = Lang.assoc "" 1 p in
    let enc = Lang.to_format v in
    let icecast_info,ogg =
      match enc with
        | Encoder.MP3 m ->
            { quality = Some (string_of_int m.Encoder.MP3.quality) ;
              bitrate = Some m.Encoder.MP3.bitrate ;
              samplerate = Some m.Encoder.MP3.samplerate ;
              channels = Some (if m.Encoder.MP3.stereo then 2 else 1)
            }, false
        | Encoder.Ogg o ->
            let info =
              match o with
                | [Encoder.Ogg.Vorbis
                     {Encoder.Vorbis.channels=n;
                                     mode=Encoder.Vorbis.VBR q;
                                     samplerate=s}]
                  ->
                    { quality = Some (string_of_float q) ;
                      bitrate = None ;
                      samplerate = Some s ;
                      channels = Some n }
                | [Encoder.Ogg.Vorbis
                     {Encoder.Vorbis.channels=n;
                                     mode=Encoder.Vorbis.ABR (_,b,_);
                                     samplerate=s}]
                | [Encoder.Ogg.Vorbis
                     {Encoder.Vorbis.channels=n;
                                     mode=Encoder.Vorbis.CBR b;
                                     samplerate=s}]
                  ->
                    { quality = None ;
                      bitrate = Some b ;
                      samplerate = Some s ;
                      channels = Some n }
                | _ ->
                    { quality = None ; bitrate = None ;
                      samplerate = None ; channels = None }
            in
            if protocol = Cry.Icy then
              raise (Lang.Invalid_value
                       (v, "icy protocol (shoutcast) does not support Ogg")) ;
              info, true
        | Encoder.WAV _ ->
            raise (Lang.Invalid_value (v, "WAV is not supported"))
    in
    let encoder_factory =
      try Encoder.get_factory enc with
        | Not_found ->
            raise (Lang.Invalid_value
                     (v, "No encoder found for that format"))
    in
      encoder_factory, ogg, icecast_info
  in

  let format =
    let f = s "format" in
      if f <> "" then Cry.content_type_of_string f else
        if ogg then Cry.ogg_application else Cry.mpeg
  in

  let mount = s "mount" in
  let name = s "name" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 (List.assoc "mount" p,
                  "Either name or mount must be defined"))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then
      if ogg then name ^ ".ogg" else name
    else
      mount
  in

  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let on_start =
    let f = List.assoc "on_start" p in
      fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
      fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in

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

  inherit Output.encoded
            ~content_kind:kind ~output_kind:"output.icecast"
            ~infallible ~autostart ~on_start ~on_stop
            ~name:mount source

  (** When is the last time we tried to connect. *)
  val mutable last_attempt = 0.

  (** File descriptor where to dump. *)
  val mutable dump = None

  val connection = Cry.create ()
  val mutable encoder = None

  method send b =
    match Cry.get_status connection with
      | Cry.Disconnected ->
          if Unix.time () > restart_delay +. last_attempt then begin
            ignore(self#reset_encoder (Hashtbl.create 0));
            self#output_start
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
                self#output_stop ;
                last_attempt <- Unix.time ()
            end

  (** It there's too much latency, we'll stop trying to catchup.
    * Reconnect to cancel the latency on the server's side too. *)
  method output_reset =
    self#output_stop ;
    self#output_start

  method output_start =

    assert (encoder = None) ;
    encoder <- Some (encoder_factory self#id) ;

    begin match dumpfile with
      | Some f -> dump <- Some (open_out_bin f)
      | None -> ()
    end ;

    assert (Cry.get_status connection = Cry.Disconnected) ;
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
        Printf.sprintf "liquidsoap %s" Configure.version
      in 
      let source = 
        Cry.connection ~host ~port ~user ~password
                       ~genre ~url ~description ~name
                       ~public ~protocol ~mount  
                       ~audio_info ~user_agent ~content_type:format ()
      in
      List.iter (fun (x,y) -> Hashtbl.add source.Cry.headers x y) headers;

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
            self#output_stop ;
            last_attempt <- Unix.time ()

  method output_stop =
    (* In some cases it might be possible to output the remaining data,
     * but it's not worth the trouble. *)
    ignore ((Utils.get_some encoder).Encoder.stop ()) ;
    encoder <- None ;
    self#log#f 3 "Closing connection..." ;
    begin match Cry.get_status connection with
      | Cry.Disconnected -> ()
      | Cry.Connected _ ->
          Cry.close connection
    end ;
    match dump with
      | Some f -> close_out f
      | None -> ()

  method encode frame ofs len =
    let enc = Utils.get_some encoder in
      enc.Encoder.encode frame ofs len

  method reset_encoder m =
    if ogg then
      (Utils.get_some encoder).Encoder.reset m
    else
      let get h k l =
        try
          (k,(Hashtbl.find h k))::l
        with _ -> l
      in
      let getd h k d l =
        try
          (k,(Hashtbl.find h k))::l
        with _ -> (k,d)::l
      in
      let def_title =
        match get m "uri" [] with
          | (_,s)::_ -> let title = Filename.basename s in
              ( try
                  String.sub title 0 (String.rindex title '.')
                with
                  | Not_found -> title )
          | [] -> "Unknown"
      in
      let default_song =
        let artist = try Hashtbl.find m "artist" with _ -> "" in
        let title = try Hashtbl.find m "title" with _ -> "Unknown" in
          artist^" - "^title
      in
      let a = Array.of_list
        (getd m "title" def_title
           (get m "artist"
              (get m "genre"
                 (get m "date"
                    (get m "album"
                       (get m "tracknum"
                          (get m "comment"
                             (* for Shoutcast *)
                             (getd m "song" default_song []))))))))
      in
      let m =
        let ret = Hashtbl.create 10 in
        let f (x,y) = Hashtbl.add ret x y in
          Array.iter f a; ret  
      in
        match Cry.get_status connection with
          | Cry.Connected _ ->
              (try Cry.update_metadata connection m ; "" with _ -> "")
          | Cry.Disconnected ->
              (* Do nothing if shout connection isn't available *)
              ""

end

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "output.icecast" ~category:Lang.Output
    ~descr:"Encode and output the stream to an icecast2 or shoutcast server."
    (proto k)
    ~kind:(Lang.Unconstrained k)
    (fun p kind -> ((new output kind p):>Source.source))
