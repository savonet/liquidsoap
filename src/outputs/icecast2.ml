(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

let error_translator =
  function
    | Cry.Error _ as e ->
       Some (Cry.string_of_error e)
    | _ -> None

let () = Utils.register_error_translator error_translator

type icecast_info = 
  {
    quality     : string option;
    bitrate     : int option;
    samplerate  : int option;
    channels    : int option
  }

module Icecast = 
struct
  type content = Cry.content_type

  let format_of_content x =
    match x with
      | x when x = Icecast_utils.mpeg_mime -> Cry.mpeg
      | x when x = Icecast_utils.ogg_application_mime -> Cry.ogg_application
      | x when x = Icecast_utils.ogg_audio_mime -> Cry.ogg_audio
      | x when x = Icecast_utils.ogg_video_mime -> Cry.ogg_video
      | _ -> Cry.content_type_of_string x

  type info = icecast_info

  let info_of_encoder =
    function
        | Encoder.MP3 m ->
            let quality,bitrate = 
              match m.Encoder.MP3.bitrate_control with
                | Encoder.MP3.CBR x -> None,(Some x)
                | Encoder.MP3.ABR x -> None,(Some x.Encoder.MP3.mean_bitrate)
                | Encoder.MP3.VBR q -> (Some (string_of_int q)),None
            in
            { quality = quality ;
              bitrate = bitrate ;
              samplerate = Some m.Encoder.MP3.samplerate ;
              channels = Some (if m.Encoder.MP3.stereo then 2 else 1)
            }
        | Encoder.Shine m ->
            { quality = None ;
              bitrate = Some m.Encoder.Shine.bitrate ;
              samplerate = Some m.Encoder.Shine.samplerate ;
              channels = Some m.Encoder.Shine.channels
            }
        | Encoder.AACPlus m ->
            { quality = None ;
              bitrate = Some m.Encoder.AACPlus.bitrate ;
              samplerate = Some m.Encoder.AACPlus.samplerate ;
              channels = Some m.Encoder.AACPlus.channels
            }
        | Encoder.VoAacEnc m ->
            { quality = None ;
              bitrate = Some m.Encoder.VoAacEnc.bitrate ;
              samplerate = Some m.Encoder.VoAacEnc.samplerate ;
              channels = Some m.Encoder.VoAacEnc.channels
            }
        | Encoder.FdkAacEnc m ->
            { quality = None ;
              bitrate = Some m.Encoder.FdkAacEnc.bitrate ;
              samplerate = Some m.Encoder.FdkAacEnc.samplerate ;
              channels = Some m.Encoder.FdkAacEnc.channels
            }
        | Encoder.External m ->
            { quality = None ;
              bitrate = None ;
              samplerate = Some m.Encoder.External.samplerate ;
              channels = Some m.Encoder.External.channels
            }
        | Encoder.GStreamer m ->
            { quality    = None ;
              bitrate    = None ;
              samplerate = None ;
              channels   = Some (Encoder.GStreamer.audio_channels m)
            }
        | Encoder.Flac m ->
            { quality = Some (string_of_int m.Encoder.Flac.compression) ;
              bitrate = None ;
              samplerate = Some m.Encoder.Flac.samplerate ;
              channels = Some m.Encoder.Flac.channels
            }
        | Encoder.WAV m ->
            { quality = None ;
              bitrate = None ;
              samplerate = Some m.Encoder.WAV.samplerate ;
              channels = Some m.Encoder.WAV.channels
            }
        | Encoder.AVI m ->
            { quality = None ;
              bitrate = None ;
              samplerate = Some m.Encoder.AVI.samplerate ;
              channels = Some m.Encoder.AVI.channels
            }
        | Encoder.Ogg o ->
            match o with
              | [Encoder.Ogg.Vorbis
                   {Encoder.Vorbis.channels=n;
                                   mode=Encoder.Vorbis.VBR q;
                                   samplerate=s;_}]
                ->
                  { quality = Some (string_of_float q) ;
                    bitrate = None ;
                    samplerate = Some s ;
                    channels = Some n }
              | [Encoder.Ogg.Vorbis
                   {Encoder.Vorbis.channels=n;
                                   mode=Encoder.Vorbis.ABR (_,b,_);
                                   samplerate=s;_}]
                ->
                  { quality = None ;
                    bitrate = b ;
                    samplerate = Some s ;
                    channels = Some n }
              | [Encoder.Ogg.Vorbis
                   {Encoder.Vorbis.channels=n;
                                   mode=Encoder.Vorbis.CBR b;
                                   samplerate=s;_}]
                ->
                  { quality = None ;
                    bitrate = Some b ;
                    samplerate = Some s ;
                    channels = Some n }
              | _ ->
                  { quality = None ; bitrate = None ;
                    samplerate = None ; channels = None }
end

module M = Icecast_utils.Icecast_v(Icecast)

open M

let no_mount = "Use [name] with .ogg extension if relevant"
let no_name = "Use [mount]"

let user_agent = Lang.product (Lang.string "User-Agent")
                              (Lang.string Http.user_agent)

let proto kind =
  Output.proto @ (Icecast_utils.base_proto kind) @
  [ "mount", Lang.string_t, Some (Lang.string no_mount),
     Some "Source mount point. Mandatory when streaming to icecast." ;
    "icy_id", Lang.int_t, Some (Lang.int 1),
     Some "Shoutcast source ID. Only supported by Shoutcast v2." ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "host", Lang.string_t, Some (Lang.string "localhost"), None ;
    "port", Lang.int_t, Some (Lang.int 8000), None ;
    "connection_timeout", Lang.float_t, Some (Lang.float 5.),
    Some "Timeout for establishing network connections (disabled is negative).";
    "timeout", Lang.float_t, Some (Lang.float 30.),
    Some "Timeout for network read and write.";
    ("user", Lang.string_t, Some (Lang.string ""),
     Some "User for shout source connection. Defaults to \"source\" for icecast connections. \
           Useful only in special cases, like with per-mountpoint users.") ;
    "password", Lang.string_t, Some (Lang.string "hackme"), None ;
    "encoding", Lang.string_t, Some (Lang.string ""),
     Some "Encoding used to send metadata. If empty, defaults to \"UTF-8\" \
           for \"http\" protocol and \"ISO-8859-1\" for \"icy\" \
           protocol." ;
    "genre", Lang.string_t, Some (Lang.string "Misc"), None ;
    "protocol", Lang.string_t, (Some (Lang.string "http")),
     Some "Protocol of the streaming server: \
          'http' for Icecast, 'icy' for shoutcast." ;
    "verb", Lang.string_t, (Some (Lang.string "source")),
     Some "Verb to use with the 'http' protocol. One of: \
           'source', 'put' or 'post'.";
    "chunked", Lang.bool_t, (Some (Lang.bool false)),
     Some "Used cunked transfer with the 'http' protocol.";
    "icy_metadata", Lang.string_t, Some (Lang.string "guess"),
     Some "Send new metadata using the ICY protocol. \
           One of: \"guess\", \"true\", \"false\"";
    "url", Lang.string_t, Some (Lang.string "http://savonet.sf.net"), None ;
    ("description", Lang.string_t,
     Some (Lang.string "Liquidsoap Radio!"), None) ;
    "on_connect",
    Lang.fun_t [] Lang.unit_t,
    Some (Lang.val_cst_fun [] Lang.unit),
    Some "Callback executed when connection is established." ;
    "on_disconnect",
    Lang.fun_t [] Lang.unit_t,
    Some (Lang.val_cst_fun [] Lang.unit),
    Some "Callback executed when connection stops."  ;
    "on_error",
    Lang.fun_t [false, "", Lang.string_t] Lang.float_t,
    Some (Lang.val_cst_fun ["", Lang.string_t, None] (Lang.float 3.)),
    Some "Callback executed when an error happens. The callback receives a \
          string representation of the error that occured and returns a float. \
          If returned value is positive, connection will be tried again after \
          this amount of time (in seconds)." ;
    "public", Lang.bool_t, Some (Lang.bool true), None ;
    ("headers", Lang.metadata_t,
     Some (Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) [user_agent]),
     Some "Additional headers.") ;
    ("dumpfile", Lang.string_t, Some (Lang.string ""), 
     Some "Dump stream to file, for debugging purpose. Disabled if empty.") ;
    "", Lang.source_t kind, None, None ]

(** Sending encoded data to a shout-compatible server.
  * It directly takes the Lang param list and extracts stuff from it. *)
class output ~kind p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let on_connect = List.assoc "on_connect" p in
  let on_disconnect = List.assoc "on_disconnect" p in
  let on_error = List.assoc "on_error" p in
  let on_connect () = ignore (Lang.apply ~t:Lang.unit_t on_connect []) in
  let on_disconnect () = ignore (Lang.apply ~t:Lang.unit_t on_disconnect []) in
  let on_error error =
    let msg = Printexc.to_string error in
    Lang.to_float (Lang.apply ~t:Lang.unit_t on_error ["", Lang.string msg]) 
  in

  let data = encoder_data p in

  let chunked = Lang.to_bool (List.assoc "chunked" p) in

  let protocol = 
    let verb =
      let v = List.assoc "verb" p in
      match Lang.to_string v with
        | "source" -> Cry.Source
        | "put"    -> Cry.Put
        | "post"   -> Cry.Post
        | _ -> raise (Lang.Invalid_value
                   (v, "Valid values are: 'source' \
                       'put' or 'post'.")) 
    in  
    let v = List.assoc "protocol" p in
    match Lang.to_string v with
      | "http" -> Cry.Http verb
      | "icy" -> Cry.Icy
      | _ ->
          raise (Lang.Invalid_value
                   (v, "Valid values are 'http' (icecast) \
                        and 'icy' (shoutcast)"))
  in
  let icy_metadata =
    let v = List.assoc "icy_metadata" p in
    let icy =
       match Lang.to_string v with
         | "guess" -> `Guess
         | "true"  -> `True
         | "false" -> `False
         | _ ->
               raise (Lang.Invalid_value
                       (v, "Valid values are 'guess', \
                           'true' or 'false'"))
    in
    match data.format, icy with
      | _, `True -> true
      | _, `False -> false
      | x, `Guess when x = mpeg ||
                       x = wav ||
                       x = aac ||
                       x = aacplus ||
                       x = flac -> true
      | x, `Guess when x = ogg_application ||
                       x = ogg_audio ||
                       x = ogg_video -> false
      | _, _ ->
           raise (Lang.Invalid_value
                    (List.assoc "icy_metadata" p,
                     "Could not guess icy_metadata for this format, \
                     please specify either 'true' or 'false'."))
  in

  let ogg =
    match data.format with
     | x when x = Cry.ogg_application -> true
     | x when x = Cry.ogg_audio -> true
     | x when x = Cry.ogg_video -> true
     | _ -> false
  in

  let out_enc =
    match Lang.to_string (List.assoc "encoding" p) with
      | "" ->
         if protocol = Cry.Icy then
           "ISO-8859-1"
         else
           "UTF-8"
      | s -> Utils.StringCompat.uppercase_ascii s
  in

  let source = Lang.assoc "" 2 p in

  let icy_id =
    Lang.to_int (List.assoc "icy_id" p)
  in

  let mount = s "mount" in
  let name = s "name" in
  let name =
    match protocol, name, mount with
      | Cry.Http _, name, mount when name = no_name && mount = no_mount ->
        raise (Lang.Invalid_value
                 (List.assoc "mount" p,
                  "Either name or mount must be defined for icecast sources."))
      | Cry.Icy, name, _ when name = no_name -> Printf.sprintf "sc#%i" icy_id
      | _, name, mount when name = no_name -> mount
      | _ -> name
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

  let host = s "host" in
  let port = e Lang.to_int "port" in
  let user =
    match protocol, s "user" with
      | Cry.Http _, "" -> "source"
      | _, user -> user 
  in
  let password = s "password" in
  let genre = s "genre" in
  let url = s "url" in
  let timeout = e Lang.to_float "timeout" in
  let connection_timeout = 
    let v = e Lang.to_float "connection_timeout" in
    if v > 0. then
      Some v
    else 
      None
  in
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
  let connection = Cry.create ~timeout ?connection_timeout () in

object (self)

  inherit Output.encoded
            ~content_kind:kind ~output_kind:"output.icecast"
            ~infallible ~autostart ~on_start ~on_stop
            ~name:mount source

  (** In this operator, we don't exactly follow the start/stop
    * mechanism of Output.encoded because we want to control
    * in a more subtle way the connection/disconnection with
    * icecast.
    * So we have specific icecast_start/stop procedures that
    * only deal with the shout connection.
    * And the global output_start/stop also deal with the encoder.
    * As a result, if shout gets disconnected, encoding will keep
    * going, and the sending will keep being attempted, which
    * will at some point trigger a restart. *)

  (** Time after which we should attempt to connect. *)
  val mutable restart_time = 0.

  (** File descriptor where to dump. *)
  val mutable dump = None

  val mutable encoder = None

  method encode frame ofs len =
    (* We assume here that there always is
     * an encoder available when the source
     * is connected. *)
    match Cry.get_status connection, encoder with
      | Cry.Connected _, Some enc ->
         enc.Encoder.encode frame ofs len
      | _ -> ""

  method insert_metadata m =
    (* Update metadata using ICY if told to.. *)
    if icy_metadata then
     begin
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
      let m = Encoder.Meta.to_metadata m in
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
        (try Hashtbl.find m "artist" ^ " - " with _ -> "") ^
        (try Hashtbl.find m "title" with _ -> "Unknown")
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
      let f = Configure.recode_tag ~out_enc in
      let a = Array.map (fun (x,y) -> (x, f y)) a in
      let m =
        let ret = Hashtbl.create 10 in
        let f (x,y) = Hashtbl.add ret x y in
          Array.iter f a; ret  
      in
        match Cry.get_status connection with
          | Cry.Connected _ ->
              (try 
                 Cry.update_metadata ~charset:out_enc connection m 
               with e -> self#log#f 3 "Metadata update may have failed with \
                                       error: %s" 
                             (Printexc.to_string e))
          | Cry.Disconnected -> ()
              (* Do nothing if shout connection isn't available *)
     end 
    else
      (* Encoder is not always present.. *)
      match encoder with
        | Some encoder -> encoder.Encoder.insert_metadata m 
        | None -> ()

  method send b =
    match Cry.get_status connection with
      | Cry.Disconnected ->
          if Unix.time () > restart_time then begin
            self#icecast_start
          end
      | Cry.Connected _ ->
          begin try
            Cry.send connection b;
            match dump with
              | Some s -> output_string s b
              | None -> () 
          with
            | e ->
                self#log#f 2 "Error while sending data: %s!" (Printexc.to_string e) ;
                let delay = on_error e in
                if delay >= 0. then
                 begin
                  (* Ask for a restart after [restart_time]. *)
                  self#icecast_stop ;
                  restart_time <- Unix.time () +. delay ;
                  self#log#f 3
                    "Will try to reconnect in %.02f seconds."
                    delay
                 end
                else
                  raise e
            end

  (** It there's too much latency, we'll stop trying to catchup.
    * Reconnect to cancel the latency on the server's side too. *)
  method output_reset =
    self#output_stop ;
    self#output_start

  method output_start =
    self#icecast_start

  method output_stop =
    self#icecast_stop

  method icecast_start =
    assert (encoder = None) ;
    let enc = data.factory self#id in
    encoder <- 
       Some (enc (Encoder.Meta.empty_metadata)) ; 
    assert (Cry.get_status connection = Cry.Disconnected) ;
    begin match dumpfile with
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
      f data.info.bitrate "bitrate" string_of_int;
      f data.info.quality "quality" (fun x -> x);
      f data.info.samplerate "samplerate" string_of_int;
      f data.info.channels "channels" string_of_int;
      let user_agent =
        try
          List.assoc "User-Agent" headers
        with
          | Not_found -> Printf.sprintf "liquidsoap %s" Configure.version
      in 
      let source = 
        Cry.connection ~host ~port ~user ~password
                       ~genre ~url ~description ~name
                       ~public ~protocol ~mount ~icy_id ~chunked 
                       ~audio_info ~user_agent ~content_type:data.format ()
      in
      List.iter (fun (x,y) -> 
                      (* User-Agent has already been passed to Cry.. *)
                      if x <> "User-Agent" then 
                        Hashtbl.add source.Cry.headers x y) headers;

      try
        Cry.connect connection source ;
        self#log#f 3 "Connection setup was successful." ;
        let c = Cry.get_connection_data connection in
        Unix.setsockopt c.Cry.data_socket Unix.TCP_NODELAY true;
        (* Execute on_connect hook. *)
        on_connect () ;
      with
        (* In restart mode, no_connect and no_login are not fatal.
         * The output will just try to reconnect later. *)
        | e ->
            self#log#f 2 "Connection failed: %s" (Printexc.to_string e) ;
            let delay = on_error e in
            if delay >= 0. then
             begin
              self#log#f 3
                "Will try again in %.02f sec."
                delay ;
              self#icecast_stop ;
              restart_time <- Unix.time () +. delay
             end
            else
              raise e

  method icecast_stop =
    (* In some cases it might be possible to output the remaining data,
     * but it's not worth the trouble. *)
    begin try
      ignore ((Utils.get_some encoder).Encoder.stop ()) ;
    with _ -> () end ;
    encoder <- None ;
    begin match Cry.get_status connection with
      | Cry.Disconnected -> ()
      | Cry.Connected _ ->
          self#log#f 3 "Closing connection..." ;
          Cry.close connection ;
          on_disconnect () ;
    end ;
    match dump with
      | Some f -> close_out f
      | None -> ()

end

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "output.icecast" ~active:true
    ~category:Lang.Output
    ~descr:"Encode and output the stream to an icecast2 or shoutcast server."
    (proto k)
    ~kind:(Lang.Unconstrained k)
    (fun p kind -> ((new output ~kind p):>Source.source))
