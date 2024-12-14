(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** Output to an icecast server. *)

module Http = Liq_http

let error_translator = function
  | Cry.Error _ as e -> Some (Cry.string_of_error e)
  | _ -> None

let () = Printexc.register_printer error_translator

type icecast_info = {
  quality : string option;
  bitrate : int option;
  samplerate : int option;
  channels : int option;
}

module Icecast = struct
  type content = Cry.content_type

  let format_of_content x =
    match x with
      | x when x = Icecast_utils.mpeg_mime -> Cry.mpeg
      | x when x = Icecast_utils.ogg_application_mime -> Cry.ogg_application
      | x when x = Icecast_utils.ogg_audio_mime -> Cry.ogg_audio
      | x when x = Icecast_utils.ogg_video_mime -> Cry.ogg_video
      | _ -> Cry.content_type_of_string x

  type info = icecast_info

  let info_of_encoder format encoder =
    match format with
      | Encoder.MP3 m ->
          let quality, bitrate =
            match m.Mp3_format.bitrate_control with
              | Mp3_format.CBR x -> (None, Some x)
              | Mp3_format.ABR x -> (None, x.Mp3_format.mean_bitrate)
              | Mp3_format.VBR x ->
                  (Some (string_of_int (Option.get x.Mp3_format.quality)), None)
          in
          {
            quality;
            bitrate;
            samplerate = Some (Lazy.force m.Mp3_format.samplerate);
            channels = Some (if m.Mp3_format.stereo then 2 else 1);
          }
      | Encoder.Shine m ->
          {
            quality = None;
            bitrate = Some m.Shine_format.bitrate;
            samplerate = Some (Lazy.force m.Shine_format.samplerate);
            channels = Some m.Shine_format.channels;
          }
      | Encoder.FdkAacEnc m ->
          {
            quality = None;
            bitrate = Some m.Fdkaac_format.bitrate;
            samplerate = Some (Lazy.force m.Fdkaac_format.samplerate);
            channels = Some m.Fdkaac_format.channels;
          }
      | Encoder.NDI _ ->
          { quality = None; bitrate = None; samplerate = None; channels = None }
      | Encoder.External m ->
          {
            quality = None;
            bitrate = None;
            samplerate = Some (Lazy.force m.External_encoder_format.samplerate);
            channels = Some m.External_encoder_format.channels;
          }
      | Encoder.Flac m ->
          {
            quality = Some (string_of_int m.Flac_format.compression);
            bitrate = None;
            samplerate = Some (Lazy.force m.Flac_format.samplerate);
            channels = Some m.Flac_format.channels;
          }
      | Encoder.Ffmpeg m ->
          let bitrate =
            Option.map (fun v -> v / 1000) Encoder.(encoder.hls.bitrate ())
          in
          let audio_stream =
            match List.assoc_opt Frame.Fields.audio m.Ffmpeg_format.streams with
              | Some (`Encode { options = `Audio options }) -> Some options
              | _ -> None
          in
          {
            quality = None;
            bitrate;
            samplerate =
              Option.map
                (fun stream -> Lazy.force stream.Ffmpeg_format.samplerate)
                audio_stream;
            channels =
              Option.map
                (fun stream -> stream.Ffmpeg_format.channels)
                audio_stream;
          }
      | Encoder.WAV m ->
          {
            quality = None;
            bitrate = None;
            samplerate = Some (Lazy.force m.Wav_format.samplerate);
            channels = Some m.Wav_format.channels;
          }
      | Encoder.AVI m ->
          {
            quality = None;
            bitrate = None;
            samplerate = Some (Lazy.force m.Avi_format.samplerate);
            channels = Some m.Avi_format.channels;
          }
      | Encoder.Ogg { Ogg_format.audio; _ } -> (
          match audio with
            | Some
                (Ogg_format.Vorbis
                  {
                    Vorbis_format.channels = n;
                    mode = Vorbis_format.VBR q;
                    samplerate = s;
                    _;
                  }) ->
                {
                  quality = Some (string_of_float q);
                  bitrate = None;
                  samplerate = Some (Lazy.force s);
                  channels = Some n;
                }
            | Some
                (Ogg_format.Vorbis
                  {
                    Vorbis_format.channels = n;
                    mode = Vorbis_format.ABR (_, b, _);
                    samplerate = s;
                    _;
                  }) ->
                {
                  quality = None;
                  bitrate = b;
                  samplerate = Some (Lazy.force s);
                  channels = Some n;
                }
            | Some
                (Ogg_format.Vorbis
                  {
                    Vorbis_format.channels = n;
                    mode = Vorbis_format.CBR b;
                    samplerate = s;
                    _;
                  }) ->
                {
                  quality = None;
                  bitrate = Some b;
                  samplerate = Some (Lazy.force s);
                  channels = Some n;
                }
            | _ ->
                {
                  quality = None;
                  bitrate = None;
                  samplerate = None;
                  channels = None;
                })
end

module M = Icecast_utils.Icecast_v (Icecast)
open M

let user_agent =
  Lang.product (Lang.string "User-Agent") (Lang.string Http.user_agent)

let default_icy_song =
  Lang.eval ~cache:false ~typecheck:false ~stdlib:`Disabled
    {|fun (m) -> begin
  title = m["title"]
  artist = m["artist"]
  if artist != "" and title != "" then
    "#{artist} - #{title}"
  elsif artist != "" then
    artist
  elsif title != "" then
    title
  else
    null()
  end
end|}

let proto frame_t =
  Output.proto
  @ Icecast_utils.base_proto frame_t
  @ [
      ("mount", Lang.string_t, None, Some "Source mount point.");
      ("icy_id", Lang.int_t, Some (Lang.int 1), Some "Shoutcast source ID.");
      ("name", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ("host", Lang.string_t, Some (Lang.string "localhost"), None);
      ("port", Lang.int_t, Some (Lang.int 8000), None);
      ( "prefer_address",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Preferred address type when resolving hostnames. One of: `\"ipv4\"` \
           or `\"ipv6\"`. Defaults to system default when `null`." );
      ( "transport",
        Lang.http_transport_base_t,
        Some (Lang.base_http_transport Http.unix_transport),
        Some
          "Http transport. Use `http.transport.ssl` or \
           `http.transport.secure_transport`, when available, to enable HTTPS \
           output" );
      ( "connection_timeout",
        Lang.float_t,
        Some (Lang.float 5.),
        Some
          "Timeout for establishing network connections (disabled is negative)."
      );
      ( "send_last_metadata_on_connect",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Send the source's last metadata when connecting to the remote \
           icecast server." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some "Timeout for network read and write." );
      ( "user",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "User for shout source connection. Defaults to \"source\" for \
           icecast connections. Useful only in special cases, like with \
           per-mountpoint users." );
      ("password", Lang.string_t, Some (Lang.string "hackme"), None);
      ( "encoding",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Encoding used to send metadata and stream info (name, genre and \
           description). If null, defaults to \"UTF-8\"." );
      ("genre", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ("protocol", Lang.string_t, Some (Lang.string "http"), None);
      ( "method",
        Lang.string_t,
        Some (Lang.string "source"),
        Some
          "Method to use with the 'http(s)' protocol. One of: 'source', 'put' \
           or 'post'." );
      ( "chunked",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Used chunked transfer with the 'http(s)' protocol." );
      ( "send_icy_metadata",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some "Send new metadata using the ICY protocol. Guessed when `null`" );
      ( "icy_metadata",
        Lang.list_t Lang.string_t,
        Some
          (Lang.list
             (List.map Lang.string
                [
                  "song";
                  "title";
                  "artist";
                  "genre";
                  "date";
                  "album";
                  "tracknum";
                  "comment";
                  "dj";
                  "next";
                ])),
        Some "List of metadata to send with ICY metadata update" );
      ( "icy_song",
        Lang.fun_t
          [(false, "", Lang.metadata_t)]
          (Lang.nullable_t Lang.string_t),
        Some default_icy_song,
        Some
          "Function used to generate the default icy \"song\" metadata. \
           Metadata is not added when returning `null`. Default: `$(artist) - \
           $(title)` if both are defined, otherwise `artist` or `title` if \
           either is defined or `null`." );
      ("url", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ("description", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ( "on_connect",
        Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Callback executed when connection is established." );
      ( "on_disconnect",
        Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Callback executed when connection stops." );
      ( "on_error",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.float_t,
        Some (Lang.val_cst_fun [("", None)] (Lang.float 3.)),
        Some
          "Callback executed when an error happens. The callback receives a \
           string representation of the error that occurred and returns a \
           float. If returned value is positive, connection will be tried \
           again after this amount of time (in seconds)." );
      ("public", Lang.bool_t, Some (Lang.bool true), None);
      ( "headers",
        Lang.metadata_t,
        Some (Lang.list [user_agent]),
        Some "Additional headers." );
      ( "dumpfile",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Dump stream to file, for debugging purpose. Disabled if null." );
      ("", Lang.source_t frame_t, None, None);
    ]

(** Sending encoded data to a shout-compatible server.
  * It directly takes the Lang param list and extracts stuff from it. *)
class output p =
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in
  let s_opt v = e (Lang.to_valued_option Lang.to_string) v in
  let on_connect = List.assoc "on_connect" p in
  let on_disconnect = List.assoc "on_disconnect" p in
  let on_error = List.assoc "on_error" p in
  let on_connect () = ignore (Lang.apply on_connect []) in
  let on_disconnect () = ignore (Lang.apply on_disconnect []) in
  let on_error error =
    let msg = Printexc.to_string error in
    Lang.to_float (Lang.apply on_error [("", Lang.string msg)])
  in
  let send_last_metadata_on_connect =
    e Lang.to_bool "send_last_metadata_on_connect"
  in
  let data = encoder_data p in
  let chunked = Lang.to_bool (List.assoc "chunked" p) in
  let protocol =
    let m =
      let m = List.assoc "method" p in
      match Lang.to_string m with
        | "source" -> Cry.Source
        | "put" -> Cry.Put
        | "post" -> Cry.Post
        | _ ->
            raise
              (Error.Invalid_value
                 (m, "Valid values are: 'source' 'put' or 'post'."))
    in
    let v = List.assoc "protocol" p in
    match Lang.to_string v with
      | "http" -> Cry.Http m
      | "icy" -> Cry.Icy
      | _ ->
          raise
            (Error.Invalid_value
               (v, "Valid values are 'http' (icecast) and 'icy' (shoutcast)"))
  in
  let send_icy_metadata =
    match
      ( data.format,
        Lang.to_valued_option Lang.to_bool (List.assoc "send_icy_metadata" p) )
    with
      | _, Some b -> b
      | format, None
        when format = mpeg || format = wav || format = aac || format = flac ->
          true
      | format, None
        when format = ogg_application || format = ogg_audio
             || format = ogg_video ->
          false
      | _ ->
          raise
            (Error.Invalid_value
               ( List.assoc "send_icy_metadata" p,
                 "Could not guess send_icy_metadata for this format, please \
                  specify either true or false." ))
  in
  let icy_metadata =
    List.map Lang.to_string (Lang.to_list (List.assoc "icy_metadata" p))
  in
  let icy_song = List.assoc "icy_song" p in
  let icy_song m =
    Lang.to_valued_option Lang.to_string (Lang.apply icy_song [("", m)])
  in
  let out_enc =
    match s_opt "encoding" with
      | None | Some "" -> Charset.utf8
      | Some s -> Charset.of_string s
  in
  let source = Lang.assoc "" 2 p in
  let icy_id = Lang.to_int (List.assoc "icy_id" p) in
  let mount = s "mount" in
  let name =
    match Lang.to_option (List.assoc "name" p) with
      | None -> mount
      | Some v -> Lang.to_string v
  in
  let name = Charset.convert ~target:out_enc name in
  let mount =
    match protocol with
      | Cry.Icy -> Cry.Icy_id icy_id
      | _ -> Cry.Icecast_mount mount
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply f [])
  in
  let host = s "host" in
  let port = e Lang.to_int "port" in
  let transport = e Lang.to_http_transport "transport" in
  let prefer_address =
    let v = List.assoc "prefer_address" p in
    match Lang.to_valued_option Lang.to_string v with
      | None -> `System_default
      | Some "ipv4" -> `Ipv4
      | Some "ipv6" -> `Ipv6
      | Some _ ->
          raise (Error.Invalid_value (v, "Valid values are: 'ipv4' or 'ipv6'."))
  in
  let transport = (transport :> Cry.transport) in
  let transport =
    object
      method name = transport#name
      method protocol = transport#protocol
      method default_port = transport#default_port

      method connect ?bind_address ?timeout ?prefer =
        transport#connect ?bind_address ?timeout
          ~prefer:(Option.value ~default:prefer_address prefer)
    end
  in
  let user =
    match (protocol, s_opt "user") with
      | Cry.Http _, None -> "source"
      | _, user -> Option.value ~default:"" user
  in
  let password = s "password" in
  let genre =
    Option.map (fun s -> Charset.convert ~target:out_enc s) (s_opt "genre")
  in
  let url = s_opt "url" in
  let timeout = e Lang.to_float "timeout" in
  let connection_timeout =
    let v = e Lang.to_float "connection_timeout" in
    if v > 0. then Some v else None
  in
  let dumpfile = s_opt "dumpfile" in
  let description =
    Option.map
      (fun s -> Charset.convert ~target:out_enc s)
      (s_opt "description")
  in
  let public = e Lang.to_bool "public" in
  let headers =
    List.map
      (fun v ->
        let f (x, y) = (Lang.to_string x, Lang.to_string y) in
        f (Lang.to_product v))
      (Lang.to_list (List.assoc "headers" p))
  in
  let connection = Cry.create ~timeout ~transport ?connection_timeout () in
  object (self)
    inherit
      [Strings.t] Output.encoded
        ~output_kind:"output.icecast" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false ~on_start ~on_stop ~name source

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

    method encode frame =
      (* We assume here that there always is
       * an encoder available when the source
       * is connected. *)
      match (Cry.get_status connection, encoder) with
        | Cry.Connected _, Some enc -> enc.Encoder.encode frame
        | _ -> Strings.empty

    method insert_metadata m =
      (* Update metadata using ICY if told to.. *)
      if send_icy_metadata then (
        let f = Charset.convert ~target:out_enc in
        let icy_meta = Hashtbl.create 10 in
        let m = Frame.Metadata.Export.to_metadata m in
        Frame.Metadata.iter
          (fun lbl v ->
            if List.mem lbl icy_metadata then Hashtbl.replace icy_meta lbl (f v))
          m;
        if not (Hashtbl.mem icy_meta "song") then (
          match icy_song (Lang.metadata m) with
            | None -> ()
            | Some v -> Hashtbl.replace icy_meta "song" (f v));
        (* Do nothing if shout connection isn't available *)
        match Cry.get_status connection with
          | Cry.Connected _ -> (
              try
                Cry.update_metadata
                  ~charset:(Charset.to_string out_enc)
                  connection icy_meta
              with e ->
                let bt = Printexc.get_backtrace () in
                Utils.log_exception ~log:self#log ~bt
                  (Printf.sprintf
                     "Metadata update may have failed with error: %s"
                     (Printexc.to_string e)))
          | Cry.Disconnected -> ())
      else (
        (* Encoder is not always present.. *)
        match encoder with
          | Some encoder -> encoder.Encoder.insert_metadata m
          | None -> ())

    method icecast_send b =
      try
        if Cry.get_status connection = Cry.Disconnected then self#icecast_start;
        Strings.iter
          (fun s offset length -> Cry.send connection ~offset ~length s)
          b;
        match dump with
          | Some s -> Strings.iter (output_substring s) b
          | None -> ()
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        self#log#severe "Error while sending data: %s!" (Printexc.to_string e);
        let delay = on_error e in
        if delay >= 0. then (
          (* Ask for a restart after [restart_time]. *)
          (try self#icecast_stop with _ -> ());
          restart_time <- Unix.gettimeofday () +. delay;
          self#log#important "Will try to reconnect in %.02f seconds." delay)
        else Printexc.raise_with_backtrace e bt

    method send b =
      match Cry.get_status connection with
        | Cry.Disconnected when Unix.time () > restart_time ->
            self#icecast_send b
        | Cry.Connected _ -> self#icecast_send b
        | _ -> ()

    (* We lazily start connection when data is available to send. *)
    method start = ()
    method stop = self#icecast_stop

    method icecast_start =
      assert (encoder = None);
      let enc = data.factory self#id Frame.Metadata.Export.empty in
      encoder <- Some enc;
      assert (Cry.get_status connection = Cry.Disconnected);
      begin
        match dumpfile with
          | Some f -> dump <- Some (open_out_bin f)
          | None -> ()
      end;
      let display_mount =
        match mount with
          | Cry.Icy_id id -> Printf.sprintf "sid#%d" id
          | Cry.Icecast_mount mount -> mount
      in
      self#log#important "Connecting mount %s for %s@%s..." display_mount user
        host;
      let audio_info = Hashtbl.create 10 in
      let f x y z =
        match x with Some q -> Hashtbl.replace audio_info y (z q) | None -> ()
      in
      let info = data.info enc in
      f info.bitrate "bitrate" string_of_int;
      f info.quality "quality" (fun x -> x);
      f info.samplerate "samplerate" string_of_int;
      f info.channels "channels" string_of_int;
      let user_agent =
        try List.assoc "User-Agent" headers
        with Not_found -> Printf.sprintf "liquidsoap %s" Configure.version
      in
      let handler =
        Cry.connection ~host ~port ~user ~password ?genre ?url ?description
          ~name ~public ~protocol ~mount ~chunked ~audio_info ~user_agent
          ~content_type:data.format ()
      in
      List.iter
        (fun (x, y) ->
          (* User-Agent has already been passed to Cry.. *)
          if x <> "User-Agent" then Hashtbl.replace handler.Cry.headers x y)
        headers;
      try
        Cry.connect connection handler;
        self#log#important "Connection setup was successful.";

        (match (Lang.to_source source)#last_metadata with
          | Some m when send_last_metadata_on_connect -> (
              try
                self#insert_metadata
                  (Frame.Metadata.Export.from_metadata ~cover:false m)
              with _ -> ())
          | _ -> ());

        (* Execute on_connect hook. *)
        on_connect ()
      with
      (* In restart mode, no_connect and no_login are not fatal.
       * The output will just try to reconnect later. *)
      | e ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log:self#log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Connection failed: %s" (Printexc.to_string e));
        self#icecast_stop;
        let delay = on_error e in
        if delay >= 0. then (
          self#log#important "Will try again in %.02f sec." delay;
          restart_time <- Unix.time () +. delay)
        else Printexc.raise_with_backtrace e bt

    method icecast_stop =
      (* In some cases it might be possible to output the remaining data,
       * but it's not worth the trouble. *)
      begin
        try ignore ((Option.get encoder).Encoder.stop ()) with _ -> ()
      end;
      encoder <- None;
      begin
        match Cry.get_status connection with
          | Cry.Disconnected -> ()
          | Cry.Connected _ ->
              self#log#important "Closing connection...";
              (try Cry.close connection
               with exn ->
                 let bt = Printexc.get_backtrace () in
                 Utils.log_exception ~log:self#log ~bt
                   (Printf.sprintf "Error while closing connection: %s"
                      (Printexc.to_string exn)));
              on_disconnect ()
      end;
      match dump with Some f -> close_out f | None -> ()
  end

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Modules.output "icecast" ~category:`Output
    ~descr:"Encode and output the stream to an icecast2 or shoutcast server."
    ~meth:Output.meth (proto return_t) ~return_t (fun p ->
      (new output p :> Output.output))
