(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let ( let* ) = Duppy.Monad.bind

module Http = Liq_http

let log = Log.make ["harbor"; "output"]

(** Output to harbor listeners. *)

module Duppy = Harbor.Http_transport.Duppy

module Icecast = struct
  type protocol = unit

  let protocol_of_icecast_protocol _ = ()

  type content = string

  let format_of_content x = x

  type info = unit

  let info_of_encoder _ _ = ()
end

module M = Icecast_utils.Icecast_v (Icecast)
open M

(* ICY metadata constants.
   Max total length for ICY metadata is 255*16.
   Format is: "StreamTitle='%s';StreamUrl='%s'"
   "StreamTitle='';"; is 15 chars long, "StreamUrl='';"
   is 13 chars long, leaving 4052 chars remaining. *)
let max_icy_title = 3852
let max_icy_url = 200

(* === Type Definitions === *)

(* Listener record. In shared mode the encoder field is always None.
   In dedicated mode it is set to Some enc at connect time. *)
type listener = {
  id : string;
  socket : Harbor.Http_transport.socket;
  close : unit -> unit;
  mutable encoder : Encoder.encoder option;
  mutable pending_data : Strings.t;
  mutable pending_length : int;
  mutable metadata_position : int;
  mutable last_sent_metadata : string;
  metadata_interval : int;
  stream_url : string option;
  mutable closed : bool;
  timeout : float;
}

(* Semantic aliases documenting the encoder invariant for each mode. *)
type shared_listener = listener
type dedicated_listener = listener

(* === ICY Metadata Functions === *)

let format_icy_title ~artist ~title =
  match (artist, title) with
    | Some a, Some t -> Some (Printf.sprintf "%s - %s" a t)
    | Some s, None | None, Some s -> Some s
    | None, None -> None

let format_icy_metadata ~url metadata =
  let title_info =
    format_icy_title
      ~artist:(Frame.Metadata.find_opt "artist" metadata)
      ~title:(Frame.Metadata.find_opt "title" metadata)
  in
  let title_part =
    match title_info with
      | Some s when String.length s > max_icy_title ->
          Printf.sprintf "StreamTitle='%s...';"
            (String.sub s 0 (max_icy_title - 3))
      | Some s -> Printf.sprintf "StreamTitle='%s';" s
      | None -> ""
  in
  let url_part =
    match url with
      | Some s when String.length s > max_icy_url ->
          Printf.sprintf "StreamUrl='%s...';" (String.sub s 0 (max_icy_url - 3))
      | Some s -> Printf.sprintf "StreamUrl='%s';" s
      | None -> ""
  in
  let meta = title_part ^ url_part in
  (* Pad string to a multiple of 16 bytes *)
  let len = String.length meta in
  let pad = (len / 16) + 1 in
  let result = Bytes.make ((pad * 16) + 1) '\000' in
  Bytes.set result 0 (Char.chr pad);
  String.blit meta 0 result 1 len;
  Bytes.unsafe_to_string result

let insert_icy_metadata ~get_metadata listener data =
  if listener.metadata_interval <= 0 then data
  else (
    let rec insert_at_intervals accumulated remaining =
      let remaining_len = Strings.length remaining in
      let bytes_until_next_meta =
        listener.metadata_interval - listener.metadata_position
      in
      if bytes_until_next_meta <= remaining_len then begin
        (* Insert metadata at this position *)
        let meta_string =
          match get_metadata () with
            | Some m ->
                let formatted =
                  format_icy_metadata ~url:listener.stream_url m
                in
                if formatted <> listener.last_sent_metadata then begin
                  listener.last_sent_metadata <- formatted;
                  formatted
                end
                else "\000"
            | None -> "\000"
        in
        let before = Strings.sub remaining 0 bytes_until_next_meta in
        let after =
          Strings.sub remaining bytes_until_next_meta
            (remaining_len - bytes_until_next_meta)
        in
        let with_meta =
          Strings.concat [accumulated; before; Strings.of_string meta_string]
        in
        listener.metadata_position <- 0;
        insert_at_intervals with_meta after
      end
      else begin
        listener.metadata_position <- listener.metadata_position + remaining_len;
        Strings.concat [accumulated; remaining]
      end
    in
    insert_at_intervals Strings.empty data)

(* === Listener Management Functions === *)

let create_listener ~id ~socket ~close ~metadata_interval ~stream_url ~timeout =
  {
    id;
    socket;
    close;
    encoder = None;
    pending_data = Strings.empty;
    pending_length = 0;
    metadata_position = 0;
    last_sent_metadata = "\000";
    metadata_interval;
    stream_url;
    closed = false;
    timeout;
  }

let append_data_to_listener ~buffer_limit listener data =
  let data_len = Strings.length data in
  let new_length = listener.pending_length + data_len in
  if new_length > buffer_limit then begin
    let drop_amount = new_length - buffer_limit in
    listener.pending_data <- Strings.drop listener.pending_data drop_amount;
    listener.pending_length <- listener.pending_length - drop_amount
  end;
  listener.pending_data <- Strings.concat [listener.pending_data; data];
  listener.pending_length <- listener.pending_length + data_len

(* === Write Handling Functions === *)

let try_write_to_socket listener =
  if listener.pending_length = 0 then 0
  else begin
    let data_bytes = Strings.to_bytes listener.pending_data in
    try
      let written =
        Harbor.write listener.socket data_bytes 0 (Bytes.length data_bytes)
      in
      if written > 0 then begin
        listener.pending_data <- Strings.drop listener.pending_data written;
        listener.pending_length <- listener.pending_length - written
      end;
      written
    with
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> 0
      | exn ->
          (match exn with
            | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> ()
            | _ ->
                log#info "Write error for %s: %s" listener.id
                  (Printexc.to_string exn));
          listener.closed <- true;
          -1
  end

(* === Burst Buffer Functions === *)

let update_burst_buffer burst_buffer ~max_size data =
  Strings.Mutable.append_strings burst_buffer data;
  Strings.Mutable.keep burst_buffer max_size

let get_burst_data burst_buffer = Strings.Mutable.to_strings burst_buffer

(* === Protocol Definition === *)

let proto frame_t =
  Output.proto
  @ Icecast_utils.base_proto frame_t
  @ [
      ("mount", Lang.string_t, None, None);
      ("port", Lang.int_t, Some (Lang.int 8000), None);
      ( "transport",
        Lang.http_transport_base_t,
        Some (Lang.base_http_transport Http.unix_transport),
        Some
          "Http transport. Use `http.transport.ssl` or \
           `http.transport.secure_transport`, when available, to enable HTTPS \
           output" );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some "Timeout for network operations (in seconds)." );
      ( "encoding",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Encoding used to send metadata. If empty, defaults to \"UTF-8\""
      );
      ("url", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ( "metaint",
        Lang.int_t,
        Some (Lang.int 8192),
        Some "Interval used to send ICY metadata" );
      ( "auth",
        Lang.nullable_t
          (Lang.fun_t
             [
               (false, "address", Lang.string_t);
               (false, "", Lang.string_t);
               (false, "", Lang.string_t);
             ]
             Lang.bool_t),
        Some Lang.null,
        Some
          "Authentication function. `f(~address, login, password)` returns \
           `true` if the listener should be granted access. When `null`, no \
           authentication is required." );
      ( "buffer",
        Lang.int_t,
        Some (Lang.int (5 * 65535)),
        Some "Maximum buffer per-client." );
      ( "burst",
        Lang.nullable_t Lang.int_t,
        Some (Lang.int 65534),
        Some
          "Initial burst of data sent to the client. Set to `null` to disable \
           burst. This feature is only available when `dedicated_encoder` is \
           `false`." );
      ( "chunk",
        Lang.int_t,
        Some (Lang.int Utils.buflen),
        Some "Send data to clients using chunks of at least this length." );
      ( "headers",
        Lang.metadata_t,
        Some (Lang.list []),
        Some "Additional headers." );
      ( "dumpfile",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Dump stream to file, for debugging purpose. Disabled if null." );
      ( "dedicated_encoder",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "When `true`, create a separate encoder instance for each listener, \
           ensuring each gets a clean encoder state. For copy-only formats \
           (e.g. `%ffmpeg` in copy mode) this has minimal overhead. For \
           encoded formats (e.g. mp3, aac) this adds a full encoder per \
           client, which can be significant under load." );
      ("", Lang.source_t frame_t, None, None);
    ]

(* === Virtual Base Class === *)

class virtual base p =
  let pos = Lang.pos p in
  let get_param name = List.assoc name p in
  let metaint = Lang.to_int (get_param "metaint") in
  let encoder_data = encoder_data p in
  let encoding = Lang.to_string (get_param "encoding") in
  let recode_metadata m =
    let target_encoding =
      match encoding with "" -> Charset.utf8 | s -> Charset.of_string s
    in
    let convert_value = Charset.convert ~target:target_encoding in
    Frame.Metadata.fold
      (fun key value acc -> Frame.Metadata.add key (convert_value value) acc)
      Frame.Metadata.empty m
  in
  let timeout = Lang.to_float (get_param "timeout") in
  let buffer_limit = Lang.to_int (get_param "buffer") in
  let burst_size = Lang.to_valued_option Lang.to_int (get_param "burst") in
  let chunk_size = Lang.to_int (get_param "chunk") in
  let () =
    if chunk_size > buffer_limit then
      raise
        (Error.Invalid_value
           (get_param "buffer", "Maximum buffering inferior to chunk length"));
    Option.iter
      (fun burst ->
        if burst > buffer_limit then
          raise
            (Error.Invalid_value
               (get_param "buffer", "Maximum buffering inferior to burst length")))
      burst_size
  in
  let source_val = Lang.assoc "" 2 p in
  let source = Lang.to_source source_val in
  let mount = Lang.to_string (get_param "mount") in
  let uri =
    let mount_path = match mount.[0] with '/' -> mount | _ -> "/" ^ mount in
    let regexp = [%string {|^%{mount_path}$|}] in
    Liquidsoap_lang.Builtins_regexp.
      { descr = regexp; flags = []; regexp = Re.Pcre.regexp regexp }
  in
  let autostart = Lang.to_bool (get_param "start") in
  let infallible = not (Lang.to_bool (get_param "fallible")) in
  let register_telnet = Lang.to_bool (get_param "register_telnet") in
  let stream_url =
    get_param "url" |> Lang.to_option |> Option.map Lang.to_string
  in
  let port = Lang.to_int (get_param "port") in
  let transport = Lang.to_http_transport (get_param "transport") in
  let auth_function = Lang.to_option (get_param "auth") in
  let login =
    Option.map
      (fun auth_function ->
        let resolve_client_address socket =
          let fd = Harbor.file_descr_of_socket socket in
          Utils.name_of_sockaddr ~rev_dns:Harbor_base.conf_revdns#get
            (Unix.getpeername fd)
        in
        let authenticate socket user password =
          let address = resolve_client_address socket in
          let user = Charset.convert user in
          let password = Charset.convert password in
          Lang.to_bool
            (Lang.apply auth_function
               [
                 ("address", Lang.string address);
                 ("", Lang.string user);
                 ("", Lang.string password);
               ])
        in
        ( "",
          fun { Harbor.socket; uri = _; user; password } ->
            authenticate socket user password ))
      auth_function
  in
  let dumpfile = Lang.to_valued_option Lang.to_string (get_param "dumpfile") in
  let extra_headers =
    List.map
      (fun v ->
        let key, value = Lang.to_product v in
        (Lang.to_string key, Lang.to_string value))
      (Lang.to_list (get_param "headers"))
  in
  object (self)
    inherit
      [Strings.t] Output.encoded
        ~output_kind:"output.harbor" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false ~name:mount source_val

    (* Immutable parameters exposed to subclasses via inheritance. *)
    val burst_size = burst_size
    val buffer_limit = buffer_limit
    val chunk_size = chunk_size
    val encoder_data = encoder_data
    val recode_metadata = recode_metadata
    val pos = pos
    val uri = uri
    val port = port
    val transport = transport
    val dumpfile = dumpfile
    val listeners : listener list Atomic.t = Atomic.make []
    val listeners_mutex = Mutex.create ()
    val burst_buffer = Strings.Mutable.empty ()
    val shared_metadata : Frame.metadata option Atomic.t = Atomic.make None
    val mutable dump_channel : out_channel option = None
    val mutable on_connect_callbacks = []
    val mutable on_disconnect_callbacks = []
    val start_stop_mutex = Mutex.create ()
    method on_connect fn = on_connect_callbacks <- fn :: on_connect_callbacks

    method on_disconnect fn =
      on_disconnect_callbacks <- fn :: on_disconnect_callbacks

    method self_sync = source#self_sync

    method private get_metadata () =
      let meta = Atomic.get shared_metadata in
      Atomic.set shared_metadata None;
      meta

    (* Called synchronously when a listener connects. Subclasses set up the
       encoder (if any) and populate the listener's initial pending_data with
       the codec header and burst data. *)
    method virtual private connect_listener
        : protocol:string -> listener -> (unit, Harbor.reply) Duppy.Monad.t

    (* Called when a listener disconnects. Subclasses stop any per-listener
       encoder. *)
    method virtual private stop_listener_encoder : listener -> unit

    method private handle_disconnect listener =
      if not listener.closed then begin
        listener.closed <- true;
        self#log#info "Listener %s disconnected" listener.id;
        self#stop_listener_encoder listener;
        listener.close ();
        List.iter (fun fn -> fn listener.id) on_disconnect_callbacks
      end

    method private write_to_all_listeners =
      let updated =
        List.filter_map
          (fun listener ->
            if listener.closed || try_write_to_socket listener < 0 then begin
              self#handle_disconnect listener;
              None
            end
            else Some listener)
          (Atomic.get listeners)
      in
      Mutex_utils.mutexify listeners_mutex
        (fun () -> Atomic.set listeners updated)
        ()

    method private add_listener ~protocol ~headers ~uri:request_uri ~query
        socket =
      let client_id =
        let fd = Harbor.file_descr_of_socket socket in
        Utils.name_of_sockaddr ~show_port:true (Unix.getpeername fd)
      in
      let metadata_interval, icy_header =
        try
          assert (List.assoc "Icy-MetaData" headers = "1");
          (metaint, Printf.sprintf "icy-metaint: %d\r\n" metaint)
        with _ -> (-1, "")
      in
      let extra_headers_str =
        String.concat ""
          (List.map
             (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v)
             extra_headers)
      in
      let http_response =
        Printf.sprintf "HTTP/%s 200 OK\r\nContent-type: %s\r\n%s%s\r\n" protocol
          encoder_data.format icy_header extra_headers_str
      in
      let close () = try Harbor.close socket with _ -> () in
      let handler =
        {
          Duppy.Monad.Io.scheduler = Tutils.scheduler;
          socket;
          data = "";
          on_error =
            (fun e ->
              let error_msg =
                match e with
                  | Duppy.Io.Timeout ->
                      Printf.sprintf "Timeout for %s" client_id
                  | Duppy.Io.Io_error ->
                      Printf.sprintf "I/O error for %s" client_id
                  | Duppy.Io.Unix (c, p, m, _) ->
                      Printf.sprintf "Unix error for %s: %s" client_id
                        (Printexc.to_string (Unix.Unix_error (c, p, m)))
                  | Duppy.Io.Unknown (e, _) -> Printexc.to_string e
              in
              self#log#info "%s" error_msg;
              List.find_opt (fun l -> l.id = client_id) (Atomic.get listeners)
              |> Option.iter self#handle_disconnect;
              Harbor.Close (Harbor.mk_simple ""));
        }
      in
      self#log#info "New listener connection from %s" client_id;
      let* () =
        match login with
          | Some login ->
              Duppy.Monad.catch
                (Duppy.Monad.Io.exec ~priority:`Maybe_blocking handler
                   (Harbor.http_auth_check ~query ~uri:request_uri ~login socket
                      headers))
                (function
                  | Harbor.Close s ->
                      self#log#info "Listener %s failed to authenticate"
                        client_id;
                      Harbor.reply s
                  | _ -> assert false)
          | None -> Duppy.Monad.return ()
      in
      let listener =
        create_listener ~id:client_id ~socket ~close ~metadata_interval
          ~stream_url ~timeout
      in
      let* () = self#connect_listener ~protocol listener in
      Mutex_utils.mutexify listeners_mutex
        (fun () -> Atomic.set listeners (listener :: Atomic.get listeners))
        ();
      self#log#info "Listener %s connected" client_id;
      List.iter
        (fun fn -> fn ~headers ~uri:request_uri ~protocol client_id)
        on_connect_callbacks;
      Duppy.Monad.Io.exec ~priority:`Maybe_blocking handler
        (Harbor.relayed http_response)

    method private register_http_handler =
      Harbor.add_http_handler ~pos ~transport ~port ~verb:`Get ~uri
        (fun ~protocol ~meth:_ ~data:_ ~headers ~query ~socket request_uri ->
          self#add_listener ~protocol ~headers ~uri:request_uri ~query socket)

    method private disconnect_all_listeners =
      List.iter self#handle_disconnect (Atomic.get listeners);
      Atomic.set listeners []
  end

(* === Shared Encoder Output ===
   A single encoder instance is created at start time and shared across all
   listeners. connect_listener seeds each new listener's buffer with the
   codec header and any accumulated burst data. *)

class shared_output p =
  object (self)
    inherit base p
    val mutable enc : Encoder.encoder option = None
    val mutable chunk_accumulator = 0

    method private connect_listener ~protocol (listener : shared_listener) =
      match enc with
        | None ->
            Harbor.reply (fun () ->
                Printf.sprintf "HTTP/%s 404 Not found\r\n" protocol)
        | Some e ->
            let burst =
              match burst_size with
                | Some _ -> get_burst_data burst_buffer
                | None -> Strings.empty
            in
            let data =
              insert_icy_metadata ~get_metadata:self#get_metadata listener
                (Strings.concat [e.Encoder.header (); burst])
            in
            listener.pending_data <- data;
            listener.pending_length <- Strings.length data;
            Duppy.Monad.return ()

    method private stop_listener_encoder (_ : shared_listener) = ()

    method encode frame =
      match enc with Some e -> e.Encoder.encode frame | None -> Strings.empty

    method encode_metadata m =
      let recoded = recode_metadata (Frame.Metadata.Export.to_metadata m) in
      Atomic.set shared_metadata (Some recoded);
      Option.iter
        (fun e ->
          e.Encoder.encode_metadata
            (Frame.Metadata.Export.from_metadata ~cover:false recoded))
        enc

    method send data =
      let len = Strings.length data in
      if len > 0 then begin
        Option.iter
          (fun max_size -> update_burst_buffer burst_buffer ~max_size data)
          burst_size;
        Option.iter
          (fun ch -> Strings.iter (output_substring ch) data)
          dump_channel;
        List.iter
          (fun listener ->
            if not listener.closed then
              append_data_to_listener ~buffer_limit listener
                (insert_icy_metadata ~get_metadata:self#get_metadata listener
                   data))
          (Atomic.get listeners);
        chunk_accumulator <- chunk_accumulator + len;
        if chunk_accumulator >= chunk_size then begin
          chunk_accumulator <- 0;
          self#write_to_all_listeners
        end
      end

    method start =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match enc with
            | Some _ -> ()
            | None ->
                let factory = encoder_data.factory self#id in
                enc <- Some (factory Frame.Metadata.Export.empty);
                self#register_http_handler;
                Option.iter
                  (fun path -> dump_channel <- Some (open_out_bin path))
                  dumpfile)
        ()

    method stop =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match enc with
            | None -> ()
            | Some e ->
                ignore (e.Encoder.stop ());
                enc <- None;
                Harbor.remove_http_handler ~port ~verb:`Get ~uri ();
                self#disconnect_all_listeners;
                Option.iter close_out dump_channel;
                dump_channel <- None)
        ()

    method! reset =
      self#stop;
      self#start
  end

(* === Dedicated Encoder Output ===
   A fresh encoder instance is created for each listener at connect time.
   encode() stores the current frame and send() encodes it independently
   per listener, ensuring a clean stream from the first byte. *)

class dedicated_output p =
  object (self)
    inherit base p

    val mutable encoder_factory
        : (Frame.Metadata.Export.t -> Encoder.encoder) option =
      None

    val mutable current_frame : Frame.t option = None

    method private connect_listener ~protocol (listener : dedicated_listener) =
      match encoder_factory with
        | None ->
            Harbor.reply (fun () ->
                Printf.sprintf "HTTP/%s 404 Not found\r\n" protocol)
        | Some factory ->
            let e = factory Frame.Metadata.Export.empty in
            listener.encoder <- Some e;
            let burst =
              match burst_size with
                | Some _ -> get_burst_data burst_buffer
                | None -> Strings.empty
            in
            let data =
              insert_icy_metadata ~get_metadata:self#get_metadata listener
                (Strings.concat [e.Encoder.header (); burst])
            in
            listener.pending_data <- data;
            listener.pending_length <- Strings.length data;
            Duppy.Monad.return ()

    method private stop_listener_encoder (listener : dedicated_listener) =
      Option.iter (fun e -> ignore (e.Encoder.stop ())) listener.encoder

    (* Stores the frame for per-listener encoding in send(). *)
    method encode frame =
      current_frame <- Some frame;
      Strings.empty

    method encode_metadata m =
      let recoded = recode_metadata (Frame.Metadata.Export.to_metadata m) in
      Atomic.set shared_metadata (Some recoded)

    method send _ =
      Option.iter
        (fun frame ->
          List.iter
            (fun listener ->
              if not listener.closed then
                Option.iter
                  (fun e ->
                    append_data_to_listener ~buffer_limit listener
                      (insert_icy_metadata ~get_metadata:self#get_metadata
                         listener (e.Encoder.encode frame)))
                  listener.encoder)
            (Atomic.get listeners);
          current_frame <- None;
          self#write_to_all_listeners)
        current_frame

    method start =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match encoder_factory with
            | Some _ -> ()
            | None ->
                encoder_factory <- Some (encoder_data.factory self#id);
                self#register_http_handler;
                Option.iter
                  (fun path -> dump_channel <- Some (open_out_bin path))
                  dumpfile)
        ()

    method stop =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match encoder_factory with
            | None -> ()
            | Some _ ->
                encoder_factory <- None;
                Harbor.remove_http_handler ~port ~verb:`Get ~uri ();
                self#disconnect_all_listeners;
                Option.iter close_out dump_channel;
                dump_channel <- None)
        ()

    method! reset =
      self#stop;
      self#start
  end

(* === Operator Registration === *)

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~category:`Output
    ~descr:"Encode and output the stream using the harbor server."
    ~callbacks:
      ([
         {
           Lang_source.name = "on_connect";
           params = [];
           descr =
             "Callback when a listener connects. Receives a record with \
              headers, uri, protocol, and ip fields.";
           register_deprecated_argument = true;
           arg_t =
             [
               ( false,
                 "",
                 Lang.record_t
                   [
                     ("headers", Lang.metadata_t);
                     ("uri", Lang.string_t);
                     ("protocol", Lang.string_t);
                     ("ip", Lang.string_t);
                   ] );
             ];
           register =
             (fun ~params:_ s on_connect ->
               let callback ~headers ~uri ~protocol ip =
                 on_connect
                   [
                     ( "",
                       Lang.record
                         [
                           ("headers", Lang.metadata_list headers);
                           ("uri", Lang.string uri);
                           ("protocol", Lang.string protocol);
                           ("ip", Lang.string ip);
                         ] );
                   ]
               in
               s#on_connect callback);
         };
         {
           name = "on_disconnect";
           params = [];
           descr = "Callback when a listener disconnects.";
           register_deprecated_argument = true;
           arg_t = [(false, "", Lang.string_t)];
           register =
             (fun ~params:_ s callback ->
               s#on_disconnect (fun ip -> callback [("", Lang.string ip)]));
         };
       ]
      @ Start_stop.callbacks ~label:"output")
    ~meth:(Start_stop.meth ()) ~base:Modules.output "harbor" (proto return_t)
    ~return_t
    (fun p ->
      if Lang.to_bool (List.assoc "dedicated_encoder" p) then
        new dedicated_output p
      else new shared_output p)
