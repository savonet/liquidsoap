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
let stopped = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"Harbor stop" (fun () ->
      Atomic.set stopped true)

(** Output to harbor listeners. *)

module Task = Duppy.Task
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

(* Listener record parameterized over the encoder type:
   - shared_listener = unit listener  (one shared encoder for all listeners)
   - dedicated_listener = Encoder.encoder listener (fresh encoder per listener) *)
type 'a listener = {
  id : string;
  socket : Harbor.Http_transport.socket;
  close : unit -> unit;
  encoder : 'a;
  pending_data : Strings.Mutable.t;
  mutable metadata_position : int;
  mutable last_sent_metadata : Frame.metadata option;
  metadata_interval : int;
  stream_url : string option;
  closed : bool Atomic.t;
  encoder_mutex : Mutex.t;
  timeout : float;
  mutable last_write_time : float;
}

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

let insert_icy_metadata ~metadata listener data =
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
          match metadata with
            | Some m when metadata != listener.last_sent_metadata ->
                listener.last_sent_metadata <- metadata;
                format_icy_metadata ~url:listener.stream_url m
            | _ -> "\000"
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

let create_listener ~id ~encoder ~socket ~close ~metadata_interval ~stream_url
    ~timeout =
  {
    id;
    socket;
    close;
    encoder;
    pending_data = Strings.Mutable.empty ();
    metadata_position = 0;
    last_sent_metadata = None;
    metadata_interval;
    stream_url;
    closed = Atomic.make false;
    encoder_mutex = Mutex.create ();
    timeout;
    last_write_time = Unix.gettimeofday ();
  }

let append_data_to_listener ~buffer_limit listener data =
  let data_len = Strings.length data in
  let new_length = Strings.Mutable.length listener.pending_data + data_len in
  if new_length > buffer_limit then
    Strings.Mutable.drop listener.pending_data (new_length - buffer_limit);
  Strings.Mutable.append_strings listener.pending_data data

let try_write_to_socket listener =
  (* Get the first available chunk from pending_data and write it directly. *)
    match
      Strings.Mutable.fold
        (fun first s src_ofs len ->
          match first with Some _ -> first | None -> Some (s, src_ofs, len))
        None listener.pending_data
    with
    | None -> 0
    | Some (chunk, chunk_ofs, chunk_len) -> (
        match
          Harbor.write listener.socket
            (Bytes.unsafe_of_string chunk)
            chunk_ofs chunk_len
        with
          | written ->
              if written > 0 then begin
                Strings.Mutable.drop listener.pending_data written;
                listener.last_write_time <- Unix.gettimeofday ()
              end;
              written
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
            ->
              0
          | exception exn ->
              (match exn with
                | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
                    log#info "Socket write for %s: %s (disconnecting)"
                      listener.id (Printexc.to_string exn)
                | _ ->
                    log#info "Socket write error for %s: %s" listener.id
                      (Printexc.to_string exn));
              -1)

let update_burst_buffer burst_buffer ~max_size data =
  Strings.Mutable.append_strings burst_buffer data;
  Strings.Mutable.keep burst_buffer max_size

let get_burst_data burst_buffer = Strings.Mutable.to_strings burst_buffer

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
               ( false,
                 "",
                 Lang.record_t
                   [
                     ("address", Lang.string_t);
                     ("login", Lang.string_t);
                     ("password", Lang.string_t);
                   ] );
             ]
             Lang.bool_t),
        Some Lang.null,
        Some
          "Authentication function. Receives a record with `address`, `login`, \
           and `password` fields. Returns `true` to grant access. When `null`, \
           no authentication is required." );
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

class virtual ['a] base p =
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
  let () =
    Option.iter
      (fun burst ->
        if burst > buffer_limit then
          raise
            (Error.Invalid_value
               ( get_param "buffer",
                 "Maximum buffering inferior to burst length",
                 [] )))
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
                 ( "",
                   Lang.record
                     [
                       ("address", Lang.string address);
                       ("login", Lang.string user);
                       ("password", Lang.string password);
                     ] );
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
    val encoder_data = encoder_data
    val recode_metadata = recode_metadata
    val pos = pos
    val uri = uri
    val port = port
    val transport = transport
    val dumpfile = dumpfile
    val listeners : 'a listener list Atomic.t = Atomic.make []
    val write_task_active : bool Atomic.t = Atomic.make false
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
    method private get_metadata = Atomic.get shared_metadata

    (* Called synchronously when a listener connects. Subclasses populate the
       listener's initial pending_data with the codec header and burst data. *)
    method virtual private connect_listener : 'a listener -> unit

    method virtual private create_listener
        : protocol:string ->
          id:string ->
          socket:Harbor.Http_transport.socket ->
          close:(unit -> unit) ->
          metadata_interval:int ->
          stream_url:string option ->
          timeout:float ->
          ('a listener, Harbor.reply) Duppy.Monad.t

    (* Called when a listener disconnects. Subclasses stop any per-listener
       encoder. *)
    method virtual private stop_listener_encoder : 'a listener -> unit

    method private get_listeners =
      List.filter (fun l -> not (Atomic.get l.closed)) (Atomic.get listeners)

    method private handle_disconnect listener =
      if Atomic.compare_and_set listener.closed false true then begin
        self#log#info "Listener %s disconnected" listener.id;
        self#stop_listener_encoder listener;
        listener.close ();
        List.iter (fun fn -> fn listener.id) on_disconnect_callbacks
      end

    initializer
      let has_stopped = ref false in
      self#on_frame
        (`Before_frame
           (fun _ ->
             if Atomic.get stopped && not !has_stopped then (
               has_stopped := true;
               List.iter self#handle_disconnect self#get_listeners)))

    method private write_task_handler events =
      (* Guard: if anything throws, reset write_task_active so ensure_write_task
         can restart the task rather than leaving it permanently stuck. *)
      (try
         let ready_fds =
           List.filter_map (function `Write fd -> Some fd | _ -> None) events
         in
         let now = Unix.gettimeofday () in
         let to_disconnect =
           List.filter_map
             (fun listener ->
               let fd = Harbor.file_descr_of_socket listener.socket in
               (* Write to ready fds; disconnect on hard error. *)
               let write_error =
                 List.mem fd ready_fds && try_write_to_socket listener < 0
               in
               (* Timeout check applies to all listeners regardless of fd
                  readiness: catches both unresponsive clients and those that
                  keep causing EAGAIN without making progress. *)
               let timed_out =
                 (not (Strings.Mutable.is_empty listener.pending_data))
                 && now -. listener.last_write_time > listener.timeout
               in
               if write_error then Some listener
               else if timed_out then (
                 self#log#info "Listener %s timed out (no progress for %.0fs)"
                   listener.id listener.timeout;
                 Some listener)
               else None)
             self#get_listeners
         in
         List.iter self#handle_disconnect to_disconnect;
         (* CAS loop: only one write task runs at a time, but concurrent
            add_listener calls may race on the list. Contention is minimal. *)
         let rec filter_closed () =
           let current = Atomic.get listeners in
           let filtered =
             List.filter (fun l -> not (Atomic.get l.closed)) current
           in
           if not (Atomic.compare_and_set listeners current filtered) then
             filter_closed ()
         in
         filter_closed ()
       with exn ->
         self#log#important "Write task error: %s" (Printexc.to_string exn);
         Atomic.set write_task_active false);
      self#write_task_next

    method private write_task_next =
      let active = self#get_listeners in
      (* Only watch fds that have data waiting; polling writable fds with nothing
         to send causes a busy-loop since they are always select-ready. *)
      let with_data =
        List.filter_map
          (fun l ->
            if Atomic.get l.closed || Strings.Mutable.is_empty l.pending_data
            then None
            else Some (`Write (Harbor.file_descr_of_socket l.socket)))
          active
      in
      match with_data with
        | [] ->
            (* No pending data right now. Stop the task, then re-check for data
               that may have arrived while we were deciding to stop, so we don't
               miss a concurrent send() whose ensure_write_task was a no-op. *)
            Atomic.set write_task_active false;
            let fresh = self#get_listeners in
            let any_data =
              List.exists
                (fun l -> not (Strings.Mutable.is_empty l.pending_data))
                fresh
            in
            if any_data && Atomic.compare_and_set write_task_active false true
            then (
              let events =
                List.filter_map
                  (fun l ->
                    if
                      Atomic.get l.closed
                      || Strings.Mutable.is_empty l.pending_data
                    then None
                    else Some (`Write (Harbor.file_descr_of_socket l.socket)))
                  fresh
              in
              match events with
                | [] ->
                    Atomic.set write_task_active false;
                    []
                | _ ->
                    [
                      {
                        Task.priority = `Non_blocking;
                        events;
                        handler = self#write_task_handler;
                      };
                    ])
            else []
        | write_events ->
            (* Add a Delay firing at the earliest pending timeout deadline, so
               the handler wakes up to disconnect a slow client even when no fd
               becomes writable. *)
            let now = Unix.gettimeofday () in
            let next_deadline =
              List.fold_left
                (fun acc l ->
                  if Strings.Mutable.is_empty l.pending_data then acc
                  else min acc (l.timeout -. (now -. l.last_write_time)))
                infinity active
            in
            let events =
              if Float.is_finite next_deadline then
                `Delay (max 0. next_deadline) :: write_events
              else write_events
            in
            [
              {
                Task.priority = `Non_blocking;
                events;
                handler = self#write_task_handler;
              };
            ]

    method private ensure_write_task =
      if Atomic.compare_and_set write_task_active false true then (
        match self#write_task_next with
          | [] -> Atomic.set write_task_active false
          | [task] -> Task.add Tutils.scheduler task
          | _ -> assert false)

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
                   (Harbor.http_auth_check ~query ~meth:"GET" ~uri:request_uri
                      ~login socket headers))
                (function
                  | Harbor.Close s ->
                      self#log#info "Listener %s failed to authenticate"
                        client_id;
                      Harbor.reply s
                  | _ -> assert false)
          | None -> Duppy.Monad.return ()
      in
      let* listener =
        self#create_listener ~protocol ~id:client_id ~socket ~close
          ~metadata_interval ~stream_url ~timeout
      in
      Strings.Mutable.append_strings listener.pending_data
        (Strings.of_string http_response);
      self#connect_listener listener;
      (* CAS loop: concurrent connects are rare, and the write task's
         filter_closed may also race on the list. Contention is minimal. *)
      let rec add_listener_atomic () =
        let current = Atomic.get listeners in
        if not (Atomic.compare_and_set listeners current (listener :: current))
        then add_listener_atomic ()
      in
      Unix.set_nonblock (Harbor.file_descr_of_socket socket);
      add_listener_atomic ();
      self#ensure_write_task;
      self#log#info "Listener %s connected" client_id;
      List.iter
        (fun fn -> fn ~headers ~uri:request_uri ~protocol client_id)
        on_connect_callbacks;
      Duppy.Monad.Io.exec ~priority:`Maybe_blocking handler (Harbor.custom ())

    method private register_http_handler =
      Harbor.add_http_handler ~pos ~transport ~port ~verb:`Get ~uri
        (fun ~protocol ~meth:_ ~data:_ ~headers ~query ~socket request_uri ->
          self#add_listener ~protocol ~headers ~uri:request_uri ~query socket)

    method private disconnect_all_listeners =
      List.iter self#handle_disconnect (Atomic.exchange listeners [])
  end

(* Shared encoder: one instance started at output startup, distributed to all
   listeners. connect_listener seeds each new listener with the codec header
   and any accumulated burst data. *)
class shared_output p =
  object (self)
    inherit [unit] base p
    val mutable enc : Encoder.encoder option = None

    method private create_listener ~protocol ~id ~socket ~close
        ~metadata_interval ~stream_url ~timeout =
      match enc with
        | None ->
            Harbor.reply (fun () ->
                Printf.sprintf "HTTP/%s 404 Not found\r\n" protocol)
        | Some _ ->
            Duppy.Monad.return
              (create_listener ~encoder:() ~id ~socket ~close ~metadata_interval
                 ~stream_url ~timeout)

    method private connect_listener listener =
      let e = Option.get enc in
      let burst =
        match burst_size with
          | Some _ -> get_burst_data burst_buffer
          | None -> Strings.empty
      in
      let data =
        insert_icy_metadata ~metadata:self#get_metadata listener
          (Strings.concat [e.Encoder.header (); burst])
      in
      Strings.Mutable.append_strings listener.pending_data data

    method private stop_listener_encoder _ = ()

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
        let metadata = self#get_metadata in
        List.iter
          (fun listener ->
            append_data_to_listener ~buffer_limit listener
              (insert_icy_metadata ~metadata listener data))
          self#get_listeners
      end;
      (* Always attempt to restart the write task, even when no new data was
         produced (len=0). This ensures pending data for slow listeners is
         flushed and the timeout mechanism stays alive after any exception. *)
      self#ensure_write_task

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

(* Dedicated encoder: a fresh instance is created per listener at connect time,
   ensuring each gets a clean stream from the first byte. encode() stores the
   current frame; send() encodes it independently per listener. *)
class dedicated_output p =
  object (self)
    inherit [Encoder.encoder] base p

    val mutable encoder_factory
        : (Frame.Metadata.Export.t -> Encoder.encoder) option =
      None

    val mutable current_frame : Frame.t option = None

    method private create_listener ~protocol ~id ~socket ~close
        ~metadata_interval ~stream_url ~timeout =
      match encoder_factory with
        | None ->
            Harbor.reply (fun () ->
                Printf.sprintf "HTTP/%s 404 Not found\r\n" protocol)
        | Some factory ->
            let encoder = factory Frame.Metadata.Export.empty in
            Duppy.Monad.return
              (create_listener ~encoder ~id ~socket ~close ~metadata_interval
                 ~stream_url ~timeout)

    method private connect_listener listener =
      let burst =
        match burst_size with
          | Some _ -> get_burst_data burst_buffer
          | None -> Strings.empty
      in
      let data =
        insert_icy_metadata ~metadata:self#get_metadata listener
          (Strings.concat [listener.encoder.Encoder.header (); burst])
      in
      Strings.Mutable.append_strings listener.pending_data data

    (* Encode frame into listener under encoder_mutex. The double-checked lock
       on closed ensures mutual exclusion with stop_listener_encoder: closed is
       set (atomically) before stop is called, so any encode that wins the mutex
       after a stop will see closed=true and bail out without touching the
       encoder. *)
    method private listener_encode ~metadata ~frame listener =
      if not (Atomic.get listener.closed) then
        Mutex_utils.mutexify listener.encoder_mutex
          (fun () ->
            if not (Atomic.get listener.closed) then
              append_data_to_listener ~buffer_limit listener
                (insert_icy_metadata ~metadata listener
                   (listener.encoder.Encoder.encode frame)))
          ()

    method private stop_listener_encoder listener =
      Mutex_utils.mutexify listener.encoder_mutex
        (fun () -> ignore (listener.encoder.Encoder.stop ()))
        ()

    method encode frame =
      current_frame <- Some frame;
      Strings.empty

    method encode_metadata m =
      let recoded = recode_metadata (Frame.Metadata.Export.to_metadata m) in
      Atomic.set shared_metadata (Some recoded)

    method send _ =
      Option.iter
        (fun frame ->
          let metadata = self#get_metadata in
          List.iter (self#listener_encode ~metadata ~frame) self#get_listeners;
          current_frame <- None;
          self#ensure_write_task)
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
