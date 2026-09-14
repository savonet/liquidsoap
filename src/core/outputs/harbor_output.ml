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

module Http = Liq_http

let log = Log.make ["harbor"; "output"]
let stopped = Atomic.make false

let conf_output =
  Dtools.Conf.void
    ~p:(Harbor.conf_harbor#plug "output")
    "Settings for output.harbor."

let conf_writers =
  Dtools.Conf.int
    ~p:(conf_output#plug "writers")
    ~d:(Domain.recommended_domain_count ())
    "Writer tasks per output.harbor"
    ~comments:
      [
        "Listeners of an output are spread over this many writer tasks, each";
        "running on its own core. Defaults to the number of cores; lower it on";
        "a small machine that also has clocks to tick.";
      ]

let () =
  Lifecycle.before_core_shutdown ~name:"Harbor stop" (fun () ->
      Atomic.set stopped true)

(** Output to harbor listeners. *)

module Task = Duppy.Task

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

(* What a listener of a shared encoder reads: the ring from [offset] on, after
   [carry], which holds the HTTP response and codec header at connect and the
   unsent tail of an ICY block after a partial write. *)
type shared_state = {
  mutable offset : int;
  mutable carry : string;
  mutable carry_ofs : int;
}

(* What a listener of a dedicated encoder reads: its own encoder's output. *)
type dedicated_state = {
  encoder : Encoder.encoder;
  pending_data : Strings.Mutable.t;
  encoder_mutex : Mutex.t;
}

(* The mutable fields belong to the writer task of the listener's shard;
   connect_listener sets them up before the listener is published. *)
type 'a listener = {
  id : string;
  socket : Harbor.Http_transport.socket;
  close : unit -> unit;
  state : 'a;
  metadata_interval : int option;
  stream_url : string option;
  closed : bool Atomic.t;
  timeout : float;
  mutable metadata_position : int;
  mutable last_sent_metadata : Frame.metadata option;
  mutable last_write_time : float;
}

(* A slice of the listeners with the writer task that serves them. *)
type 'a shard = {
  members : 'a listener list Atomic.t;
  wake : (Unix.file_descr * Unix.file_descr) option Atomic.t;
  drain : Bytes.t;
  staging : Bytes.t;
}

(* Length byte plus 255 blocks of 16 bytes. *)
let max_icy_block = (255 * 16) + 1

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

(* The block due at a boundary: the metadata when it changed since the last
   block sent to this listener, an empty block otherwise. *)
let icy_block ~metadata ~last_sent listener =
  match metadata with
    | Some m when metadata != last_sent ->
        format_icy_metadata ~url:listener.stream_url m
    | _ -> "\000"

let insert_icy_metadata ~metadata listener data =
  match listener.metadata_interval with
    | None -> data
    | Some metadata_interval ->
        let rec insert_at_intervals accumulated remaining =
          let remaining_len = Strings.length remaining in
          let bytes_until_next_meta =
            metadata_interval - listener.metadata_position
          in
          if bytes_until_next_meta <= remaining_len then begin
            let meta_string =
              icy_block ~metadata ~last_sent:listener.last_sent_metadata
                listener
            in
            listener.last_sent_metadata <- metadata;
            let before = Strings.sub remaining 0 bytes_until_next_meta in
            let after =
              Strings.sub remaining bytes_until_next_meta
                (remaining_len - bytes_until_next_meta)
            in
            let with_meta =
              Strings.concat
                [accumulated; before; Strings.of_string meta_string]
            in
            listener.metadata_position <- 0;
            insert_at_intervals with_meta after
          end
          else begin
            listener.metadata_position <-
              listener.metadata_position + remaining_len;
            Strings.concat [accumulated; remaining]
          end
        in
        insert_at_intervals Strings.empty data

let create_listener ~id ~state ~socket ~close ~metadata_interval ~stream_url
    ~timeout =
  {
    id;
    socket;
    close;
    state;
    metadata_interval;
    stream_url;
    closed = Atomic.make false;
    timeout;
    metadata_position = 0;
    last_sent_metadata = None;
    last_write_time = Unix.gettimeofday ();
  }

(* Returns [false] when appending would exceed [buffer_limit]: the data is not
   appended and the listener should be disconnected. Dropping data instead
   would glitch the stream and permanently desync ICY metadata offsets for the
   client. *)
let append_data_to_listener ~buffer_limit listener data =
  let new_length =
    Strings.Mutable.length listener.state.pending_data + Strings.length data
  in
  if new_length > buffer_limit then false
  else begin
    Strings.Mutable.append_strings listener.state.pending_data data;
    true
  end

(* Returns the bytes accepted by the socket, 0 when it would block, -1 on a
   hard error. Over TLS the write blocks until everything is sent, which stalls
   this listener's shard for that long. *)
let write_socket listener bytes ofs len =
  match Harbor.write listener.socket bytes ofs len with
    | written ->
        if written > 0 then listener.last_write_time <- Unix.gettimeofday ();
        written
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> 0
    | exception exn ->
        (match exn with
          | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
              log#info "Socket write for %s: %s (disconnecting)" listener.id
                (Printexc.to_string exn)
          | _ ->
              log#info "Socket write error for %s: %s" listener.id
                (Printexc.to_string exn));
        -1

let try_write_to_socket listener =
  (* The buffer is locked while the write syscall runs so concurrent appends
     cannot move the bytes being written. *)
  Strings.Mutable.write listener.state.pending_data (write_socket listener)

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
          "Http transport. Use `http.transport.ssl`, when available, to enable \
           HTTPS output" );
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
    let descr = [%string {|^%{mount_path}$|}] in
    Liquidsoap_lang.Lang_regexp.
      { descr; flags = []; regexp = Re.Pcre.regexp descr }
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
          Utils.name_of_sockaddr ~rev_dns:Harbor.conf_revdns#get
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
    val mutable shards : 'a shard array = [||]
    val next_shard = Atomic.make 0
    val shared_metadata : Frame.metadata option Atomic.t = Atomic.make None
    val mutable dump_channel : out_channel option = None
    val on_connect_callbacks = Callbacks.create ()
    val on_disconnect_callbacks = Callbacks.create ()
    val start_stop_mutex = Mutex.create ()
    method register_on_connect fn = Callbacks.register on_connect_callbacks fn
    method on_connect fn = Callbacks.add on_connect_callbacks fn

    method register_on_disconnect fn =
      Callbacks.register on_disconnect_callbacks fn

    method on_disconnect fn = Callbacks.add on_disconnect_callbacks fn
    method self_sync = source#self_sync
    method private get_metadata = Atomic.get shared_metadata

    (* Called synchronously when a listener connects, before it is published
       to its shard. Subclasses queue the HTTP response, the codec header and
       any burst for it. *)
    method virtual private connect_listener
        : http_response:string -> 'a listener -> unit

    method virtual private has_pending : 'a listener -> bool

    (* Scratch space a shard's writer needs per flush. *)
    method virtual private staging_size : int

    (* Writes what the listener is owed, disconnecting it on a hard error or
       when it has fallen further behind than [buffer]. Runs on the writer
       task of the listener's shard only. *)
    method virtual private flush_listener : 'a shard -> 'a listener -> unit

    method virtual private create_listener
        : protocol:string ->
          id:string ->
          socket:Harbor.Http_transport.socket ->
          close:(unit -> unit) ->
          metadata_interval:int option ->
          stream_url:string option ->
          timeout:float ->
          'a listener

    (* Called when a listener disconnects. Subclasses stop any per-listener
       encoder. *)
    method virtual private stop_listener_encoder : 'a listener -> unit

    method private get_listeners =
      Array.fold_left
        (fun acc shard ->
          List.fold_left
            (fun acc l -> if Atomic.get l.closed then acc else l :: acc)
            acc (Atomic.get shard.members))
        [] shards

    method private handle_disconnect listener =
      if Atomic.compare_and_set listener.closed false true then begin
        self#log#info "Listener %s disconnected" listener.id;
        (* The socket is closed by the write task once the listener is out of
           its select set: closing it here could make a concurrent select fail
           on the closed fd. *)
        self#wake_write_task;
        (* Encoder teardown and user callbacks can be slow: run them on the
           Maybe_blocking queue so they never delay the streaming thread or
           the non-blocking write task. *)
        Task.add Tutils.scheduler
          {
            Task.priority = `Threaded;
            events = [`Delay 0.];
            handler =
              (fun _ ->
                self#stop_listener_encoder listener;
                List.iter
                  (fun fn -> fn listener.id)
                  (Callbacks.elements on_disconnect_callbacks);
                []);
          }
      end

    (* Remove closed listeners from the shard and close their sockets. Only
       called from the shard's write task so a closed fd never lingers in its
       select set. CAS loop: concurrent add_listener calls may race on the
       list. Contention is minimal. *)
    method private remove_closed_listeners shard =
      let rec remove () =
        let current = Atomic.get shard.members in
        let open_listeners, closed_listeners =
          List.partition (fun l -> not (Atomic.get l.closed)) current
        in
        if not (Atomic.compare_and_set shard.members current open_listeners)
        then remove ()
        else List.iter (fun l -> l.close ()) closed_listeners
      in
      remove ()

    method private disconnect_overflowed listener =
      self#log#info
        "Listener %s buffer overflow (client too slow), disconnecting"
        listener.id;
      self#handle_disconnect listener

    initializer
      let has_stopped = ref false in
      self#on_frame
        (`Before_frame
           (fun _ ->
             if Atomic.get stopped && not !has_stopped then (
               has_stopped := true;
               List.iter self#handle_disconnect self#get_listeners)))

    (* A shard's task waits on its wake pipe and a one second heartbeat for
       the timeout check. It does not watch listener sockets for
       writability: on Windows [Unix.select] falls back to edge-triggered
       WSAEventSelect whenever any non-socket fd is present in the scheduler,
       and FD_WRITE then only fires once after connect, so pending data would
       never be flushed. Instead [send] wakes every shard each streaming
       cycle and each firing attempts a non-blocking write to every listener
       of the shard with pending data. *)
    method private write_task_handler shard ~wake_out events =
      match Atomic.get shard.wake with
        | None ->
            (* The output was stopped: close the listeners disconnected by
               stop, then this task owns the read end, close it and
               terminate. *)
            self#remove_closed_listeners shard;
            (try Unix.close wake_out with _ -> ());
            []
        | Some _ ->
            (try
               if List.exists (( = ) (`Read wake_out)) events then
                 ignore
                   (Unix.read wake_out shard.drain 0 (Bytes.length shard.drain));
               let now = Unix.gettimeofday () in
               List.iter
                 (fun listener ->
                   if
                     (not (Atomic.get listener.closed))
                     && self#has_pending listener
                   then begin
                     self#flush_listener shard listener;
                     (* A client that keeps causing EAGAIN without making
                        progress is disconnected once it exceeds its
                        timeout. *)
                     if
                       (not (Atomic.get listener.closed))
                       && now -. listener.last_write_time > listener.timeout
                     then begin
                       self#log#info
                         "Listener %s timed out (no progress for %.0fs)"
                         listener.id listener.timeout;
                       self#handle_disconnect listener
                     end
                   end)
                 (Atomic.get shard.members);
               self#remove_closed_listeners shard
             with exn ->
               self#log#important "Write task error: %s"
                 (Printexc.to_string exn));
            [
              {
                Task.priority = `Blocking;
                events = [`Read wake_out; `Delay 1.];
                handler = self#write_task_handler shard ~wake_out;
              };
            ]

    method private start_write_tasks =
      if Array.length shards = 0 then begin
        let count = max 1 conf_writers#get in
        shards <-
          Array.init count (fun _ ->
              (* A socket pair rather than a pipe: on Windows only sockets
                 can be made non-blocking, and a blocking wake-up write could
                 hang the streaming thread. *)
              let wake_out, wake_in = Unix_utils.socketpair ~cloexec:true () in
              Unix.set_nonblock wake_in;
              {
                members = Atomic.make [];
                wake = Atomic.make (Some (wake_out, wake_in));
                drain = Bytes.create 1024;
                staging = Bytes.create self#staging_size;
              });
        Array.iter
          (fun shard ->
            match Atomic.get shard.wake with
              | Some (wake_out, _) ->
                  Task.add Tutils.scheduler
                    {
                      Task.priority = `Blocking;
                      events = [`Read wake_out; `Delay 1.];
                      handler = self#write_task_handler shard ~wake_out;
                    }
              | None -> ())
          shards
      end

    method private stop_write_tasks =
      Array.iter
        (fun shard ->
          match Atomic.exchange shard.wake None with
            | None -> ()
            | Some (_, wake_in) -> (
                (* Wake the task so it observes the state change and closes
                   the read end; we own and close the write end. Failures
                   are fine: they can only mean the task is already shutting
                   down. *)
                (try ignore (Unix.write wake_in (Bytes.make 1 ' ') 0 1)
                 with _ -> ());
                try Unix.close wake_in with _ -> ()))
        shards;
      shards <- [||]

    (* Signal a shard's write task that pending data or listener state
       changed. A full pipe means a wake is already pending. *)
    method private wake_shard shard =
      match Atomic.get shard.wake with
        | Some (_, wake_in) -> (
            try ignore (Unix.write wake_in (Bytes.make 1 ' ') 0 1)
            with _ -> ())
        | None -> ()

    method private wake_write_task = Array.iter self#wake_shard shards

    method private add_listener ~protocol ~headers ~uri:request_uri ~query
        socket =
      let client_id =
        let fd = Harbor.file_descr_of_socket socket in
        Utils.name_of_sockaddr ~show_port:true (Unix.getpeername fd)
      in
      let metadata_interval, icy_header =
        match List.assoc_opt "icy-metadata" headers with
          | Some "1" ->
              (Some metaint, Printf.sprintf "icy-metaint: %d\r\n" metaint)
          | _ | (exception _) -> (None, "")
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
      let on_failure exn =
        let error_msg =
          match exn with
            | Duppy.Io.Error Duppy.Io.Timeout ->
                Printf.sprintf "Timeout for %s" client_id
            | Duppy.Io.Error Duppy.Io.Io_error ->
                Printf.sprintf "I/O error for %s" client_id
            | Duppy.Io.Error (Duppy.Io.Unix (c, p, m, _)) ->
                Printf.sprintf "Unix error for %s: %s" client_id
                  (Printexc.to_string (Unix.Unix_error (c, p, m)))
            | e -> Printexc.to_string e
        in
        self#log#info "%s" error_msg;
        List.find_opt (fun l -> l.id = client_id) self#get_listeners
        |> Option.iter self#handle_disconnect;
        Harbor.simple_reply ""
      in
      self#log#info "New listener connection from %s" client_id;
      (match login with
        | Some login -> (
            Duppy.reschedule ~priority:`Threaded Tutils.scheduler;
            try
              Harbor.http_auth_check ~query ~meth:"GET" ~uri:request_uri ~login
                socket headers
            with
              | Harbor.Reply (Harbor.Close s) ->
                  self#log#info "Listener %s failed to authenticate" client_id;
                  Harbor.reply s
              | Harbor.Reply _ as e -> raise e
              | e -> on_failure e)
        | None -> ());
      let listener =
        self#create_listener ~protocol ~id:client_id ~socket ~close
          ~metadata_interval ~stream_url ~timeout
      in
      if Array.length shards = 0 then
        Harbor.reply (fun () ->
            Printf.sprintf "HTTP/%s 503 Service Unavailable\r\n" protocol);
      self#connect_listener ~http_response listener;
      let shard =
        shards.(Atomic.fetch_and_add next_shard 1 mod Array.length shards)
      in
      (* CAS loop: concurrent connects are rare, and the shard's write task
         may also race on the list. Contention is minimal. *)
      let rec add_listener_atomic () =
        let current = Atomic.get shard.members in
        if
          not
            (Atomic.compare_and_set shard.members current (listener :: current))
        then add_listener_atomic ()
      in
      Unix.set_nonblock (Harbor.file_descr_of_socket socket);
      add_listener_atomic ();
      self#wake_shard shard;
      self#log#info "Listener %s connected" client_id;
      List.iter
        (fun fn -> fn ~headers ~uri:request_uri ~protocol client_id)
        (Callbacks.elements on_connect_callbacks);
      Duppy.reschedule ~priority:`Threaded Tutils.scheduler;
      Harbor.custom ()

    method private register_http_handler =
      Harbor.add_http_handler ~pos ~transport ~port ~verb:`Get ~uri
        (fun ~protocol ~meth:_ ~data:_ ~headers ~query ~socket request_uri ->
          self#add_listener ~protocol ~headers ~uri:request_uri ~query socket)

    (* Listeners are only marked as closed here: the write tasks remove them
       from their shards and close their sockets. *)
    method private disconnect_all_listeners =
      Array.iter
        (fun shard ->
          List.iter self#handle_disconnect (Atomic.get shard.members))
        shards
  end

(* Shared encoder: one instance started at output startup, its output kept
   in a ring every listener reads from at its own offset. A new listener
   starts with the codec header and, when bursting, [burst] bytes behind the
   tail of the ring. *)
class shared_output p =
  object (self)
    inherit [shared_state] base p
    val mutable enc : Encoder.encoder option = None

    (* Twice [buffer]: a listener is disconnected once it lags by [buffer],
       so the writer cannot wrap over bytes a reader is copying unless it
       appends another [buffer] worth meanwhile, which the read detects. *)
    val ring =
      ByteRing.create ~capacity:(2 * Lang.to_int (List.assoc "buffer" p))

    method private create_listener ~protocol ~id ~socket ~close
        ~metadata_interval ~stream_url ~timeout =
      match enc with
        | None ->
            Harbor.reply (fun () ->
                Printf.sprintf "HTTP/%s 404 Not found\r\n" protocol)
        | Some _ ->
            create_listener
              ~state:{ offset = 0; carry = ""; carry_ofs = 0 }
              ~id ~socket ~close ~metadata_interval ~stream_url ~timeout

    method private connect_listener ~http_response listener =
      let e = Option.get enc in
      let header =
        insert_icy_metadata ~metadata:self#get_metadata listener
          (e.Encoder.header ())
      in
      listener.state.carry <- http_response ^ Strings.to_string header;
      listener.state.carry_ofs <- 0;
      let tail = ByteRing.tail ring in
      listener.state.offset <-
        (match burst_size with
          | Some burst -> max 0 (tail - burst)
          | None -> tail)

    method private stop_listener_encoder _ = ()
    method private staging_size = buffer_limit + max_icy_block

    method private has_pending listener =
      listener.state.carry_ofs < String.length listener.state.carry
      || listener.state.offset < ByteRing.tail ring

    (* Stages the carry, then ring data cut at ICY boundaries with the
       metadata block at each, writes it all in one syscall, and consumes
       what the socket took segment by segment. A metadata block the socket
       did not take whole moves to the carry, so it counts as sent. *)
    method private flush_listener shard listener =
      let tail = ByteRing.tail ring in
      let behind = tail - listener.state.offset in
      if behind > buffer_limit then self#disconnect_overflowed listener
      else begin
        let staging = shard.staging in
        let room = Bytes.length staging in
        let staged = ref 0 in
        let segments = ref [] in
        let carry_len =
          String.length listener.state.carry - listener.state.carry_ofs
        in
        if carry_len > 0 then begin
          let n = min carry_len room in
          Bytes.blit_string listener.state.carry listener.state.carry_ofs
            staging 0 n;
          staged := n;
          segments := [`Carry n]
        end;
        let torn = ref false in
        if !staged = carry_len then begin
          let metadata = self#get_metadata in
          let position = ref listener.metadata_position in
          let last_sent = ref listener.last_sent_metadata in
          let remaining = ref behind in
          let continue = ref true in
          while !continue && !remaining > 0 do
            match listener.metadata_interval with
              | Some interval when !position >= interval ->
                  let block =
                    icy_block ~metadata ~last_sent:!last_sent listener
                  in
                  let len = String.length block in
                  if !staged + len > room then continue := false
                  else begin
                    Bytes.blit_string block 0 staging !staged len;
                    staged := !staged + len;
                    last_sent := metadata;
                    segments := `Meta (block, metadata) :: !segments;
                    position := 0
                  end
              | interval ->
                  let chunk = min !remaining (room - !staged) in
                  let chunk =
                    match interval with
                      | Some interval -> min chunk (interval - !position)
                      | None -> chunk
                  in
                  if chunk <= 0 then continue := false
                  else if
                    not
                      (ByteRing.read ring ~ofs:(tail - !remaining) staging
                         !staged chunk)
                  then begin
                    torn := true;
                    continue := false
                  end
                  else begin
                    staged := !staged + chunk;
                    segments := `Data chunk :: !segments;
                    position := !position + chunk;
                    remaining := !remaining - chunk
                  end
          done
        end;
        if !torn then self#disconnect_overflowed listener
        else if !staged > 0 then begin
          let written = write_socket listener staging 0 !staged in
          if written < 0 then self#handle_disconnect listener
          else begin
            let rec consume written = function
              | [] -> ()
              | `Carry n :: rest ->
                  let k = min written n in
                  listener.state.carry_ofs <- listener.state.carry_ofs + k;
                  if
                    listener.state.carry_ofs
                    = String.length listener.state.carry
                  then begin
                    listener.state.carry <- "";
                    listener.state.carry_ofs <- 0
                  end;
                  if k = n then consume (written - k) rest
              | `Data n :: rest ->
                  let k = min written n in
                  listener.state.offset <- listener.state.offset + k;
                  listener.metadata_position <- listener.metadata_position + k;
                  if k = n then consume (written - k) rest
              | `Meta (block, sent) :: rest ->
                  let n = String.length block in
                  let k = min written n in
                  listener.metadata_position <- 0;
                  listener.last_sent_metadata <- sent;
                  if k = n then consume (written - k) rest
                  else begin
                    listener.state.carry <- String.sub block k (n - k);
                    listener.state.carry_ofs <- 0
                  end
            in
            consume written (List.rev !segments)
          end
        end
      end

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
      if Strings.length data > 0 then begin
        ByteRing.append ring data;
        Option.iter
          (fun ch -> Strings.iter (output_substring ch) data)
          dump_channel
      end;
      (* Always wake the write tasks, even when no new data was produced, so
         pending data for slow listeners keeps getting flushed. *)
      self#wake_write_task

    method start =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match enc with
            | Some _ -> ()
            | None ->
                let factory = encoder_data.factory self#id in
                enc <- Some (factory Frame.Metadata.Export.empty);
                self#start_write_tasks;
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
                self#stop_write_tasks;
                Option.iter close_out dump_channel;
                dump_channel <- None)
        ()
  end

(* Dedicated encoder: a fresh instance is created per listener at connect time,
   ensuring each gets a clean stream from the first byte. encode() stores the
   current frame; send() encodes it independently per listener. *)
class dedicated_output p =
  object (self)
    inherit [dedicated_state] base p

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
            let state =
              {
                encoder = factory Frame.Metadata.Export.empty;
                pending_data = Strings.Mutable.empty ();
                encoder_mutex = Mutex.create ();
              }
            in
            create_listener ~state ~id ~socket ~close ~metadata_interval
              ~stream_url ~timeout

    method private connect_listener ~http_response listener =
      let header =
        insert_icy_metadata ~metadata:self#get_metadata listener
          (listener.state.encoder.Encoder.header ())
      in
      Strings.Mutable.append_strings listener.state.pending_data
        (Strings.concat [Strings.of_string http_response; header])

    method private has_pending listener =
      not (Strings.Mutable.is_empty listener.state.pending_data)

    method private staging_size = 0

    method private flush_listener _ listener =
      if try_write_to_socket listener < 0 then self#handle_disconnect listener

    (* Encode frame into listener under encoder_mutex. The double-checked lock
       on closed ensures mutual exclusion with stop_listener_encoder: closed is
       set (atomically) before stop is called, so any encode that wins the mutex
       after a stop will see closed=true and bail out without touching the
       encoder. *)
    method private listener_encode ~metadata ~frame listener =
      let appended =
        if Atomic.get listener.closed then true
        else
          Mutex_utils.mutexify listener.state.encoder_mutex
            (fun () ->
              if Atomic.get listener.closed then true
              else
                append_data_to_listener ~buffer_limit listener
                  (insert_icy_metadata ~metadata listener
                     (listener.state.encoder.Encoder.encode frame)))
            ()
      in
      (* Disconnect outside encoder_mutex: the deferred encoder teardown takes
         the same lock. *)
      if not appended then self#disconnect_overflowed listener

    method private stop_listener_encoder listener =
      Mutex_utils.mutexify listener.state.encoder_mutex
        (fun () -> ignore (listener.state.encoder.Encoder.stop ()))
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
          self#wake_write_task)
        current_frame

    method start =
      Mutex_utils.mutexify start_stop_mutex
        (fun () ->
          match encoder_factory with
            | Some _ -> ()
            | None ->
                encoder_factory <- Some (encoder_data.factory self#id);
                self#start_write_tasks;
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
                self#stop_write_tasks;
                Option.iter close_out dump_channel;
                dump_channel <- None)
        ()
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
               s#register_on_connect callback);
         };
         {
           name = "on_disconnect";
           params = [];
           descr = "Callback when a listener disconnects.";
           register_deprecated_argument = true;
           arg_t = [(false, "", Lang.string_t)];
           register =
             (fun ~params:_ s callback ->
               s#register_on_disconnect (fun ip ->
                   callback [("", Lang.string ip)]));
         };
       ]
      @ Start_stop.output_callbacks ())
    ~meth:(Start_stop.meth ()) ~base:Modules.output "harbor" (proto return_t)
    ~return_t
    (fun p ->
      if Lang.to_bool (List.assoc "dedicated_encoder" p) then
        new dedicated_output p
      else new shared_output p)
