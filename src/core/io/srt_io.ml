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

(** SRT input *)

exception Done
exception Not_connected

type prefer_address = [ `System_default | `Ipv4 | `Ipv6 ]
type socket_mode = [ `Connect | `Listen | `Incoming | `Close ]

let conf_srt =
  Dtools.Conf.void ~p:(Configure.conf#plug "srt") "SRT configuration"

let conf_prefer_address =
  Dtools.Conf.string
    ~p:(conf_srt#plug "prefer_address")
    ~d:"system"
    "Set preference for resolving addresses. One of: `\"system\"`, `\"ipv4\"` \
     or `\"ipv6\"`."

let conf_log =
  Dtools.Conf.bool ~p:(conf_srt#plug "log") ~d:true
    "Route srt logs through liquidsoap's logs"

let conf_verbosity =
  Dtools.Conf.string
    ~p:(conf_log#plug "verbosity")
    "Verbosity" ~d:"warning"
    ~comments:
      [
        "Set SRT log level, one of: \"critical\", \"error\", ";
        "\"warning\", \"notice\" or \"debug\"";
      ]

let conf_level = Dtools.Conf.int ~p:(conf_log#plug "level") ~d:4 "Level"
let conf_poll = Dtools.Conf.void ~p:(conf_srt#plug "poll") "Poll configuration"

let conf_timeout =
  Dtools.Conf.float ~p:(conf_poll#plug "timeout") ~d:0.1
    "Timeout for polling loop, in seconda."

let conf_enforced_encryption =
  Dtools.Conf.bool
    ~p:(conf_srt#plug "enforced_encryption")
    ~d:true
    "Enforce consistent encryption settings on both end of any connection."

let string_of_address = function
  | Unix.ADDR_UNIX _ -> assert false
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let getaddrinfo ~(log : Log.t) ~prefer_address address port =
  let hints =
    match prefer_address with
      | `System_default -> []
      | `Ipv4 -> [Unix.AI_FAMILY Unix.PF_INET]
      | `Ipv6 -> [Unix.AI_FAMILY Unix.PF_INET6]
  in
  let hints = Unix.AI_SOCKTYPE Unix.SOCK_STREAM :: hints in
  match Unix.getaddrinfo address (string_of_int port) hints with
    | sockaddr :: _ ->
        if log#active 5 then
          log#f 5 "Address %s:%n resolved to: %s" address port
            (string_of_address sockaddr.Unix.ai_addr);
        sockaddr
    | [] ->
        Runtime_error.raise ~pos:[]
          ~message:
            (Printf.sprintf "getaddrinfo could not resolve address: %s:%i"
               address port)
          "srt"

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "srt"
end)

let sync_source = SyncSource.make ()

let mode_of_value v =
  match Lang.to_string v with
    | "listener" -> `Listener
    | "caller" -> `Caller
    | _ ->
        raise
          (Error.Invalid_value
             ( v,
               "Invalid mode! Should be one of: `\"listener\"` or `\"caller\"`."
             ))

let string_of_mode = function `Listener -> "listener" | `Caller -> "caller"

let common_options ~mode =
  [
    ( "mode",
      Lang.string_t,
      Some (Lang.string (string_of_mode mode)),
      Some
        "Mode to operate on. One of: `\"listener\"` (waits for connection to \
         come in) or `\"caller\"` (initiate connection to a remote server)" );
    ( "listen_callback",
      Lang.nullable_t
        (Lang.fun_t
           [
             (false, "hs_version", Lang.int_t);
             (false, "peeraddr", Lang.string_t);
             (false, "streamid", Lang.nullable_t Lang.string_t);
             (false, "", Builtins_srt.Socket_value.base_t);
           ]
           Lang.bool_t),
      Some Lang.null,
      Some
        "Callback used to decide whether to accept new incoming connections. \
         Used in listener mode only." );
    ( "streamid",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "Set `streamid`. This value can be retrieved by the listener side when \
         connecting to it. Used in caller mode only." );
    ( "ipv6only",
      Lang.nullable_t Lang.bool_t,
      Some Lang.null,
      Some
        "If `true` and `mode` is set to `listen`, only ipv6 connections are \
         accepted. When `null`, defaults to `true` when the `bind_address` is \
         a ipv6 address and system defaults otherwise." );
    ( "passphrase",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "When set to a non-empty string, this option enables encryption and \
         sets the passphrase for it. See `libsrt` documentation for more \
         details." );
    ( "pbkeylen",
      Lang.nullable_t Lang.int_t,
      Some Lang.null,
      Some
        "Set encryption key length. See `libsrt` documentation for more \
         details." );
    ( "enforced_encryption",
      Lang.nullable_t Lang.bool_t,
      Some Lang.null,
      Some
        "Enforces that both connection parties have the same passphrase set, \
         or both do not set the passphrase, otherwise the connection is \
         rejected." );
    ( "host",
      Lang.string_t,
      Some (Lang.string "localhost"),
      Some "Address to connect to. Used only in caller mode." );
    ( "port",
      Lang.int_t,
      Some (Lang.int 8000),
      Some
        "Port to bind on the local machine (listener mode) or to connect to \
         (caller mode). The term `port` as used in SRT is occasionally \
         identical to the term `UDP port`. However SRT offers more flexibility \
         than UDP because it manages ports as its own resources. For example, \
         one port may be shared between various services." );
    ( "bind_address",
      Lang.string_t,
      Some (Lang.string "0.0.0.0"),
      Some "Address to bind on the local machine. Used only in listener mode" );
    ( "prefer_address",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "Preferred address type when resolving hostnames. One of: \
         `\"system\"`, `\"ipv4\"` or `\"ipv6\"`. Defaults to global \
         `srt.prefer_connection` settings when `null`." );
    ( "on_socket",
      Lang.nullable_t
        (Lang.fun_t
           [
             (false, "mode", Lang.string_t);
             (false, "", Builtins_srt.Socket_value.base_t);
           ]
           Lang.unit_t),
      Some Lang.null,
      Some
        "Callback executed when a new SRT socket is created to set additional \
         options, add monitoring, etc. `mode` should be one of: `\"connect\"` \
         (socket created before connecting to a remote address), `\"listen\"` \
         (socket created before binding for receiving new incoming \
         connections), `\"incoming\"` (socket received as incoming connection) \
         or `\"close\"` (socket is about to closed)." );
    ( "polling_delay",
      Lang.float_t,
      Some (Lang.float 2.),
      Some "Delay between connection attempts. Used only in caller mode." );
    ( "read_timeout",
      Lang.nullable_t Lang.float_t,
      Some (Lang.float 1.),
      Some
        "Timeout, in seconds, after which read operations are aborted if no \
         data was received, indefinite if `null`." );
    ( "write_timeout",
      Lang.nullable_t Lang.float_t,
      Some (Lang.float 1.),
      Some
        "Timeout, in seconds, after which write operations are aborted if no \
         data was received, indefinite if `null`." );
    ( "connection_timeout",
      Lang.nullable_t Lang.float_t,
      Some Lang.null,
      Some
        "Timeout, in seconds, after which initial connection operations are \
         aborted if no data was received. Uses library's default if `null`. \
         Used only in `client` mode." );
    ("payload_size", Lang.int_t, Some (Lang.int 1316), Some "Payload size.");
    ("messageapi", Lang.bool_t, Some (Lang.bool true), Some "Use message api");
    ( "on_connect",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to execute when connected." );
    ( "on_disconnect",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to execute when disconnected" );
  ]

let meth () =
  Lang.
    [
      {
        name = "sockets";
        scheme =
          ( [],
            Lang.fun_t []
              (Lang.list_t
                 (Lang.product_t Lang.string_t Builtins_srt.Socket_value.base_t))
          );
        descr = "List of `(connected_address, connected_socket)`";
        value =
          (fun s ->
            Lang.val_fun [] (fun _ ->
                Lang.list
                  (List.map
                     (fun (origin, s) ->
                       Lang.product
                         (Lang.string (Utils.name_of_sockaddr origin))
                         (Builtins_srt.Socket_value.to_base_value s))
                     s#get_sockets)));
      };
      {
        name = "connect";
        scheme = ([], Lang.fun_t [] Lang.unit_t);
        descr =
          "In sender mode, connect to remote server. In listener mode, setup \
           listening socket.";
        value =
          (fun s ->
            Lang.val_fun [] (fun _ ->
                s#set_should_stop false;
                s#connect;
                Lang.unit));
      };
      {
        name = "disconnect";
        scheme = ([], Lang.fun_t [] Lang.unit_t);
        descr = "Disconnect all connected socket.";
        value =
          (fun s ->
            Lang.val_fun [] (fun _ ->
                s#set_should_stop true;
                s#disconnect;
                Lang.unit));
      };
    ]

type common_options = {
  mode : [ `Listener | `Caller ];
  hostname : string;
  port : int;
  bind_address : string;
  prefer_address : [ `System_default | `Ipv4 | `Ipv6 ];
  on_socket : mode:socket_mode -> Srt.socket -> unit;
  listen_callback : Srt.listen_callback option;
  streamid : string option;
  pbkeylen : int option;
  enforced_encryption : bool option;
  ipv6only : bool option;
  passphrase : string option;
  polling_delay : float;
  read_timeout : int option;
  write_timeout : int option;
  connection_timeout : int option;
  payload_size : int;
  messageapi : bool;
  on_connect : (unit -> unit) ref;
  on_disconnect : (unit -> unit) ref;
}

let parse_common_options p =
  let bind_address = Lang.to_string (List.assoc "bind_address" p) in
  let prefer_address =
    let v = List.assoc "prefer_address" p in
    match
      Option.value ~default:conf_prefer_address#get
        (Lang.to_valued_option Lang.to_string v)
    with
      | "system" -> `System_default
      | "ipv4" -> `Ipv4
      | "ipv6" -> `Ipv6
      | _ ->
          raise
            (Error.Invalid_value
               (v, "Valid values are: `\"system\"`, `\"ipv4\"` or `\"ipv6\"`."))
  in
  let ipv6only = Lang.to_valued_option Lang.to_bool (List.assoc "ipv6only" p) in
  let on_socket =
    match Lang.to_option (List.assoc "on_socket" p) with
      | None -> fun ~mode:_ _ -> ()
      | Some fn ->
          fun ~mode s ->
            let mode =
              match mode with
                | `Connect -> "connect"
                | `Listen -> "listen"
                | `Incoming -> "incoming"
                | `Close -> "close"
            in
            ignore
              (Lang.apply fn
                 [
                   ("mode", Lang.string mode);
                   ("", Builtins_srt.Socket_value.to_base_value s);
                 ])
  in
  let passphrase_v = List.assoc "passphrase" p in
  let passphrase = Lang.to_valued_option Lang.to_string passphrase_v in
  (match passphrase with
    | Some s when String.length s < 10 ->
        raise
          (Error.Invalid_value
             (passphrase_v, "Passphrase must be at least 10 characters long!"))
    | Some s when String.length s > 79 ->
        raise
          (Error.Invalid_value
             (passphrase_v, "Passphrase must be at most 79 characters long!"))
    | _ -> ());
  let streamid =
    Lang.to_valued_option Lang.to_string (List.assoc "streamid" p)
  in
  let pbkeylen = Lang.to_valued_option Lang.to_int (List.assoc "pbkeylen" p) in
  let enforced_encryption =
    Lang.to_valued_option Lang.to_bool (List.assoc "enforced_encryption" p)
  in
  let listen_callback =
    let fn = List.assoc "listen_callback" p in
    Option.map
      (fun fn socket hs_version peeraddr streamid ->
        Lang.to_bool
          (Lang.apply fn
             [
               ("hs_version", Lang.int hs_version);
               ("peeraddr", Lang.string (Utils.name_of_sockaddr peeraddr));
               ( "streamid",
                 match streamid with
                   | None -> Lang.null
                   | Some s -> Lang.string s );
               ("", Builtins_srt.Socket_value.to_base_value socket);
             ]))
      (Lang.to_option fn)
  in
  let on_connect = List.assoc "on_connect" p in
  let on_disconnect = List.assoc "on_disconnect" p in
  let polling_delay = Lang.to_float (List.assoc "polling_delay" p) in
  let read_timeout =
    Lang.to_valued_option
      (fun v -> int_of_float (1000. *. Lang.to_float v))
      (List.assoc "read_timeout" p)
  in
  let write_timeout =
    Lang.to_valued_option
      (fun v -> int_of_float (1000. *. Lang.to_float v))
      (List.assoc "write_timeout" p)
  in
  let connection_timeout =
    Lang.to_valued_option
      (fun v -> int_of_float (1000. *. Lang.to_float v))
      (List.assoc "connection_timeout" p)
  in
  {
    mode = mode_of_value (List.assoc "mode" p);
    hostname = Lang.to_string (List.assoc "host" p);
    port = Lang.to_int (List.assoc "port" p);
    bind_address;
    prefer_address;
    on_socket;
    listen_callback;
    pbkeylen;
    enforced_encryption;
    passphrase;
    streamid;
    ipv6only;
    polling_delay;
    read_timeout;
    write_timeout;
    connection_timeout;
    payload_size = Lang.to_int (List.assoc "payload_size" p);
    messageapi = Lang.to_bool (List.assoc "messageapi" p);
    on_connect = ref (fun () -> ignore (Lang.apply on_connect []));
    on_disconnect = ref (fun () -> ignore (Lang.apply on_disconnect []));
  }

let log = Log.make ["srt"]

let log_handler { Srt.Log.message } =
  let message =
    Re.Pcre.substitute
      ~rex:(Re.Pcre.regexp "[ \r\n]+$")
      ~subst:(fun _ -> "")
      message
  in
  log#f conf_level#get "%s" message

(** Common polling task for all srt input/output. sockets entering a poll are
    always set to non-blocking and set back to blocking when exiting. They are
    also always removed from the poll when done. *)
module Poll = struct
  type t = {
    p : Srt.Poll.t;
    handlers : (Srt.socket, Srt.Poll.flag * (Srt.socket -> unit)) Hashtbl.t;
  }

  let t =
    let p = Srt.Poll.create () in
    let handlers = Hashtbl.create 0 in
    { p; handlers }

  exception Empty

  let process () =
    try
      if List.length (Srt.Poll.sockets t.p) = 0 then raise Empty;
      let read, write =
        Srt.Poll.wait t.p ~timeout:(int_of_float (1000. *. conf_timeout#get))
      in
      let apply fn s =
        try fn s
        with exn ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log ~bt
            (Printf.sprintf "Error while executing asynchronous callback: %s"
               (Printexc.to_string exn))
      in
      List.iter
        (fun (event, sockets) ->
          List.iter
            (fun fd ->
              Srt.Poll.remove_usock t.p fd;
              let event', fn = Hashtbl.find t.handlers fd in
              if event = event' then apply fn fd)
            sockets)
        [(`Read, read); (`Write, write)];
      0.
    with
      | Empty | Srt.Error (`Epollempty, _) -> -1.
      | Srt.Error (`Etimeout, _) -> 0.
      | exn ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log ~bt
            (Printf.sprintf "Error while processing SRT socket pool: %s"
               (Printexc.to_string exn));
          -1.

  let task = Duppy.Async.add ~priority:`Blocking Tutils.scheduler process

  let add_socket ~mode socket fn =
    Srt.setsockflag socket Srt.sndsyn false;
    Srt.setsockflag socket Srt.rcvsyn false;
    Hashtbl.replace t.handlers socket (mode, fn);
    Srt.Poll.add_usock t.p socket ~flags:[(mode :> Srt.Poll.flag)];
    Duppy.Async.wake_up task

  let remove_socket socket =
    Hashtbl.remove t.handlers socket;
    if List.mem socket (Srt.Poll.sockets t.p) then
      Srt.Poll.remove_usock t.p socket
end

let init =
  lazy
    (Lifecycle.on_start ~name:"srt initialization" (fun () ->
         Srt.startup ();
         if conf_log#get then (
           let level =
             match conf_verbosity#get with
               | "critical" -> `Critical
               | "error" -> `Error
               | "warning" -> `Warning
               | "notice" -> `Notice
               | "debug" -> `Debug
               | _ ->
                   log#severe "Invalid value for \"srt.log.verbosity\"!";
                   `Error
           in
           Srt.Log.setloglevel level;
           Srt.Log.set_handler log_handler));

     Lifecycle.on_final_cleanup ~name:"set cleanup" (fun () ->
         Srt.Poll.release Poll.t.Poll.p;
         Srt.cleanup ()))

let string_of_address = function
  | Unix.ADDR_UNIX _ -> assert false
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let mk_socket ~mode ~on_socket ~payload_size ~messageapi () =
  let s = Srt.create_socket () in
  Srt.setsockflag s Srt.payloadsize payload_size;
  Srt.setsockflag s Srt.transtype `Live;
  Srt.setsockflag s Srt.messageapi messageapi;
  Srt.setsockflag s Srt.enforced_encryption conf_enforced_encryption#get;
  on_socket ~mode s;
  s

let close_socket ~on_socket s =
  on_socket ~mode:`Close s;
  Srt.close s

let shutdown = Atomic.make false

let () =
  Lifecycle.on_core_shutdown ~name:"srt shutdown" (fun () ->
      Atomic.set shutdown true)

let id =
  let counter = Atomic.make 0 in
  fun () -> Atomic.fetch_and_add counter 1

class virtual base () =
  let () = Lazy.force init in
  object
    val should_stop = Atomic.make false
    val id = id ()
    method private should_stop = Atomic.get shutdown || Atomic.get should_stop
    method set_should_stop v = Atomic.set should_stop v
    method srt_id = id
  end

class virtual networking_agent =
  object
    method virtual private connect : unit
    method virtual private disconnect : unit
    method virtual private is_connected : bool
    method virtual private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b
    method virtual private should_stop : bool
  end

class virtual input_networking_agent =
  object
    inherit networking_agent
    method virtual private get_socket : Unix.sockaddr * Srt.socket
  end

class virtual output_networking_agent =
  object
    inherit networking_agent
    method virtual get_sockets : (Unix.sockaddr * Srt.socket) list

    method virtual private client_error
        : Srt.socket -> exn -> Printexc.raw_backtrace -> unit
  end

module ToDisconnect = Weak.Make (struct
  type t = < disconnect : unit ; srt_id : int >

  let equal t t' = t#srt_id = t'#srt_id
  let hash t = t#srt_id
end)

let to_disconnect = ToDisconnect.create 10

let () =
  Lifecycle.on_core_shutdown ~name:"Srt disconnect" (fun () ->
      ToDisconnect.iter (fun s -> s#disconnect) to_disconnect)

class virtual caller ~enforced_encryption ~pbkeylen ~passphrase ~streamid
  ~polling_delay ~payload_size ~messageapi ~on_socket ~hostname ~port
  ~prefer_address ~connection_timeout ~read_timeout ~write_timeout ~on_connect
  ~on_disconnect =
  object (self)
    method virtual id : string
    method virtual should_stop : bool
    val mutable connect_task = None
    val task_should_stop = Atomic.make false
    val socket = Atomic.make None
    initializer ToDisconnect.add to_disconnect (self :> ToDisconnect.data)

    method private get_socket =
      match Atomic.get socket with Some s -> s | None -> raise Not_connected

    method virtual private log : Log.t
    method private is_connected = Atomic.get socket <> None

    method private connect_fn () =
      try
        let sockaddr =
          getaddrinfo ~log:self#log ~prefer_address hostname port
        in
        self#log#important "Connecting to srt://%s:%d.." hostname port;
        (match Atomic.exchange socket None with
          | None -> ()
          | Some (_, s) -> close_socket ~on_socket s);
        let s =
          mk_socket ~mode:`Connect ~on_socket ~payload_size ~messageapi ()
        in
        try
          Srt.setsockflag s Srt.sndsyn true;
          Srt.setsockflag s Srt.rcvsyn true;
          Utils.optional_apply
            (fun id -> Srt.(setsockflag s streamid id))
            streamid;
          Utils.optional_apply
            (fun b -> Srt.(setsockflag s enforced_encryption b))
            enforced_encryption;
          Utils.optional_apply
            (fun len -> Srt.(setsockflag s pbkeylen len))
            pbkeylen;
          Utils.optional_apply
            (fun p -> Srt.(setsockflag s passphrase p))
            passphrase;
          Utils.optional_apply
            (fun v -> Srt.(setsockflag s conntimeo v))
            connection_timeout;
          Utils.optional_apply
            (fun v -> Srt.(setsockflag s sndtimeo v))
            write_timeout;
          Utils.optional_apply
            (fun v -> Srt.(setsockflag s rcvtimeo v))
            read_timeout;
          Srt.connect s sockaddr.Unix.ai_addr;
          self#log#important "Client connected!";
          !on_connect ();
          Atomic.set socket (Some (sockaddr.Unix.ai_addr, s));
          -1.
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Srt.close s;
          Printexc.raise_with_backtrace exn bt
      with exn ->
        self#log#important "Connect failed: %s" (Printexc.to_string exn);
        if not (Atomic.get task_should_stop) then polling_delay else -1.

    method connect =
      Atomic.set task_should_stop false;
      match connect_task with
        | Some t -> Duppy.Async.wake_up t
        | None ->
            let t =
              Duppy.Async.add ~priority:`Blocking Tutils.scheduler
                self#connect_fn
            in
            connect_task <- Some t;
            Duppy.Async.wake_up t

    method disconnect =
      (match Atomic.exchange socket None with
        | None -> ()
        | Some (_, socket) ->
            close_socket ~on_socket socket;
            !on_disconnect ());
      Atomic.set task_should_stop true;
      match connect_task with
        | None -> ()
        | Some t ->
            Duppy.Async.stop t;
            connect_task <- None
  end

class virtual listener ~enforced_encryption ~pbkeylen ~passphrase ~max_clients
  ~listen_callback ~payload_size ~messageapi ~bind_address ~port ~prefer_address
  ~on_socket ~read_timeout ~ipv6only ~write_timeout ~on_connect ~on_disconnect
  () =
  object (self)
    val mutable client_sockets = []
    method virtual id : string
    method virtual log : Log.t
    method virtual should_stop : bool
    method virtual mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b
    val listening_socket = Atomic.make None
    initializer ToDisconnect.add to_disconnect (self :> ToDisconnect.data)

    method private is_connected =
      self#mutexify (fun () -> client_sockets <> []) ()

    method get_sockets = self#mutexify (fun () -> client_sockets) ()

    method private listening_socket =
      match Atomic.get listening_socket with
        | Some s -> s
        | None -> (
            let s =
              mk_socket ~mode:`Listen ~on_socket ~payload_size ~messageapi ()
            in
            try
              let bind_address =
                getaddrinfo ~log:self#log ~prefer_address bind_address port
              in
              let () =
                match (ipv6only, bind_address.Unix.ai_family) with
                  | Some v, _ -> Srt.(setsockflag s ipv6only v)
                  | None, PF_INET6 -> Srt.(setsockflag s ipv6only true)
                  | _ -> ()
              in
              Srt.bind s bind_address.Unix.ai_addr;
              let max_clients_callback =
                Option.map
                  (fun n _ _ _ _ ->
                    self#mutexify (fun () -> List.length client_sockets < n) ())
                  max_clients
              in
              let listen_callback =
                List.fold_left
                  (fun cur v ->
                    match (cur, v) with
                      | None, _ -> v
                      | Some _, None -> cur
                      | Some cur, Some fn ->
                          Some
                            (fun s hs_version peeraddr streamid ->
                              cur s hs_version peeraddr streamid
                              && fn s hs_version peeraddr streamid))
                  None
                  [max_clients_callback; listen_callback]
              in
              Utils.optional_apply
                (fun fn -> Srt.listen_callback s fn)
                listen_callback;
              Utils.optional_apply
                (fun b -> Srt.(setsockflag s enforced_encryption b))
                enforced_encryption;
              Utils.optional_apply
                (fun len -> Srt.(setsockflag s pbkeylen len))
                pbkeylen;
              Utils.optional_apply
                (fun p -> Srt.(setsockflag s passphrase p))
                passphrase;
              Srt.listen s (Option.value ~default:1 max_clients);
              self#log#info "Setting up socket to listen at %s"
                (string_of_address bind_address.Unix.ai_addr);
              Atomic.set listening_socket (Some s);
              s
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              Srt.close s;
              Printexc.raise_with_backtrace exn bt)

    method connect =
      let rec accept_connection s =
        try
          let client, origin = Srt.accept s in
          (try self#log#info "New connection from %s" (string_of_address origin)
           with exn ->
             self#log#important "Error while fetching connection source: %s"
               (Printexc.to_string exn));
          try
            Poll.add_socket ~mode:`Read s accept_connection;
            Srt.(setsockflag client sndsyn true);
            Srt.(setsockflag client rcvsyn true);
            Utils.optional_apply
              (fun v -> Srt.(setsockflag client sndtimeo v))
              write_timeout;
            Utils.optional_apply
              (fun v -> Srt.(setsockflag client rcvtimeo v))
              read_timeout;
            on_socket ~mode:`Incoming client;
            if self#should_stop then (
              close_socket ~on_socket client;
              raise Done);
            self#mutexify
              (fun () ->
                client_sockets <- (origin, client) :: client_sockets;
                !on_connect ())
              ()
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Srt.close client;
            Printexc.raise_with_backtrace exn bt
        with exn ->
          self#log#debug "Failed to connect: %s" (Printexc.to_string exn)
      in
      if not self#should_stop then
        self#mutexify
          (fun () ->
            Poll.add_socket ~mode:`Read self#listening_socket accept_connection)
          ()

    method disconnect =
      let should_stop = self#should_stop in
      self#mutexify
        (fun () ->
          List.iter (fun (_, s) -> close_socket ~on_socket s) client_sockets;
          client_sockets <- [];
          (match (should_stop, Atomic.get listening_socket) with
            | true, Some s ->
                Poll.remove_socket s;
                close_socket ~on_socket s;
                Atomic.set listening_socket None
            | _ -> ());
          !on_disconnect ())
        ()
  end

class virtual input_base ~max ~self_sync ~on_connect ~on_disconnect
  ~payload_size ~dump ~autostart format =
  let max_length = Some (Frame.main_of_seconds max) in
  object (self)
    inherit input_networking_agent
    inherit base ()

    inherit
      Start_stop.active_source ~name:"input.srt" ~autostart ~fallible:true () as super

    val mutable decoder_data = None
    val mutable dump_chan = None

    initializer
      let on_connect_cur = !on_connect in
      (on_connect :=
         fun () ->
           (match dump with
             | Some fname -> dump_chan <- Some (open_out_bin fname)
             | None -> ());
           on_connect_cur ());
      let on_disconnect_cur = !on_disconnect in
      on_disconnect :=
        fun () ->
          (match decoder_data with
            | Some (d, _) -> d.Decoder.close ()
            | None -> ());
          decoder_data <- None;
          (match dump_chan with
            | Some chan ->
                close_out_noerr chan;
                dump_chan <- None
            | None -> ());
          on_disconnect_cur ()

    initializer
      self#on_wake_up (fun () ->
          Generator.set_max_length self#buffer max_length)

    method seek_source = (self :> Source.source)
    method remaining = -1
    method abort_track = Generator.add_track_mark self#buffer

    method private can_generate_frame =
      super#started && (not self#should_stop) && self#is_connected

    method self_sync =
      if self_sync then
        (`Dynamic, if self#is_connected then Some sync_source else None)
      else (`Static, None)

    method private create_decoder socket =
      let create_decoder =
        match Decoder.get_stream_decoder ~ctype:self#content_type format with
          | Some d -> d
          | None -> raise Harbor.Unknown_codec
      in
      let buf = Buffer.create payload_size in
      let tmp = Bytes.create payload_size in
      let eof_seen = ref false in
      Srt.setsockflag socket Srt.sndsyn true;
      Srt.setsockflag socket Srt.rcvsyn true;
      let read bytes ofs len =
        if self#should_stop then raise Done;
        if !eof_seen && Buffer.length buf = 0 then raise End_of_file;
        if (not !eof_seen) && Buffer.length buf < len then (
          let input = Srt.recvmsg socket tmp payload_size in
          if input = 0 then eof_seen := true;
          Buffer.add_subbytes buf tmp 0 input;
          match dump_chan with
            | Some chan -> output chan tmp 0 input
            | None -> ());
        let len = min len (Buffer.length buf) in
        Buffer.blit buf 0 bytes ofs len;
        Utils.buffer_drop buf len;
        len
      in
      create_decoder { Decoder.read; tell = None; length = None; lseek = None }

    method private generate_frame =
      let size = Lazy.force Frame.size in
      try
        let _, socket = self#get_socket in
        let decoder, buffer =
          match decoder_data with
            | None ->
                let buffer =
                  Decoder.mk_buffer ~ctype:self#content_type self#buffer
                in
                let decoder = self#create_decoder socket in
                decoder_data <- Some (decoder, buffer);
                Generator.add_track_mark self#buffer;
                (decoder, buffer)
            | Some d -> d
        in
        while Generator.length self#buffer < size do
          decoder.Decoder.decode buffer
        done;
        Generator.slice self#buffer size
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Feeding failed: %s" (Printexc.to_string exn));
        self#disconnect;
        if not self#should_stop then self#connect;
        Frame.append (Generator.slice self#buffer size) self#end_of_track

    method private start =
      self#set_should_stop false;
      self#connect

    method private stop =
      self#set_should_stop true;
      self#disconnect
  end

class input_listener ~enforced_encryption ~pbkeylen ~passphrase ~listen_callback
  ~bind_address ~port ~prefer_address ~on_socket ~max ~payload_size ~self_sync
  ~on_connect ~on_disconnect ~read_timeout ~write_timeout ~messageapi ~dump
  ~ipv6only ~autostart format =
  object (self)
    inherit
      input_base
        ~max ~payload_size ~self_sync ~on_connect ~on_disconnect ~dump
          ~autostart format

    inherit
      listener
        ~enforced_encryption ~pbkeylen ~passphrase ~listen_callback
          ~max_clients:(Some 1) ~bind_address ~port ~prefer_address ~on_socket
          ~payload_size ~read_timeout ~write_timeout ~messageapi ~on_connect
          ~on_disconnect ~ipv6only ()

    method private get_socket =
      match self#get_sockets with
        | [s] -> s
        | [] -> raise Not_connected
        | _ -> assert false
  end

class input_caller ~enforced_encryption ~pbkeylen ~passphrase ~streamid
  ~polling_delay ~hostname ~port ~prefer_address ~max ~payload_size ~self_sync
  ~on_connect ~on_disconnect ~read_timeout ~write_timeout ~connection_timeout
  ~messageapi ~dump ~on_socket ~autostart format =
  object (self)
    inherit
      input_base
        ~max ~payload_size ~self_sync ~on_connect ~on_disconnect ~dump
          ~autostart format

    inherit
      caller
        ~enforced_encryption ~pbkeylen ~passphrase ~streamid ~polling_delay
          ~hostname ~port ~prefer_address ~payload_size ~read_timeout
          ~write_timeout ~connection_timeout ~messageapi ~on_connect
          ~on_disconnect ~on_socket

    method get_sockets =
      match self#get_socket with s -> [s] | exception Not_found -> []
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.input "srt" ~return_t ~category:`Input
    ~meth:(meth () @ Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~descr:"Receive a SRT stream from a distant agent."
    (common_options ~mode:`Listener
    @ Start_stop.active_source_proto ~fallible_opt:`Nope
    @ [
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum duration of the buffered data." );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some
            "`true` if the source controls its own latency (i.e. the SRT \
             stream is in `live` mode), `false` otherwise (i.e. the stream is \
             in `file` mode." );
        ( "dump",
          Lang.string_t,
          Some (Lang.string ""),
          Some
            "Dump received data to the given file for debugging. Unused is \
             empty." );
        ( "content_type",
          Lang.string_t,
          Some (Lang.string "application/ffmpeg"),
          Some
            "Content-Type (mime type) used to find a decoder for the input \
             stream." );
      ])
    (fun p ->
      let {
        mode;
        hostname;
        port;
        prefer_address;
        streamid;
        enforced_encryption;
        pbkeylen;
        passphrase;
        bind_address;
        on_socket;
        listen_callback;
        polling_delay;
        read_timeout;
        write_timeout;
        connection_timeout;
        ipv6only;
        payload_size;
        messageapi;
        on_connect;
        on_disconnect;
      } =
        parse_common_options p
      in
      let dump =
        match Lang.to_string (List.assoc "dump" p) with
          | s when s = "" -> None
          | s -> Some s
      in
      let max = Lang.to_float (List.assoc "max" p) in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let format = Lang.to_string (List.assoc "content_type" p) in
      match mode with
        | `Listener ->
            (new input_listener
               ~enforced_encryption ~pbkeylen ~passphrase ~listen_callback
               ~bind_address ~port ~prefer_address ~on_socket ~read_timeout
               ~write_timeout ~payload_size ~self_sync ~on_connect
               ~on_disconnect ~messageapi ~max ~dump ~autostart ~ipv6only format
              :> < Start_stop.active_source
                 ; get_sockets : (Unix.sockaddr * Srt.socket) list
                 ; set_should_stop : bool -> unit
                 ; connect : unit
                 ; disconnect : unit >)
        | `Caller ->
            (new input_caller
               ~enforced_encryption ~pbkeylen ~passphrase ~streamid
               ~polling_delay ~hostname ~port ~prefer_address ~payload_size
               ~self_sync ~on_connect ~read_timeout ~write_timeout
               ~connection_timeout ~on_disconnect ~messageapi ~max ~dump
               ~on_socket ~autostart format
              :> < Start_stop.active_source
                 ; get_sockets : (Unix.sockaddr * Srt.socket) list
                 ; set_should_stop : bool -> unit
                 ; connect : unit
                 ; disconnect : unit >))

class virtual output_base ~payload_size ~messageapi ~infallible ~register_telnet
  ~autostart ~on_disconnect ~encoder_factory source =
  let buffer = Strings.Mutable.empty () in
  let tmp = Bytes.create payload_size in
  object (self)
    inherit output_networking_agent
    inherit base ()

    inherit
      [Strings.t] Output.encoded
        ~output_kind:"srt" ~infallible ~register_telnet
          ~export_cover_metadata:false ~autostart ~name:"output.srt" source

    val mutable encoder = Atomic.make None

    initializer
      let on_disconnect_cur = !on_disconnect in
      on_disconnect :=
        fun () ->
          ignore (Strings.Mutable.flush buffer);
          Atomic.set encoder None;
          on_disconnect_cur ()

    method private send_chunk =
      self#mutexify
        (fun () ->
          Strings.Mutable.blit buffer 0 tmp 0 payload_size;
          Strings.Mutable.drop buffer payload_size)
        ();
      let send data socket =
        if messageapi then Srt.sendmsg socket data (-1) false
        else Srt.send socket data
      in
      let rec f pos socket =
        match pos with
          | pos when pos < payload_size ->
              let ret = send (Bytes.sub tmp pos (payload_size - pos)) socket in
              f (pos + ret) socket
          | _ -> ()
      in
      let f pos (_, socket) =
        try f pos socket
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          self#stop_encoder;
          self#client_error socket exn bt
      in
      List.iter (f 0) self#get_sockets

    method private send_chunks =
      let len = self#mutexify (fun () -> Strings.Mutable.length buffer) in
      while payload_size <= len () do
        self#send_chunk
      done

    method private get_encoder =
      self#mutexify
        (fun () ->
          match Atomic.get encoder with
            | Some enc -> enc
            | None ->
                let enc = encoder_factory self#id Frame.Metadata.Export.empty in
                Atomic.set encoder (Some enc);
                enc)
        ()

    method private start =
      self#mutexify (fun () -> Atomic.set should_stop false) ();
      self#connect

    method! private reset =
      self#start;
      self#stop

    method private stop =
      self#mutexify (fun () -> Atomic.set should_stop true) ();
      self#stop_encoder;
      self#send_chunks;
      self#disconnect

    method stop_encoder =
      if self#is_connected then (
        try
          self#mutexify
            (Strings.Mutable.append_strings buffer)
            (self#get_encoder.Encoder.stop ())
        with exn ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log:self#log ~bt
            (Printf.sprintf "Error while stopping encoder: %s"
               (Printexc.to_string exn)));
      ignore (Strings.Mutable.flush buffer);
      Atomic.set encoder None

    method private encode frame =
      if self#is_connected then self#get_encoder.Encoder.encode frame
      else Strings.empty

    method private encode_metadata m =
      if self#is_connected then self#get_encoder.Encoder.encode_metadata m

    method private send data =
      if self#is_connected then (
        self#mutexify (Strings.Mutable.append_strings buffer) data;
        self#send_chunks)
  end

class output_caller ~enforced_encryption ~pbkeylen ~passphrase ~streamid
  ~polling_delay ~payload_size ~messageapi ~infallible ~register_telnet
  ~autostart ~on_socket ~on_connect ~on_disconnect ~prefer_address ~port
  ~hostname ~read_timeout ~write_timeout ~connection_timeout ~encoder_factory
  source_val =
  let source = Lang.to_source source_val in
  object (self)
    inherit
      output_base
        ~payload_size ~messageapi ~infallible ~register_telnet ~autostart
          ~on_disconnect ~encoder_factory source_val

    inherit
      caller
        ~enforced_encryption ~pbkeylen ~passphrase ~streamid ~polling_delay
          ~hostname ~port ~prefer_address ~payload_size ~read_timeout
          ~write_timeout ~connection_timeout ~messageapi ~on_connect
          ~on_disconnect ~on_socket

    method self_sync = source#self_sync

    method private get_sockets =
      try [self#get_socket] with Not_connected -> []

    method private client_error _ exn bt =
      Utils.log_exception ~log:self#log
        ~bt:(Printexc.raw_backtrace_to_string bt)
        (Printf.sprintf "Error while sending client data: %s"
           (Printexc.to_string exn));
      self#disconnect;
      if not self#should_stop then self#connect
  end

class output_listener ~enforced_encryption ~pbkeylen ~passphrase
  ~listen_callback ~max_clients ~payload_size ~messageapi ~infallible
  ~register_telnet ~autostart ~on_connect ~on_disconnect ~bind_address ~port
  ~prefer_address ~on_socket ~read_timeout ~write_timeout ~encoder_factory
  ~ipv6only source_val =
  let source = Lang.to_source source_val in
  object (self)
    inherit
      output_base
        ~payload_size ~messageapi ~infallible ~register_telnet ~autostart
          ~on_disconnect ~encoder_factory source_val

    inherit
      listener
        ~bind_address ~port ~prefer_address ~on_socket ~payload_size
          ~read_timeout ~write_timeout ~messageapi ~on_connect ~on_disconnect
          ~enforced_encryption ~pbkeylen ~passphrase ~listen_callback
          ~max_clients ~ipv6only ()

    method self_sync = source#self_sync

    method private client_error socket exn bt =
      Utils.log_exception ~log:self#log
        ~bt:(Printexc.raw_backtrace_to_string bt)
        (Printf.sprintf "Error while sending client data: %s"
           (Printexc.to_string exn));
      self#mutexify
        (fun () ->
          close_socket ~on_socket socket;
          client_sockets <-
            List.filter (fun (_, s) -> s <> socket) client_sockets)
        ()
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let output_meth =
    List.map
      (fun m ->
        { m with Lang.value = (fun s -> m.Lang.value (s :> Output.output)) })
      Output.meth
  in
  let callbacks =
    List.map
      (fun m ->
        {
          m with
          Lang.register =
            (fun ~params s fn ->
              m.Lang.register ~params (s :> Output.output) fn);
        })
      Output.callbacks
  in
  Lang.add_operator ~base:Modules.output "srt" ~return_t ~category:`Output
    ~meth:(meth () @ output_meth)
    ~callbacks ~descr:"Send a SRT stream to a distant agent."
    (Output.proto
    @ common_options ~mode:`Caller
    @ [
        ( "max_clients",
          Lang.nullable_t Lang.int_t,
          Some Lang.null,
          Some "Max number of connected clients (listener mode only)" );
        ("", Lang.format_t return_t, None, Some "Encoding format.");
        ("", Lang.source_t return_t, None, None);
      ])
    (fun p ->
      let {
        mode;
        hostname;
        port;
        prefer_address;
        streamid;
        enforced_encryption;
        pbkeylen;
        passphrase;
        bind_address;
        on_socket;
        ipv6only;
        listen_callback;
        polling_delay;
        read_timeout;
        write_timeout;
        connection_timeout;
        payload_size;
        messageapi;
        on_connect;
        on_disconnect;
      } =
        parse_common_options p
      in
      let source = Lang.assoc "" 2 p in
      let max_clients =
        Lang.to_valued_option Lang.to_int (List.assoc "max_clients" p)
      in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let format_val = Lang.assoc "" 1 p in
      let format = Lang.to_format format_val in
      let encoder_factory =
        try (Encoder.get_factory format) ~pos:(Value.pos format_val)
        with Not_found ->
          raise
            (Error.Invalid_value
               (format_val, "Cannot get a stream encoder for that format"))
      in
      match mode with
        | `Caller ->
            (new output_caller
               ~enforced_encryption ~pbkeylen ~passphrase ~streamid
               ~polling_delay ~hostname ~port ~prefer_address ~payload_size
               ~autostart ~read_timeout ~write_timeout ~connection_timeout
               ~infallible ~register_telnet ~messageapi ~encoder_factory
               ~on_socket ~on_connect ~on_disconnect source
              :> < Output.output
                 ; get_sockets : (Unix.sockaddr * Srt.socket) list
                 ; set_should_stop : bool -> unit
                 ; connect : unit
                 ; disconnect : unit >)
        | `Listener ->
            (new output_listener
               ~enforced_encryption ~pbkeylen ~passphrase ~bind_address ~port
               ~prefer_address ~on_socket ~read_timeout ~write_timeout
               ~payload_size ~autostart ~infallible ~register_telnet ~messageapi
               ~encoder_factory ~on_connect ~on_disconnect ~listen_callback
               ~max_clients ~ipv6only source
              :> < Output.output
                 ; get_sockets : (Unix.sockaddr * Srt.socket) list
                 ; set_should_stop : bool -> unit
                 ; connect : unit
                 ; disconnect : unit >))
