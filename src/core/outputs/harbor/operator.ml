module Mime = Icecast_utils.Icecast_v (struct
  type content = string

  let format_of_content mime = mime

  type info = unit

  let info_of_encoder _ _ = ()
end)

(* Every started output registers how to close its listeners, so that the
   shutdown hook can close them without waiting for a frame. *)
let live_outputs : (int, unit -> unit) Hashtbl.t = Hashtbl.create 10
let live_outputs_lock = Mutex.create ()
let live_output_key = Atomic.make 0

let () =
  Lifecycle.before_core_shutdown ~name:"output.harbor listeners" (fun () ->
      List.iter
        (fun close -> close ())
        (Mutex.protect live_outputs_lock (fun () ->
             Hashtbl.fold (fun _ close l -> close :: l) live_outputs [])))

type run = {
  groups : Group.t array;
  registration : Harbor.registration;
  live_key : int;
}

type parameters = {
  mount : string;
  port : int;
  transport : Liq_http.transport;
  timeout : float;
  encoding : Charset.t option;
  url : string option;
  metaint : int;
  auth : Lang.value option;
  accept : Lang.value option;
  buffer : int;
  burst : int option;
  headers : (string * string) list;
  allow_unsynced_clock : bool;
  override_mount : bool;
}

(* The encoder-mode contract of modes.md, as the operator uses it; [encode]
   returns the mode's send step. *)
type mode = {
  start :
    name:string -> last:(Frame.Metadata.Export.t * Icy.event) option -> unit;
  stop : unit -> unit;
  running : unit -> bool;
  encode : Frame.t -> unit -> unit;
  metadata : Frame.Metadata.Export.t -> Icy.event -> unit;
  join :
    head:string ->
    icy:bool ->
    preroll:string option ->
    Listener.t ->
    (pending:(unit -> bool) ->
    flush:(scratch:bytes -> unit) ->
    teardown:(unit -> unit) ->
    Group.member) ->
    Group.member option;
}

let shared_mode ~factory parameters =
  let t =
    Shared.create ~factory ~buffer:parameters.buffer ~burst:parameters.burst
      ~metaint:parameters.metaint ()
  in
  {
    start = Shared.start t;
    stop = (fun () -> Shared.stop t);
    running = (fun () -> Shared.running t);
    encode =
      (fun frame ->
        let encoded = Shared.encode t frame in
        fun () -> Shared.send t encoded);
    metadata = Shared.metadata t;
    join =
      (fun ~head ~icy ~preroll listener make ->
        Shared.join t ~head ~icy ~preroll listener make);
  }

let dedicated_mode ~factory parameters =
  let t =
    Dedicated.create ~factory ~buffer:parameters.buffer
      ~metaint:parameters.metaint ()
  in
  {
    start = Dedicated.start t;
    stop = (fun () -> Dedicated.stop t);
    running = (fun () -> Dedicated.running t);
    encode = (fun frame () -> Dedicated.send t frame);
    metadata = Dedicated.metadata t;
    join =
      (fun ~head ~icy ~preroll listener make ->
        Dedicated.join t ~head ~icy ~preroll listener make);
  }

let invalid value message = raise (Error.Invalid_value (value, message, []))

let refusal_t =
  Lang.optional_method_t
    (Lang.record_t [("status", Lang.int_t); ("reason", Lang.string_t)])
    [
      ( "headers",
        ([], Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)),
        "Reply headers." );
    ]

let decision_t =
  Lang.optional_record_t
    [
      ("refuse", Lang.nullable_t refusal_t);
      ("time_limit", Lang.nullable_t Lang.float_t);
      ("preroll", Lang.nullable_t Lang.string_t);
    ]

type admission = { time_limit : float option; preroll : string option }

let rec decision_of_value value =
  match Lang.to_option value with
    | None -> `Admit { time_limit = None; preroll = None }
    | Some value -> (
        let fields, _ = Lang.split_meths value in
        let field name convert =
          Option.bind
            (List.assoc_opt name fields)
            (Lang.to_valued_option convert)
        in
        match field "refuse" refusal_of_value with
          | Some refusal -> `Refuse refusal
          | None ->
              `Admit
                {
                  time_limit =
                    field "time_limit" (fun limit ->
                        let limit = Lang.to_float limit in
                        if limit < 0. then
                          invalid value "Time limit must not be negative.";
                        limit);
                  preroll = field "preroll" Lang.to_string;
                })

and refusal_of_value value =
  let fields, _ = Lang.split_meths value in
  let refusal =
    {
      Connection.status = Lang.to_int (List.assoc "status" fields);
      reason = Lang.to_string (List.assoc "reason" fields);
      headers =
        (match List.assoc_opt "headers" fields with
          | None -> []
          | Some headers ->
              List.map
                (fun header ->
                  let name, value = Lang.to_product header in
                  (Lang.to_string name, Lang.to_string value))
                (Lang.to_list headers));
    }
  in
  match Connection.refusal_error refusal with
    | Some message -> invalid value message
    | None -> refusal

let parse p =
  let arg name = List.assoc name p in
  let positive name to_value =
    let value = arg name in
    if to_value value <= 0. then invalid value "Must be positive.";
    value
  in
  let mount = Lang.to_string (arg "mount") in
  if mount = "" then invalid (arg "mount") "Mount must not be empty.";
  let mount = if mount.[0] = '/' then mount else "/" ^ mount in
  let int_positive name =
    Lang.to_int (positive name (fun v -> float_of_int (Lang.to_int v)))
  in
  let metaint = int_positive "metaint" in
  let buffer = int_positive "buffer" in
  let timeout = Lang.to_float (positive "timeout" Lang.to_float) in
  let dedicated = Lang.to_bool (arg "dedicated_encoder") in
  let burst = Lang.to_valued_option Lang.to_int (arg "burst") in
  (match burst with
    | Some burst when (not dedicated) && burst > buffer ->
        invalid (arg "buffer") "Maximum buffering inferior to burst length"
    | _ -> ());
  let headers =
    List.map
      (fun value ->
        let name, content = Lang.to_product value in
        let header = (Lang.to_string name, Lang.to_string content) in
        Option.iter (invalid value) (Connection.header_error header);
        header)
      (Lang.to_list (arg "headers"))
  in
  let encoding =
    match Lang.to_string (arg "encoding") with
      | "" -> None
      | name -> (
          try Some (Charset.of_string name)
          with Charset.Unknown_encoding _ ->
            invalid (arg "encoding") "Unknown charset.")
  in
  ( {
      mount;
      port = Lang.to_int (arg "port");
      transport = Lang.to_http_transport (arg "transport");
      timeout;
      encoding;
      url = Lang.to_valued_option Lang.to_string (arg "url");
      metaint;
      auth = Lang.to_option (arg "auth");
      accept = Lang.to_option (arg "accept");
      buffer;
      burst = (if dedicated then None else burst);
      headers;
      allow_unsynced_clock = Lang.to_bool (arg "allow_unsynced_clock");
      override_mount = Lang.to_bool (arg "override_mount");
    },
    dedicated )

let peer_address = function
  | Unix.ADDR_INET (address, port) ->
      let ip = Unix.string_of_inet_addr address in
      (ip, Printf.sprintf "%s:%d" ip port)
  | Unix.ADDR_UNIX path -> (path, path ^ ":0")

class output p =
  let parameters, dedicated = parse p in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
  let source = Lang.assoc "" 2 p in
  let pos = Lang.pos p in
  let { Mime.factory; format = mime; _ } = Mime.encoder_data p in
  object (self)
    inherit
      [unit -> unit] Output.encoded
        ~output_kind:"output.harbor" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false
        ~name:(Lang.to_string (List.assoc "mount" p))
        source

    val mode =
      if dedicated then dedicated_mode ~factory parameters
      else shared_mode ~factory parameters

    method private self_sync = (Lang.to_source source)#cached_self_sync
    val run : run option Atomic.t = Atomic.make None
    val start_stop = Mutex.create ()
    val next_id = Atomic.make 0
    val generation = Atomic.make 0

    val last_metadata : (Frame.Metadata.Export.t * Icy.event) option Atomic.t =
      Atomic.make None

    val connected : (int, Listener.t) Hashtbl.t = Hashtbl.create 10
    val connected_lock = Mutex.create ()
    val on_connect : (Lang.value -> unit) Callbacks.t = Callbacks.create ()
    val on_disconnect : (Lang.value -> unit) Callbacks.t = Callbacks.create ()
    method register_on_connect fn = Callbacks.register on_connect fn
    method register_on_disconnect fn = Callbacks.register on_disconnect fn

    method listeners =
      Mutex.protect connected_lock (fun () ->
          Hashtbl.fold
            (fun _ listener l ->
              if Listener.is_closed listener then l else listener :: l)
            connected [])

    method private fire callbacks record =
      List.iter
        (fun fn ->
          try fn record
          with exn ->
            let bt = Printexc.get_backtrace () in
            Utils.log_exception ~log:self#log ~bt
              (Printf.sprintf "Listener callback failed: %s"
                 (Printexc.to_string exn)))
        (Callbacks.elements callbacks)

    method private teardown listener =
      Mutex.protect connected_lock (fun () ->
          Hashtbl.remove connected (Listener.info listener).id);
      self#fire on_disconnect (Listener.to_value listener)

    method private convert metadata =
      let target = Option.value ~default:Charset.utf8 parameters.encoding in
      Frame.Metadata.Export.from_metadata ~cover:false
        (Frame.Metadata.from_list
           (List.map
              (fun (key, value) ->
                ( key,
                  try Charset.convert ~fail:true ~target value
                  with exn ->
                    self#log#info "Could not convert metadata %s: %s" key
                      (Printexc.to_string exn);
                    value ))
              (Frame.Metadata.Export.to_list metadata)))

    method private encode_metadata metadata =
      let metadata = self#convert metadata in
      let event =
        Icy.event
          ~generation:(Atomic.fetch_and_add generation 1)
          ~url:parameters.url
          (Frame.Metadata.Export.to_list metadata)
      in
      Atomic.set last_metadata (Some (metadata, event));
      mode.metadata metadata event

    method private encode frame = mode.encode frame

    method private send data =
      data ();
      match Atomic.get run with
        | Some { groups; _ } ->
            Array.iter
              (fun g -> if Group.has_members g then Group.wake g)
              groups
        | None -> ()

    method private check_clock =
      match Clock.sync_mode self#clock with
        | (`Unsynced | `Passive) as sync
          when not parameters.allow_unsynced_clock ->
            Lang.raise_error ~pos
              ~message:
                (Printf.sprintf
                   "output.harbor %s: clock %s is not paced in real time \
                    (sync: %s). Listeners expect data at the real-time rate; \
                    set allow_unsynced_clock=true to allow it."
                   self#id (Clock.name self#clock)
                   (Clock.string_of_sync_mode sync))
              "clock"
        | _ -> ()

    method private start =
      Mutex.protect start_stop (fun () ->
          if Atomic.get run = None then (
            self#check_clock;
            mode.start ~name:self#id ~last:(Atomic.get last_metadata);
            let groups =
              Array.init (Scheduler.max_domains ()) (fun _ ->
                  Group.create ~log:self#log ~timeout:parameters.timeout ())
            in
            let registration =
              try
                Harbor.add_http_handler ~override:parameters.override_mount ~pos
                  ~transport:parameters.transport ~port:parameters.port
                  ~verb:`Get ~path:(`Literal parameters.mount) self#handle
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                mode.stop ();
                Printexc.raise_with_backtrace exn bt
            in
            let live_key = Atomic.fetch_and_add live_output_key 1 in
            Mutex.protect live_outputs_lock (fun () ->
                Hashtbl.replace live_outputs live_key (fun () ->
                    List.iter Listener.mark_closed self#listeners));
            Atomic.set run (Some { groups; registration; live_key })))

    method private stop =
      Mutex.protect start_stop (fun () ->
          match Atomic.get run with
            | None -> ()
            | Some { groups; registration; live_key } ->
                Harbor.remove_http_handler registration;
                Atomic.set run None;
                mode.stop ();
                Array.iter Group.stop groups;
                Mutex.protect live_outputs_lock (fun () ->
                    Hashtbl.remove live_outputs live_key))

    method private authenticate ~peer ~sockaddr ~headers ~query =
      match parameters.auth with
        | None -> ()
        | Some auth -> (
            let refuse reply =
              self#log#info "Listener %s failed to authenticate" peer;
              Harbor.simple_reply reply
            in
            match Connection.credentials ~headers ~query with
              | None -> refuse (Connection.unauthorized `No_credentials)
              | Some (login, password) -> (
                  let address =
                    Utils.name_of_sockaddr ~rev_dns:Harbor.conf_revdns#get
                      ~show_port:false sockaddr
                  in
                  let record =
                    Lang.record
                      [
                        ("address", Lang.string address);
                        ("login", Lang.string (Charset.convert login));
                        ("password", Lang.string (Charset.convert password));
                      ]
                  in
                  match Lang.to_bool (Lang.apply auth [("", record)]) with
                    | true -> ()
                    | false ->
                        refuse (Connection.unauthorized `Wrong_credentials)
                    | exception
                        Runtime_error.Runtime_error
                          { kind = "error.icecast.server.mount_taken"; _ } ->
                        refuse Connection.mount_taken
                    | exception exn ->
                        let bt = Printexc.get_backtrace () in
                        Utils.log_exception ~log:self#log ~bt
                          (Printf.sprintf "Authentication of %s failed: %s" peer
                             (Printexc.to_string exn));
                        refuse ""))

    method private decision info =
      match parameters.accept with
        | None -> `Admit { time_limit = None; preroll = None }
        | Some accept -> (
            try
              decision_of_value
                (Lang.apply accept [("", Listener.info_to_value info)])
            with exn ->
              let bt = Printexc.get_backtrace () in
              Utils.log_exception ~log:self#log ~bt
                (Printf.sprintf "Admission predicate failed for %s: %s"
                   info.Listener.peer (Printexc.to_string exn));
              `Refuse Connection.forbidden)

    method private handle ~protocol ~meth:_ ~data:_ ~headers ~query
        ~query_string ~socket uri =
      let sockaddr = Unix.getpeername socket#file_descr in
      let ip, peer = peer_address sockaddr in
      self#log#info "New listener connection from %s" peer;
      self#authenticate ~peer ~sockaddr ~headers ~query;
      let unavailable () =
        Harbor.simple_reply (Connection.unavailable ~protocol)
      in
      let groups =
        match Atomic.get run with
          | Some { groups; _ } when mode.running () -> groups
          | _ -> unavailable ()
      in
      let info =
        {
          Listener.id = Atomic.fetch_and_add next_id 1;
          ip;
          peer;
          uri;
          query_string;
          protocol;
          headers;
          connected_at = Unix.gettimeofday ();
        }
      in
      let { time_limit; preroll } =
        match self#decision info with
          | `Admit admission -> admission
          | `Refuse refusal ->
              self#log#info "Listener %s refused: %d %s" peer
                refusal.Connection.status refusal.Connection.reason;
              Harbor.simple_reply (Connection.refusal_reply ~protocol refusal)
      in
      if not (mode.running ()) then unavailable ();
      let group = groups.(info.id mod Array.length groups) in
      let listener =
        Listener.create
          ?deadline:
            (Option.map (fun limit -> Listener.now () +. limit) time_limit)
          ~log:self#log
          ~wake:(fun () -> Group.wake group)
          info socket
      in
      self#fire on_connect (Listener.to_value listener);
      Mutex.protect connected_lock (fun () ->
          Hashtbl.replace connected info.id listener);
      self#admit ~protocol ~headers ~preroll ~group listener;
      Harbor.custom ()

    (* Steps 7 to 9 of admission: from here on, every failure ends with the
       listener closed and both callbacks fired. *)
    method private admit ~protocol ~headers ~preroll ~group listener =
      let fail ?member reason =
        self#log#info "Listener %s failed to connect: %s"
          (Listener.info listener).peer reason;
        Listener.mark_closed listener;
        Listener.close_socket listener;
        match member with
          | Some member -> member.Group.teardown ()
          | None -> self#teardown listener
      in
      let icy = Icy.negotiated ~mime headers in
      let head =
        Connection.response_head ~protocol ~mime
          ~metaint:(if icy then Some parameters.metaint else None)
          parameters.headers
      in
      match
        mode.join ~head ~icy ~preroll listener (fun ~pending ~flush ~teardown ->
            {
              Group.listener;
              pending;
              flush;
              teardown =
                (fun () ->
                  (try teardown ()
                   with exn ->
                     self#log#info "Listener %s encoder stop failed: %s"
                       (Listener.info listener).peer (Printexc.to_string exn));
                  self#teardown listener);
            })
      with
        | exception exn -> fail (Printexc.to_string exn)
        | None -> fail "output stopped"
        | Some member -> (
            match (Listener.socket listener)#non_blocking true with
              | exception exn -> fail ~member (Printexc.to_string exn)
              | () ->
                  if Group.publish group member then
                    self#log#info "Listener %s connected"
                      (Listener.info listener).peer
                  else fail ~member "output stopped")
  end

let auth_t =
  Lang.fun_t
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
    Lang.bool_t

let proto frame_t =
  Output.proto
  @ Icecast_utils.base_proto frame_t
  @ [
      ( "mount",
        Lang.string_t,
        None,
        Some "Mount point to serve, matched literally." );
      ("port", Lang.int_t, Some (Lang.int 8000), Some "Port to serve on.");
      ( "transport",
        Lang.http_transport_base_t,
        Some (Lang.base_http_transport Liq_http.unix_transport),
        Some
          "Http transport. Use `http.transport.ssl` or `http.transport.tls`, \
           when available, to enable HTTPS output" );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some
          "Seconds a listener owed bytes may go without accepting any before \
           it is disconnected." );
      ( "encoding",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Encoding used to send metadata. If empty, defaults to \"UTF-8\"."
      );
      ( "url",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Url sent as `StreamUrl` in ICY metadata." );
      ( "metaint",
        Lang.int_t,
        Some (Lang.int 8192),
        Some "Interval used to send ICY metadata, in stream bytes." );
      ( "auth",
        Lang.nullable_t auth_t,
        Some Lang.null,
        Some
          "Authentication function. `f(address, login, password)` returns \
           `true` if the user should be granted access for this login. When \
           null, listeners are not asked for credentials." );
      ( "accept",
        Lang.nullable_t
          (Lang.fun_t
             [(false, "", Listener.record_t)]
             (Lang.nullable_t decision_t)),
        Some Lang.null,
        Some
          "Admission predicate, called with the listener record of an \
           authenticated request before `on_connect`. It returns `null` to \
           admit the request, or `{time_limit?, preroll?}` to admit it, \
           disconnect it after `time_limit` seconds and send it the bytes of \
           `preroll` before the stream, or `{refuse = {status, reason, \
           headers?}}` to refuse it with that status line and those headers \
           and an empty body. A preroll is sent as is: it must be in the \
           stream's format, for instance MP3 or ADTS AAC with the same \
           parameters. A predicate that raises refuses with `403 Forbidden`. \
           When null, every authenticated request is admitted." );
      ( "buffer",
        Lang.int_t,
        Some (Lang.int (5 * 65535)),
        Some
          "How far, in stream bytes, a listener may fall behind the live \
           stream before it is disconnected." );
      ( "burst",
        Lang.nullable_t Lang.int_t,
        Some (Lang.int 65534),
        Some
          "Bytes of already-encoded stream a new listener receives first. \
           `null` for none. Ignored with `dedicated_encoder`." );
      ( "headers",
        Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
        Some (Lang.list []),
        Some "Additional headers of the response." );
      ( "dedicated_encoder",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Give each listener its own encoder, so that every listener's stream \
           starts from an encoder's first byte. Costs one encoding per \
           listener." );
      ( "allow_unsynced_clock",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Allow running on a clock that is not paced in real time (sync \
           `none` or passive)." );
      ( "override_mount",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Advanced: take over the mount when another registration on the port \
           already serves it, instead of failing to start. The most recent \
           registration serves the mount." );
      ("", Lang.source_t frame_t, None, None);
    ]

let listener_callback name descr register =
  {
    Lang_source.name;
    params = [];
    descr;
    register_deprecated_argument = false;
    arg_t = [(false, "", Listener.record_t)];
    register =
      (fun ~params:_ s fn -> register s (fun record -> fn [("", record)]));
  }

let _ =
  let frame_t = Lang.univ_t () in
  Lang.add_operator ~base:Modules.output "harbor" ~category:`Output
    ~descr:"Encode and output the stream using the harbor server."
    ~meth:
      (Start_stop.meth ()
      @ [
          {
            Lang.name = "listeners";
            scheme = ([], Lang.fun_t [] (Lang.list_t Listener.record_t));
            descr = "Listeners currently connected.";
            value =
              (fun (s : output) ->
                Lang.val_fun [] (fun _ ->
                    Lang.list (List.map Listener.to_value s#listeners)));
          };
        ])
    ~callbacks:
      (Start_stop.output_callbacks ()
      @ [
          listener_callback "on_connect" "when a listener is admitted"
            (fun s fn -> s#register_on_connect fn);
          listener_callback "on_disconnect" "when a listener is disconnected"
            (fun s fn -> s#register_on_disconnect fn);
        ])
    (proto frame_t) ~return_t:frame_t
    (fun p -> new output p)
