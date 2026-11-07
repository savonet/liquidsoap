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

(** The connection roles (spec/connection.md, caller.md, listener.md). An
    operator builds one [connection] from its parameters and its kind's
    reactions, calls [start] and [stop] from its own start and stop, and
    [connect], [disconnect] and [sockets] from its script methods.

    Role state changes under one lock, tagged with a generation that every late
    result is checked against; every event and every kind reaction runs
    afterwards on the operator's event thread. *)

type prefer_address = [ `System | `Ipv4 | `Ipv6 ]

type listen_callback =
  hs_version:int ->
  peeraddr:string ->
  streamid:string option ->
  Srt.socket ->
  bool

type params = {
  mode : [ `Caller | `Listener ];
  host : string;
  port : int;
  bind_address : string;
  prefer_address : prefer_address;
  ipv6only : bool option;
  listen_callback : listen_callback option;
  streamid : string option;
  passphrase : string option;
  pbkeylen : int option;
  enforced_encryption : bool option;
  polling_delay : float;
  connection_timeout : float option;
  payload_size : int;
  messageapi : bool;
}

(** What a kind sees of a peer: [fail] reports a failed exchange, once or many
    times, from any thread. *)
type peer_handle = {
  socket : Srt.socket;
  address : Unix.sockaddr;
  fail : unit -> unit;
}

(** A kind's part (spec/connection.md §5 and §8): [setup_socket] sets the
    options it needs for exchanging data; [connected] is its reaction to a new
    peer and [live] runs once the peer is live; [ending] runs before the socket
    closes and calls its continuation once the socket may close; [ended] runs
    after. [shutdown_wait] bounds the wait for a clean end at shutdown. *)
type 'reaction kind = {
  client_limit : int option;
  setup_socket : Srt.socket -> unit;
  connected : peer_handle -> 'reaction;
  live : 'reaction -> unit;
  ending : 'reaction -> clean:bool -> (unit -> unit) -> unit;
  ended : 'reaction -> unit;
  shutdown_wait : float;
}

(** [announced]: [on_connect] fired, so [on_disconnect] is owed. [created]: the
    socket received its creation event, so its close event is owed. *)
type 'reaction peer = {
  handle : peer_handle;
  mutable created : bool;
  mutable reaction : 'reaction option;
  mutable announced : bool;
  mutable live_since : float;
  live : bool Atomic.t;
  ended : bool Atomic.t;
}

(** The socket of an attempt in flight, closed by a release so that a connect
    blocked on it returns. *)
type attempt = { mutable attempt_socket : Srt.socket option }

type 'reaction state =
  | Idle
  | Attempting of attempt
  | Waiting
  | Connected of 'reaction peer
  | Listening of { listener : Srt.socket; mutable peers : 'reaction peer list }

let is_attempting = function Attempting _ -> true | _ -> false
let is_waiting = function Waiting -> true | _ -> false
let backlog = function Some n -> max n 1 | None -> 128

class ['reaction] connection ~(log : unit -> Log.t) ~name ~params
  ~(kind : 'reaction kind) () =
  let events = Srt_lib.Events.create name in
  object (self)
    val m = Mutex.create ()
    val mutable state : 'reaction state = Idle
    val mutable generation = 0
    val mutable active = false
    val mutable started = false
    val held = Atomic.make false
    val open_sockets = Atomic.make 0
    val on_connect : (unit -> unit) Callbacks.t = Callbacks.create ()
    val on_disconnect : (unit -> unit) Callbacks.t = Callbacks.create ()

    val on_socket : (string -> Srt.socket -> unit) Callbacks.t =
      Callbacks.create ()

    val mutable shutdown_hook = None

    initializer
      shutdown_hook <-
        Some (Srt_lib.register_shutdown (fun () -> self#release_at_shutdown))

    method register_on_connect fn = Callbacks.register on_connect fn
    method register_on_disconnect fn = Callbacks.register on_disconnect fn
    method register_on_socket fn = Callbacks.register on_socket fn

    (** Whether the connection is wanted (spec/common.md §5). *)
    method wanted = Mutex.protect m (fun () -> active)

    method sockets =
      let live p = Atomic.get p.live in
      let entry p = (Srt_lib.ip_of_address p.handle.address, p.handle.socket) in
      Mutex.protect m (fun () ->
          match state with
            | Connected p when live p -> [entry p]
            | Listening { peers } -> List.map entry (List.filter live peers)
            | _ -> [])

    method start =
      Atomic.set held false;
      Mutex.protect m (fun () -> started <- true);
      self#update

    method stop =
      Mutex.protect m (fun () -> started <- false);
      self#update

    method connect =
      Atomic.set held false;
      self#update

    method disconnect =
      Atomic.set held true;
      self#update

    method private fire : 'a. string -> 'a Callbacks.t -> ('a -> unit) -> unit =
      fun callback_name callbacks call ->
        List.iter
          (fun fn ->
            try call fn
            with exn ->
              (log ())#severe "Error in %s callback: %s" callback_name
                (Printexc.to_string exn))
          (Callbacks.elements callbacks)

    method private socket_event mode socket =
      self#fire "on_socket" on_socket (fun fn -> fn mode socket)

    method private update =
      let action =
        Mutex.protect m (fun () ->
            let wanted =
              started
              && (not (Atomic.get held))
              && not (Atomic.get Srt_lib.shutting_down)
            in
            if wanted && not active then (
              active <- true;
              generation <- generation + 1;
              `Connect generation)
            else if active && not wanted then (
              active <- false;
              generation <- generation + 1;
              let previous = state in
              state <- Idle;
              `Release previous)
            else `Nothing)
      in
      match action with
        | `Connect generation -> self#connect_role generation
        | `Release previous -> self#release previous
        | `Nothing -> ()

    method private current generation' = generation = generation'

    method private new_socket =
      let socket =
        Srt_lib.create_socket ~payload_size:params.payload_size
          ~messageapi:params.messageapi ()
      in
      Atomic.incr open_sockets;
      socket

    method private close_socket ~created socket =
      if created then self#socket_event "close" socket;
      Srt_lib.close_quietly socket;
      Atomic.decr open_sockets

    method private set_encryption socket =
      Option.iter
        (Srt.setsockflag socket Srt.enforced_encryption)
        params.enforced_encryption;
      Option.iter (Srt.setsockflag socket Srt.pbkeylen) params.pbkeylen;
      match params.passphrase with
        | Some passphrase when passphrase <> "" ->
            Srt.setsockflag socket Srt.passphrase passphrase
        | _ -> ()

    method private make_peer ~created socket address =
      let rec peer =
        {
          handle =
            {
              socket;
              address;
              fail = (fun () -> self#end_peer peer ~clean:false);
            };
          created;
          reaction = None;
          announced = false;
          live_since = 0.;
          live = Atomic.make false;
          ended = Atomic.make false;
        }
      in
      peer

    method private connect_role generation =
      match params.mode with
        | `Caller ->
            let attempt = { attempt_socket = None } in
            Mutex.protect m (fun () -> state <- Attempting attempt);
            self#spawn_attempt generation attempt
        | `Listener -> self#listen ~first:true generation

    method private peer_connected peer =
      match kind.connected peer.handle with
        | exception exn ->
            (log ())#important "Connection setup failed: %s"
              (Printexc.to_string exn);
            self#end_peer peer ~clean:false
        | reaction ->
            peer.reaction <- Some reaction;
            peer.announced <- true;
            self#fire "on_connect" on_connect (fun fn -> fn ());
            if not (Atomic.get peer.ended) then (
              peer.live_since <- Unix.gettimeofday ();
              Atomic.set peer.live true;
              kind.live reaction)

    method private end_peer peer ~clean =
      if Atomic.compare_and_set peer.ended false true then
        Srt_lib.Events.post events (fun () -> self#finish_peer peer ~clean)

    method private finish_peer peer ~clean =
      Atomic.set peer.live false;
      let retry =
        Mutex.protect m (fun () ->
            match state with
              | Connected p when p == peer ->
                  generation <- generation + 1;
                  state <- Waiting;
                  let lived = Unix.gettimeofday () -. peer.live_since in
                  Some
                    ( generation,
                      if peer.announced && lived >= params.polling_delay then 0.
                      else params.polling_delay )
              | Listening l ->
                  l.peers <- List.filter (fun p -> p != peer) l.peers;
                  None
              | _ -> None)
      in
      let close () =
        Srt_lib.Events.post events (fun () ->
            self#close_peer peer;
            Option.iter
              (fun (generation, delay) ->
                self#schedule_attempt generation delay)
              retry)
      in
      match peer.reaction with
        | Some reaction -> kind.ending reaction ~clean close
        | None -> close ()

    method private close_peer peer =
      self#close_socket ~created:peer.created peer.handle.socket;
      Option.iter kind.ended peer.reaction;
      if peer.announced then
        self#fire "on_disconnect" on_disconnect (fun fn -> fn ())

    method private release previous =
      match previous with
        (* Closing a connecting socket blocks, and a release may run on the
           streaming thread. *)
        | Attempting { attempt_socket = Some socket } ->
            ignore (Thread.create Srt_lib.close_quietly socket)
        | Idle | Attempting _ | Waiting -> ()
        | Connected peer -> self#end_peer peer ~clean:true
        | Listening { listener; peers } ->
            Srt_lib.Events.post events (fun () ->
                Srt_poll.remove listener;
                self#close_socket ~created:true listener);
            List.iter (fun peer -> self#end_peer peer ~clean:true) peers

    method private release_at_shutdown =
      self#update;
      let deadline = Unix.gettimeofday () +. kind.shutdown_wait in
      (* ponytail: polls the socket count; shutdown runs once. *)
      while Atomic.get open_sockets > 0 && Unix.gettimeofday () < deadline do
        Thread.delay 0.02
      done

    method private schedule_attempt generation delay =
      let start () =
        let attempt = { attempt_socket = None } in
        let go =
          Mutex.protect m (fun () ->
              self#current generation && is_waiting state
              &&
              (state <- Attempting attempt;
               true))
        in
        if go then self#spawn_attempt generation attempt
      in
      if delay <= 0. then start ()
      else
        ignore
          (Thread.create
             (fun () ->
               Thread.delay delay;
               Srt_lib.Events.post events start)
             ())

    method private spawn_attempt generation attempt =
      ignore
        (Thread.create
           (fun () ->
             let result =
               try
                 let addresses =
                   Srt_lib.resolve ~prefer_address:params.prefer_address
                     params.host params.port
                 in
                 (log ())#info "Connecting to srt://%s:%d" params.host
                   params.port;
                 Ok (self#connect_addresses generation attempt addresses)
               with exn -> Error exn
             in
             Srt_lib.Events.post events (fun () ->
                 self#attempt_done generation result))
           ())

    method private set_caller_options socket =
      Option.iter (Srt.setsockflag socket Srt.streamid) params.streamid;
      self#set_encryption socket;
      Option.iter
        (fun timeout ->
          Srt.setsockflag socket Srt.conntimeo (Srt_lib.ms_of_seconds timeout))
        params.connection_timeout;
      kind.setup_socket socket

    method private connect_addresses generation attempt =
      function
      | [] -> assert false
      | address :: rest -> (
          let socket = self#new_socket in
          Mutex.protect m (fun () -> attempt.attempt_socket <- Some socket);
          let created = ref false in
          match
            self#set_caller_options socket;
            Srt_lib.Events.run events (fun () ->
                created := true;
                self#socket_event "connect" socket);
            if not (Mutex.protect m (fun () -> self#current generation)) then
              failwith "connection abandoned";
            Srt_lib.blocking (fun () -> Srt.connect socket address)
          with
            | () -> (socket, address, !created)
            | exception exn ->
                let exn =
                  match exn with
                    | Srt.Error (`Econnrej, system, message) ->
                        Srt.Error
                          ( `Econnrej,
                            system,
                            message ^ ": " ^ Srt.getrejectreason socket )
                    | exn -> exn
                in
                Srt_lib.Events.run events (fun () ->
                    self#close_socket ~created:!created socket);
                if rest = [] then raise exn
                else self#connect_addresses generation attempt rest)

    method private attempt_done generation result =
      let current () = self#current generation && is_attempting state in
      match result with
        | Ok (socket, address, created) -> (
            let peer =
              Mutex.protect m (fun () ->
                  if current () then (
                    let peer = self#make_peer ~created socket address in
                    state <- Connected peer;
                    Some peer)
                  else None)
            in
            match peer with
              | Some peer ->
                  (log ())#important "Connected to srt://%s"
                    (Srt_lib.string_of_address address);
                  self#peer_connected peer
              | None -> self#close_socket ~created socket)
        | Error exn ->
            let permanent = Srt_lib.is_permanent exn in
            let still_current =
              Mutex.protect m (fun () ->
                  current ()
                  &&
                  (state <- (if permanent then Idle else Waiting);
                   true))
            in
            if still_current then (
              let message =
                Printf.sprintf "Connection to srt://%s:%d failed: %s"
                  params.host params.port (Printexc.to_string exn)
              in
              if permanent then (log ())#severe "%s" message
              else (
                (log ())#important "%s" message;
                self#schedule_attempt generation params.polling_delay))

    method private listen ~first generation =
      let attempt () =
        try Ok (self#setup_listener generation) with exn -> Error exn
      in
      if first && not (Srt_lib.Events.on_runner events) then
        self#listen_result ~raise_permanent:true generation
          (Srt_lib.Events.run events attempt)
      else
        Srt_lib.Events.post events (fun () ->
            self#listen_result ~raise_permanent:false generation (attempt ()))

    method private listen_result ~raise_permanent generation =
      function
      | Ok () -> ()
      | Error exn ->
          let message =
            Printf.sprintf "Listening on srt://%s:%d failed: %s"
              params.bind_address params.port (Printexc.to_string exn)
          in
          if Srt_lib.is_permanent exn then (
            if raise_permanent then
              failwith (Printf.sprintf "%s: %s" name message);
            (log ())#severe "%s" message)
          else (
            (log ())#important "%s" message;
            let waiting =
              Mutex.protect m (fun () ->
                  self#current generation
                  &&
                  (state <- Waiting;
                   true))
            in
            if waiting then
              ignore
                (Thread.create
                   (fun () ->
                     Thread.delay params.polling_delay;
                     if Mutex.protect m (fun () -> self#current generation) then
                       self#listen ~first:false generation)
                   ()))

    method private setup_listener generation =
      let bind_address =
        match (params.bind_address, params.prefer_address) with
          | "0.0.0.0", `Ipv6 -> "::"
          | bind_address, _ -> bind_address
      in
      let address =
        List.hd
          (Srt_lib.resolve ~prefer_address:params.prefer_address bind_address
             params.port)
      in
      let socket = self#new_socket in
      let created = ref false in
      try
        (match params.ipv6only with
          | Some v -> Srt.setsockflag socket Srt.ipv6only v
          | None ->
              if Srt_lib.is_ipv6 address then
                Srt.setsockflag socket Srt.ipv6only true);
        self#set_encryption socket;
        created := true;
        self#socket_event "listen" socket;
        Srt.bind socket address;
        Srt.listen_callback socket (self#admit generation);
        Srt.listen socket (backlog kind.client_limit);
        (log ())#info "Listening on srt://%s"
          (Srt_lib.string_of_address address);
        let listening =
          Mutex.protect m (fun () ->
              self#current generation
              &&
              (state <- Listening { listener = socket; peers = [] };
               true))
        in
        if listening then (
          Srt_poll.register socket (self#accept generation);
          self#accept generation socket)
        else self#close_socket ~created:true socket
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        self#close_socket ~created:!created socket;
        Printexc.raise_with_backtrace exn bt

    method private admitted_count generation =
      Mutex.protect m (fun () ->
          match state with
            | Listening { peers } when self#current generation ->
                Some (List.length peers)
            | _ -> None)

    method private admit generation socket hs_version peeraddr streamid =
      match (self#admitted_count generation, kind.client_limit) with
        | None, _ -> false
        | Some count, Some limit when count >= limit -> false
        | Some _, _ -> (
            match params.listen_callback with
              | None -> true
              | Some fn -> (
                  try
                    fn ~hs_version
                      ~peeraddr:(Srt_lib.ip_of_address peeraddr)
                      ~streamid socket
                  with exn ->
                    (log ())#severe "Error in listen_callback: %s"
                      (Printexc.to_string exn);
                    false))

    method private accept generation listener =
      match Srt.accept listener with
        | exception Srt.Error (`Easyncrcv, _, _) -> ()
        | exception exn ->
            (log ())#important "Failed to accept new connection: %s"
              (Printexc.to_string exn)
        | socket, origin ->
            self#admit_accepted generation socket origin;
            self#accept generation listener

    method private admit_accepted generation socket origin =
      Atomic.incr open_sockets;
      (log ())#info "New connection from %s" (Srt_lib.string_of_address origin);
      let peer =
        Mutex.protect m (fun () ->
            match state with
              | Listening l
                when self#current generation
                     && Option.fold ~none:true
                          ~some:(fun n -> List.length l.peers < n)
                          kind.client_limit ->
                  let peer = self#make_peer ~created:false socket origin in
                  l.peers <- peer :: l.peers;
                  Some peer
              | _ -> None)
      in
      match peer with
        | None -> self#close_socket ~created:false socket
        | Some peer -> Srt_lib.Events.post events (fun () -> self#accepted peer)

    method private accepted peer =
      match
        kind.setup_socket peer.handle.socket;
        peer.created <- true;
        self#socket_event "incoming" peer.handle.socket
      with
        | () -> self#peer_connected peer
        | exception exn ->
            (log ())#important "Setting up new connection failed: %s"
              (Printexc.to_string exn);
            self#end_peer peer ~clean:false
  end
