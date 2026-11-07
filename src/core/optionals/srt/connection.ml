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

type peer_handle = {
  socket : Srt.socket;
  address : Unix.sockaddr;
  fail : unit -> unit;
}

type 'reaction kind = {
  client_limit : int option;
  setup_socket : Srt.socket -> unit;
  connected : peer_handle -> 'reaction;
  live : 'reaction -> unit;
  ending : 'reaction -> clean:bool -> (unit -> unit) -> unit;
  ended : 'reaction -> unit;
  shutdown_wait : float;
}

type 'reaction peer = {
  handle : peer_handle;
  mutable created : bool;
  mutable reaction : 'reaction option;
  mutable announced : bool;
  mutable live_since : float;
  live : bool Atomic.t;
  ended : bool Atomic.t;
}

type attempt = { mutable attempt_socket : Srt.socket option }

type 'reaction state =
  | Idle
  | Attempting of attempt
  | Waiting
  | Connected of 'reaction peer
  | Listening of { listener : Srt.socket; mutable peers : 'reaction peer list }

let is_attempting = function Attempting _ -> true | _ -> false
let is_waiting = function Waiting -> true | _ -> false

(** How long a release at shutdown waits for closes, beyond a clean end. *)
let close_wait = 1.

class virtual ['reaction] connection ~(log : unit -> Log.t) ~name ~params
  ~(kind : 'reaction kind) () =
  object (self)
    val events = Events.create name
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
        Some (Library.register_shutdown (fun () -> self#release_at_shutdown))

    method register_on_connect fn = Callbacks.register on_connect fn
    method register_on_disconnect fn = Callbacks.register on_disconnect fn
    method register_on_socket fn = Callbacks.register on_socket fn
    method wanted = Mutex.protect m (fun () -> active)

    method sockets =
      let live p = Atomic.get p.live in
      let entry p =
        (Utils.name_of_sockaddr p.handle.address, p.handle.socket)
      in
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
              && not (Atomic.get Library.shutting_down)
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
    method virtual private connect_role : int -> unit
    method virtual private retry : int -> float -> unit

    method private new_socket =
      let socket =
        Library.create_socket ~payload_size:params.payload_size
          ~messageapi:params.messageapi ()
      in
      Atomic.incr open_sockets;
      socket

    method private close_socket ~created socket =
      if created then self#socket_event "close" socket;
      Library.close_quietly socket;
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
        Events.post events (fun () -> self#finish_peer peer ~clean)

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
        Events.post events (fun () ->
            self#close_peer peer;
            Option.iter
              (fun (generation, delay) -> self#retry generation delay)
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
            Library.spawn (fun () -> Library.close_quietly socket)
        | Idle | Attempting _ | Waiting -> ()
        | Connected peer -> self#end_peer peer ~clean:true
        | Listening { listener; peers } ->
            Events.post events (fun () ->
                Poll.remove listener;
                self#close_socket ~created:true listener);
            List.iter (fun peer -> self#end_peer peer ~clean:true) peers

    method private release_at_shutdown =
      self#update;
      let deadline = Unix.gettimeofday () +. kind.shutdown_wait +. close_wait in
      (* ponytail: polls the socket count; shutdown runs once. *)
      while Atomic.get open_sockets > 0 && Unix.gettimeofday () < deadline do
        Thread.delay 0.02
      done
  end
