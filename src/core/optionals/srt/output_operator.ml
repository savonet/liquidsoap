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

type stream = {
  handle : Connection.peer_handle;
  payload_size : int;
  m : Mutex.t;
  ready : Condition.t;
  payloads : string Queue.t;
  mutable queued : int;
  pending : Buffer.t;
  mutable joined : bool;
  mutable closing : [ `Open | `Drain of unit -> unit | `Abort | `Stopped ];
}

let create_stream ~payload_size handle =
  {
    handle;
    payload_size;
    m = Mutex.create ();
    ready = Condition.create ();
    payloads = Queue.create ();
    queued = 0;
    pending = Buffer.create payload_size;
    joined = false;
    closing = `Open;
  }

(** Protects the process's memory against a peer that reads too slowly. *)
let peer_backlog_limit = 8 * 1024 * 1024

let enqueue stream payload =
  Queue.push payload stream.payloads;
  stream.queued <- stream.queued + String.length payload;
  Condition.signal stream.ready

(** Under [stream.m]. [false] once the stream is past [peer_backlog_limit]. *)
let push stream data =
  Strings.iter (Buffer.add_substring stream.pending) data;
  let size = stream.payload_size in
  let length = Buffer.length stream.pending in
  if length >= size then (
    let contents = Buffer.contents stream.pending in
    let full = length / size * size in
    for i = 0 to (length / size) - 1 do
      enqueue stream (String.sub contents (i * size) size)
    done;
    Buffer.clear stream.pending;
    Buffer.add_substring stream.pending contents full (length - full));
  stream.queued <= peer_backlog_limit

let flush stream =
  if Buffer.length stream.pending > 0 then (
    enqueue stream (Buffer.contents stream.pending);
    Buffer.clear stream.pending)

let shutdown_drain = 2.

(** The longest a clean end may take (spec/output.md §6), [None] for no limit.
*)
let drain_limit ~write_timeout ~shutting_down =
  match write_timeout with
    | Some t -> Some t
    | None -> if shutting_down then Some shutdown_drain else None

let once k =
  let called = Atomic.make false in
  fun () -> if not (Atomic.exchange called true) then k ()

(* The receiver drops what it holds back for timestamp-based delivery when the
   close arrives first, so the close also waits for the peer's latency. The
   drain limit closes the socket, which ends the wait. *)
let wait_sent socket =
  try
    (* ponytail: libsrt has no wait for an empty send buffer; polls its statistics. *)
    while (Srt.Stats.bstats socket).Srt.Stats.pktSndBuf > 0 do
      Thread.delay 0.01
    done;
    Thread.delay (float (Srt.getsockflag socket Srt.peerlatency) /. 1000.)
  with _ -> ()

(* A sender blocked on a peer that stopped reading returns once the socket
   closes. *)
let abort_drain stream =
  let drain =
    Mutex.protect stream.m (fun () ->
        match stream.closing with
          | `Drain k ->
              stream.closing <- `Abort;
              Condition.signal stream.ready;
              Some k
          | _ -> None)
  in
  Option.iter (fun k -> k ()) drain

let next stream =
  Mutex.protect stream.m (fun () ->
      let rec wait () =
        match (stream.closing, Queue.take_opt stream.payloads) with
          | (`Abort | `Stopped), _ -> `Stop
          | _, Some payload ->
              stream.queued <- stream.queued - String.length payload;
              `Send payload
          | `Drain k, None -> `Drained k
          | `Open, None ->
              Condition.wait stream.ready stream.m;
              wait ()
      in
      wait ())

let rec send_loop ~(log : Log.t) stream =
  match next stream with
    | `Stop -> ()
    | `Drained k ->
        wait_sent stream.handle.socket;
        k ()
    | `Send payload -> (
        match
          Srt.sendmsg stream.handle.socket
            (Bytes.unsafe_of_string payload)
            (-1) false
        with
          | _ -> send_loop ~log stream
          | exception exn -> (
              let closing =
                Mutex.protect stream.m (fun () ->
                    let closing = stream.closing in
                    (match closing with
                      | `Open -> stream.closing <- `Stopped
                      | _ -> ());
                    closing)
              in
              match closing with
                | `Drain k -> k ()
                | `Abort | `Stopped -> ()
                | `Open ->
                    log#important "Sending to %s failed: %s"
                      (Utils.name_of_sockaddr ~show_port:true
                         stream.handle.address)
                      (Printexc.to_string exn);
                    stream.handle.fail ()))

class output ~params ~write_timeout ~max_clients ~format_val ~infallible
  ~register_telnet ~autostart source_val =
  let factory =
    match Encoder.get_factory (Lang.to_format format_val) with
      | factory -> factory ~hls:false ~pos:(Value.pos format_val)
      | exception Not_found ->
          raise
            (Error.Invalid_value
               (format_val, "Cannot get a stream encoder for that format", []))
  in
  let source = Lang.to_source source_val in
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"output.srt" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false ~name:"output.srt" source_val

    val encoder_m = Mutex.create ()
    val mutable encoder : Encoder.encoder option = None

    (** Streams of live peers, under [encoder_m]. *)
    val mutable streams : stream list = []

    val mutable connection = None
    method connection : stream Connection.connection = Option.get connection

    initializer
      connection <-
        Some
          (Common.connection
             ~log:(fun () -> self#log)
             ~name:"output.srt" ~params ~kind:self#kind ())

    method private kind =
      {
        Connection.client_limit = max_clients;
        setup_socket =
          (fun socket ->
            Srt.setsockflag socket Srt.sndtimeo
              (match write_timeout with
                | Some t -> Library.ms_of_seconds t
                | None -> -1));
        connected =
          (fun handle ->
            let stream =
              create_stream ~payload_size:params.Connection.payload_size handle
            in
            Library.spawn (fun () -> send_loop ~log:self#log stream);
            stream);
        live =
          (fun stream ->
            Mutex.protect encoder_m (fun () -> streams <- stream :: streams));
        ending = self#end_stream;
        ended = (fun _ -> ());
        shutdown_wait =
          Option.get (drain_limit ~write_timeout ~shutting_down:true);
      }

    method private stop_encoder ~deliver =
      match encoder with
        | None -> ()
        | Some enc ->
            encoder <- None;
            let final =
              try enc.Encoder.stop ()
              with exn ->
                let bt = Printexc.get_backtrace () in
                Utils.log_exception ~log:self#log ~bt
                  (Printf.sprintf "Error while stopping encoder: %s"
                     (Printexc.to_string exn));
                Strings.empty
            in
            if deliver then
              List.iter
                (fun s ->
                  if s.joined then
                    ignore (Mutex.protect s.m (fun () -> push s final)))
                streams

    (* A clean end comes only from the output ending every peer itself, so the
       first stream to end delivers the trailer to all of them. *)
    method private end_stream stream ~clean k =
      let k = once k in
      Mutex.protect encoder_m (fun () ->
          if clean then self#stop_encoder ~deliver:true;
          streams <- List.filter (fun s -> s != stream) streams;
          if streams = [] then self#stop_encoder ~deliver:false);
      (* A sender that stopped on a failure performs no drain. *)
      let drains =
        Mutex.protect stream.m (fun () ->
            let drains =
              clean && match stream.closing with `Stopped -> false | _ -> true
            in
            if drains then (
              flush stream;
              stream.closing <- `Drain k)
            else stream.closing <- `Abort;
            Condition.signal stream.ready;
            drains)
      in
      if not drains then k ()
      else
        Option.iter
          (fun limit -> Library.after limit (fun () -> abort_drain stream))
          (drain_limit ~write_timeout
             ~shutting_down:(Atomic.get Library.shutting_down))

    method private running_encoder =
      match (streams, encoder) with
        | [], _ -> None
        | _, Some enc -> Some enc
        | _, None ->
            let enc = factory self#id Frame.Metadata.Export.empty in
            encoder <- Some enc;
            List.iter (fun s -> s.joined <- true) streams;
            Some enc

    method private distribute enc data =
      let restart = enc.Encoder.restart_offset () in
      List.iter
        (fun stream ->
          let data =
            if stream.joined then data
            else (
              stream.joined <- true;
              match restart with
                | Some offset ->
                    Strings.sub data offset (Strings.length data - offset)
                | None -> Strings.append (enc.Encoder.header ()) data)
          in
          if not (Mutex.protect stream.m (fun () -> push stream data)) then
            stream.handle.fail ())
        streams

    method private encode frame =
      Mutex.protect encoder_m (fun () ->
          Option.iter
            (fun enc -> self#distribute enc (enc.Encoder.encode frame))
            self#running_encoder)

    method private encode_metadata metadata =
      Mutex.protect encoder_m (fun () ->
          Option.iter
            (fun enc -> enc.Encoder.encode_metadata metadata)
            self#running_encoder)

    method private send () = ()
    method private start = self#connection#start
    method private stop = self#connection#stop
    method private self_sync = source#cached_self_sync
  end

let max_clients_proto =
  ( "max_clients",
    Lang.nullable_t Lang.int_t,
    Some Lang.null,
    Some
      "Maximum number of clients, at least 1. `null` means no limit. Used only \
       in listener mode." )

let parse_max_clients p =
  let value = List.assoc "max_clients" p in
  match Lang.to_valued_option Lang.to_int value with
    | Some n when n < 1 ->
        raise
          (Error.Invalid_value (value, "Maximum clients must be at least 1!", []))
    | max_clients -> max_clients

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Modules.output "srt"
    (Common.proto ~mode:`Caller
    @ (max_clients_proto :: Output.proto)
    @ [
        ("", Lang.format_t return_t, None, Some "Encoding format.");
        ("", Lang.source_t return_t, None, None);
      ])
    ~return_t ~category:`Output ~meth:(Common.meth ())
    ~callbacks:(Common.callbacks () @ Start_stop.output_callbacks ())
    ~descr:"Send a SRT stream, as a caller or as a listener."
    (fun p ->
      Library.init ();
      let params, _, write_timeout = Common.parse p in
      new output
        ~params ~write_timeout ~max_clients:(parse_max_clients p)
        ~format_val:(Lang.assoc "" 1 p)
        ~infallible:(not (Lang.to_bool (List.assoc "fallible" p)))
        ~register_telnet:(Lang.to_bool (List.assoc "register_telnet" p))
        ~autostart:(Lang.to_bool (List.assoc "start" p))
        (Lang.assoc "" 2 p))
