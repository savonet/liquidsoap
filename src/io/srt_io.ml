(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

module G = Generator
module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

let mode_of_value v =
  match Lang.to_string v with
    | "listener" -> `Listener
    | "caller" -> `Caller
    | _ -> raise (Lang_errors.Invalid_value (v, "Invalid mode!"))

let string_of_mode = function `Listener -> "listener" | `Caller -> "caller"

let common_options ~mode =
  [
    ( "mode",
      Lang.string_t,
      Some (Lang.string (string_of_mode mode)),
      Some
        "Mode to operate on. One of: `\"listener\"` (waits for connection to \
         come in) or `\"caller\"` (initiate connection to a remote server)" );
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
    ("payload_size", Lang.int_t, Some (Lang.int 1316), Some "Payload size.");
    ("messageapi", Lang.bool_t, Some (Lang.bool true), Some "Use message api");
    ( "on_connect",
      Lang.fun_t [(false, "", Lang.unit_t)] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to execute when connected." );
    ( "on_disconnect",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to excecute when disconnected" );
  ]

let parse_common_options p =
  let bind_address = Lang.to_string (List.assoc "bind_address" p) in
  let bind_address =
    try Unix.inet_addr_of_string bind_address
    with exn ->
      raise
        (Lang_errors.Invalid_value
           ( List.assoc "bind_address" p,
             Printf.sprintf "Invalid address: %s" (Printexc.to_string exn) ))
  in
  let port = Lang.to_int (List.assoc "port" p) in
  let bind_address = Unix.ADDR_INET (bind_address, port) in
  let on_connect = List.assoc "on_connect" p in
  let on_disconnect = List.assoc "on_disconnect" p in
  ( mode_of_value (List.assoc "mode" p),
    Lang.to_string (List.assoc "host" p),
    Lang.to_int (List.assoc "port" p),
    bind_address,
    Lang.to_int (List.assoc "payload_size" p),
    Lang.to_bool (List.assoc "messageapi" p),
    ref (fun () -> ignore (Lang.apply on_connect [])),
    ref (fun () -> ignore (Lang.apply on_disconnect [])) )

let conf_srt =
  Dtools.Conf.void ~p:(Configure.conf#plug "srt") "SRT configuration"

let conf_log =
  Dtools.Conf.bool ~p:(conf_srt#plug "log") ~d:true
    "Route srt logs through liquidsoap's logs"

let conf_level = Dtools.Conf.int ~p:(conf_log#plug "level") ~d:5 "Level"
let conf_poll = Dtools.Conf.void ~p:(conf_srt#plug "poll") "Poll configuration"

let conf_timeout =
  Dtools.Conf.int ~p:(conf_poll#plug "timeout") ~d:100
    "Timeout for polling loop, in ms"

let conf_enforced_encryption =
  Dtools.Conf.bool
    ~p:(conf_srt#plug "enforced_encryption")
    ~d:true
    "Enforce consistent encryption settings on both end of any connection."

let log = Log.make ["srt"]

let log_handler { Srt.Log.message } =
  let message = Pcre.substitute ~pat:"[ \r\n]+$" ~subst:(fun _ -> "") message in
  log#f conf_level#get "%s" message

(** Common polling task for all srt input/output.
  * sockets entering a poll are always set to non-blocking
  * and set back to blocking when exiting. They are also always
  * removed from the poll when done. *)
module Poll = struct
  exception Empty

  type t = {
    p : Srt.Poll.t;
    m : Mutex.t;
    mutable max_fds : int;
    handlers : (Srt.socket, Srt.Poll.flag * (Srt.socket -> unit)) Hashtbl.t;
  }

  let t =
    let p = Srt.Poll.create () in
    let m = Mutex.create () in
    let handlers = Hashtbl.create 0 in
    { p; m; max_fds = 0; handlers }

  let process () =
    try
      let max_fds = Tutils.mutexify t.m (fun () -> t.max_fds) () in
      if max_fds = 0 then raise Empty;
      let events = Srt.Poll.uwait t.p ~max_fds ~timeout:conf_timeout#get in
      let handlers =
        Tutils.mutexify t.m
          (fun () ->
            t.max_fds <- t.max_fds - List.length events;
            t.handlers)
          ()
      in
      let apply fn s =
        try fn s
        with exn ->
          log#important "Error while execiting asynchronous callback: %s"
            (Printexc.to_string exn)
      in
      List.iter
        (fun { Srt.Poll.fd; events } ->
          Srt.setsockflag fd Srt.sndsyn true;
          Srt.setsockflag fd Srt.rcvsyn true;
          Srt.Poll.remove_usock t.p fd;
          let event, fn = Hashtbl.find handlers fd in
          if List.mem event events then apply fn fd)
        events;
      0.
    with
      | Empty -> -1.
      | Srt.Error (`Etimeout, _) -> 0.
      | exn ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log ~bt
            (Printf.sprintf "Error while processing SRT socket pool: %s"
               (Printexc.to_string exn));
          -1.

  let task = Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler process

  let add_socket ~mode socket fn =
    Srt.setsockflag socket Srt.sndsyn false;
    Srt.setsockflag socket Srt.rcvsyn false;
    Tutils.mutexify t.m
      (fun () ->
        Hashtbl.add t.handlers socket (mode, fn);
        Srt.Poll.add_usock t.p socket (mode :> Srt.Poll.flag);
        t.max_fds <- t.max_fds + 1)
      ();
    Duppy.Async.wake_up task
end

let () =
  Srt.startup ();
  Lifecycle.before_start (fun () ->
      if conf_log#get then Srt.Log.set_handler log_handler);
  Lifecycle.after_scheduler_shutdown (fun () ->
      Srt.Poll.release Poll.t.Poll.p;
      Srt.cleanup ())

let string_of_address = function
  | Unix.ADDR_UNIX _ -> assert false
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let mk_socket ~payload_size ~messageapi () =
  let s = Srt.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Srt.setsockflag s Srt.payloadsize payload_size;
  Srt.setsockflag s Srt.transtype `Live;
  Srt.setsockflag s Srt.messageapi messageapi;
  Srt.setsockflag s Srt.enforced_encryption conf_enforced_encryption#get;
  s

let close_socket s = Srt.close s

class virtual base =
  let m = Mutex.create () in
  object (self)
    method virtual id : string

    val mutable clock = None

    method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b = Tutils.mutexify m

    val mutable should_stop = false

    method private should_stop = self#mutexify (fun () -> should_stop) ()

    method private set_should_stop = self#mutexify (fun b -> should_stop <- b)
  end

class virtual networking_agent =
  object
    method virtual private connect : unit

    method virtual private disconnect : unit

    method virtual private is_connected : bool

    method virtual private get_socket : Srt.socket
  end

class virtual caller ~payload_size ~messageapi ~hostname ~port ~on_connect
  ~on_disconnect =
  object (self)
    inherit networking_agent

    method virtual should_stop : bool

    val mutable connect_task = None

    val mutable task_should_stop = false

    val mutable socket = None

    method private get_socket =
      self#mutexify
        (fun () ->
          match socket with Some s -> s | None -> raise Not_connected)
        ()

    method virtual private log : Log.t

    method virtual private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

    method private is_connected = self#mutexify (fun () -> socket <> None) ()

    method private connect_fn () =
      self#mutexify
        (fun () ->
          try
            let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
            let sockaddr = Unix.ADDR_INET (ipaddr, port) in
            self#log#important "Connecting to srt://%s:%d.." hostname port;
            ignore (Option.map close_socket socket);
            let s = mk_socket ~payload_size ~messageapi () in
            Srt.setsockflag s Srt.sndsyn true;
            Srt.setsockflag s Srt.rcvsyn true;
            Srt.connect s sockaddr;
            socket <- Some s;
            self#log#important "Client connected!";
            !on_connect ();
            -1.
          with exn ->
            self#log#important "Connect failed: %s" (Printexc.to_string exn);
            !on_disconnect ();
            if not task_should_stop then 0. else -1.)
        ()

    method private connect =
      self#mutexify
        (fun () ->
          task_should_stop <- false;
          match connect_task with
            | Some t -> Duppy.Async.wake_up t
            | None ->
                let t =
                  Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler
                    self#connect_fn
                in
                connect_task <- Some t;
                Duppy.Async.wake_up t)
        ()

    method private disconnect =
      self#mutexify
        (fun () ->
          ignore (Option.map close_socket socket);
          socket <- None;
          task_should_stop <- true;
          match connect_task with
            | None -> ()
            | Some t ->
                Duppy.Async.stop t;
                connect_task <- None)
        ()
  end

class virtual listener ~payload_size ~messageapi ~bind_address ~on_connect
  ~on_disconnect =
  object (self)
    inherit networking_agent

    val mutable client_data = None

    method virtual log : Log.t

    method virtual should_stop : bool

    method virtual mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

    val mutable listening_socket = None

    method private is_connected =
      self#mutexify (fun () -> client_data <> None) ()

    method private get_socket =
      self#mutexify
        (fun () ->
          match client_data with Some s -> s | None -> raise Not_connected)
        ()

    method private log_origin s =
      try self#log#info "New connection from %s" (string_of_address s)
      with exn ->
        self#log#important "Error while fetching connection source: %s"
          (Printexc.to_string exn)

    method private handle_client socket =
      if self#should_stop then raise Done;
      self#mutexify
        (fun () ->
          client_data <- Some socket;
          !on_connect ())
        ()

    method private connect =
      let on_connect s =
        try
          let client, origin = Srt.accept s in
          self#log_origin origin;
          Srt.setsockflag client Srt.sndsyn true;
          Srt.setsockflag client Srt.rcvsyn true;
          self#handle_client client
        with exn ->
          self#log#debug "Failed to connect: %s." (Printexc.to_string exn);
          self#connect
      in
      if not self#should_stop then
        self#mutexify
          (fun () ->
            assert (listening_socket = None);
            let s = mk_socket ~payload_size ~messageapi () in
            Srt.bind s bind_address;
            Srt.listen s 1;
            Srt.setsockflag s Srt.rcvsyn true;
            self#log#info "Setting up socket to listen at %s"
              (string_of_address bind_address);
            listening_socket <- Some s;
            Poll.add_socket ~mode:`Read s on_connect)
          ()

    method private disconnect =
      self#mutexify
        (fun () ->
          ignore (Option.map (fun socket -> close_socket socket) client_data);
          client_data <- None;
          ignore (Option.map close_socket listening_socket);
          listening_socket <- None;
          !on_disconnect ())
        ()
  end

class virtual input_base ~kind ~max ~log_overfull ~clock_safe ~on_connect
  ~on_disconnect ~payload_size ~dump format =
  let max_ticks = Frame.master_of_seconds max in
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let generator =
    Generator.create ~log ~log_overfull ~overfull:(`Drop_old max_ticks)
      `Undefined
  in
  object (self)
    inherit networking_agent

    inherit base

    inherit Source.source ~name:"input.srt" kind as super

    val mutable decoder_data = None

    val mutable dump_chan = None

    initializer
    log_ref := self#log#info "%s";
    let on_connect_cur = !on_connect in
    (on_connect :=
       fun () ->
         Generator.set_mode generator `Undefined;
         ( match dump with
           | Some fname -> dump_chan <- Some (open_out_bin fname)
           | None -> () );
         on_connect_cur ());
    let on_disconnect_cur = !on_disconnect in
    on_disconnect :=
      fun () ->
        decoder_data <- None;
        ( match dump_chan with
          | Some chan ->
              close_out_noerr chan;
              dump_chan <- None
          | None -> () );
        on_disconnect_cur ()

    method stype = Source.Fallible

    method seek _ = 0

    method remaining = -1

    method abort_track = Generator.add_break generator

    method is_ready = (not self#should_stop) && self#is_connected

    method self_sync = self#is_connected

    method private create_decoder socket =
      let create_decoder =
        match Decoder.get_stream_decoder ~ctype:self#ctype format with
          | Some d -> d
          | None -> raise Harbor.Unknown_codec
      in
      let buf = Buffer.create payload_size in
      let tmp = Bytes.create payload_size in
      let read bytes ofs len =
        if self#should_stop then raise Done;
        if Buffer.length buf < len then (
          let input = Srt.recvmsg socket tmp payload_size in
          if input = 0 then raise End_of_file;
          Buffer.add_subbytes buf tmp 0 input;
          match dump_chan with
            | Some chan -> output chan tmp 0 input
            | None -> () );
        let len = min len (Buffer.length buf) in
        Buffer.blit buf 0 bytes ofs len;
        Utils.buffer_drop buf len;
        len
      in
      create_decoder { Decoder.read; tell = None; length = None; lseek = None }

    method private get_frame frame =
      let pos = Frame.position frame in
      try
        let socket = self#get_socket in
        let decoder =
          match decoder_data with
            | None ->
                let decoder = self#create_decoder socket in
                decoder_data <- Some decoder;
                decoder
            | Some d -> d
        in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while Generator.length generator < Lazy.force Frame.size do
          decoder.Decoder.decode buffer
        done;
        Generator.fill generator frame
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Feeding failed: %s" (Printexc.to_string exn));
        self#disconnect;
        if not self#should_stop then self#connect;
        Frame.add_break frame pos

    method private get_clock =
      match clock with
        | Some c -> c
        | None ->
            let c = new Clock.clock "srt" in
            clock <- Some c;
            c

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (self#get_clock :> Clock.clock))

    method wake_up act =
      super#wake_up act;
      self#set_should_stop false;
      self#connect

    method sleep =
      self#set_should_stop true;
      self#disconnect;
      super#sleep
  end

class input_listener ~bind_address ~kind ~max ~log_overfull ~payload_size
  ~clock_safe ~on_connect ~on_disconnect ~messageapi ~dump format =
  object
    inherit
      input_base
        ~kind ~max ~log_overfull ~payload_size ~clock_safe ~on_connect
          ~on_disconnect ~dump format

    inherit
      listener
        ~bind_address ~payload_size ~messageapi ~on_connect ~on_disconnect
  end

class input_caller ~hostname ~port ~kind ~max ~log_overfull ~payload_size
  ~clock_safe ~on_connect ~on_disconnect ~messageapi ~dump format =
  object
    inherit
      input_base
        ~kind ~max ~log_overfull ~payload_size ~clock_safe ~on_connect
          ~on_disconnect ~dump format

    inherit
      caller
        ~hostname ~port ~payload_size ~messageapi ~on_connect ~on_disconnect
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "input.srt" ~return_t ~category:Lang.Input
    ~descr:"Receive a SRT stream from a distant agent."
    ( common_options ~mode:`Listener
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of a decicated clock." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum duration of the buffered data." );
        ( "log_overfull",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Log when the source's buffer is overfull." );
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
      ] )
    (fun p ->
      let ( mode,
            hostname,
            port,
            bind_address,
            payload_size,
            messageapi,
            on_connect,
            on_disconnect ) =
        parse_common_options p
      in
      let dump =
        match Lang.to_string (List.assoc "dump" p) with
          | s when s = "" -> None
          | s -> Some s
      in
      let max = Lang.to_float (List.assoc "max" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let format = Lang.to_string (List.assoc "content_type" p) in
      match mode with
        | `Listener ->
            ( new input_listener
                ~kind ~bind_address ~payload_size ~clock_safe ~on_connect
                ~on_disconnect ~messageapi ~max ~log_overfull ~dump format
              :> Source.source )
        | `Caller ->
            ( new input_caller
                ~kind ~hostname ~port ~payload_size ~clock_safe ~on_connect
                ~on_disconnect ~messageapi ~max ~log_overfull ~dump format
              :> Source.source ))

class virtual output_base ~kind ~payload_size ~messageapi ~on_start ~on_stop
  ~infallible ~autostart ~on_connect:_ ~on_disconnect ~encoder_factory source =
  let buffer = Strings.Mutable.empty () in
  let tmp = Bytes.create payload_size in
  object (self)
    inherit networking_agent

    inherit base

    inherit
      Output.encoded
        ~output_kind:"srt" ~content_kind:kind ~on_start ~on_stop ~infallible
          ~autostart ~name:"output.srt" source

    val mutable encoder = None

    initializer
    let on_disconnect_cur = !on_disconnect in
    on_disconnect :=
      fun () ->
        ignore (Strings.Mutable.flush buffer);
        encoder <- None;
        on_disconnect_cur ()

    method private send_chunk =
      let socket = self#get_socket in
      let send data =
        if messageapi then Srt.sendmsg socket data (-1) false
        else Srt.send socket data
      in
      self#mutexify
        (fun () ->
          Strings.Mutable.blit buffer 0 tmp 0 payload_size;
          Strings.Mutable.drop buffer payload_size)
        ();
      let rec f = function
        | pos when pos < payload_size ->
            let ret = send (Bytes.sub tmp pos (payload_size - pos)) in
            f (pos + ret)
        | _ -> ()
      in
      f 0

    method private send_chunks =
      try
        let len = self#mutexify (fun () -> Strings.Mutable.length buffer) in
        while payload_size <= len () do
          self#send_chunk
        done
      with exn ->
        self#log#important "Error while sending client data: %s"
          (Printexc.to_string exn);
        self#disconnect;
        if not self#should_stop then self#connect

    method private get_encoder =
      self#mutexify
        (fun () ->
          match encoder with
            | Some enc -> enc
            | None ->
                let enc = encoder_factory self#id Meta_format.empty_metadata in
                encoder <- Some enc;
                enc)
        ()

    method private output_start =
      self#mutexify (fun () -> should_stop <- false) ();
      self#connect

    method private output_reset =
      self#output_start;
      self#output_stop

    method private output_stop =
      self#mutexify (fun () -> should_stop <- true) ();
      self#disconnect

    method private encode frame ofs len =
      if self#is_connected then self#get_encoder.Encoder.encode frame ofs len
      else Strings.empty

    method private insert_metadata m =
      if self#is_connected then self#get_encoder.Encoder.insert_metadata m

    method private send data =
      if self#is_connected then (
        self#mutexify (Strings.Mutable.append_strings buffer) data;
        self#send_chunks )
  end

class output_caller ~kind ~payload_size ~messageapi ~on_start ~on_stop
  ~infallible ~autostart ~on_connect ~on_disconnect ~port ~hostname
  ~encoder_factory source =
  object
    inherit
      output_base
        ~kind ~payload_size ~messageapi ~on_start ~on_stop ~infallible
          ~autostart ~on_connect ~on_disconnect ~encoder_factory source

    inherit
      caller
        ~hostname ~port ~payload_size ~messageapi ~on_connect ~on_disconnect
  end

class output_listener ~kind ~payload_size ~messageapi ~on_start ~on_stop
  ~infallible ~autostart ~on_connect ~on_disconnect ~bind_address
  ~encoder_factory source =
  object
    inherit
      output_base
        ~kind ~payload_size ~messageapi ~on_start ~on_stop ~infallible
          ~autostart ~on_connect ~on_disconnect ~encoder_factory source

    inherit
      listener
        ~bind_address ~payload_size ~messageapi ~on_connect ~on_disconnect
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "output.srt" ~active:true ~return_t ~category:Lang.Output
    ~descr:"Send a SRT stream to a distant agent."
    ( Output.proto
    @ common_options ~mode:`Caller
    @ [
        ("", Lang.format_t return_t, None, Some "Encoding format.");
        ("", Lang.source_t return_t, None, None);
      ] )
    (fun p ->
      let ( mode,
            hostname,
            port,
            bind_address,
            payload_size,
            messageapi,
            on_connect,
            on_disconnect ) =
        parse_common_options p
      in
      let source = Lang.assoc "" 2 p in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let format_val = Lang.assoc "" 1 p in
      let format = Lang.to_format format_val in
      let kind = Encoder.kind_of_format format in
      let encoder_factory =
        try Encoder.get_factory format
        with Not_found ->
          raise
            (Lang_errors.Invalid_value
               (format_val, "Cannot get a stream encoder for that format"))
      in
      match mode with
        | `Caller ->
            ( new output_caller
                ~kind ~hostname ~port ~payload_size ~autostart ~on_start
                ~on_stop ~infallible ~messageapi ~encoder_factory ~on_connect
                ~on_disconnect source
              :> Source.source )
        | `Listener ->
            ( new output_listener
                ~kind ~bind_address ~payload_size ~autostart ~on_start ~on_stop
                ~infallible ~messageapi ~encoder_factory ~on_connect
                ~on_disconnect source
              :> Source.source ))
