(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module G = Generator
module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

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

let log_handler {Srt.Log.message} =
  let message =
    Pcre.substitute ~pat:"[ \r\n]+$" ~subst:(fun _ -> "") message
  in
  log#f conf_level#get "%s" message

(** Common polling task for all srt input/output.
  * sockets entering a poll are always set to non-blocking
  * and set back to blocking when exiting. They are also always
  * removed from the poll when done. *)
module Poll = struct
  exception Empty

  type t = {
    p: Srt.Poll.t;
    m: Mutex.t;
    mutable max_read: int;
    mutable max_write: int;
    handlers: (Srt.socket, Srt.socket -> unit) Hashtbl.t;
  }

  let t =
    let p = Srt.Poll.create () in
    let m = Mutex.create () in
    let handlers = Hashtbl.create 0 in
    {p; m; max_read= 0; max_write= 0; handlers}

  let process () =
    try
      let max_read, max_write =
        Tutils.mutexify t.m
          (fun () ->
            let {max_read; max_write} = t in
            (max_read, max_write))
          ()
      in
      if max_read = 0 && max_write = 0 then raise Empty ;
      let r, w =
        Srt.Poll.wait t.p ~max_read ~max_write ~timeout:conf_timeout#get
      in
      let handlers =
        Tutils.mutexify t.m
          (fun () ->
            t.max_read <- t.max_read - List.length r ;
            t.max_write <- t.max_write - List.length w ;
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
        (fun socket ->
          Srt.setsockflag socket Srt.sndsyn true ;
          Srt.setsockflag socket Srt.rcvsyn true ;
          Srt.Poll.remove_usock t.p socket ;
          let fn = Hashtbl.find handlers socket in
          apply fn socket)
        (r @ w) ;
      0.
    with
      | Empty ->
          -1.
      | Srt.Error (`Etimeout, _) ->
          0.

  let task = Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler process

  let add_socket ~mode socket fn =
    Srt.setsockflag socket Srt.sndsyn false ;
    Srt.setsockflag socket Srt.rcvsyn false ;
    Tutils.mutexify t.m
      (fun () ->
        Hashtbl.add t.handlers socket fn ;
        Srt.Poll.add_usock t.p socket (mode :> Srt.Poll.flag) ;
        match mode with
          | `Read ->
              t.max_read <- t.max_read + 1
          | `Write ->
              t.max_write <- t.max_write + 1)
      () ;
    Duppy.Async.wake_up task
end

let () =
  Srt.startup () ;
  ignore
    (Dtools.Init.at_start (fun () ->
         if conf_log#get then Srt.Log.set_handler log_handler)) ;
  ignore (Dtools.Init.at_stop Srt.cleanup) ;
  ignore
    (Dtools.Init.make ~after:[Tutils.scheduler_shutdown_atom] (fun () ->
         Srt.Poll.release Poll.t.Poll.p))

class virtual base ~payload_size ~messageapi =
  object (self)
    val mutex = Mutex.create ()

    val mutable socket = None

    method virtual id : string

    val mutable clock = None

    method private get_clock =
      match clock with
        | Some c ->
            c
        | None ->
            let c = new Clock.clock "srt" in
            clock <- Some c ;
            c

    method private string_of_address =
      function
      | Unix.ADDR_UNIX _ ->
          assert false
      | Unix.ADDR_INET (addr, port) ->
          Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

    (* No blocking operation in prepare_socket, plz! *)
    method virtual private prepare_socket : Srt.socket -> unit

    method private get_socket =
      Tutils.mutexify mutex
        (fun () ->
          match socket with
            | Some socket ->
                socket
            | None ->
                let s = Srt.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
                Srt.setsockflag s Srt.payloadsize payload_size ;
                Srt.setsockflag s Srt.transtype `Live ;
                Srt.setsockflag s Srt.messageapi messageapi ;
                Srt.setsockflag s Srt.enforced_encryption
                  conf_enforced_encryption#get ;
                self#prepare_socket s ;
                socket <- Some s ;
                s)
        ()

    method private close_socket =
      Tutils.mutexify mutex
        (fun () ->
          match socket with
            | None ->
                ()
            | Some s ->
                Srt.close s ;
                socket <- None)
        ()
  end

class input ~kind ~bind_address ~max ~payload_size ~clock_safe ~on_connect
  ~on_disconnect ~messageapi ~dump format =
  let max_ticks = Frame.master_of_seconds max in
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let generator =
    Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined
  in
  object (self)
    inherit base ~payload_size ~messageapi

    inherit Source.source ~name:"input.srt" kind as super

    val input_mutex = Mutex.create ()

    val mutable client_data = None

    val mutable decoder_data = None

    val mutable should_stop = false

    val mutable dump_chan = None

    method stype = Source.Fallible

    method seek _ = 0

    method remaining = -1

    method abort_track = Generator.add_break generator

    method is_ready =
      Tutils.mutexify input_mutex
        (fun () -> not (should_stop || client_data = None))
        ()

    method self_sync = client_data <> None

    method private log_origin s =
      try (self#log)#info "New connection from %s" (self#string_of_address s)
      with exn ->
        (self#log)#important "Error while fetching connection source: %s"
          (Printexc.to_string exn)

    method private should_stop =
      Tutils.mutexify input_mutex (fun () -> should_stop) ()

    method private prepare_socket s =
      Srt.bind s bind_address ;
      Srt.listen s 1 ;
      (self#log)#info "Setting up socket to listen at %s"
        (self#string_of_address bind_address)

    method private create_decoder socket =
      let create_decoder =
        match Decoder.get_stream_decoder format kind with
          | Some d ->
              d
          | None ->
              raise Harbor.Unknown_codec
      in
      let buf = Buffer.create payload_size in
      let tmp = Bytes.create payload_size in
      let read bytes ofs len =
        if self#should_stop then raise Done ;
        if Buffer.length buf < len then (
          let input = Srt.recvmsg socket tmp payload_size in
          if input = 0 then raise End_of_file ;
          Buffer.add_subbytes buf tmp 0 input ;
          match dump_chan with
            | Some chan ->
                output chan tmp 0 input
            | None ->
                () ) ;
        let len = min len (Buffer.length buf) in
        Buffer.blit buf 0 bytes ofs len ;
        Utils.buffer_drop buf len ;
        len
      in
      create_decoder {Decoder.read; tell= None; length= None; lseek= None}

    method private handle_client socket =
      if self#should_stop then raise Done ;
      Srt.setsockflag socket Srt.sndsyn true ;
      Srt.setsockflag socket Srt.rcvsyn true ;
      Tutils.mutexify input_mutex
        (fun () ->
          Generator.set_mode generator `Undefined ;
          client_data <- Some socket ;
          match dump with
            | Some fname ->
                dump_chan <- Some (open_out_bin fname)
            | None ->
                ())
        () ;
      on_connect ()

    method private close_client =
      on_disconnect () ;
      Tutils.mutexify input_mutex
        (fun () ->
          match client_data with
            | None ->
                ()
            | Some socket -> (
                Srt.close socket ;
                decoder_data <- None ;
                client_data <- None ;
                match dump_chan with
                  | Some chan ->
                      close_out_noerr chan ;
                      dump_chan <- None
                  | None ->
                      () ))
        () ;
      self#connect

    method private connect =
      let on_connect s =
        try
          let client, origin = Srt.accept s in
          self#log_origin origin ; self#handle_client client
        with exn ->
          (self#log)#debug "Failed to connect: %s." (Printexc.to_string exn) ;
          self#connect
      in
      if not self#should_stop then (
        self#close_socket ;
        Poll.add_socket ~mode:`Read self#get_socket on_connect )

    method private get_frame frame =
      let pos = Frame.position frame in
      try
        let socket = Utils.get_some client_data in
        let decoder =
          match decoder_data with
            | None ->
                let decoder = self#create_decoder socket in
                decoder_data <- Some decoder ;
                decoder
            | Some d ->
                d
        in
        while Generator.length generator < Lazy.force Frame.size do
          decoder.Decoder.decode generator
        done ;
        Generator.fill generator frame
      with exn ->
        (self#log)#important "Feeding failed: %s" (Printexc.to_string exn) ;
        self#close_client ;
        Frame.add_break frame pos

    method private set_clock =
      super#set_clock ;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (self#get_clock :> Clock.clock))

    method wake_up act =
      super#wake_up act ;
      Tutils.mutexify input_mutex (fun () -> should_stop <- false) () ;
      self#connect

    method sleep =
      Tutils.mutexify input_mutex (fun () -> should_stop <- true) () ;
      self#close_client ;
      super#sleep
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "input.srt" ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Input
    ~descr:"Start a SRT agent in listener mode to receive and decode a stream."
    [ ( "bind_address",
        Lang.string_t,
        Some (Lang.string "0.0.0.0"),
        Some "Address to bind on the local machine." );
      ( "port",
        Lang.int_t,
        Some (Lang.int 8000),
        Some
          "Port to bind on the local machine. The term `port` as used in SRT \
           is occasionally identical to the term `UDP port`. However SRT \
           offers more flexibility than UDP because it manages ports as its \
           own resources. For example, one port may be shared between various \
           services." );
      ( "clock_safe",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Force the use of a decicated clock." );
      ( "on_connect",
        Lang.fun_t [(false, "", Lang.unit_t)] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Function to execute when a source is connected." );
      ( "on_disconnect",
        Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Function to excecute when a stream is disconnected" );
      ( "max",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." );
      ("payload_size", Lang.int_t, Some (Lang.int 1316), Some "Payload size.");
      ("messageapi", Lang.bool_t, Some (Lang.bool true), Some "Use message api");
      ( "dump",
        Lang.string_t,
        Some (Lang.string ""),
        Some
          "Dump received data to the given file for debugging. Unused is empty."
      );
      ( "content_type",
        Lang.string_t,
        Some (Lang.string "application/ffmpeg"),
        Some
          "Content-Type (mime type) used to find a decoder for the input \
           stream." ) ]
    (fun p kind ->
      let bind_address = Lang.to_string (List.assoc "bind_address" p) in
      let bind_address =
        try Unix.inet_addr_of_string bind_address
        with exn ->
          raise
            (Lang_errors.Invalid_value
               ( List.assoc "bind_address" p,
                 Printf.sprintf "Invalid address: %s" (Printexc.to_string exn)
               ))
      in
      let port = Lang.to_int (List.assoc "port" p) in
      let bind_address = Unix.ADDR_INET (bind_address, port) in
      let dump =
        match Lang.to_string (List.assoc "dump" p) with
          | s when s = "" ->
              None
          | s ->
              Some s
      in
      let max = Lang.to_float (List.assoc "max" p) in
      let messageapi = Lang.to_bool (List.assoc "messageapi" p) in
      let payload_size = Lang.to_int (List.assoc "payload_size" p) in
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let on_connect () =
        ignore (Lang.apply ~t:Lang.unit_t (List.assoc "on_connect" p) [])
      in
      let on_disconnect () =
        ignore (Lang.apply ~t:Lang.unit_t (List.assoc "on_disconnect" p) [])
      in
      let format = Lang.to_string (List.assoc "content_type" p) in
      ( match Decoder.get_stream_decoder format kind with
        | None ->
            raise
              (Lang_errors.Invalid_value
                 ( List.assoc "content_type" p,
                   "Couldn't find a decoder for this format" ))
        | _ ->
            () ) ;
      ( new input
          ~kind ~bind_address ~payload_size ~clock_safe ~on_connect
          ~on_disconnect ~messageapi ~max ~dump format
        :> Source.source ))

class output ~kind ~payload_size ~messageapi ~on_start ~on_stop ~infallible
  ~autostart ~clock_safe ~port ~hostname ~encoder_factory source =
  object (self)
    inherit base ~payload_size ~messageapi

    inherit
      Output.encoded
        ~output_kind:"srt" ~content_kind:kind ~on_start ~on_stop ~infallible
          ~autostart ~name:"output.srt" source as super

    val output_mutex = Mutex.create ()

    val buffer = Strings.Mutable.empty ()

    val tmp = Bytes.create payload_size

    val mutable encoder = None

    val mutable connect_task = None

    val mutable state = `Idle

    method self_sync = state = `Started

    method private is s = Tutils.mutexify output_mutex (fun () -> state = s) ()

    method private prepare_socket socket =
      Srt.setsockflag socket Srt.sndsyn true ;
      Srt.setsockflag socket Srt.rcvsyn true

    method private send_chunk =
      let socket = self#get_socket in
      try
        let send data =
          if messageapi then Srt.sendmsg socket data (-1) false
          else Srt.send socket data
        in
        Tutils.mutexify output_mutex
          (fun () ->
            Strings.Mutable.blit buffer 0 tmp 0 payload_size ;
            Strings.Mutable.drop buffer payload_size)
          () ;
        let rec f = function
          | pos when pos < payload_size ->
              let ret = send (Bytes.sub tmp pos (payload_size - pos)) in
              f (pos + ret)
          | _ ->
              ()
        in
        f 0
      with exn ->
        (self#log)#important "Error while send client data: %s"
          (Printexc.to_string exn) ;
        self#clear_encoder ;
        self#close_socket ;
        if not (self#is `Stopped) then self#start_connect_task

    method private send_chunks =
      let len =
        Tutils.mutexify output_mutex (fun () -> Strings.Mutable.length buffer)
      in
      while payload_size <= len () do
        self#send_chunk
      done

    method private get_encoder =
      Tutils.mutexify output_mutex
        (fun () ->
          match encoder with
            | Some enc ->
                enc
            | None ->
                let enc = encoder_factory self#id Meta_format.empty_metadata in
                encoder <- Some enc ;
                enc)
        ()

    method private clear_encoder =
      Tutils.mutexify output_mutex
        (fun () ->
          ignore (Strings.Mutable.flush buffer) ;
          encoder <- None)
        ()

    method private connect_fn () =
      let socket = self#get_socket in
      Tutils.mutexify output_mutex (fun () -> state <- `Connecting) () ;
      try
        let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
        let sockaddr = Unix.ADDR_INET (ipaddr, port) in
        (self#log)#important "Connecting to srt://%s:%d.." hostname port ;
        Srt.connect socket sockaddr ;
        Tutils.mutexify output_mutex (fun () -> state <- `Connected) () ;
        (self#log)#important "Output connected!" ;
        -1.
      with Srt.Error (_, _) as exn ->
        (self#log)#important "Connect failed: %s" (Printexc.to_string exn) ;
        self#clear_encoder ;
        self#close_socket ;
        if not (self#is `Stopped) then 0. else -1.

    method private start_connect_task =
      match connect_task with
        | Some t ->
            Duppy.Async.wake_up t
        | None ->
            let t =
              Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler
                self#connect_fn
            in
            connect_task <- Some t ;
            Duppy.Async.wake_up t

    method private stop_connect_task =
      match connect_task with
        | None ->
            ()
        | Some t ->
            Duppy.Async.stop t ;
            connect_task <- None

    method private set_clock =
      super#set_clock ;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (self#get_clock :> Clock.clock))

    method private output_start =
      Tutils.mutexify output_mutex (fun () -> state <- `Started) () ;
      self#start_connect_task

    method private output_reset = self#output_start ; self#output_stop

    method private output_stop =
      Tutils.mutexify output_mutex (fun () -> state <- `Stopped) () ;
      self#stop_connect_task

    method private encode frame ofs len =
      if self#is `Connected then self#get_encoder.Encoder.encode frame ofs len
      else Strings.empty

    method private insert_metadata m =
      if self#is `Connected then self#get_encoder.Encoder.insert_metadata m

    method private send data =
      if self#is `Connected then (
        Tutils.mutexify output_mutex
          (Strings.Mutable.append_strings buffer)
          data ;
        self#send_chunks )
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "output.srt" ~active:true ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Output ~descr:"Send a SRT stream to a distant host."
    ( Output.proto
    @ [ ( "host",
          Lang.string_t,
          Some (Lang.string "localhost"),
          Some "Address to connect to." );
        ( "port",
          Lang.int_t,
          Some (Lang.int 8000),
          Some
            "Port to bind on the local machine. The term `port` as used in \
             SRT is occasionally identical to the term `UDP port`. However \
             SRT offers more flexibility than UDP because it manages ports as \
             its own resources. For example, one port may be shared between \
             various services." );
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of a decicated clock." );
        ("payload_size", Lang.int_t, Some (Lang.int 1316), Some "Payload size.");
        ( "messageapi",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Use message api" );
        ("", Lang.format_t kind, None, Some "Encoding format.");
        ("", Lang.source_t kind, None, None) ] )
    (fun p kind ->
      let hostname = Lang.to_string (List.assoc "host" p) in
      let port = Lang.to_int (List.assoc "port" p) in
      let messageapi = Lang.to_bool (List.assoc "messageapi" p) in
      let payload_size = Lang.to_int (List.assoc "payload_size" p) in
      let source = Lang.assoc "" 2 p in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let encoder_factory =
        let fmt = Lang.assoc "" 1 p in
        try Encoder.get_factory (Lang.to_format fmt)
        with Not_found ->
          raise
            (Lang_errors.Invalid_value
               (fmt, "Cannot get a stream encoder for that format"))
      in
      ( new output
          ~kind ~hostname ~port ~payload_size ~autostart ~on_start ~on_stop
          ~infallible ~messageapi ~clock_safe ~encoder_factory source
        :> Source.source ))
