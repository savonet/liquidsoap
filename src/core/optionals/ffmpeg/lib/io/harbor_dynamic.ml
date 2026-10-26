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

module F = Liquidsoap_ffmpeg_formats.Encoder_format
module Connection = Harbor_input.Connection

let log = Log.make ["input"; "harbor"; "dynamic"]

(* io.md §10.2. *)
type state =
  [ `Opening
  | `Offered of Avutil.input Avutil.container * Stream_description.t
  | `Feeding of Avutil.input Avutil.container * Decode_loop.t
  | `Ended ]

type cause = [ `End_of_input | `Stopped | `Fault of string | `Deadline ]

exception Read_aborted
exception Read_timeout

(* io.md §10.3 step 1. *)
let format_of_mime mime =
  match String.lowercase_ascii (Harbor_input.extract_mime mime) with
    | "audio/mpeg" | "audio/mp3" -> Some "mp3"
    | "audio/ogg" | "application/ogg" -> Some "ogg"
    | "audio/flac" -> Some "flac"
    | "audio/aac" | "audio/aacp" -> Some "aac"
    | "video/x-matroska" | "audio/x-matroska" -> Some "matroska"
    | "video/webm" | "audio/webm" -> Some "webm"
    | "video/mp4" | "audio/mp4" -> Some "mp4"
    | "video/x-flv" -> Some "flv"
    | "video/mpeg" -> Some "mpegts"
    | _ -> None

let mime_of_format name =
  F.mime
    {
      F.format = Some name;
      mime_type = None;
      output = `Stream;
      streams = [];
      interleaved = `Default;
      metadata = [];
      opts = [];
    }

(* stream-description.md §7. *)
let stream_value (stream : Stream_description.stream) =
  let field =
    ("field", Lang.string (Frame.Fields.string_of_field stream.field))
  in
  let fields =
    match stream.media with
      | `Audio p ->
          let layout = Avcodec.Audio.get_channel_layout p in
          [
            ("type", Lang.string "audio");
            ( "codec",
              Lang.string
                (Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id p)) );
            ("samplerate", Lang.int (Avcodec.Audio.get_sample_rate p));
            ("channels", Lang.int (Avutil.Channel_layout.get_nb_channels layout));
            ( "channel_layout",
              Lang.string (Avutil.Channel_layout.get_description layout) );
          ]
      | `Video (p, frame_rate) ->
          [
            ("type", Lang.string "video");
            ( "codec",
              Lang.string
                (Avcodec.Video.string_of_id (Avcodec.Video.get_params_id p)) );
            ("width", Lang.int (Avcodec.Video.get_width p));
            ("height", Lang.int (Avcodec.Video.get_height p));
            ( "pixel_format",
              Lang.string
                (match Avcodec.Video.get_pixel_format p with
                  | Some f ->
                      Option.value ~default:"unknown"
                        (Avutil.Pixel_format.to_string f)
                  | None -> "unknown") );
            ( "frame_rate",
              match frame_rate with
                | Some { Avutil.num; den } when den > 0 ->
                    Lang.float (float num /. float den)
                | _ -> Lang.null );
          ]
      | `Subtitle p ->
          [
            ("type", Lang.string "subtitle");
            ( "codec",
              Lang.string
                (Avcodec.Subtitle.string_of_id
                   (Avcodec.Subtitle.get_params_id p)) );
          ]
      | `Data p ->
          [
            ("type", Lang.string "data");
            ( "codec",
              Lang.string
                (Avcodec.Unknown.string_of_id (Avcodec.Unknown.get_params_id p))
            );
          ]
  in
  Lang.record (field :: fields)

(* io.md §10.5. *)
let copy_encoder ~(description : Stream_description.t) ~mime_type =
  Lang.val_fun
    [
      ("", "format", Some Lang.null); ("options", "options", Some (Lang.list []));
    ]
    (fun p ->
      let format =
        match Lang.to_valued_option Lang.to_string (List.assoc "format" p) with
          | Some f -> Some f
          | None -> description.format
      in
      let opts =
        List.map
          (fun v ->
            let k, v = Lang.to_product v in
            (Lang.to_string k, `String (Lang.to_string v)))
          (Lang.to_list (List.assoc "options" p))
      in
      let streams =
        List.filter_map
          (fun (stream : Stream_description.stream) ->
            match stream.media with
              | `Data _ -> None
              | _ ->
                  Some
                    ( Frame.Fields.string_of_field stream.field,
                      `Copy `Wait_for_keyframe ))
          description.streams
      in
      Lang_encoder.L.format
        (Encoder.Ffmpeg
           {
             F.format;
             mime_type;
             output = `Stream;
             streams;
             interleaved = `Default;
             metadata = [];
             opts;
           }))

let should_shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"input.harbor.dynamic shutdown"
    (fun () -> Atomic.set should_shutdown true)

(* One source client: its source and its life (io.md §10.2). *)
class client ~(args : Lang.regexp Harbor_input.parse_result) ~request_uri ~id
  ~mime ~connection ~(client_read : Harbor.socket -> bytes -> int -> int -> int)
  ~(on_end : client -> unit) =
  object (self)
    inherit
      Harbor_input.http_input_base
        ~dumpfile:None ~logfile:None ~bufferize:args.bufferize ~max:args.max
          ~replay_meta:args.replay_meta ~login:args.login ~debug:args.debug
          ~timeout:args.timeout () as super

    val state : state Atomic.t = Atomic.make `Opening
    val mutable connected = false
    val mutable peer_closed = false
    val mutable headers = []

    initializer
      self#set_id id;
      Atomic.set relay (Some connection);
      Atomic.set mime_type (Some mime);
      self#on_wake_up (fun () -> self#start)

    method private register_decoder _ = ()
    method client_state = Atomic.get state
    method set_headers h = headers <- h

    (* The handler may start the output that wakes the source before the
       client is offered: that wake-up is answered here. *)
    method offer container description =
      Atomic.compare_and_set state `Opening (`Offered (container, description))
      && begin
        if self#is_up then self#start;
        true
      end

    (* io.md §10.3, the client read function. *)
    method read buf ofs len =
      if Atomic.get state = `Ended || Atomic.get should_shutdown then
        raise Read_aborted;
      (* FFmpeg reads again after an end of input: the closed socket would
         block that read until the timeout. *)
      if peer_closed then 0 else self#read_socket buf ofs len

    method private read_socket buf ofs len =
      let socket = connection.Connection.socket in
      let rec read () =
        try
          socket#wait_for ~log:(self#log#info "%s") `Read args.timeout;
          client_read socket buf ofs len
        with Harbor.Retry -> read ()
      in
      match read () with
        | 0 ->
            peer_closed <- true;
            0
        | n ->
            if Atomic.get state = `Ended then raise Read_aborted;
            Option.iter
              (fun oc ->
                output oc buf ofs n;
                flush oc)
              dump;
            Option.iter
              (fun oc -> Printf.fprintf oc "%f %d\n%!" (Unix.gettimeofday ()) n)
              logf;
            n
        | exception Tutils.Timeout _ -> raise Read_timeout
        | exception Tutils.Exit ->
            peer_closed <- true;
            0

    (* io.md §10.6. *)
    method private start =
      match Atomic.get state with
        | `Offered (container, description) as current -> (
            let ctype = self#content_type in
            match
              if not (Decode_loop.can_produce ~ctype description) then
                failwith (Stream_description.describe description);
              Decode_loop.create ~decode_first_metadata:true ~ctype ~description
                container
            with
              | exception exn ->
                  self#finish
                    ~cause:
                      (`Fault
                         (Printf.sprintf
                            "Stream of %s cannot produce content of type %s: %s"
                            request_uri
                            (Frame.string_of_content_type ctype)
                            (Printexc.to_string exn)))
              | loop ->
                  if
                    Atomic.compare_and_set state current
                      (`Feeding (container, loop))
                  then (
                    Option.iter
                      (Generator.add_metadata self#buffer)
                      pending_metadata;
                    pending_metadata <- None;
                    connected <- true;
                    List.iter
                      (fun fn -> fn headers)
                      (Callbacks.elements on_connect);
                    self#feed_loop loop)
                  else Decode_loop.close loop)
        | _ -> ()

    method private feed_loop loop =
      let buffer = Decoder.mk_buffer ~ctype:self#content_type self#buffer in
      Scheduler.Task.add
        {
          Scheduler.Task.priority = `Threaded;
          events = [`Delay 0.];
          handler =
            (fun _ ->
              let cause =
                try
                  while true do
                    Decode_loop.decode loop buffer
                  done;
                  `Stopped
                with
                  | End_of_file -> `End_of_input
                  | _
                    when Atomic.get state = `Ended || Atomic.get should_shutdown
                    ->
                      `Stopped
                  | Read_timeout ->
                      `Fault (Printf.sprintf "no data for %.0fs" args.timeout)
                  | exn -> `Fault (Printexc.to_string exn)
              in
              (match cause with
                | `End_of_input -> (
                    try Decode_loop.eof loop buffer
                    with exn ->
                      self#log#severe "Draining failed: %s"
                        (Printexc.to_string exn))
                | _ -> ());
              (try Decode_loop.close loop with _ -> ());
              ignore self#end_state;
              self#cleanup ~cause;
              []);
        }

    (* [true] when this call moved the state into [Ended]. *)
    method private end_state =
      match Atomic.get state with
        | `Ended -> false
        | current ->
            Atomic.compare_and_set state current `Ended || self#end_state

    val cleaned = Atomic.make false

    (* io.md §10.2: what ending a client releases, exactly once. *)
    method private cleanup ~(cause : cause) =
      if Atomic.compare_and_set cleaned false true then (
        (* A source that never woke up has no buffer to end a track in. *)
        let end_track () =
          if self#is_up then Generator.add_track_mark self#buffer
        in
        (match cause with
          | `Fault message ->
              self#log#severe "Client %s ended: %s" request_uri message;
              end_track ()
          | `Deadline ->
              self#log#important
                "Client %s: no output used its source within %.0fs, \
                 disconnected."
                request_uri args.timeout
          | `End_of_input -> end_track ()
          | `Stopped -> ());
        Connection.abort connection;
        Option.iter close_out dump;
        Option.iter close_out logf;
        dump <- None;
        logf <- None;
        on_end (self :> client);
        if connected then
          List.iter (fun fn -> fn ()) (Callbacks.elements on_disconnect))

    (* The feed task, when one runs, finishes what it started: it holds the
       container until its read returns. *)
    method finish ~(cause : cause) =
      match Atomic.get state with
        | `Feeding _ -> if self#end_state then Connection.abort connection
        | `Offered (container, _) ->
            if self#end_state then (
              (try Av.close container with _ -> ());
              self#cleanup ~cause)
        | `Opening -> if self#end_state then self#cleanup ~cause
        | `Ended -> ()

    method open_files =
      Option.iter
        (fun f ->
          try dump <- Some (open_out_bin (Lang_string.home_unrelate f))
          with e ->
            self#log#severe "Could not open dump file: %s"
              (Printexc.to_string e))
        args.dumpfile;
      Option.iter
        (fun f ->
          try logf <- Some (open_out_bin (Lang_string.home_unrelate f))
          with e ->
            self#log#severe "Could not open log file: %s" (Printexc.to_string e))
        args.logfile

    method! private can_generate_frame =
      (Atomic.get state <> `Ended && super#can_generate_frame)
      || Generator.length self#buffer > 0

    method! disconnect = self#finish ~cause:`Stopped
  end

(* io.md §10.1: the live clients of one registration, by mount. *)
type table = { mutex : Mutex.t; mutable clients : (string * client) list }

let mount_of_uri uri =
  if String.length uri > 1 && uri.[0] = '/' then
    String.sub uri 1 (String.length uri - 1)
  else uri

let find table mount =
  Mutex.protect table.mutex (fun () ->
      List.assoc_opt (mount_of_uri mount) table.clients)

let remove table client =
  Mutex.protect table.mutex (fun () ->
      table.clients <- List.filter (fun (_, c) -> c != client) table.clients)

let source_value client =
  Lang.meth
    (Lang.source (client :> Source.source))
    (List.map
       (fun (m : _ Lang.meth) -> (m.name, m.value client))
       (Harbor_input.meth ())
    @ List.map
        (fun cb ->
          let (m : _ Lang.meth) = Lang_source.callback cb in
          (m.name, m.value client))
        (Harbor_input.callbacks ()))

(* io.md §10.3. *)
let open_client ~args ~handler ~connection ~(relay : Harbor.relay_info) client =
  if Connection.acquire connection then
    Scheduler.Task.add
      {
        Scheduler.Task.priority = `Threaded;
        events = [`Delay 0.];
        handler =
          (fun _ ->
            (try
               let format =
                 Option.bind
                   (format_of_mime relay.stype)
                   Av.Format.find_input_format
               in
               let container =
                 Fun.protect
                   ~finally:(fun () -> Connection.release connection)
                   (fun () ->
                     client#open_files;
                     Av.open_input_stream ?format client#read)
               in
               let options =
                 { Input_options.format = None; opts = Hashtbl.create 0 }
               in
               let description =
                 Ffmpeg_decoder.describe ~options ~url:relay.uri container
               in
               log#info "Container: %s"
                 (Stream_description.describe description);
               let mime = Harbor_input.extract_mime relay.stype in
               let mime_type =
                 if String.contains mime '/' then Some mime
                 else Option.map mime_of_format description.format
               in
               let record =
                 Lang.record
                   [
                     ("uri", Lang.string relay.uri);
                     ( "query",
                       Lang.list
                         (List.map
                            (fun (k, v) ->
                              Lang.product (Lang.string k) (Lang.string v))
                            relay.groups) );
                     ( "format",
                       match description.format with
                         | Some f -> Lang.string f
                         | None -> Lang.null );
                     ( "mime_type",
                       match mime_type with
                         | Some m -> Lang.string m
                         | None -> Lang.null );
                     ( "streams",
                       Lang.list (List.map stream_value description.streams) );
                     ( "headers",
                       Lang.list
                         (List.map
                            (fun (k, v) ->
                              Lang.product (Lang.string k) (Lang.string v))
                            relay.headers) );
                     ("copy_encoder", copy_encoder ~description ~mime_type);
                   ]
               in
               client#set_headers relay.headers;
               let fn = Lang.apply handler [("", record)] in
               ignore (Lang.apply fn [("", source_value client)]);
               if client#offer container description then
                 Scheduler.Task.add
                   {
                     Scheduler.Task.priority = `Non_blocking;
                     events = [`Delay args.Harbor_input.timeout];
                     handler =
                       (fun _ ->
                         (match client#client_state with
                           | `Offered _ -> client#finish ~cause:`Deadline
                           | _ -> ());
                         []);
                   }
               else (try Av.close container with _ -> ())
             with exn ->
               client#finish
                 ~cause:
                   (`Fault
                      (Printf.sprintf "Error while opening container: %s!"
                         (Printexc.to_string exn))));
            []);
      }
  else client#finish ~cause:`Stopped

(* io.md §10.1: one harbor registration, one table of clients. *)
let register ~(args : Lang.regexp Harbor_input.parse_result) ~handler =
  Configure.conf_force_start#set true;
  let table = { mutex = Mutex.create (); clients = [] } in
  let relay (info : Harbor.relay_info) =
    let connection = Connection.create info.socket in
    let mount = mount_of_uri info.uri in
    let id =
      if mount = "" || mount = "/" then "input.harbor.dynamic" else mount
    in
    let client =
      new client
        ~args ~request_uri:info.uri ~id
        ~mime:(Harbor_input.extract_mime info.stype)
        ~connection
        ~client_read:(Option.value ~default:Harbor.read info.read)
        ~on_end:(remove table)
    in
    Mutex.protect table.mutex (fun () ->
        table.clients <- table.clients @ [(mount, client)]);
    open_client ~args ~handler ~connection ~relay:info client;
    fun () -> Connection.release connection
  in
  Harbor.add_source ~pos:args.pos ~transport:args.transport ~port:args.port
    ~mountpoint:args.mountpoint ~icy:args.icy
    {
      Harbor.relay;
      login = args.login;
      icy_charset = args.icy_charset;
      meta_charset = args.meta_charset;
      encode_metadata =
        (fun ~mount m ->
          Option.iter (fun c -> c#encode_metadata m) (find table mount));
      get_mime_type =
        (fun ~mount ->
          Option.bind (find table mount) (fun c -> c#get_mime_type));
    }

let stream_t =
  List.fold_left
    (fun t (name, typ, optional) -> Type.meth ~optional name ([], typ) t)
    Lang.unit_t
    [
      ("width", Lang.int_t, true);
      ("type", Lang.string_t, false);
      ("samplerate", Lang.int_t, true);
      ("pixel_format", Lang.string_t, true);
      ("height", Lang.int_t, true);
      ("frame_rate", Lang.nullable_t Lang.float_t, true);
      ("field", Lang.string_t, false);
      ("codec", Lang.string_t, false);
      ("channels", Lang.int_t, true);
      ("channel_layout", Lang.string_t, true);
    ]

let handler_t frame_t =
  let record_t =
    Lang.record_t
      [
        ( "copy_encoder",
          Lang.fun_t
            [
              (true, "", Lang.nullable_t Lang.string_t);
              ( true,
                "options",
                Lang.list_t (Lang.product_t Lang.string_t Lang.string_t) );
            ]
            (Lang.format_t frame_t) );
        ("format", Lang.nullable_t Lang.string_t);
        ("headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
        ("mime_type", Lang.nullable_t Lang.string_t);
        ("query", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
        ("streams", Lang.list_t stream_t);
        ("uri", Lang.string_t);
      ]
  in
  let source_t =
    Lang.method_t
      (Lang.source_t ~methods:true frame_t)
      (List.map
         (fun (m : _ Lang.meth) -> (m.name, m.scheme, m.descr))
         (Harbor_input.meth ())
      @ List.map
          (fun cb ->
            let (m : _ Lang.meth) = Lang_source.callback cb in
            (m.name, m.scheme, m.descr))
          (Harbor_input.callbacks ()))
  in
  Lang.fun_t
    [(false, "", record_t)]
    (Lang.fun_t [(false, "", source_t)] Lang.unit_t)

let dynamic = Lang.add_module ~base:Harbor_input.input_harbor "dynamic"

let _ =
  let frame_t = Lang.univ_t () in
  Lang.add_builtin ~base:dynamic "regexp"
    ~category:(`Source (`Input `Active))
    ~descr:
      "Receive any number of source clients on the mountpoints a regexp \
       matches. Each client is decoded with FFmpeg into a source of its own, \
       handed to `on_connect` with the client's connection record. A source is \
       single-use: a client that reconnects gets a new one. A source that no \
       output uses within `timeout` is disconnected."
    (Harbor_input.proto Lang.regexp_t
    @ [
        ( "on_connect",
          handler_t frame_t,
          None,
          Some
            "Called with the connection record of each client; returns the \
             function given the client's source." );
      ])
    Lang.unit_t
    (fun p ->
      let args = Harbor_input.parse_args ~parse_mountpoint:Lang.to_regexp p in
      register ~args ~handler:(List.assoc "on_connect" p);
      Lang.unit)
