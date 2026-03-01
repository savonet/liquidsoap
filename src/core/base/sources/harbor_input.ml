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

let address_resolver s =
  let s = Harbor.file_descr_of_socket s in
  Utils.name_of_sockaddr ~rev_dns:Harbor_base.conf_revdns#get
    (Unix.getpeername s)

let should_shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"input.harbor shutdown" (fun () ->
      Atomic.set should_shutdown true)

let extract_mime stype =
  try
    let sub = Re.Pcre.exec ~rex:(Re.Pcre.regexp "^([^;]+);.*$") stype in
    Re.Pcre.get_substring sub 1
  with Not_found -> stype

(* Create a regexp that exactly matches the given string *)
let regexp_of_string s =
  let escaped = Re.Pcre.quote s in
  let descr = "^" ^ escaped ^ "$" in
  {
    Liquidsoap_lang.Builtins_regexp.descr;
    flags = [];
    regexp = Re.Pcre.regexp descr;
  }

class virtual http_input_base ~dumpfile ~logfile ~bufferize ~max ~replay_meta
  ~login ~debug ~timeout () =
  object (self)
    inherit Source.active_source ~name:"input.harbor" ()
    inherit! Generated.source ~empty_on_abort:false ~replay_meta ~bufferize ()
    val relay_socket = Atomic.make None
    val mutable pending_headers : (string * string) list = []

    (** Function to read on socket. *)
    val mutable relay_read = fun _ _ _ -> assert false

    val mutable create_decoder = fun _ -> assert false
    val mime_type : string option Atomic.t = Atomic.make None
    val mutable dump = None
    val mutable logf = None
    val mutable on_connect : ((string * string) list -> unit) list = []

    initializer
      self#on_wake_up (fun () ->
          Generator.set_max_length self#buffer
            (Some (Frame.main_of_seconds max)));
      self#on_sleep (fun () -> self#disconnect)

    method on_connect fn = on_connect <- on_connect @ [fn]
    val mutable on_disconnect = []
    method on_disconnect fn = on_disconnect <- on_disconnect @ [fn]
    val mutable on_relay : (unit -> unit) option = None
    method on_relay fn = on_relay <- Some fn

    method connected_client =
      Option.map address_resolver (Atomic.get relay_socket)

    method status_cmd =
      match self#connected_client with
        | Some addr -> Printf.sprintf "source client connected from %s" addr
        | None -> "no source client connected"

    method private output = if self#is_ready then ignore self#get_frame
    method reset = self#disconnect
    method buffer_length_cmd = Frame.seconds_of_audio self#length
    method login : string * (Harbor.login_args -> bool) = login
    method fallible = true

    (* Insert metadata *)
    method encode_metadata m =
      if self#is_up then (
        (* Metadata may contain only the "song" value
         * or "artist" and "title". Here, we use "song"
         * as the "title" field if "title" is not provided. *)
        let m =
          if not (Frame.Metadata.mem "title" m) then (
            try Frame.Metadata.add "title" (Frame.Metadata.find "song" m) m
            with _ -> m)
          else m
        in
        self#log#important "New metadata chunk %s -- %s."
          (try Frame.Metadata.find "artist" m with _ -> "?")
          (try Frame.Metadata.find "title" m with _ -> "?");
        Generator.add_metadata self#buffer m)

    method get_mime_type = Atomic.get mime_type

    method feed =
      self#log#important "Decoding...";
      let t0 = Unix.gettimeofday () in
      let read buf ofs len =
        let input =
          (fun buf len ->
            match Atomic.get relay_socket with
              | None -> 0
              | Some socket -> (
                  try
                    let rec f () =
                      try
                        let fd = Harbor.file_descr_of_socket socket in
                        (* Wait for `Read event on socket. *)
                        Tutils.wait_for ~log:(self#log#info "%s") (`Read fd)
                          timeout;

                        (* Now read. *)
                        relay_read socket buf ofs len
                      with Harbor.Retry -> f ()
                    in
                    f ()
                  with
                    | Tutils.Exit -> 0
                    | e ->
                        let bt = Printexc.get_backtrace () in
                        Utils.log_exception ~log:self#log ~bt
                          (Printf.sprintf "Error while reading from client: %s"
                             (Printexc.to_string e));
                        (try self#disconnect with _ -> ());
                        0))
            buf len
        in
        begin match dump with
          | Some b ->
              output_string b (Bytes.sub_string buf 0 input);
              flush b
          | None -> ()
        end;
        begin match logf with
          | Some b ->
              let time = (Unix.gettimeofday () -. t0) /. 60. in
              Printf.fprintf b "%f %d\n%!" time self#length
          | None -> ()
        end;
        input
      in
      let input = { Decoder.read; tell = None; length = None; lseek = None } in
      try
        let decoder, buffer = create_decoder input in
        Fun.protect ~finally:decoder.Decoder.close (fun () ->
            while true do
              if Atomic.get relay_socket = None then failwith "relaying stopped";
              if Atomic.get should_shutdown then failwith "shutdown called";
              decoder.Decoder.decode buffer
            done)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        (* Feeding has stopped: adding a break here. *)
        Generator.add_track_mark self#buffer;
        Utils.log_exception ~log:self#log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Feeding stopped: %s" (Printexc.to_string exn));
        self#disconnect;
        if debug then Printexc.raise_with_backtrace exn bt

    method virtual private register_decoder : string -> unit

    method private start_feed =
      self#register_decoder (Option.get (Atomic.get mime_type));
      List.iter (fun fn -> fn pending_headers) on_connect;
      begin match dumpfile with
        | Some f -> (
            try dump <- Some (open_out_bin (Lang_string.home_unrelate f))
            with e ->
              self#log#severe "Could not open dump file: %s"
                (Printexc.to_string e))
        | None -> ()
      end;
      begin match logfile with
        | Some f -> (
            try logf <- Some (open_out_bin (Lang_string.home_unrelate f))
            with e ->
              self#log#severe "Could not open log file: %s"
                (Printexc.to_string e))
        | None -> ()
      end;
      ignore (Tutils.create (fun () -> self#feed) () "harbor source feeding")

    val mutable uri = ""
    method uri = uri
    val mutable groups = []
    method groups = groups

    method relay
        {
          Harbor.stype;
          uri = relay_uri;
          groups = relay_groups;
          headers;
          read;
          socket;
        } =
      uri <- relay_uri;
      groups <- relay_groups;
      let read = Option.value ~default:Harbor.read read in
      if not (Atomic.compare_and_set relay_socket None (Some socket)) then
        raise Harbor.Mount_taken;
      let mime = extract_mime stype in
      Atomic.set mime_type (Some mime);
      pending_headers <- headers;
      relay_read <- read;
      match on_relay with Some fn -> fn () | None -> ()

    method disconnect =
      match Atomic.exchange relay_socket None with
        | None -> ()
        | Some s ->
            (try Harbor.close s with _ -> ());
            (match dump with
              | Some f ->
                  close_out f;
                  dump <- None
              | None -> ());
            (match logf with
              | Some f ->
                  close_out f;
                  logf <- None
              | None -> ());
            List.iter (fun fn -> fn ()) on_disconnect
  end

class http_input_server ~pos ~transport ~dumpfile ~logfile ~bufferize ~max ~icy
  ~port ~meta_charset ~icy_charset ~replay_meta ~mountpoint ~login ~debug
  ~timeout () =
  let mountpoint = regexp_of_string mountpoint in
  object (self)
    inherit
      http_input_base
        ~dumpfile ~logfile ~bufferize ~max ~replay_meta ~login ~debug ~timeout
          ()

    val mutable is_registered = false

    initializer
      self#on_wake_up (fun () ->
          Harbor.add_source ~pos ~transport ~port ~mountpoint ~icy
            {
              Harbor.relay = self#relay;
              login = self#login;
              icy_charset;
              meta_charset;
              encode_metadata = self#encode_metadata;
              get_mime_type = (fun () -> self#get_mime_type);
            };
          is_registered <- true);

      self#on_relay (fun () -> self#start_feed);

      self#on_sleep (fun () ->
          if is_registered then Harbor.remove_source ~port ~mountpoint ();
          is_registered <- false)

    method private register_decoder mime =
      match Decoder.get_stream_decoder ~ctype:self#content_type mime with
        | Some decoder ->
            let decoder args =
              let buffer =
                Decoder.mk_buffer ~ctype:self#content_type self#buffer
              in
              (decoder args, buffer)
            in
            create_decoder <- decoder
        | None -> raise Harbor.Unknown_codec
  end

let proto ?(buffer_default = 12.) mountpoint_t =
  [
    ( "buffer",
      Lang.float_t,
      Some (Lang.float buffer_default),
      Some
        "Duration of the pre-buffered data (in seconds). Default value is set \
         to make it possible to use `crossfade` transitions with \
         `input.harbor`. You might be able to reduce it but, in this case, \
         make sure to not use the operator with `crossfade` or make sure that \
         it has enough buffered data for it." );
    ( "max",
      Lang.float_t,
      Some (Lang.float 20.),
      Some "Maximum duration of the buffered data (in seconds)." );
    ( "timeout",
      Lang.float_t,
      Some (Lang.float 30.),
      Some "Timeout for source connectionn." );
    ("user", Lang.string_t, Some (Lang.string "source"), Some "Source user.");
    ( "password",
      Lang.string_t,
      Some (Lang.string "hackme"),
      Some "Source password." );
    ( "port",
      Lang.int_t,
      Some (Lang.int 8005),
      Some "Port used to connect to the source." );
    ( "transport",
      Lang.http_transport_base_t,
      Some (Lang.base_http_transport Http.unix_transport),
      Some
        "Http transport. Use `http.transport.ssl` or \
         `http.transport.secure_transport`, when available, to enable HTTPS \
         output" );
    ( "icy",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Enable ICY (shoutcast) protocol." );
    ( "icy_metadata_charset",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "ICY (shoutcast) metadata charset. Guessed if null. Default for \
         shoutcast is ISO-8859-1. Set to that value if all your clients send \
         metadata using this charset and automatic detection is not working \
         for you." );
    ( "metadata_charset",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "Metadata charset for non-ICY (shoutcast) source protocols. Guessed if \
         null." );
    ( "replay_metadata",
      Lang.bool_t,
      Some (Lang.bool false),
      Some
        "Replay last known metadata when switching back to this source. This \
         helps when source has dropped due to temporary connection issues." );
    ( "auth",
      Lang.nullable_t
        (Lang.fun_t
           [
             ( false,
               "",
               Lang.record_t
                 [
                   ("address", Lang.string_t);
                   ("uri", Lang.string_t);
                   ("user", Lang.string_t);
                   ("password", Lang.string_t);
                 ] );
           ]
           Lang.bool_t),
      Some Lang.null,
      Some
        "Authentication function. Receives a record with: `user`, `password`, \
         `uri` (mountpoint) and `address` (client network address) and returns \
         `true` if the user should be granted access for this login. Override \
         any other method if used." );
    ( "dumpfile",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "Dump stream to file, for debugging purpose. Disabled if null." );
    ( "logfile",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "Log buffer status to file, for debugging purpose. Disabled if null."
    );
    ( "debug",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Run in debugging mode by not catching some exceptions." );
    ("", mountpoint_t, None, Some "Mountpoint to look for.");
  ]

type 'a parse_result = {
  pos : Liquidsoap_lang.Pos.t list;
  mountpoint : 'a;
  login : string * (Harbor.login_args -> bool);
  debug : bool;
  timeout : float;
  icy : bool;
  icy_charset : string option;
  meta_charset : string option;
  replay_meta : bool;
  port : int;
  transport : Http.transport;
  dumpfile : string option;
  logfile : string option;
  bufferize : float;
  max : float;
}

let parse_mountpoint_string v =
  let mountpoint = Lang.to_string v in
  if mountpoint <> "" && mountpoint.[0] = '/' then mountpoint
  else Printf.sprintf "/%s" mountpoint

let parse_args ~parse_mountpoint p =
  let mountpoint = parse_mountpoint (List.assoc "" p) in
  let default_user = Lang.to_string (List.assoc "user" p) in
  let default_password = Lang.to_string (List.assoc "password" p) in
  let debug = Lang.to_bool (List.assoc "debug" p) in
  let timeout = Lang.to_float (List.assoc "timeout" p) in
  let icy = Lang.to_bool (List.assoc "icy" p) in
  let icy_charset =
    Lang.to_valued_option Lang.to_string (List.assoc "icy_metadata_charset" p)
  in
  let meta_charset =
    Lang.to_valued_option Lang.to_string (List.assoc "metadata_charset" p)
  in
  let replay_meta = Lang.to_bool (List.assoc "replay_metadata" p) in
  let port = Lang.to_int (List.assoc "port" p) in
  let transport = Lang.to_http_transport (List.assoc "transport" p) in
  let auth_function = Lang.to_option (List.assoc "auth" p) in
  let login { Harbor.socket; uri; user; password } =
    let user, password =
      let f = Charset.convert in
      (f user, f password)
    in
    let default_login = user = default_user && password = default_password in
    match auth_function with
      | Some auth_function ->
          Lang.to_bool
            (Lang.apply auth_function
               [
                 ( "",
                   Lang.record
                     [
                       ("address", Lang.string (address_resolver socket));
                       ("uri", Lang.string uri);
                       ("user", Lang.string user);
                       ("password", Lang.string password);
                     ] );
               ])
      | None -> default_login
  in
  let login = (default_user, login) in
  let dumpfile =
    Lang.to_valued_option Lang.to_string (List.assoc "dumpfile" p)
  in
  let logfile = Lang.to_valued_option Lang.to_string (List.assoc "logfile" p) in
  let bufferize = Lang.to_float (List.assoc "buffer" p) in
  let max = Lang.to_float (List.assoc "max" p) in
  if bufferize >= max then
    raise
      (Error.Invalid_value
         (List.assoc "max" p, "Maximum buffering inferior to pre-buffered data"));
  let pos = Lang.pos p in
  {
    pos;
    mountpoint;
    login;
    debug;
    timeout;
    icy;
    icy_charset;
    meta_charset;
    replay_meta;
    port;
    transport;
    dumpfile;
    logfile;
    bufferize;
    max;
  }

let meth () =
  [
    Lang.
      {
        name = "stop";
        scheme = ([], Lang.fun_t [] Lang.unit_t);
        descr =
          "Disconnect the client currently connected to the harbor. Does \
           nothing if no client is connected.";
        value =
          (fun s ->
            Lang.val_fun [] (fun _ ->
                s#disconnect;
                Lang.unit));
      };
    {
      name = "connected_client";
      scheme = ([], Lang.fun_t [] (Lang.nullable_t Lang.string_t));
      descr =
        "Returns the address of the client currently connected, if there is \
         one.";
      value =
        (fun s ->
          Lang.val_fun [] (fun _ ->
              match s#connected_client with
                | Some c -> Lang.string c
                | None -> Lang.null));
    };
    {
      name = "status";
      scheme = ([], Lang.fun_t [] Lang.string_t);
      descr = "Current status of the input.";
      value = (fun s -> Lang.val_fun [] (fun _ -> Lang.string s#status_cmd));
    };
    {
      name = "buffer_length";
      scheme = ([], Lang.fun_t [] Lang.float_t);
      descr = "Length of the buffer (in seconds).";
      value =
        (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#buffer_length_cmd));
    };
  ]

let callbacks () =
  [
    {
      Lang_source.name = "on_connect";
      params = [];
      descr =
        "when a source is connected. Its receives the list of headers, of the \
         form: (<label>,<value>). All labels are lowercase.";
      register_deprecated_argument = true;
      arg_t = [(false, "", Lang.metadata_t)];
      register =
        (fun ~params:_ s on_connect ->
          let on_connect m = on_connect [("", Lang.metadata_list m)] in
          s#on_connect on_connect);
    };
    {
      name = "on_disconnect";
      params = [];
      descr = "when a source is disconnected.";
      register_deprecated_argument = true;
      arg_t = [];
      register = (fun ~params:_ s f -> s#on_disconnect (fun () -> f []));
    };
  ]

let input_harbor =
  Lang.add_operator ~base:Modules.input "harbor" ~return_t:(Lang.univ_t ())
    ~callbacks:(callbacks ()) ~meth:(meth ()) ~category:`Input
    ~descr:
      "Create a source that receives a http/icecast stream and forwards it as \
       a stream." (proto Lang.string_t) (fun p ->
      let {
        pos;
        mountpoint;
        login;
        debug;
        timeout;
        icy;
        icy_charset;
        meta_charset;
        replay_meta;
        port;
        transport;
        dumpfile;
        logfile;
        bufferize;
        max;
      } =
        parse_args ~parse_mountpoint:parse_mountpoint_string p
      in
      new http_input_server
        ~pos ~transport ~timeout ~bufferize ~max ~login ~mountpoint ~dumpfile
        ~logfile ~icy ~port ~icy_charset ~meta_charset ~replay_meta ~debug ())
