(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(* This is a tricky implementation due to some technical details of libcurl.
   Here's the gist of it:
     - Stream data is passed immediately by libcurl via a asynchronous handler.
     - Decoder expect a blocking API when reading data. No way around it..
     - We want to keep our internal data buffer capped.

   Thus, we need to:
     - Use a condition to wait on data when we don't have enough of it.
     - Track how much data we actually need to possibly increase the max buffer if needed.
     - Pause/resume transfer if the data buffer exceeds the max buffer limit.

  This is the spirit of this implementation. If there's ever a bug, well somethin
  wasn't implemented properly.. 😅

  Couple of gotchas:
     - We need to know if the transfer is still active before waiting on the read
       conditional. Libcurl doesn't provide an easy API for this so we manually 
       keep track of it..
     - We can't use [remove] after [remove_finished]. Again, no way to know unless
       we keep track of it as said above.
     - Libcurl operations seem to SEGFAULT when doing anything past liquidsoap core
       shutdown so we simply stop doing anything when shutting down.
*)

let transfers_log = Log.make ["http"; "transfers"]
let default_timeout = 0.1

(* Libcurl does not have an API to query if a transfer is done.. *)
let transfers = Hashtbl.create 10
let transfers_m = Mutex.create ()
let transfer_id = Tutils.mutexify transfers_m (Hashtbl.find transfers)
let transfer_active = Tutils.mutexify transfers_m (Hashtbl.mem transfers)
let on_transfer_done = Tutils.mutexify transfers_m (Hashtbl.remove transfers)
let on_transfer_started = Tutils.mutexify transfers_m (Hashtbl.add transfers)

let mt =
  lazy
    (let mt = Curl.Multi.create () in
     let rec mark_transfers_done () =
       match Curl.Multi.remove_finished mt with
         | Some (h, r) ->
             let id = transfer_id h in
             transfers_log#info
               "Transfer for source %s finished with status: %s" id
               (Liqcurl.string_of_curl_code r);
             on_transfer_done h;
             Curl.cleanup h;
             mark_transfers_done ()
         | None -> ()
     in
     let rec perform () =
       while (not (Liqcurl.is_shutdown ())) && Curl.Multi.perform mt > 0 do
         mark_transfers_done ();
         ignore (Curl.Multi.wait ~timeout_ms:200 mt)
       done;
       if not (Liqcurl.is_shutdown ()) then perform ()
       else mark_transfers_done ()
     in
     ignore (Tutils.create perform () "input.http curl processing");
     mt)

let stream_request ?headers ?http_version ?interface ~connection_timeout ~url
    ~request ~on_response_header_data ~on_body_data ~source_id () =
  let connection =
    Liqcurl.http_connection ?headers ?http_version ?interface
      ~connection_timeout ~url ~request ~on_response_header_data ~on_body_data
      ()
  in
  connection#set_followlocation true;
  Curl.Multi.add (Lazy.force mt) connection#handle;
  on_transfer_started connection#handle source_id;
  connection

(** Utility for reading icy metadata *)
let parse_metadata chunk =
  let h = Hashtbl.create 10 in
  let rec parse chunk =
    try
      let mid = String.index chunk '=' in
      let close = String.index chunk ';' in
      let key = Configure.recode_tag (String.sub chunk 0 mid) in
      let value =
        Configure.recode_tag (String.sub chunk (mid + 2) (close - mid - 3))
      in
      let key =
        match key with
          | "StreamTitle" -> "title"
          | "StreamUrl" -> "url"
          | _ -> key
      in
      Hashtbl.add h key value;
      parse (String.sub chunk (close + 1) (String.length chunk - close - 1))
    with _ -> ()
  in
  parse chunk;
  h

exception Transfer_done
exception Timeout

let read_metadata ~wait_for_data on_metadata =
  let old_chunk = ref "" in
  fun position data ->
    let size = 16 * int_of_char (Buffer.nth data 0) in
    wait_for_data (size + 1);
    let chunk = Bytes.create size in
    Buffer.blit data 1 chunk 0 size;
    Utils.buffer_drop data (size + 1);
    position := 0;
    let chunk = Bytes.unsafe_to_string chunk in
    if chunk <> "" && chunk <> !old_chunk then (
      old_chunk := chunk;
      on_metadata (parse_metadata chunk) )

let read data ~wait_for_data buf ofs len =
  wait_for_data 1;
  let ret = min (Buffer.length data) len in
  Buffer.blit data 0 buf ofs ret;
  Utils.buffer_drop data ret;
  ret

let read_stream ~on_metadata ~wait_for_data =
  let position = ref 0 in
  let read_metadata = read_metadata ~wait_for_data on_metadata in
  let read = read ~wait_for_data in
  fun metaint data buf ofs len ->
    match metaint with
      | None -> read data buf ofs len
      | Some metaint ->
          if !position = metaint then read_metadata position data;

          let len = min len (metaint - !position) in
          let ret = read data buf ofs len in
          position := !position + ret;
          ret

(** HTTP input *)

module G = Generator
module Generator = Generator.From_audio_video_plus

class http ~kind ~poll_delay ~track_on_meta ?(force_mime = None) ~bind_address
  ~autostart ~clock_safe ~self_sync ~connection_timeout ~timeout ~debug
  ~on_connect ~on_disconnect ?(logfile = None) ~user_agent url =
  (* Since we control the size of the buffer, this should
     never happen. *)
  let max_ticks = Frame.main_of_seconds 20. in
  (* We need a temporary log until the source has an ID. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let generator =
    Generator.create ~log ~log_overfull:true ~overfull:(`Drop_old max_ticks)
      `Undefined
  in
  object (self)
    inherit Source.source ~name:"input.http" kind as super

    val read_c = Condition.create ()

    val mutable curl_handler = None

    val mutable decoder = None

    val mutable response_parsed = false

    val mutable stream_paused = false

    val mutable source_shutdown = false

    val mutable metaint = None

    (* 64k bytes *)
    val mutable min_data_buffer = 64_000

    (* 1m byte *)
    val mutable max_data_buffer = 1_000_000

    (** Log file for the timestamps of read events. *)
    val mutable logf = None

    val mutable relaying = autostart

    val mutable url = url

    val mutable connect_task = None

    val mutable clock = None

    method stype = Source.Fallible

    method self_sync = self_sync && self#is_ready

    method is_ready = self#mutexify (fun () -> decoder <> None) ()

    method abort_track =
      Generator.add_break generator;
      self#reconnect

    method remaining =
      self#mutexify (fun () -> Generator.remaining generator) ()

    method start_cmd =
      self#mutexify (fun () -> relaying <- true) ();
      self#connect

    method stop_cmd =
      self#mutexify (fun () -> relaying <- false) ();
      self#disconnect

    method set_url_cmd fn = self#mutexify (fun () -> url <- fn) ()

    method url_cmd = self#mutexify (fun () -> url ()) ()

    method status_cmd =
      self#mutexify
        (fun () ->
          match (curl_handler, relaying) with
            | Some _, _ -> "connected"
            | None, true -> "connecting"
            | None, false -> "stopped")
        ()

    method buffer_length_cmd =
      self#mutexify (fun () -> Frame.seconds_of_audio self#length) ()

    method length = Generator.length generator

    (* Insert metadata *)
    method insert_metadata m =
      self#log#important "New metadata chunk: %s -- %s."
        (try Hashtbl.find m "artist" with _ -> "?")
        (try Hashtbl.find m "title" with _ -> "?");
      Generator.add_metadata generator m;
      if track_on_meta then Generator.add_break generator

    method mk_decoder ~url create_decoder =
      let response_headers = Buffer.create 1024 in
      begin
        match logfile with
        | Some f -> (
            try logf <- Some (open_out_bin (Utils.home_unrelate f))
            with e ->
              self#log#severe "Could not open log file: %s"
                (Printexc.to_string e) )
        | None -> ()
      end;
      let on_response_header_data =
        self#mutexify (Buffer.add_string response_headers)
      in
      let data = Buffer.create 1024 in
      let on_body_data s =
        try
          self#mutexify
            (fun s ->
              if not response_parsed then (
                let _, _, _, headers =
                  Liqcurl.parse_response_headers
                    (Buffer.contents response_headers)
                in
                on_connect headers;
                metaint <-
                  Option.map
                    (fun (_, v) -> int_of_string v)
                    (List.find_opt
                       (fun (lbl, _) ->
                         String.lowercase_ascii lbl = "icy-metaint")
                       headers);
                response_parsed <- true );
              Condition.signal read_c;
              let n = Buffer.length data in
              if (not (self_sync || stream_paused)) && max_data_buffer <= n then (
                self#log#debug "Pausing transfer (min: %d, max: %d, buffer: %d)"
                  min_data_buffer max_data_buffer n;
                stream_paused <- true;
                `Pause )
              else (
                Buffer.add_string data s;
                `Continue ))
            s
        with exn ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log:self#log ~bt
            (Printf.sprintf "Read callback failed: %s" (Printexc.to_string exn));
          raise exn
      in
      let connection =
        stream_request
          ~headers:[("User-Agent", user_agent); ("Icy-MetaData", "1")]
          ~connection_timeout ~url ~request:`Get ?interface:bind_address
          ~on_response_header_data ~on_body_data ~source_id:self#id ()
      in
      curl_handler <- Some connection;
      connection#set_noprogress false;
      connection#set_progressfunction (fun _ _ _ _ ->
          if Liqcurl.is_shutdown () then (
            self#mutexify (fun () -> Condition.signal read_c) ();
            true )
          else (
            ( try
                (* [Curl.pause] calls the write function inside the same
                   thread so we cannot keep the mutex locked. *)
                if
                  self#mutexify
                    (fun () ->
                      Condition.signal read_c;
                      let n = Buffer.length data in
                      if n <= max_data_buffer && stream_paused then (
                        self#log#debug
                          "Resuming transfer (min: %d, max: %d, buffer: %d)"
                          min_data_buffer max_data_buffer n;
                        stream_paused <- false;
                        true )
                      else false)
                    ()
                then Curl.pause connection#handle []
              with exn ->
                let bt = Printexc.get_backtrace () in
                Utils.log_exception ~log:self#log ~bt
                  (Printf.sprintf "Progress callback failed: %s"
                     (Printexc.to_string exn)) );
            false ));
      let wait_for_data len =
        max_data_buffer <- max len max_data_buffer;
        let start_time = Unix.gettimeofday () in
        while
          transfer_active connection#handle
          && (not (Liqcurl.is_shutdown ()))
          && Buffer.length data < len
        do
          Condition.wait read_c self#mutex;
          if float timeout <= Unix.gettimeofday () -. start_time then
            raise Timeout
        done;
        if Buffer.length data < len then raise Transfer_done
      in
      let read_stream =
        read_stream ~on_metadata:self#insert_metadata ~wait_for_data
      in
      let read buf ofs len =
        self#mutexify
          (fun () ->
            try
              if not response_parsed then wait_for_data min_data_buffer;
              read_stream metaint data buf ofs len
            with Transfer_done -> 0)
          ()
      in
      let read =
        match logf with
          | None -> read
          | Some f ->
              let t0 = Unix.gettimeofday () in
              fun buf ofs len ->
                let ret = read buf ofs len in
                let time = (Unix.gettimeofday () -. t0) /. 60. in
                Printf.fprintf f "%f %d\n%!" time self#length;
                ret
      in
      let input = { Decoder.read; tell = None; length = None; lseek = None } in
      try decoder <- Some (create_decoder input)
      with e ->
        begin
          match logf with
          | Some f ->
              close_out f;
              logf <- None
          | None -> ()
        end;
        raise e

    method private connect_fn () =
      if source_shutdown || Liqcurl.is_shutdown () then -1.
      else (
        try
          let url = url () in
          let content_type =
            match force_mime with
              | Some m -> m
              | None -> (
                  match
                    let _, _, _, headers =
                      Liqcurl.http_request ~follow_redirect:true
                        ~connection_timeout ~timeout ~url ~request:`Head
                        ?interface:bind_address
                        ~on_body_data:(fun _ -> `Continue)
                        ()
                    in
                    List.find_opt
                      (fun (lbl, _) ->
                        String.lowercase_ascii lbl = "content-type")
                      headers
                  with
                    | Some (_, m) -> m
                    | None | (exception _) -> "application/octet-stream" )
          in
          Generator.set_mode generator `Undefined;
          let dec =
            match Decoder.get_stream_decoder ~ctype:self#ctype content_type with
              | Some d -> d
              | None -> failwith "Unknown format!"
          in
          Generator.set_rewrite_metadata generator (fun m ->
              Hashtbl.add m "source_url" url;
              m);
          self#mk_decoder ~url dec;
          -1.
        with e ->
          self#log#info "Connection failed: %s" (Printexc.to_string e);
          self#disconnect;
          if debug then raise e;
          poll_delay )

    method private connect =
      self#mutexify
        (fun () ->
          if relaying && curl_handler = None then (
            match connect_task with
              | Some t -> Duppy.Async.wake_up t
              | None ->
                  let t =
                    Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler
                      self#connect_fn
                  in
                  connect_task <- Some t;
                  Duppy.Async.wake_up t ))
        ()

    method disconnect =
      self#mutexify
        (fun () ->
          if response_parsed then on_disconnect ();
          decoder <- None;
          curl_handler <- None;
          stream_paused <- false;
          response_parsed <- false;
          metaint <- None;
          ignore
            (Option.map
               (fun h ->
                 if transfer_active h#handle then (
                   Curl.Multi.remove (Lazy.force mt) h#handle;
                   h#cleanup );
                 on_transfer_done h#handle)
               curl_handler);
          Condition.signal read_c)
        ()

    method private reconnect =
      self#disconnect;
      self#connect

    method private get_frame frame =
      let pos = Frame.position frame in
      try
        let decoder = self#mutexify (fun () -> Option.get decoder) () in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while Generator.length generator < Lazy.force Frame.size do
          decoder.Decoder.decode buffer
        done;
        Generator.fill generator frame
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Feeding failed: %s" (Printexc.to_string exn));
        Frame.add_break frame pos;
        self#reconnect

    method private get_clock =
      match clock with
        | Some c -> c
        | None ->
            let c = new Clock.clock "input.http" in
            clock <- Some c;
            c

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (self#get_clock :> Clock.clock))

    method wake_up act =
      super#wake_up act;
      (* Now we can create the log function *)
      (log_ref := fun s -> self#log#important "%s" s);
      self#connect

    method sleep =
      source_shutdown <- true;
      self#disconnect;
      super#sleep
  end

let () =
  Lang.add_operator "input.http" ~return_t:(Lang.univ_t ())
    ~meth:
      [
        ( "start",
          ([], Lang.fun_t [] Lang.unit_t),
          "Start reading input.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                s#start_cmd;
                Lang.unit) );
        ( "stop",
          ([], Lang.fun_t [] Lang.unit_t),
          "Stop reading input.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                s#stop_cmd;
                Lang.unit) );
        ( "url",
          ([], Lang.fun_t [] Lang.string_t),
          "URL of the input.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.string s#url_cmd) );
        ( "set_url",
          ([], Lang.fun_t [(false, "", Lang.fun_t [] Lang.string_t)] Lang.unit_t),
          "Set the url of the input.",
          fun s ->
            Lang.val_fun [("", "", None)] (fun p ->
                let fn = List.assoc "" p in
                let fn () = Lang.to_string (Lang.apply fn []) in
                s#set_url_cmd fn;
                Lang.unit) );
        ( "status",
          ([], Lang.fun_t [] Lang.string_t),
          "Current status of the input.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.string s#status_cmd) );
        ( "buffer_length",
          ([], Lang.fun_t [] Lang.float_t),
          "Length of the buffer length (in seconds).",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#buffer_length_cmd) );
      ]
    ~category:Lang.Input ~descr:"Create a source that fetches a HTTP stream."
    [
      ( "autostart",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Initially start relaying or not." );
      ( "bind_address",
        Lang.string_t,
        Some (Lang.string ""),
        Some
          "Address to bind on the local machine. This option can be useful if \
           your machine is bound to multiple IPs. Empty means no bind address."
      );
      ( "connection_timeout",
        Lang.int_t,
        Some (Lang.int 2),
        Some "Maximum time to wait while trying to connect to the given url." );
      ( "timeout",
        Lang.int_t,
        Some (Lang.int 10),
        Some "Maximum time to wait while waiting for data." );
      ( "on_connect",
        Lang.fun_t [(false, "", Lang.metadata_t)] Lang.unit_t,
        Some (Lang.val_cst_fun [("", None)] Lang.unit),
        Some
          "Function to execute when a source is connected. Its receives the \
           list of headers, of the form: (<label>,<value>). All labels are \
           lowercase." );
      ( "on_disconnect",
        Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Function to excecute when a source is disconnected" );
      ( "active",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "If `true`, the source's data is continuously pulled. Otherwise, it \
           accumulates and you need to explicitely connect it to an output to \
           make sure it is consumed." );
      ( "new_track_on_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Treat new metadata as new track." );
      ( "force_mime",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Force mime data type." );
      ( "poll_delay",
        Lang.float_t,
        Some (Lang.float 2.),
        Some "Polling delay when trying to connect to the stream." );
      ( "logfile",
        Lang.string_t,
        Some (Lang.string ""),
        Some
          "Log buffer status to file, for debugging purpose. Disabled if empty."
      );
      ( "debug",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Run in debugging mode, not catching some exceptions." );
      ( "user_agent",
        Lang.string_t,
        Some (Lang.string Http.user_agent),
        Some "User agent." );
      ( "self_sync",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Should the source control its own timing? Typically, should be \
           `true` for icecast sources and `false` for regular `http` requests."
      );
      ( "clock_safe",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Should the source be in its own clock. Should be the same value as \
           `self_sync` unless the source is mixed with other `clock_safe` \
           sources like `input.ao`" );
      ("", Lang.getter_t Lang.string_t, None, Some "URL of an HTTP stream");
    ]
    (fun p ->
      let url = Lang.to_string_getter (List.assoc "" p) in
      let autostart = Lang.to_bool (List.assoc "autostart" p) in
      let bind_address = Lang.to_string (List.assoc "bind_address" p) in
      let user_agent = Lang.to_string (List.assoc "user_agent" p) in
      let connection_timeout =
        Lang.to_int (List.assoc "connection_timeout" p)
      in
      let timeout = Lang.to_int (List.assoc "timeout" p) in
      let track_on_meta = Lang.to_bool (List.assoc "new_track_on_metadata" p) in
      let active = Lang.to_bool (List.assoc "active" p) in
      let debug = Lang.to_bool (List.assoc "debug" p) in
      let logfile =
        match Lang.to_string (List.assoc "logfile" p) with
          | "" -> None
          | s -> Some s
      in
      let bind_address = match bind_address with "" -> None | s -> Some s in
      let force_mime =
        Lang.to_valued_option Lang.to_string (List.assoc "force_mime" p)
      in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let clock_safe =
        Lang.to_default_option ~default:self_sync Lang.to_bool
          (List.assoc "clock_safe" p)
      in
      let on_connect l =
        let l =
          List.map
            (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
            l
        in
        let arg = Lang.list l in
        ignore (Lang.apply (List.assoc "on_connect" p) [("", arg)])
      in
      let on_disconnect () =
        ignore (Lang.apply (List.assoc "on_disconnect" p) [])
      in
      let poll_delay = Lang.to_float (List.assoc "poll_delay" p) in
      let kind = Source.Kind.of_kind Lang.any in
      let source =
        new http
          ~kind ~autostart ~track_on_meta ~force_mime ~bind_address ~poll_delay
          ~connection_timeout ~timeout ~on_connect ~on_disconnect ~self_sync
          ~clock_safe ~debug ~logfile ~user_agent url
      in
      if active then
        ignore
          (new Output.dummy
             ~register_commands:false ~kind ~autostart:true ~infallible:false
             ~on_start:(fun _ -> ())
             ~on_stop:(fun _ -> ())
             (Lang.source (source :> Source.source)));
      source)
