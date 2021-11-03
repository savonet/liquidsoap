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

exception Not_connected

let normalize_metadata =
  List.map (fun (lbl, v) ->
      let lbl =
        match lbl with
          | "StreamTitle" -> "title"
          | "StreamUrl" -> "url"
          | _ -> lbl
      in
      (lbl, v))

class input ?(name = "input.ffmpeg") ~autostart ~self_sync ~poll_delay ~debug
  ~clock_safe ~max_buffer ~log_overfull ~kind ~on_stop ~on_start ~on_connect
  ~on_disconnect ~new_track_on_metadata ?format ~opts url =
  let max_ticks = Frame.main_of_seconds max_buffer in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let generator =
    Generator.create ~log ~log_overfull ~overfull:(`Drop_old max_ticks)
      `Undefined
  in
  object (self)
    inherit
      Start_stop.active_source
        ~name ~content_kind:kind ~fallible:true ~clock_safe ~on_start ~on_stop
          ~autostart () as super

    val mutable connect_task = None
    val mutable container = None
    val generator = generator
    method remaining = -1
    method abort_track = Generator.add_break generator

    method is_ready =
      super#is_ready && self#mutexify (fun () -> container <> None) ()

    method self_sync =
      (`Dynamic, self_sync && self#mutexify (fun () -> container <> None) ())

    method private start = self#connect
    method private stop = self#disconnect
    val mutable url = url
    method url = url ()
    method set_url u = url <- u
    method buffer_length = Frame.seconds_of_audio (Generator.length generator)

    val mutable source_status : [ `Stopped | `Polling | `Connected of string ] =
      `Stopped

    method source_status = source_status

    method private connect_fn () =
      try
        source_status <- `Polling;
        let opts = Hashtbl.copy opts in
        let url = self#url in
        let input = Av.open_input ?format ~opts url in
        if Hashtbl.length opts > 0 then
          failwith
            (Printf.sprintf "Unrecognized options: %s"
               (Ffmpeg_format.string_of_options opts));
        let content_type =
          Ffmpeg_decoder.get_type ~ctype:self#ctype ~url input
        in
        if not (Decoder.can_decode_type content_type self#ctype) then
          failwith
            (Printf.sprintf "url %S cannot produce content of type %s" url
               (Frame.string_of_content_type self#ctype));
        let audio, video = Ffmpeg_decoder.mk_streams ~ctype:self#ctype input in
        let decoder =
          Ffmpeg_decoder.mk_decoder ?audio ?video ~decode_first_metadata:true
            ~target_position:(ref None) input
        in
        let get_metadata () =
          normalize_metadata
            (Av.get_input_metadata input
            @ (match audio with
                | Some (`Packet (_, stream, _)) -> Av.get_metadata stream
                | Some (`Frame (_, stream, _)) -> Av.get_metadata stream
                | None -> [])
            @
            match video with
              | Some (`Packet (_, stream, _)) -> Av.get_metadata stream
              | Some (`Frame (_, stream, _)) -> Av.get_metadata stream
              | None -> [])
        in
        container <- Some (input, decoder, get_metadata);
        source_status <- `Connected url;
        on_connect input;
        -1.
      with e ->
        self#log#info "Connection failed: %s" (Printexc.to_string e);
        self#disconnect;
        if debug then raise e;
        poll_delay

    method private connect =
      self#mutexify
        (fun () ->
          if container = None then (
            match connect_task with
              | Some t -> Duppy.Async.wake_up t
              | None ->
                  let t =
                    Duppy.Async.add ~priority:`Blocking Tutils.scheduler
                      self#connect_fn
                  in
                  connect_task <- Some t;
                  Duppy.Async.wake_up t))
        ()

    method private disconnect =
      self#mutexify
        (fun () ->
          match container with
            | None -> ()
            | Some (input, _, _) ->
                (try Av.close input
                 with exn ->
                   let bt = Printexc.get_backtrace () in
                   Utils.log_exception ~log:self#log ~bt
                     (Printf.sprintf "Error while disconnecting: %s"
                        (Printexc.to_string exn)));
                container <- None;
                source_status <- `Stopped;
                on_disconnect ())
        ()

    method private reconnect =
      self#disconnect;
      self#connect

    val mutable last_metadata = []

    method private get_frame frame =
      let pos = Frame.position frame in
      try
        let _, decoder, get_metadata =
          self#mutexify (fun () -> Option.get container) ()
        in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while Generator.length generator < Lazy.force Frame.size do
          decoder buffer
        done;
        Generator.fill generator frame;
        let m = get_metadata () in
        if last_metadata <> m then (
          let meta = Hashtbl.create (List.length m) in
          List.iter (fun (lbl, v) -> Hashtbl.replace meta lbl v) m;
          Generator.add_metadata generator meta;
          if new_track_on_metadata then Generator.add_break generator;
          last_metadata <- m)
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Feeding failed: %s" (Printexc.to_string exn));
        Frame.add_break frame pos;
        self#reconnect

    method wake_up act =
      super#wake_up act;
      (* Now we can create the log function *)
      log_ref := fun s -> self#log#important "%s" s
  end

let http_log = Log.make ["input"; "http"]

class http_input ~autostart ~self_sync ~poll_delay ~debug ~clock_safe
  ~max_buffer ~log_overfull ~kind ~on_connect ~on_disconnect ?format ~opts
  ~user_agent ~timeout ~on_start ~on_stop ~new_track_on_metadata url =
  let () =
    Hashtbl.replace opts "icy" (`Int 1);
    Hashtbl.replace opts "user_agent" (`String user_agent);
    Hashtbl.replace opts "rw_timeout"
      (`Int64 (Int64.of_float (timeout *. 1000000.)))
  in
  let on_connect input =
    let icy_headers =
      try
        let icy_headers =
          Avutil.Options.get_string ~search_children:true
            ~name:"icy_metadata_headers" (Av.input_obj input)
        in
        let icy_headers = Pcre.split ~rex:(Pcre.regexp "[\r]?\n") icy_headers in
        List.fold_left
          (fun ret header ->
            if header <> "" then (
              try
                let res = Pcre.exec ~pat:"([^:]*):\\s*(.*)" header in
                (Pcre.get_substring res 1, Pcre.get_substring res 2) :: ret
              with Not_found -> ret)
            else ret)
          [] icy_headers
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:http_log ~bt
          (Printf.sprintf "Error while fetching icy headers: %s"
             (Printexc.to_string exn));
        []
    in
    on_connect icy_headers
  in
  object
    inherit
      input
        ~name:"input.http" ~autostart ~self_sync ~poll_delay ~debug ~clock_safe
          ~max_buffer ~log_overfull ~kind ~on_stop ~on_start ~on_disconnect
          ~on_connect ?format ~opts ~new_track_on_metadata url
  end

let parse_args ~t name p opts =
  let name = name ^ "_args" in
  let args = List.assoc name p in
  let args = Lang.to_list args in
  let extract_pair extractor v =
    let label, value = Lang.to_product v in
    Hashtbl.replace opts (Lang.to_string label) (extractor value)
  in
  let extract =
    match t with
      | `Int -> fun v -> extract_pair (fun v -> `Int (Lang.to_int v)) v
      | `Float -> fun v -> extract_pair (fun v -> `Float (Lang.to_float v)) v
      | `String -> fun v -> extract_pair (fun v -> `String (Lang.to_string v)) v
  in
  List.iter extract args

let register_input is_http =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  let args ?t name =
    let t =
      match t with
        | Some t -> Lang.product_t Lang.string_t t
        | None -> Lang.string_t
    in
    (name ^ "_args", Lang.list_t t, Some (Lang.list []), None)
  in
  let name, descr =
    if is_http then ("input.http", "Create a http stream using ffmpeg")
    else ("input.ffmpeg", "Create a stream using ffmpeg")
  in
  Lang.add_operator name ~descr ~category:`Input
    (Start_stop.active_source_proto ~clock_safe:false ~fallible_opt:`Nope
    @ (if is_http then
       [
         ( "user_agent",
           Lang.string_t,
           Some (Lang.string Http.user_agent),
           Some "User agent." );
         ( "timeout",
           Lang.float_t,
           Some (Lang.float 10.),
           Some "Timeout for source connection." );
       ]
      else [])
    @ (if is_http then
       [
         ( "on_connect",
           Lang.fun_t [(false, "", Lang.metadata_t)] Lang.unit_t,
           Some (Lang.val_cst_fun [("", None)] Lang.unit),
           Some
             "Function to execute when a source is connected. Its receives the \
              list of ICY-specific headers, if available." );
       ]
      else
        [
          ( "on_connect",
            Lang.fun_t [] Lang.unit_t,
            Some (Lang.val_cst_fun [] Lang.unit),
            Some "Function to execute when a source is connected." );
        ])
    @ [
        args ~t:Lang.int_t "int";
        args ~t:Lang.float_t "float";
        args ~t:Lang.string_t "string";
        ( "new_track_on_metadata",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Treat new metadata as new track." );
        ( "on_disconnect",
          Lang.fun_t [] Lang.unit_t,
          Some (Lang.val_cst_fun [] Lang.unit),
          Some "Function to excecute when a source is disconnected" );
        ( "max_buffer",
          Lang.float_t,
          Some (Lang.float 5.),
          Some "Maximum uration of buffered data" );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool false),
          Some
            "Should the source control its own timing? Set to `true` if you \
             are having synchronization issues. Should be `false` for most \
             typical cases." );
        ( "debug",
          Lang.bool_t,
          Some (Lang.bool false),
          Some "Run in debugging mode, not catching some exceptions." );
        ( "poll_delay",
          Lang.float_t,
          Some (Lang.float 2.),
          Some "Polling delay when trying to connect to the stream." );
        ( "log_overfull",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Log when the source's buffer is overfull." );
        ( "format",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some
            "Force a specific input format. Autodetected when passed a null \
             argument" );
        ("", Lang.getter_t Lang.string_t, None, Some "URL to decode.");
      ])
    ~return_t:k
    ~meth:
      Lang.(
        Start_stop.meth ()
        @ [
            ( "url",
              ([], fun_t [] string_t),
              "Return the source's current url.",
              fun s -> val_fun [] (fun _ -> string s#url) );
            ( "set_url",
              ([], fun_t [(false, "", getter_t string_t)] unit_t),
              "Set the source's url.",
              fun s ->
                val_fun [("", "", None)] (fun p ->
                    s#set_url (to_string_getter (List.assoc "" p));
                    unit) );
            ( "status",
              ([], fun_t [] string_t),
              "Return the current status of the source, either \"stopped\" \
               (the source isn't trying to relay the HTTP stream), \"polling\" \
               (attempting to connect to the HTTP stream) or \"connected \
               <url>\" (connected to <url>, buffering or playing back the \
               stream).",
              fun s ->
                val_fun [] (fun _ ->
                    string
                      (match s#source_status with
                        | `Stopped -> "stopped"
                        | `Polling -> "polling"
                        | `Connected url -> Printf.sprintf "connected %s" url))
            );
            ( "buffer_length",
              ([], fun_t [] float_t),
              "Get the buffer's length in seconds.",
              fun s -> val_fun [] (fun _ -> float s#buffer_length) );
          ])
    (fun p ->
      let format = Lang.to_option (List.assoc "format" p) in
      let format =
        Option.map
          (fun format ->
            let format = Lang.to_string format in
            match Av.Format.find_input_format format with
              | Some f -> f
              | None ->
                  raise
                    (Error.Invalid_value
                       ( Lang.string format,
                         "Could not find ffmpeg input format with that name" )))
          format
      in
      let opts = Hashtbl.create 10 in
      parse_args ~t:`Int "int" p opts;
      parse_args ~t:`Float "float" p opts;
      parse_args ~t:`String "string" p opts;
      let max_buffer = Lang.to_float (List.assoc "max_buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let debug = Lang.to_bool (List.assoc "debug" p) in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun _ -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_disconnect () =
        ignore (Lang.apply (List.assoc "on_disconnect" p) [])
      in
      let new_track_on_metadata =
        Lang.to_bool (List.assoc "new_track_on_metadata" p)
      in
      let poll_delay = Lang.to_float (List.assoc "poll_delay" p) in
      let url = Lang.to_string_getter (Lang.assoc "" 1 p) in
      let kind = Kind.of_kind kind in
      if is_http then (
        let timeout = Lang.to_float (List.assoc "timeout" p) in
        let user_agent = Lang.to_string (List.assoc "user_agent" p) in
        let on_connect l =
          let l =
            List.map
              (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
              l
          in
          let arg = Lang.list l in
          ignore (Lang.apply (List.assoc "on_connect" p) [("", arg)])
        in
        (new http_input
           ~kind ~debug ~autostart ~self_sync ~clock_safe ~poll_delay
           ~on_connect ~on_disconnect ~user_agent ~new_track_on_metadata
           ~max_buffer ~log_overfull ?format ~opts ~timeout ~on_start ~on_stop
           url
          :> input))
      else (
        let on_connect _ = ignore (Lang.apply (List.assoc "on_connect" p) []) in
        new input
          ~kind ~autostart ~debug ~self_sync ~clock_safe ~poll_delay ~on_start
          ~on_stop ~on_connect ~on_disconnect ~max_buffer ~log_overfull ?format
          ~opts ~new_track_on_metadata url))

let () =
  register_input true;
  register_input false
