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

module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

class input ~self_sync ~poll_delay ~debug ~clock_safe ~bufferize ~log_overfull
  ~kind ~on_stop ~on_start ?format ~opts url =
  let max_ticks = 2 * Frame.main_of_seconds bufferize in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let generator =
    Generator.create ~log ~log_overfull ~overfull:(`Drop_old max_ticks)
      `Undefined
  in
  object (self)
    inherit Source.active_source ~name:"input.ffmpeg" kind as super

    val mutable connect_task = None

    val mutable container = None

    val mutable clock = None

    val generator = generator

    (* Regular source methods. *)
    method stype = Source.Fallible

    method seek _ = 0

    method remaining = -1

    method abort_track = Generator.add_break generator

    method is_ready = self#mutexify (fun () -> container <> None) ()

    method self_sync = self_sync && self#is_ready

    (* Active source methods. *)
    method is_active = self#is_ready

    method output =
      if self#is_ready && AFrame.is_partial self#memo then
        self#get_frame self#memo

    method output_reset = self#reconnect

    method output_get_ready = ()

    method private connect_fn () =
      try
        let opts = Hashtbl.copy opts in
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
          Ffmpeg_decoder.mk_decoder ?audio ?video ~target_position:(ref None)
            input
        in
        container <- Some (input, decoder);
        on_start input;
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
                    Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler
                      self#connect_fn
                  in
                  connect_task <- Some t;
                  Duppy.Async.wake_up t ))
        ()

    method private disconnect =
      self#mutexify
        (fun () ->
          match container with
            | None -> ()
            | Some (input, _) ->
                Av.close input;
                container <- None;
                on_stop ())
        ()

    method private reconnect =
      self#disconnect;
      self#connect

    method private get_frame frame =
      let pos = Frame.position frame in
      try
        let _, decoder = self#mutexify (fun () -> Option.get container) () in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while Generator.length generator < Lazy.force Frame.size do
          decoder buffer
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
            let c = new Clock.clock "input.ffmpeg" in
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
      self#disconnect;
      super#sleep
  end

let http_log = Log.make ["input"; "http"]

class http_input ~self_sync ~poll_delay ~debug ~clock_safe ~bufferize
  ~log_overfull ~kind ~on_connect ~on_disconnect ?format ~opts ~user_agent
  ~new_track_on_metadata url =
  let () =
    Hashtbl.add opts "icy" (`Int 1);
    Hashtbl.add opts "user_agent" (`String user_agent)
  in
  let on_start input =
    try
      let icy_headers =
        Avutil.Options.get_string ~search_children:true
          ~name:"icy_metadata_headers" (Av.input_obj input)
      in
      let icy_headers = Pcre.split ~rex:(Pcre.regexp "[\r]?\n") icy_headers in
      let icy_headers =
        List.fold_left
          (fun ret header ->
            if header <> "" then (
              try
                let res = Pcre.exec ~pat:"([^:]*):\\s*(.*)" header in
                (Pcre.get_substring res 1, Pcre.get_substring res 2) :: ret
              with Not_found -> ret )
            else ret)
          [] icy_headers
      in
      on_connect icy_headers
    with exn ->
      let bt = Printexc.get_backtrace () in
      Utils.log_exception ~log:http_log ~bt
        (Printf.sprintf "Error while fetching icy headers: %s"
           (Printexc.to_string exn))
  in
  object (self)
    inherit
      input
        ~self_sync ~poll_delay ~debug ~clock_safe ~bufferize ~log_overfull ~kind
          ~on_stop:on_disconnect ~on_start ?format ~opts url as super

    val mutable latest_metadata = ""

    method private insert_metadata chunk =
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
      self#log#important "New metadata chunk: %s -- %s."
        (try Hashtbl.find h "artist" with _ -> "?")
        (try Hashtbl.find h "title" with _ -> "?");
      Generator.add_metadata generator h;
      if new_track_on_metadata then Generator.add_break generator

    method get_frame frame =
      super#get_frame frame;
      try
        let input, _ = self#mutexify (fun () -> Option.get container) () in
        let m =
          Avutil.Options.get_string ~search_children:true
            ~name:"icy_metadata_packet" (Av.input_obj input)
        in
        if latest_metadata <> m && m <> "" then (
          latest_metadata <- m;
          self#insert_metadata m )
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Error while fetching metadata: %s"
             (Printexc.to_string exn))
  end

let parse_args ~t name p opts =
  let name = name ^ "_args" in
  let args = List.assoc name p in
  let args = Lang.to_list args in
  let extract_pair extractor v =
    let label, value = Lang.to_product v in
    Hashtbl.add opts (Lang.to_string label) (extractor value)
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
  Lang.add_operator name ~active:true ~descr ~category:Lang.Input
    ( ( if is_http then
        [
          ( "user_agent",
            Lang.string_t,
            Some (Lang.string Http.user_agent),
            Some "User agent." );
          ( "new_track_on_metadata",
            Lang.bool_t,
            Some (Lang.bool true),
            Some "Treat new metadata as new track." );
          ( "on_connect",
            Lang.fun_t [(false, "", Lang.metadata_t)] Lang.unit_t,
            Some (Lang.val_cst_fun [("", None)] Lang.unit),
            Some
              "Function to execute when a source is connected. Its receives \
               the list of ICY-specific headers, if available." );
          ( "on_disconnect",
            Lang.fun_t [] Lang.unit_t,
            Some (Lang.val_cst_fun [] Lang.unit),
            Some "Function to excecute when a source is disconnected" );
        ]
      else
        [
          ( "on_start",
            Lang.fun_t [] Lang.unit_t,
            Some (Lang.val_cst_fun [] Lang.unit),
            Some "Callback executed when input starts." );
          ( "on_stop",
            Lang.fun_t [] Lang.unit_t,
            Some (Lang.val_cst_fun [] Lang.unit),
            Some "Callback executed when input stops." );
        ] )
    @ [
        args ~t:Lang.int_t "int";
        args ~t:Lang.float_t "float";
        args ~t:Lang.string_t "string";
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 5.),
          Some "Duration of buffered data before starting playout." );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some
            "Should the source control its own timing? Typically, should be \
             `true` for streaming protocols such as `rtmp` and `false` \
             otherwise." );
        ( "clock_safe",
          Lang.nullable_t Lang.bool_t,
          Some Lang.null,
          Some
            "Should the source be in its own clock. Should be the same value \
             as `self_sync` unless the source is mixed with other `clock_safe` \
             sources like `input.ao`" );
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
        ("", Lang.string_t, None, Some "URL to decode.");
      ] )
    ~return_t:k
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
                    (Lang_errors.Invalid_value
                       ( Lang.string format,
                         "Could not find ffmpeg input format with that name" )))
          format
      in
      let opts = Hashtbl.create 10 in
      parse_args ~t:`Int "int" p opts;
      parse_args ~t:`Float "float" p opts;
      parse_args ~t:`String "string" p opts;
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let debug = Lang.to_bool (List.assoc "debug" p) in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let clock_safe =
        Lang.to_default_option ~default:self_sync Lang.to_bool
          (List.assoc "clock_safe" p)
      in
      let poll_delay = Lang.to_float (List.assoc "poll_delay" p) in
      let url = Lang.to_string (Lang.assoc "" 1 p) in
      let kind = Source.Kind.of_kind kind in
      if is_http then (
        let user_agent = Lang.to_string (List.assoc "user_agent" p) in
        let new_track_on_metadata =
          Lang.to_bool (List.assoc "new_track_on_metadata" p)
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
        ( new http_input
            ~kind ~debug ~self_sync ~clock_safe ~poll_delay ~on_connect
            ~on_disconnect ~user_agent ~new_track_on_metadata ~bufferize
            ~log_overfull ?format ~opts url
          :> Source.source ) )
      else (
        let on_start =
          let f = List.assoc "on_start" p in
          fun _ -> ignore (Lang.apply f [])
        in
        let on_stop =
          let f = List.assoc "on_stop" p in
          fun () -> ignore (Lang.apply f [])
        in
        ( new input
            ~kind ~debug ~self_sync ~clock_safe ~poll_delay ~on_start ~on_stop
            ~bufferize ~log_overfull ?format ~opts url
          :> Source.source ) ))

let () =
  register_input true;
  register_input false
