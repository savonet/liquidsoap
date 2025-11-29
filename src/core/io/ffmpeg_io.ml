(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

module Http = Liq_http

module Metadata = struct
  include Map.Make (struct
    type t = string

    let compare = String.compare
  end)

  let of_metadata m = List.fold_left (fun m (k, v) -> add k v m) empty m
  let equal = equal String.equal
  let to_metadata = bindings
end

let normalize_metadata =
  List.map (fun (lbl, v) ->
      let lbl =
        match lbl with
          | "StreamTitle" -> "title"
          | "StreamUrl" -> "url"
          | _ -> lbl
      in
      let v = try Charset.convert ~target:Charset.utf8 v with _ -> v in
      (lbl, v))

exception Stopped

type container = {
  input : Avutil.input Avutil.container;
  decoder : Decoder.buffer -> unit;
  buffer : Decoder.buffer;
  get_metadata : unit -> (string * string) list;
  closed : bool Atomic.t;
}

let shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"input.ffmpeg shutdown" (fun () ->
      Atomic.set shutdown true)

class input ?(name = "input.ffmpeg") ~autostart ~self_sync ~poll_delay ~debug
  ~max_buffer ~metadata_filter ~new_track_on_metadata ?format ~opts ~trim_url
  url =
  let max_length = Some (Frame.main_of_seconds max_buffer) in
  object (self)
    inherit Start_stop.active_source ~name ~fallible:true ~autostart () as super
    val connect_task = Atomic.make None
    method effective_source = (self :> Source.source)
    method remaining = -1
    method abort_track = Generator.add_track_mark self#buffer

    val source_status
        : [ `Stopped
          | `Starting
          | `Polling
          | `Connected of string * container
          | `Stopping ]
          Atomic.t =
      Atomic.make `Stopped

    method source_status = Atomic.get source_status

    method private is_connected =
      match Atomic.get source_status with `Connected _ -> true | _ -> false

    method can_generate_frame = super#started && self#is_connected

    method private get_self_sync =
      match self_sync () with Some v -> v | None -> false

    method self_sync =
      (`Dynamic, self#source_sync (self#get_self_sync && self#is_connected))

    val mutable on_connect = []
    method on_connect fn = on_connect <- on_connect @ [fn]
    method on_connect_metadata_map _ : (string * string) list = []
    val mutable on_disconnect = []
    method on_disconnect fn = on_disconnect <- on_disconnect @ [fn]
    val mutable on_error = []
    method on_error fn = on_error <- on_error @ [fn]
    method private start = self#connect
    method private stop = self#disconnect
    val mutable url = url

    method url =
      let u = url () in
      if trim_url then String.trim u else u

    method set_url u = url <- u
    method buffer_length = Frame.seconds_of_audio (Generator.length self#buffer)

    method private connect_task () =
      Generator.set_max_length self#buffer max_length;
      try
        if self#source_status = `Stopping then raise Stopped;
        assert (self#source_status = `Starting);
        Atomic.set source_status `Polling;
        let opts = Hashtbl.copy opts in
        let url = self#url in
        let closed = Atomic.make false in
        let input =
          Av.open_input
            ~interrupt:(fun () -> Atomic.get shutdown || Atomic.get closed)
            ?format ~opts url
        in
        if Hashtbl.length opts > 0 then
          failwith
            (Printf.sprintf "Unrecognized options: %s"
               (Ffmpeg_format.string_of_options opts));
        let content_type =
          Ffmpeg_decoder.get_type ~format ~ctype:self#content_type ~url input
        in
        if not (Decoder.can_decode_type content_type self#content_type) then
          failwith
            (Printf.sprintf "url %S cannot produce content of type %s" url
               (Frame.string_of_content_type self#content_type));
        let streams =
          Ffmpeg_decoder.mk_streams ~ctype:self#content_type
            ~decode_first_metadata:true input
        in
        let decoder =
          Ffmpeg_decoder.mk_decoder ~streams ~target_position:(ref None) input
        in
        let buffer = Decoder.mk_buffer ~ctype:self#content_type self#buffer in
        (* FFmpeg has memory leaks with chained ogg stream so we manually
           reset the metadata after fetching it. *)
        let get_metadata stream =
          let m = Av.get_metadata stream in
          Av.set_metadata stream [];
          m
        in
        let get_metadata () =
          normalize_metadata
            (Ffmpeg_decoder.Streams.fold
               (fun _ stream m ->
                 m
                 @
                   match stream with
                   | `Audio_frame (stream, _) -> get_metadata stream
                   | `Audio_packet (stream, _) -> get_metadata stream
                   | `Video_frame (stream, _) -> get_metadata stream
                   | `Video_packet (stream, _) -> get_metadata stream
                   | `Data_packet _ -> [])
               streams
               (Av.get_input_metadata input))
        in
        let last_meta = ref [] in
        let get_metadata () =
          let m = get_metadata () in
          if m <> !last_meta then (
            last_meta := m;
            m)
          else []
        in
        let m = self#on_connect_metadata_map input in
        List.iter (fun fn -> fn m) on_connect;
        Generator.add_track_mark self#buffer;
        let container = { input; decoder; buffer; get_metadata; closed } in
        Atomic.set source_status (`Connected (url, container));
        -1.
      with
        | Stopped ->
            Atomic.set source_status `Stopped;
            -1.
        | e ->
            let bt = Printexc.get_raw_backtrace () in
            Utils.log_exception ~log:self#log
              ~bt:(Printexc.raw_backtrace_to_string bt)
              (Printf.sprintf "Decoding failed: %s" (Printexc.to_string e));
            let err = Lang.runtime_error_of_exception ~bt ~kind:"ffmpeg" e in
            List.iter (fun fn -> fn err) on_error;
            if debug then Printexc.raise_with_backtrace e bt;
            Atomic.set source_status `Starting;
            poll_delay

    method private connect =
      match self#source_status with
        | `Starting | `Polling | `Connected _ -> ()
        | `Stopping | `Stopped -> (
            Atomic.set source_status `Starting;
            match Atomic.get connect_task with
              | Some t -> Duppy.Async.wake_up t
              | None ->
                  let t =
                    Duppy.Async.add ~priority:`Blocking Tutils.scheduler
                      self#connect_task
                  in
                  Atomic.set connect_task (Some t);
                  Duppy.Async.wake_up t)

    method private disconnect =
      let stop_task () =
        match Atomic.get connect_task with
          | None -> Atomic.set source_status `Stopped
          | Some t ->
              Atomic.set source_status `Stopping;
              Duppy.Async.wake_up t
      in
      match self#source_status with
        | `Stopping | `Stopped -> ()
        | `Polling | `Starting -> stop_task ()
        | `Connected (_, { input; closed }) ->
            Atomic.set closed true;
            self#mutexify
              (fun () ->
                try Av.close input
                with exn ->
                  let bt = Printexc.get_backtrace () in
                  Utils.log_exception ~log:self#log ~bt
                    (Printf.sprintf "Error while disconnecting: %s"
                       (Printexc.to_string exn)))
              ();
            List.iter (fun fn -> fn ()) on_disconnect;
            stop_task ()

    method private reconnect =
      match self#source_status with
        | `Stopping | `Stopped | `Polling | `Starting -> ()
        | `Connected _ ->
            self#disconnect;
            self#connect

    method private get_connected_container =
      match self#source_status with
        | `Connected (_, c) -> c
        | _ -> raise Not_connected

    method private generate_frame =
      let size = Lazy.force Frame.size in
      try
        let { decoder; buffer; closed } = self#get_connected_container in
        while Generator.length self#buffer < Lazy.force Frame.size do
          if Atomic.get shutdown || Atomic.get closed then raise Not_connected;
          self#mutexify (fun () -> decoder buffer) ()
        done;
        let { get_metadata } = self#get_connected_container in
        let meta = get_metadata () in
        if meta <> [] then (
          Generator.add_metadata self#buffer (Frame.Metadata.from_list meta);
          if new_track_on_metadata then Generator.add_track_mark self#buffer);
        let frame = Generator.slice self#buffer size in
        (* Metadata can be added by the decoder and the demuxer so we filter at the frame level. *)
        let metadata =
          List.fold_left
            (fun metadata (p, m) ->
              let m = metadata_filter m in
              if 0 < Frame.Metadata.cardinal m then (p, m) :: metadata
              else metadata)
            []
            (Frame.get_all_metadata frame)
        in
        Frame.add_all_metadata frame metadata
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Utils.log_exception ~log:self#log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Feeding failed: %s" (Printexc.to_string exn));
        let err = Lang.runtime_error_of_exception ~bt ~kind:"ffmpeg" exn in
        List.iter (fun fn -> fn err) on_error;
        self#reconnect;
        Frame.append (Generator.slice self#buffer size) self#end_of_track
  end

let http_log = Log.make ["input"; "http"]

class http_input ~autostart ~self_sync ~poll_delay ~debug ~max_buffer ?format
  ~opts ~user_agent ~timeout ~metadata_filter ~new_track_on_metadata ~trim_url
  url =
  let () =
    Hashtbl.replace opts "icy" (`Int 1);
    Hashtbl.replace opts "user_agent" (`String user_agent);
    Hashtbl.replace opts "rw_timeout"
      (`Int64 (Int64.of_float (timeout *. 1000000.)))
  in
  let is_icy = Atomic.make false in
  let self_sync () =
    match (self_sync (), Atomic.get is_icy) with
      | Some v, _ -> Some v
      | None, v -> Some v
  in
  object (self)
    inherit
      input
        ~name:"input.http" ~autostart ~self_sync ~poll_delay ~debug ~max_buffer
          ~metadata_filter ?format ~opts ~new_track_on_metadata ~trim_url url

    method! on_connect_metadata_map input =
      let icy_headers =
        try
          let icy_headers =
            Avutil.Options.get_string ~search_children:true
              ~name:"icy_metadata_headers" (Av.input_obj input)
          in
          let icy_headers =
            Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r]?\n") icy_headers
          in
          List.fold_left
            (fun ret header ->
              if header <> "" then (
                try
                  let res =
                    Re.Pcre.exec ~rex:(Re.Pcre.regexp "([^:]*):\\s*(.*)") header
                  in
                  (Re.Pcre.get_substring res 1, Re.Pcre.get_substring res 2)
                  :: ret
                with Not_found -> ret)
              else ret)
            [] icy_headers
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log:self#log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Error while fetching icy headers: %s"
               (Printexc.to_string exn));
          let err = Lang.runtime_error_of_exception ~bt ~kind:"ffmpeg" exn in
          List.iter (fun fn -> fn err) on_error;
          []
      in
      Atomic.set is_icy (icy_headers <> []);
      icy_headers
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
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let args ?t name =
    let t =
      match t with
        | Some t -> Lang.product_t Lang.string_t t
        | None -> Lang.string_t
    in
    (name ^ "_args", Lang.list_t t, Some (Lang.list []), None)
  in
  let name, descr =
    if is_http then ("http", "Create a http stream using ffmpeg")
    else ("ffmpeg", "Create a stream using ffmpeg")
  in
  ignore
    (Lang.add_operator ~base:Modules.input name ~descr ~category:`Input
       (Start_stop.active_source_proto ~fallible_opt:`Nope
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
       @ [
           args ~t:Lang.int_t "int";
           args ~t:Lang.float_t "float";
           args ~t:Lang.string_t "string";
           ( "metadata_filter",
             Lang.nullable_t
               (Lang.fun_t [(false, "", Lang.metadata_t)] Lang.metadata_t),
             Some Lang.null,
             Some
               "Metadata filter function. Returned metadata are set a \
                metadata. Default: filter `id3v2_priv` metadata." );
           ( "deduplicate_metadata",
             Lang.bool_t,
             Some (Lang.bool true),
             Some "Prevent duplicated metadata." );
           ( "new_track_on_metadata",
             Lang.bool_t,
             Some (Lang.bool true),
             Some "Treat new metadata as new track." );
           ( "max_buffer",
             Lang.float_t,
             Some (Lang.float 5.),
             Some "Maximum uration of buffered data" );
           (if is_http then
              ( "self_sync",
                Lang.getter_t (Lang.nullable_t Lang.bool_t),
                Some Lang.null,
                Some
                  "Should the source control its own timing? If `null`, the \
                   source will control its latency if it can be detected that \
                   it is connecting to an `icecast` or `shoutcast` server. \
                   Otherwise, see `input.ffmpeg` for more details about this \
                   option." )
            else
              ( "self_sync",
                Lang.getter_t Lang.bool_t,
                Some (Lang.bool false),
                Some
                  "Should the source control its own timing? Set to `true` if \
                   you are having synchronization issues. Should be `false` \
                   for most typical cases." ));
           ( "debug",
             Lang.bool_t,
             Some (Lang.bool false),
             Some "Run in debugging mode, not catching some exceptions." );
           ( "poll_delay",
             Lang.float_t,
             Some (Lang.float 2.),
             Some "Polling delay when trying to connect to the stream." );
           ( "format",
             Lang.nullable_t Lang.string_t,
             Some Lang.null,
             Some
               "Force a specific input format. Autodetected when passed a null \
                argument" );
           ( "trim_url",
             Lang.bool_t,
             Some (Lang.bool true),
             Some "Trim input URL." );
           ("", Lang.getter_t Lang.string_t, None, Some "URL to decode.");
         ])
       ~return_t
       ~callbacks:
         (Start_stop.callbacks ~label:"source"
         @ (if is_http then
              [
                {
                  Lang_source.name = "on_connect";
                  params = [];
                  descr =
                    "when a source is connected. Its receives the list of \
                     ICY-specific headers, if available.";
                  register_deprecated_argument = true;
                  arg_t = [(false, "", Lang.metadata_t)];
                  register =
                    (fun ~params:_ s on_connect ->
                      let on_connect m =
                        on_connect [("", Lang.metadata_list m)]
                      in
                      s#on_connect on_connect);
                };
              ]
            else
              [
                {
                  name = "on_connect";
                  params = [];
                  descr = "Function to execute when a source is connected.";
                  register_deprecated_argument = true;
                  arg_t = [];
                  register =
                    (fun ~params:_ s on_connect ->
                      let on_connect _ = on_connect [] in
                      s#on_connect on_connect);
                };
              ])
         @ [
             {
               name = "on_disconnect";
               params = [];
               descr = "when a source is disconnected.";
               register_deprecated_argument = true;
               arg_t = [];
               register =
                 (fun ~params:_ s f -> s#on_disconnect (fun () -> f []));
             };
             {
               name = "on_error";
               params = [];
               descr = "when an error occurs.";
               register_deprecated_argument = true;
               arg_t = [(false, "", Lang.error_t)];
               register =
                 (fun ~params:_ s f ->
                   s#on_error (fun err -> f [("", Lang.error err)]));
             };
           ])
       ~meth:
         Lang.(
           Start_stop.meth ()
           @ [
               {
                 name = "url";
                 scheme = ([], fun_t [] string_t);
                 descr = "Return the source's current url.";
                 value = (fun s -> val_fun [] (fun _ -> string s#url));
               };
               {
                 name = "set_url";
                 scheme = ([], fun_t [(false, "", getter_t string_t)] unit_t);
                 descr = "Set the source's url.";
                 value =
                   (fun s ->
                     val_fun
                       [("", "", None)]
                       (fun p ->
                         s#set_url (to_string_getter (List.assoc "" p));
                         unit));
               };
               {
                 name = "status";
                 scheme = ([], fun_t [] string_t);
                 descr =
                   "Return the current status of the source, either \
                    \"stopped\" (the source isn't trying to relay the HTTP \
                    stream), \"starting\" (polling task is about to begin) \
                    \"polling\" (attempting to connect to the HTTP stream), \
                    \"connected <url>\" (connected to <url>, buffering or \
                    playing back the stream) or \"stopping\" (source is \
                    stopping).";
                 value =
                   (fun s ->
                     val_fun [] (fun _ ->
                         string
                           (match s#source_status with
                             | `Stopped -> "stopped"
                             | `Starting -> "starting"
                             | `Stopping -> "stopping"
                             | `Polling -> "polling"
                             | `Connected (url, _) ->
                                 Printf.sprintf "connected %s" url)));
               };
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
                            "Could not find ffmpeg input format with that name"
                          )))
             format
         in
         let opts = Hashtbl.create 10 in
         parse_args ~t:`Int "int" p opts;
         parse_args ~t:`Float "float" p opts;
         parse_args ~t:`String "string" p opts;
         let max_buffer = Lang.to_float (List.assoc "max_buffer" p) in
         let debug = Lang.to_bool (List.assoc "debug" p) in
         let self_sync = Lang.to_getter (List.assoc "self_sync" p) in
         let self_sync () =
           if is_http then Lang.to_valued_option Lang.to_bool (self_sync ())
           else Some (Lang.to_bool (self_sync ()))
         in
         let autostart = Lang.to_bool (List.assoc "start" p) in
         let metadata_filter =
           match Lang.to_option (List.assoc "metadata_filter" p) with
             | Some fn ->
                 fun m ->
                   Lang.to_metadata_list
                     (Lang.apply fn [("", Lang.metadata_list m)])
             | None ->
                 List.filter (fun (k, _) ->
                     not (Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "^id3v2_priv") k))
         in
         let deduplicate_metadata =
           Lang.to_bool (List.assoc "deduplicate_metadata" p)
         in
         let metadata_filter =
           if not deduplicate_metadata then metadata_filter
           else (
             let last_meta = ref Metadata.empty in
             fun m ->
               let m = metadata_filter m in
               let m' = Metadata.of_metadata m in
               if m = [] || Metadata.equal !last_meta m' then []
               else (
                 last_meta := m';
                 m))
         in
         let metadata_filter m =
           let m = metadata_filter (Frame.Metadata.to_list m) in
           Frame.Metadata.from_list m
         in
         let new_track_on_metadata =
           Lang.to_bool (List.assoc "new_track_on_metadata" p)
         in
         let poll_delay = Lang.to_float (List.assoc "poll_delay" p) in
         let url = Lang.to_string_getter (Lang.assoc "" 1 p) in
         let trim_url = Lang.to_bool (List.assoc "trim_url" p) in
         if is_http then (
           let timeout = Lang.to_float (List.assoc "timeout" p) in
           let user_agent = Lang.to_string (List.assoc "user_agent" p) in
           (new http_input
              ~metadata_filter ~debug ~autostart ~self_sync ~poll_delay
              ~user_agent ~new_track_on_metadata ~max_buffer ?format ~opts
              ~timeout ~trim_url url
             :> input))
         else
           new input
             ~metadata_filter ~autostart ~debug ~self_sync ~poll_delay
             ~max_buffer ?format ~opts ~new_track_on_metadata ~trim_url url))

let () =
  register_input true;
  register_input false
