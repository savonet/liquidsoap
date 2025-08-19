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

let request = Modules.request
let should_stop = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"builtin source shutdown" (fun () ->
      Atomic.set should_stop true)

let _ =
  Lang.add_builtin ~base:request "all" ~category:`Liquidsoap
    ~descr:"Return all the requests currently available." []
    (Lang.list_t Request.Value.t) (fun _ ->
      Lang.list (List.map Request.Value.to_value (Request.all ())))

let _ =
  Lang.add_builtin ~base:request "is_static" ~category:`Liquidsoap
    ~descr:"`true` if the given URI is assumed to be static, e.g. a file."
    [("", Lang.string_t, None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Request.is_static (Lang.to_string (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:request "create" ~category:`Liquidsoap
    ~descr:"Create a request from an URI."
    [
      ( "cue_in_metadata",
        Lang.nullable_t Lang.string_t,
        Some (Lang.string "liq_cue_in"),
        Some "Metadata for cue in points. Disabled if `null`." );
      ( "cue_out_metadata",
        Lang.nullable_t Lang.string_t,
        Some (Lang.string "liq_cue_out"),
        Some "Metadata for cue out points. Disabled if `null`." );
      ( "persistent",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is persistent, i.e. that it may be used \
           again once it has been played." );
      ( "resolve_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Set to `false` to prevent metadata resolution on this request." );
      ( "excluded_metadata_resolvers",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "List of metadata resolves to exclude when resolving metadata." );
      ( "temporary",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is a temporary file: it will be destroyed \
           after being played." );
      ("", Lang.string_t, None, None);
    ]
    Request.Value.t
    (fun p ->
      let persistent = Lang.to_bool (List.assoc "persistent" p) in
      let resolve_metadata = Lang.to_bool (List.assoc "resolve_metadata" p) in
      let excluded_metadata_resolvers =
        List.map Lang.to_string
          (Lang.to_list (List.assoc "excluded_metadata_resolvers" p))
      in
      let cue_in_metadata =
        Lang.to_valued_option Lang.to_string (List.assoc "cue_in_metadata" p)
      in
      let cue_out_metadata =
        Lang.to_valued_option Lang.to_string (List.assoc "cue_out_metadata" p)
      in
      let initial = Lang.to_string (List.assoc "" p) in
      let l = String.length initial in
      let initial =
        (* Remove trailing newline *)
        if l > 0 && initial.[l - 1] = '\n' then String.sub initial 0 (l - 1)
        else initial
      in
      let temporary = List.assoc "temporary" p |> Lang.to_bool in
      Request.Value.to_value
        (Request.create ~resolve_metadata ~persistent
           ~excluded_metadata_resolvers ~cue_in_metadata ~cue_out_metadata
           ~temporary initial))

let _ =
  Lang.add_builtin ~base:request "resolve" ~category:`Liquidsoap
    [
      ( "timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some
          "Limit in seconds to the duration of the request resolution. \
           Defaults to `settings.request.timeout` when `null`." );
      ( "content_type",
        Lang.nullable_t (Lang.source_t (Lang.univ_t ())),
        Some Lang.null,
        Some
          "Check that the request can decode content suitable for the given \
           source." );
      ("", Request.Value.t, None, None);
    ]
    Lang.bool_t
    ~descr:
      "Resolve a request, i.e. attempt to get a valid local file. The \
       operation can take some time. Return true if the resolving was \
       successful, false otherwise (timeout or invalid URI)."
    (fun p ->
      let timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
      in
      let source =
        Lang.to_valued_option Lang.to_source (List.assoc "content_type" p)
      in
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.bool
        (match (Request.resolve ?timeout r, source) with
          | `Resolved, Some s -> (
              try Request.get_decoder ~ctype:s#content_type r <> None
              with _ -> false)
          | `Resolved, None -> true
          | _ | (exception _) -> false))

let _ =
  Lang.add_builtin ~base:request "metadata" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.metadata_t ~descr:"Get the metadata associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.metadata (Request.metadata r))

let _ =
  Lang.add_builtin ~base:request "log" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.string_t ~descr:"Get log data associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (Request.log r))

let _ =
  Lang.add_builtin ~base:request "resolved" ~category:`Liquidsoap
    ~descr:
      "Check if a request is resolved, i.e. is associated to a valid local \
       file."
    [("", Request.Value.t, None, None)]
    Lang.bool_t
    (fun p ->
      let e = Request.Value.of_value (List.assoc "" p) in
      Lang.bool (Request.resolved e))

let _ =
  Lang.add_builtin ~base:request "uri" ~category:`Liquidsoap
    ~descr:"Initial URI of a request."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (Request.initial_uri r))

let _ =
  Lang.add_builtin ~base:request "filename" ~category:`Liquidsoap
    ~descr:
      "Return a valid local filename if the request is ready, and the empty \
       string otherwise."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (match Request.get_filename r with Some f -> f | None -> ""))

let _ =
  Lang.add_builtin ~base:request "destroy" ~category:`Liquidsoap
    ~descr:
      "Destroying a request causes any temporary associated file to be \
       deleted, and releases its RID. Persistent requests resist to \
       destroying, unless forced."
    [
      ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Destroy the request even if it is persistent." );
      ("", Request.Value.t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let force = Lang.to_bool (List.assoc "force" p) in
      let e = Request.Value.of_value (List.assoc "" p) in
      Request.destroy ~force e;
      Lang.unit)

let _ =
  let add_duration_resolver ~base ~name ~resolver () =
    Lang.add_builtin ~base name ~category:`Liquidsoap
      ((if resolver = None then
          [
            ( "resolvers",
              Lang.nullable_t (Lang.list_t Lang.string_t),
              Some Lang.null,
              Some
                "Set to a list of resolvers to only resolve duration using a \
                 specific decoder." );
          ]
        else [])
      @ [
          ( "resolve_metadata",
            Lang.bool_t,
            Some (Lang.bool true),
            Some
              "Set to `false` to prevent metadata resolution on this request."
          );
          ( "metadata",
            Lang.metadata_t,
            Some (Lang.list []),
            Some
              "Optional metadata used to decode the file, e.g. \
               `ffmpeg_options`." );
          ( "timeout",
            Lang.nullable_t Lang.float_t,
            Some Lang.null,
            Some
              "Limit in seconds to the duration of request resolution. \
               Defaults to `settings.request.timeout` when `null`." );
          ("", Lang.string_t, None, None);
        ])
      (Lang.nullable_t Lang.float_t)
      ~descr:
        (Printf.sprintf
           "Compute the duration in seconds of audio data contained in a \
            request%s. The computation may be expensive. Returns `null` if \
            computation failed, typically if the file was not recognized as \
            valid audio."
           (match resolver with
             | Some r -> " using the " ^ r ^ " decoder"
             | None -> ""))
      (fun p ->
        let f = Lang.to_string (List.assoc "" p) in
        let resolve_metadata = Lang.to_bool (List.assoc "resolve_metadata" p) in
        let resolvers =
          match resolver with
            | None ->
                Option.map (List.map Lang.to_string)
                  (Lang.to_valued_option Lang.to_list (List.assoc "resolvers" p))
            | Some r -> Some [r]
        in
        let metadata = Lang.to_metadata (List.assoc "metadata" p) in
        let timeout =
          Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
        in
        let r =
          Request.create ~resolve_metadata ~metadata ~cue_in_metadata:None
            ~cue_out_metadata:None f
        in
        if Request.resolve ?timeout r = `Resolved then (
          match
            Request.duration ?resolvers ~metadata:(Request.metadata r)
              (Option.get (Request.get_filename r))
          with
            | Some f -> Lang.float f
            | None -> Lang.null
            | exception exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"failure" exn)
        else Lang.null)
  in
  let base =
    add_duration_resolver ~base:request ~name:"duration" ~resolver:None ()
  in
  List.iter
    (fun name ->
      ignore (add_duration_resolver ~base ~name ~resolver:(Some name) ()))
    Request.conf_dresolvers#get

let _ =
  Lang.add_builtin ~base:request "id" ~category:`Liquidsoap
    ~descr:"Identifier of a request."
    [("", Request.Value.t, None, None)]
    Lang.int_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.int (Request.id r))

let _ =
  Lang.add_builtin ~base:request "status" ~category:`Liquidsoap
    ~descr:
      "Current status of a request. Can be idle, resolving, ready, playing or \
       destroyed."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      let s =
        match Request.status r with
          | `Idle -> "idle"
          | `Resolving _ -> "resolving"
          | `Ready -> "ready"
          | `Destroyed -> "destroyed"
          | `Failed -> "failed"
      in
      Lang.string s)

exception Process_failed

class process ~name r =
  object (self)
    inherit
      Request_dynamic.dynamic
        ~name
        ~retry_delay:(fun _ -> 0.1)
        ~available:(fun _ -> true)
        ~prefetch:1 ~timeout:None ~synchronous:true
        (Lang.val_fun [] (fun _ -> Lang.null))

    initializer
      self#on_wake_up (fun () ->
          match Request.get_decoder ~ctype:self#content_type r with
            | Some _ -> self#set_queue [r]
            | None | (exception _) -> raise Process_failed)
  end

let process_request ~log ~name ~ratio ~timeout ~sleep_latency ~process r =
  let module Time = (val Clock.time_implementation () : Liq_time.T) in
  let open Time in
  let start_time = Time.time () in
  match Request.resolve ~timeout r with
    | `Failed | `Timeout -> ()
    | `Resolved -> (
        let timeout = Time.of_float timeout in
        let timeout_time = Time.(start_time |+| timeout) in
        try
          let s = new process ~name r in
          let s = (process (s :> Source.source) :> Source.source) in
          let clock =
            Clock.create ~id:name
              ~sync:(`Passive (s :> Clock.passive_controller))
              ~on_error:(fun exn bt ->
                Utils.log_exception ~log
                  ~bt:(Printexc.raw_backtrace_to_string bt)
                  (Printf.sprintf "Error while processing source: %s"
                     (Printexc.to_string exn));
                raise Process_failed)
              ()
          in
          Fun.protect
            ~finally:(fun () -> try Clock.stop clock with _ -> ())
            (fun () ->
              let started = ref false in
              let stopped = ref false in
              let o =
                new Output.dummy
                  ~clock ~infallible:false ~register_telnet:false
                  ~autostart:true (Lang.source s)
              in
              o#on_start (fun () -> started := true);
              o#on_stop (fun () -> stopped := true);
              Clock.start ~force:true clock;
              log#info "Start streaming loop (ratio: %.02fx)" ratio;
              let sleep_latency = Time.of_float sleep_latency in
              let target_time () =
                Time.(
                  start_time |+| sleep_latency
                  |+| of_float (Clock.time clock /. ratio))
              in
              while (not (Atomic.get should_stop)) && not !stopped do
                if (not !started) && Time.(timeout_time |<=| Time.time ()) then (
                  log#important
                    "Timeout while waiting for the source to be ready!";
                  raise Process_failed)
                else (
                  Clock.tick clock;
                  let target_time = target_time () in
                  if Time.(time () |<| (target_time |+| sleep_latency)) then
                    sleep_until target_time)
              done;
              let processing_time = Time.(to_float (time () |-| start_time)) in
              let effective_ratio = Clock.time clock /. processing_time in
              log#info
                "Request processed. Total processing time: %.02fs, effective \
                 ratio: %.02fx"
                processing_time effective_ratio)
        with Process_failed | Clock.Has_stopped -> ())

let _ =
  let log = Log.make ["request"; "dump"] in
  let kind = Lang.univ_t () in
  Lang.add_builtin ~base:request "dump" ~category:(`Source `Liquidsoap)
    ~descr:"Immediately encode the whole contents of a request into a file."
    ~flags:[`Experimental]
    [
      ("", Lang.format_t kind, None, Some "Encoding format.");
      ("", Lang.string_t, None, Some "Name of the file.");
      ("", Request.Value.t, None, Some "Request to encode.");
      ( "ratio",
        Lang.float_t,
        Some (Lang.float 50.),
        Some
          "Time ratio. A value of `50` means process data at `50x` real rate, \
           when possible." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 1.),
        Some
          "Stop processing the source if it has not started after the given \
           timeout." );
      ( "sleep_latency",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "How much time ahead, in seconds, should we should be before pausing \
           the processing." );
    ]
    Lang.unit_t
    (fun p ->
      let proto =
        let p = Pipe_output.file_proto (Lang.univ_t ()) in
        List.filter_map (fun (l, _, v, _) -> Option.map (fun v -> (l, v)) v) p
      in
      let proto = ("fallible", Lang.bool true) :: proto in
      let format = Lang.assoc "" 1 p in
      let file = Lang.assoc "" 2 p in
      let r = Request.Value.of_value (Lang.assoc "" 3 p) in
      let process s =
        let p =
          ("id", Lang.string "request.drop")
          :: ("", format) :: ("", file)
          :: ("", Lang.source s)
          :: (p @ proto)
        in
        Pipe_output.new_file_output p
      in
      let ratio = Lang.to_float (List.assoc "ratio" p) in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let sleep_latency = Lang.to_float (List.assoc "sleep_latency" p) in
      process_request ~log ~name:"request.dump" ~ratio ~timeout ~sleep_latency
        ~process r;
      log#info "Request dumped.";
      Lang.unit)

let _ =
  let log = Log.make ["request"; "process"] in
  Lang.add_builtin ~base:request "process" ~category:(`Source `Liquidsoap)
    ~descr:
      "Given a request and an optional function to process this request, \
       animate the source as fast as possible until the request is fully \
       processed."
    [
      ("", Request.Value.t, None, Some "Request to process");
      ( "process",
        Lang.fun_t
          [(false, "", Lang.source_t (Lang.univ_t ()))]
          (Lang.source_t (Lang.univ_t ())),
        Some (Lang.val_fun [("", "", None)] (fun p -> List.assoc "" p)),
        Some "Callback to create the source to animate." );
      ( "ratio",
        Lang.float_t,
        Some (Lang.float 50.),
        Some
          "Time ratio. A value of `50` means process data at `50x` real rate, \
           when possible." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 1.),
        Some
          "Stop processing the source if it has not started after the given \
           timeout." );
      ( "sleep_latency",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "How much time ahead, in seconds, should we should be before pausing \
           the processing." );
    ]
    Lang.unit_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      let process = List.assoc "process" p in
      let process s =
        Lang.to_source (Lang.apply process [("", Lang.source s)])
      in
      let ratio = Lang.to_float (List.assoc "ratio" p) in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let sleep_latency = Lang.to_float (List.assoc "sleep_latency" p) in
      process_request ~log ~name:"request.process" ~ratio ~timeout
        ~sleep_latency ~process r;
      Lang.unit)
