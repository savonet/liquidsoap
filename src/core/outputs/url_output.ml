let shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"output.url shutdown" (fun () ->
      Atomic.set shutdown true)

type attempt = { abort : bool Atomic.t }

(* Every state value is freshly allocated, so physical equality of the whole
   state is attempt identity (io.md §11.1). *)
type state =
  [ `Stopped
  | `Connecting of attempt
  | `Connected of attempt * Encoder.encoder
  | `Waiting of float
  | `Fatal of exn * Printexc.raw_backtrace ]

(* faults.md §4's [fault_log_interval]. *)
let fault_log_interval = 10.

let now () =
  let module T = (val Clock.time_implementation () : Liq_time.T) in
  T.to_float (T.time ())

(* io.md §11.3: only failures deterministic by construction fail the script,
   and only before the output ever connected. *)
let is_configuration_error = function
  | Error.Invalid_value _ -> true
  | Runtime_error.Runtime_error { kind = "encoder"; _ } -> true
  | _ -> false

class url_output ~url ~format ~format_val ~restart_delay ~timeout ~self_sync p =
  let source_val = Lang.assoc "" 2 p in
  let e f v = f (List.assoc v p) in
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"output.url" ~name:"output.url"
        ~infallible:(not (e Lang.to_bool "fallible"))
        ~register_telnet:(e Lang.to_bool "register_telnet")
        ~autostart:(e Lang.to_bool "start")
        ~export_cover_metadata:(e Lang.to_bool "export_cover_metadata")
        source_val

    val connection : state Atomic.t = Atomic.make `Stopped
    val held : Frame.t Queue.t = Queue.create ()
    val mutable held_length = 0
    val mutable dropped = 0
    val mutable ever_connected = false
    val mutable metadata = Frame.Metadata.Export.empty
    val on_error = Callbacks.create ()
    val mutable last_fault_log = neg_infinity
    method register_on_error fn = Callbacks.register on_error fn
    method private self_sync = (`Static, self#source_sync self_sync)

    method private interrupt attempt () =
      Atomic.get attempt.abort || Atomic.get shutdown

    method private build attempt =
      let format =
        {
          format with
          Liquidsoap_ffmpeg_formats.Encoder_format.output = `Url url;
        }
      in
      (Encoder.get_factory (Encoder.Ffmpeg format))
        ~hls:false ~interrupt:(self#interrupt attempt)
        ~pos:(Value.pos format_val) self#id metadata

    method private start_attempt ~from =
      let attempt = { abort = Atomic.make false } in
      let connecting = `Connecting attempt in
      if Atomic.compare_and_set connection from connecting then
        Scheduler.Task.add
          {
            Scheduler.Task.priority = `Threaded;
            events = [`Delay 0.];
            handler =
              (fun _ ->
                self#connect attempt connecting;
                []);
          }

    method private connect attempt connecting =
      match self#build attempt with
        | encoder ->
            if
              not
                (Atomic.compare_and_set connection connecting
                   (`Connected (attempt, encoder)))
            then ignore (try encoder.Encoder.stop () with _ -> Strings.empty)
            else ever_connected <- true
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            if Atomic.get connection == connecting then
              self#failed ~from:connecting ~bt exn

    method private failed ~from ~bt exn =
      if is_configuration_error exn && not ever_connected then
        ignore (Atomic.compare_and_set connection from (`Fatal (exn, bt)))
      else (
        let next =
          match restart_delay with
            | Some delay -> `Waiting (now () +. delay)
            | None -> `Stopped
        in
        if Atomic.compare_and_set connection from next then (
          let now = now () in
          if now -. last_fault_log >= fault_log_interval then (
            last_fault_log <- now;
            self#log#severe "Error while connecting: %s\n%s"
              (Printexc.to_string exn)
              (Printexc.raw_backtrace_to_string bt);
            Option.iter
              (fun delay ->
                self#log#important "Will try again in %.2f seconds." delay)
              restart_delay);
          let error = Lang.runtime_error_of_exception ~bt ~kind:"output" exn in
          List.iter (fun fn -> fn error) (Callbacks.elements on_error);
          if restart_delay = None then self#transition_to `Stopped))

    method private hold frame =
      Queue.push frame held;
      held_length <- held_length + Frame.position frame;
      let limit = Frame.main_of_seconds timeout in
      while held_length > limit && not (Queue.is_empty held) do
        let oldest = Queue.pop held in
        held_length <- held_length - Frame.position oldest;
        dropped <- dropped + Frame.position oldest
      done

    (* An encoder in error releases its container without draining at
       [stop] (encoder.md §1). *)
    method private encode_with ~from encoder frame =
      try ignore (encoder.Encoder.encode frame)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        (try ignore (encoder.Encoder.stop ()) with _ -> ());
        self#failed ~from ~bt exn

    method private encode frame =
      if Frame.position frame > 0 then (
        match Atomic.get connection with
          | `Fatal (exn, bt) -> Printexc.raise_with_backtrace exn bt
          | `Connected (_, encoder) as current ->
              if dropped > 0 then (
                self#log#important
                  "Dropped %.2fs of stream while the output was not connected."
                  (Frame.seconds_of_main dropped);
                dropped <- 0);
              Queue.iter (self#encode_with ~from:current encoder) held;
              Queue.clear held;
              held_length <- 0;
              if Atomic.get connection == current then
                self#encode_with ~from:current encoder frame
          | `Connecting _ -> self#hold frame
          | `Waiting t as current when now () >= t ->
              self#start_attempt ~from:current;
              self#hold frame
          | `Waiting _ -> dropped <- dropped + Frame.position frame
          | `Stopped -> ())

    method private send () = ()

    method private encode_metadata m =
      metadata <- m;
      match Atomic.get connection with
        | `Connected (_, encoder) as current -> (
            try encoder.Encoder.encode_metadata m
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              (try ignore (encoder.Encoder.stop ()) with _ -> ());
              self#failed ~from:current ~bt exn)
        | _ -> ()

    method start =
      match Atomic.get connection with
        | `Stopped as from -> self#start_attempt ~from
        | _ -> ()

    method stop =
      let rec go () =
        match Atomic.get connection with
          | `Connected (attempt, encoder) as current ->
              if Atomic.compare_and_set connection current `Stopped then (
                (try ignore (encoder.Encoder.stop ())
                 with exn ->
                   self#log#severe "Error while stopping: %s"
                     (Printexc.to_string exn));
                Atomic.set attempt.abort true)
              else go ()
          | `Connecting attempt as current ->
              if Atomic.compare_and_set connection current `Stopped then
                Atomic.set attempt.abort true
              else go ()
          | (`Waiting _ | `Fatal _) as current ->
              if not (Atomic.compare_and_set connection current `Stopped) then
                go ()
          | `Stopped -> ()
      in
      go ();
      Queue.clear held;
      held_length <- 0

    method! reset = ()
  end

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Modules.output "url"
    (Output.proto
    @ [
        ( "export_cover_metadata",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Export cover metadata." );
        ("url", Lang.string_t, None, Some "Output URL, handed to the muxer.");
        ( "restart_delay",
          Lang.nullable_t Lang.float_t,
          Some (Lang.float 2.),
          Some
            "Seconds before a retry after a failure; `null`: the output stops \
             at its first failure." );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool false),
          Some "Declare the output as its clock's sync source." );
        ( "timeout",
          Lang.float_t,
          Some (Lang.float 10.),
          Some
            "Longest a single blocking write may last, in seconds, and the \
             stream time held while connecting." );
        ("", Lang.format_t return_t, None, Some "Encoding format (`%ffmpeg`).");
        ("", Lang.source_t return_t, None, None);
      ])
    ~return_t ~category:`Output
    ~callbacks:
      (Lang_source.
         [
           {
             name = "on_error";
             params = [];
             descr = "on each connection or streaming failure.";
             register_deprecated_argument = false;
             arg_t = [(false, "", Lang.error_t)];
             register =
               (fun ~params:_ s f ->
                 s#register_on_error (fun e -> f [("", Lang.error e)]));
           };
         ]
      @ Start_stop.output_callbacks ())
    ~descr:"Encode and send the stream to a URL with FFmpeg."
    (fun p ->
      let format_val = Lang.assoc "" 1 p in
      let format =
        match Lang.to_format format_val with
          | Encoder.Ffmpeg format -> format
          | _ ->
              raise
                (Error.Invalid_value
                   ( format_val,
                     "Encoding format does not support output url!",
                     [] ))
      in
      new url_output
        ~url:(Lang.to_string (List.assoc "url" p))
        ~format ~format_val
        ~restart_delay:
          (Lang.to_valued_option Lang.to_float (List.assoc "restart_delay" p))
        ~timeout:(Lang.to_float (List.assoc "timeout" p))
        ~self_sync:(Lang.to_bool (List.assoc "self_sync" p))
        p)
