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

(** base class *)

let output = Modules.output

let create_writer format_val =
  match Encoder.create_writer (Lang.to_format format_val) with
    | create_writer ->
        fun creation ->
          create_writer { creation with Encoder.pos = Value.pos format_val }
    | exception Not_found ->
        raise
          (Error.Invalid_value (format_val, "Unsupported encoding format", []))

let cover_proto ~default =
  ( "export_cover_metadata",
    Lang.bool_t,
    Some (Lang.bool default),
    Some "Export cover metadata." )

let base_proto = Output.proto

class virtual base ?clock ~source:source_val ~name p =
  let e f v = f (List.assoc v p) in
  (* Output settings *)
  let autostart = e Lang.to_bool "start" in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
  let export_cover_metadata =
    Lang.to_bool (List.assoc "export_cover_metadata" p)
  in
  object (self)
    inherit
      [unit] Output.encoded
        ?clock ~infallible ~register_telnet ~autostart ~export_cover_metadata
          ~output_kind:"output.file" ~name source_val

    val mutable encoder : [ `None | `Pending | `Created of Encoder.writer ] =
      `None

    method virtual start : unit
    method virtual stop : unit
    method virtual private create_writer : Encoder.creation -> Encoder.writer
    method virtual write_pipe : string -> int -> int -> unit

    method private sink =
      {
        Encoder.write = Strings.iter self#write_pipe;
        mark = (fun _ -> ());
        seek = None;
        read = None;
        size = None;
        name = None;
      }

    method private capabilities = Encoder.fresh_stream
    method create_encoder = encoder <- `Pending

    method get_encoder =
      match encoder with
        | `None -> None
        | `Pending ->
            let enc =
              self#create_writer
                {
                  Encoder.pos = None;
                  name = self#id;
                  capabilities = self#capabilities;
                  sink = self#sink;
                  initial_metadata = self#initial_metadata;
                }
            in
            encoder <- `Created enc;
            Some enc
        | `Created enc -> Some enc

    (* The writer's final bytes go to the pipe: it is still open. *)
    method close_encoder =
      let writer = encoder in
      encoder <- `None;
      match writer with
        | `None | `Pending -> ()
        | `Created writer -> writer.Encoder.finish `Clean

    method! reset = ()

    method encode frame =
      if 0 < Frame.position frame then
        Option.iter (fun writer -> writer.Encoder.write frame) self#get_encoder

    method send () = ()

    (* A writer yet to be built starts with the current metadata, which holds
       this one. *)
    method encode_metadata m =
      match encoder with
        | `Created writer -> writer.Encoder.metadata m
        | `Pending | `None -> ()
  end

(** Piped virtual class: open/close pipe, implements metadata interpolation and
    takes care of the various reload mechanisms. *)

let default_reopen_on_error =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_) -> null"

let reopen_on_error_proto default =
  ( "reopen_on_error",
    Lang.fun_t
      [(false, "", Lang.nullable_t Lang.error_t)]
      (Lang.nullable_t Lang.float_t),
    Some default,
    Some
      "Callback called when there is an error. Error is raised when returning \
       `null`. Otherwise, the output is reopened after the returned delay, in \
       seconds." )

(** Return the time at which the output should be reopened after an error,
    according to a `reopen_on_error` callback. The error is re-raised when the
    callback returns `null` or a negative delay. *)
let reopen_time_on_error ~(log : Log.t) reopen_on_error ~bt exn =
  let error = Lang.runtime_error_of_exception ~bt ~kind:"output" exn in
  match
    Lang.to_valued_option Lang.to_float
      (Lang.apply reopen_on_error [("", Lang.error error)])
  with
    | Some delay when 0. <= delay ->
        log#important "Error while streaming: %s, will re-open in %.02fs"
          (Printexc.to_string exn) delay;
        Unix.gettimeofday () +. delay
    | _ -> Printexc.raise_with_backtrace exn bt

let default_reopen_on_metadata =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_) -> false"

let default_reopen_when =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun () -> false"

let pipe_proto frame_t arg_doc =
  base_proto
  @ [
      reopen_on_error_proto default_reopen_on_error;
      ( "reopen_on_metadata",
        Lang.fun_t [(false, "", Lang.metadata_t)] Lang.bool_t,
        Some default_reopen_on_metadata,
        Some
          "Callback called on metadata. If returned value is `true`, the file \
           is reopened." );
      ( "reopen_when",
        Lang.fun_t [] Lang.bool_t,
        Some default_reopen_when,
        Some
          "Callback called on each frame. If returned value is `true`, the \
           file is reopened." );
      ( "reopen_delay",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 120.),
        Some
          "Prevent re-opening within that delay, in seconds. Only applies to \
           `reopen_when`." );
      ("", Lang.format_t frame_t, None, Some "Encoding format.");
      ("", Lang.getter_t Lang.string_t, None, Some arg_doc);
      ("", Lang.source_t frame_t, None, None);
    ]

let pipe_callbacks =
  Lang.
    [
      {
        name = "on_reopen";
        params = [];
        descr = "when the output is reopened.";
        register_deprecated_argument = true;
        arg_t = [];
        register = (fun ~params:_ s f -> s#register_on_reopen (fun () -> f []));
      };
    ]

let pipe_meth =
  {
    Lang.name = "reopen";
    scheme = ([], Lang.fun_t [] Lang.unit_t);
    descr =
      "Reopen the output pipe. The actual reopening happens the next time the \
       output has some data to output.";
    value =
      (fun s ->
        Lang.val_fun [] (fun _ ->
            s#need_reopen;
            Lang.unit));
  }
  :: Start_stop.meth ()

class virtual piped_output ?clock ~name p =
  let source = Lang.assoc "" 3 p in
  let reopen_on_error = List.assoc "reopen_on_error" p in
  let reopen_on_metadata = List.assoc "reopen_on_metadata" p in
  let reopen_on_metadata m =
    let m = Lang.metadata m in
    Lang.to_bool (Lang.apply reopen_on_metadata [("", m)])
  in
  let reopen_when = List.assoc "reopen_when" p in
  let reopen_when () = Lang.to_bool (Lang.apply reopen_when []) in
  let reopen_delay = Lang.to_float_getter (List.assoc "reopen_delay" p) in
  object (self)
    inherit base ?clock ~source ~name p as base
    val mutable open_date = 0.
    val need_reopen = Atomic.make false
    method need_reopen = Atomic.set need_reopen true
    method virtual open_pipe : unit
    method virtual close_pipe : unit
    method virtual is_open : bool

    method interpolate ?(subst = fun x -> x) s =
      let current_metadata = self#current_metadata in
      Utils.interpolate
        (fun x -> subst (Frame.Metadata.find x current_metadata))
        s

    method prepare_pipe =
      self#open_pipe;
      open_date <- Unix.gettimeofday ();
      Atomic.set need_reopen false;
      self#create_encoder

    method cleanup_pipe =
      if self#is_open then (
        (try self#close_encoder with _ -> ());
        self#close_pipe)

    val on_reopen = Callbacks.create ()
    method register_on_reopen fn = Callbacks.register on_reopen fn
    method on_reopen fn = Callbacks.add on_reopen fn
    method start = self#prepare_pipe
    method stop = self#cleanup_pipe

    method reopen =
      self#log#important "Re-opening output pipe.";
      self#cleanup_pipe;
      self#prepare_pipe;
      List.iter (fun fn -> fn ()) (Callbacks.elements on_reopen)

    method private reopen_on_error ~bt exn =
      open_date <- reopen_time_on_error ~log:self#log reopen_on_error ~bt exn

    (* [open_date] is in the future while we are waiting to re-open after an
       error. Failing to open the pipe leaves the output in its [`Idle] state so,
       without this guard, the next streaming cycle would retry right away and
       the delay returned by [reopen_on_error] would be ignored. *)
    method! output =
      if self#is_open || open_date <= Unix.gettimeofday () then (
        try base#output
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          (try self#cleanup_pipe with _ -> ());
          self#reopen_on_error ~bt exn)

    method! encode frame =
      (match self#is_open with
        | false when open_date <= Unix.gettimeofday () -> self#prepare_pipe
        | true when Atomic.get need_reopen ->
            if open_date <= Unix.gettimeofday () then self#reopen
        | true
          when open_date +. reopen_delay () <= Unix.gettimeofday ()
               && reopen_when () ->
            self#reopen
        | _ -> ());
      base#encode frame

    method! private on_metadata metadata =
      if reopen_on_metadata metadata then self#reopen
  end

(** Out channel virtual class: takes care of current out channel and writing to
    it. *)
let chan_proto frame_t arg_doc =
  [
    ( "flush",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Perform a flush after each write." );
  ]
  @ pipe_proto frame_t arg_doc

(* What an output's channel is asked to do, off the streaming thread. *)
type operation = Write of string | Seek of int | Stop

(* Bytes an output may owe its channel before the streaming thread waits for
   it to catch up. *)
let max_pending = 16 * 1024 * 1024

class virtual ['a] chan_output p =
  let flush = Lang.to_bool (List.assoc "flush" p) in
  object (self)
    val mutable chan : 'a option = None
    method virtual open_chan : 'a
    method virtual close_chan : 'a -> unit
    method virtual output_substring : 'a -> string -> int -> int -> unit
    method virtual seek_chan : 'a -> int -> unit
    method virtual flush : 'a -> unit

    (* The writes run on a worker: a slow disk or a slow process holds the
       worker, not the clock. The first failure is raised by the next call of
       the streaming thread, for the output's restart policy. *)
    val lock = Mutex.create ()
    val changed = Condition.create ()
    val queue : operation Queue.t = Queue.create ()
    val mutable pending = 0
    val mutable busy = false
    val mutable failure = None
    val mutable worker = None

    method private run_worker =
      let rec loop () =
        let operation =
          Mutex.protect lock (fun () ->
              while Queue.is_empty queue do
                Condition.wait changed lock
              done;
              busy <- true;
              Queue.pop queue)
        in
        (try
           match (operation, chan) with
             | Write data, Some channel ->
                 self#output_substring channel data 0 (String.length data);
                 if flush then self#flush channel
             | Seek offset, Some channel ->
                 self#flush channel;
                 self#seek_chan channel offset
             | (Write _ | Seek _), None | Stop, _ -> ()
         with exn ->
           Mutex.protect lock (fun () ->
               if failure = None then failure <- Some exn));
        Mutex.protect lock (fun () ->
            (match operation with
              | Write data -> pending <- pending - String.length data
              | _ -> ());
            busy <- false;
            Condition.broadcast changed);
        if operation <> Stop then loop ()
      in
      loop ()

    method private raise_failure =
      match Mutex.protect lock (fun () -> failure) with
        | None -> ()
        | Some exn ->
            Mutex.protect lock (fun () -> failure <- None);
            Lang.raise_as_runtime ~bt:(Printexc.get_callstack 0) ~kind:"system"
              exn

    method private push operation =
      self#raise_failure;
      if worker = None then
        worker <-
          Some (Tutils.create (fun () -> self#run_worker) () "output writer");
      Mutex.protect lock (fun () ->
          while pending > max_pending do
            Condition.wait changed lock
          done;
          (match operation with
            | Write data -> pending <- pending + String.length data
            | _ -> ());
          Queue.push operation queue;
          Condition.broadcast changed)

    (* Waits for every operation asked so far to be done. *)
    method private drain =
      if worker <> None then
        Mutex.protect lock (fun () ->
            while busy || not (Queue.is_empty queue) do
              Condition.wait changed lock
            done);
      self#raise_failure

    method open_pipe = chan <- Some self#open_chan
    method write_pipe b ofs len = self#push (Write (String.sub b ofs len))
    method private seek_pipe offset = self#push (Seek offset)

    (* Runs [f] with the channel once the writes asked before are done. *)
    method private with_drained_chan : 'b. ('a -> 'b) -> 'b =
      fun f ->
        self#drain;
        let channel = Option.get chan in
        self#flush channel;
        f channel

    (* The channel is closed whatever the worker met, then its failure is
       raised. *)
    method close_pipe =
      let failure =
        try
          self#drain;
          None
        with exn -> Some exn
      in
      if worker <> None then (
        Mutex.protect lock (fun () ->
            Queue.push Stop queue;
            Condition.broadcast changed);
        worker <- None);
      (match chan with
        | None -> ()
        | Some ch ->
            chan <- None;
            self#close_chan ch);
      Option.iter raise failure

    method is_open = chan <> None
  end

(** File output *)

class virtual ['a] file_output_base p =
  let source = Lang.to_source (Lang.assoc "" 3 p) in
  let filename = Lang.to_string_getter (Lang.assoc "" 2 p) in
  let on_close = List.assoc "on_close" p in
  let on_close s = Lang.to_unit (Lang.apply on_close [("", Lang.string s)]) in
  let perm = Lang.to_int (List.assoc "perm" p) in
  let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
  let append = Lang.to_bool (List.assoc "append" p) in
  object (self)
    val current_filename = Atomic.make None
    method virtual interpolate : ?subst:(string -> string) -> string -> string

    method private filename =
      let filename = filename () in
      let filename = Lang_string.home_unrelate filename in
      (* Avoid / in metas for filename.. *)
      let subst m = String.concat "-" (String.split_on_char '/' m) in
      self#interpolate ~subst filename

    method virtual open_out_gen : open_flag list -> int -> string -> 'a
    method private self_sync = source#cached_self_sync

    method private prepare_filename =
      let mode =
        Open_wronly :: Open_creat
        :: (if append then [Open_append] else [Open_trunc])
      in
      match Atomic.get current_filename with
        | Some filename -> (filename, mode, perm)
        | None -> (
            let filename = self#filename in
            try
              Utils.ensure_dir ~perm:dir_perm filename;
              Atomic.set current_filename (Some filename);
              (filename, mode, perm)
            with (Sys_error _ | Unix.Unix_error _) as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Lang.raise_as_runtime ~bt ~kind:"system" exn)

    method open_chan =
      try
        let filename, mode, perm = self#prepare_filename in
        self#open_out_gen mode perm filename
      with (Sys_error _ | Unix.Unix_error _) as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"system" exn

    method virtual close_out : 'a -> unit

    method close_chan fd =
      try
        self#close_out fd;
        (match Atomic.get current_filename with
          | Some f -> self#on_close f
          | None -> ());
        Atomic.set current_filename None
      with (Sys_error _ | Unix.Unix_error _) as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"system" exn

    method private on_close = on_close
  end

class file_output ?clock ~format_val p =
  object (self)
    inherit piped_output ?clock ~name:"output.file" p as base
    inherit [out_channel] chan_output p
    inherit [out_channel] file_output_base p
    method private create_writer = create_writer format_val
    val mutable file_capabilities = Encoder.fresh_stream
    method! private capabilities = file_capabilities

    (* An appended file is opened without O_APPEND, at its end: its writer may
       seek back to rewrite a header. *)
    method open_out_gen mode perm filename =
      let appending = List.mem Open_append mode in
      let mode = List.filter (fun flag -> flag <> Open_append) mode in
      let channel = open_out_gen mode perm filename in
      set_binary_mode_out channel true;
      let regular =
        (Unix.fstat (Unix.descr_of_out_channel channel)).Unix.st_kind
        = Unix.S_REG
      in
      let existing = if regular then out_channel_length channel else 0 in
      if appending && regular then seek_out channel existing;
      file_capabilities <-
        {
          Encoder.fresh_stream with
          seekable = regular;
          continuing = appending && 0 < existing;
        };
      channel

    method! private sink =
      let read offset length =
        self#with_drained_chan (fun _ ->
            In_channel.with_open_bin
              (Option.get (Atomic.get current_filename))
              (fun input ->
                In_channel.seek input (Int64.of_int offset);
                (* Up to [length] bytes: fewer at the end of the file. *)
                let buffer = Bytes.create length in
                let rec fill filled =
                  if filled = length then filled
                  else (
                    match
                      In_channel.input input buffer filled (length - filled)
                    with
                      | 0 -> filled
                      | n -> fill (filled + n))
                in
                Bytes.sub_string buffer 0 (fill 0)))
      in
      let seekable = file_capabilities.Encoder.seekable in
      {
        (base#sink) with
        Encoder.name = Atomic.get current_filename;
        seek = (if seekable then Some self#seek_pipe else None);
        read = (if seekable then Some read else None);
        size =
          (if seekable then
             Some (fun () -> self#with_drained_chan out_channel_length)
           else None);
      }

    method seek_chan = seek_out
    method output_substring = output_substring
    method flush = flush
    method close_out = close_out
  end

let file_proto frame_t =
  [
    cover_proto ~default:true;
    ( "append",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Do not truncate but append in the file if it exists." );
    ( "perm",
      Lang.int_t,
      Some (Lang.int 0o666),
      Some
        "Permission of the file if it has to be created, up to umask. You can \
         and should write this number in octal notation: 0oXXX. The default \
         value is however displayed in decimal (0o666 = 6×8^2 + 6×8 + 6 = \
         438)." );
    ( "dir_perm",
      Lang.int_t,
      Some (Lang.int 0o777),
      Some
        "Permission of the directories if some have to be created, up to \
         umask. Although you can enter values in octal notation (0oXXX) they \
         will be displayed in decimal (for instance, 0o777 = 7×8^2 + 7×8 + 7 = \
         511)." );
    ( "on_close",
      Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
      Some (Lang.val_cst_fun [("", None)] Lang.unit),
      Some
        "This function will be called for each file, after that it is finished \
         and closed. The filename will be passed as argument." );
  ]
  @ chan_proto frame_t "Filename where to output the stream."

let new_file_output ?clock p =
  let format_val = Lang.assoc "" 1 p in
  let capabilities =
    {
      Encoder.fresh_stream with
      seekable = true;
      continuing = Lang.to_bool (List.assoc "append" p);
    }
  in
  let (_ : Encoder.format) =
    Lang_encoder.output_format ~capabilities format_val
  in
  (new file_output ?clock ~format_val p :> piped_output)

let output_file =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:output "file" (file_proto return_t) ~return_t
    ~category:`Output ~meth:pipe_meth
    ~callbacks:(pipe_callbacks @ Start_stop.output_callbacks ())
    ~descr:"Output the source stream to a file."
    (fun p -> new_file_output p)

(** External output *)

class external_output ?clock p =
  let format_val = Lang.assoc "" 1 p in
  let (_ : Encoder.format) =
    Lang_encoder.output_format ~capabilities:Encoder.fresh_stream format_val
  in
  let process = Lang.to_string_getter (Lang.assoc "" 2 p) in
  let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
  object (self)
    inherit piped_output ?clock ~name:"output.external" p
    inherit [out_channel] chan_output p
    method private create_writer = create_writer format_val
    method private self_sync = (`Static, self#source_sync self_sync)

    method open_chan =
      let process = process () in
      let process = self#interpolate process in
      Unix.open_process_out process

    method close_chan chan =
      try ignore (Unix.close_process_out chan) with Sys_error _ -> ()

    method seek_chan _ _ = failwith "a process cannot seek"
    method output_substring = output_substring
    method flush = flush
    method close_out = close_out
  end

let pipe_proto frame_t descr =
  cover_proto ~default:false
  :: ( "self_sync",
       Lang.bool_t,
       Some (Lang.bool false),
       Some
         "Set to `true` if the process is expected to control the output's \
          latency. Typical example: `ffmpeg` with the `-re` command-line \
          option." )
  :: chan_proto frame_t descr

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:output "external"
    (pipe_proto return_t "Process to pipe data to.")
    ~return_t ~category:`Output ~meth:pipe_meth
    ~callbacks:(pipe_callbacks @ Start_stop.output_callbacks ())
    ~descr:"Send the stream to a process' standard input."
    (fun p -> (new external_output p :> piped_output))
