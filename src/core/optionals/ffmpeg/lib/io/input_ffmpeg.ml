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

let shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"ffmpeg inputs shutdown" (fun () ->
      Atomic.set shutdown true)

type attempt = { abort : bool Atomic.t; read_since : float option Atomic.t }

type connection = {
  loop : Decode_loop.t;
  decoder_buffer : Decoder.buffer;
  mutable delivered : bool;
}

(* Every state value is freshly allocated, so physical equality of the whole
   state is attempt identity (io.md §2.1). *)
type state =
  [ `Stopped
  | `Starting of attempt
  | `Polling of attempt
  | `Connected of attempt * string * connection
  | `Stopping of attempt
  | `Failed of string * exn ]

let status = function
  | `Stopped -> "stopped"
  | `Starting _ -> "starting"
  | `Polling _ -> "polling"
  | `Connected (_, url, _) -> "connected " ^ url
  | `Stopping _ -> "stopping"
  | `Failed _ -> "failed"

type http = {
  user_agent : string;
  http_self_sync : unit -> bool option;
  mutable icy : bool;
}

type kind = [ `Ffmpeg of unit -> bool | `Http of http ]

let new_attempt () =
  { abort = Atomic.make false; read_since = Atomic.make None }

let is_listening (opts : Avutil.opts) =
  let nonzero = function
    | `Int 0 | `Int64 0L -> false
    | `String ("0" | "") -> false
    | _ -> true
  in
  (match Hashtbl.find_opt opts "listen" with
    | Some v -> nonzero v
    | None -> false)
  || Hashtbl.find_opt opts "mode" = Some (`String "listener")

let after ~delay f =
  Scheduler.Task.add
    {
      Scheduler.Task.priority = `Threaded;
      events = [`Delay delay];
      handler =
        (fun _ ->
          f ();
          []);
    }

exception Leftover_options of string
exception Content_mismatch of string

class input ~kind ~url ~options ~format ~poll_delay ~timeout ~max_buffer
  ~metadata_filter ~deduplicate_metadata ~new_track_on_metadata ~debug ~trim_url
  ~autostart () =
  let listening = is_listening options in
  object (self)
    inherit
      Start_stop.active_source
        ~name:
          (match kind with
            | `Ffmpeg _ -> "input.ffmpeg"
            | `Http _ -> "input.http")
        ~fallible:true ~autostart () as super

    val connection_state : state Atomic.t = Atomic.make `Stopped
    val mutable url = url
    val on_connect = Callbacks.create ()
    val on_disconnect = Callbacks.create ()
    val on_error = Callbacks.create ()
    val mutable last_metadata = None
    val mutable dropping_cycles = 0
    val mutable sync_source = None
    method register_on_connect fn = Callbacks.register on_connect fn
    method register_on_disconnect fn = Callbacks.register on_disconnect fn
    method register_on_error fn = Callbacks.register on_error fn
    method url = if trim_url then String.trim (url ()) else url ()

    method set_url getter =
      url <- getter;
      match Atomic.get connection_state with
        | `Failed _ as failed -> self#leave_failed failed
        | _ -> ()

    method status = status (Atomic.get connection_state)
    method fallible = true
    method effective_source = (self :> Source.source)

    method private own_sync_source =
      match sync_source with
        | Some s -> s
        | None ->
            let s = Clock.Sync_source.make ~name:self#id `Self_paced in
            sync_source <- Some s;
            s

    method private self_sync =
      match Atomic.get connection_state with
        | `Connected _ -> (
            match kind with
              | `Ffmpeg self_sync ->
                  ( `Dynamic,
                    if self_sync () then Some self#own_sync_source else None )
              | `Http http -> (
                  match http.http_self_sync () with
                    | Some true -> (`Dynamic, Some self#own_sync_source)
                    | Some false -> (`Dynamic, None)
                    | None -> (`Dynamic, self#source_sync http.icy)))
        | _ -> (`Dynamic, None)

    method private interrupt attempt () =
      Atomic.get attempt.abort || Atomic.get shutdown
      || (not listening)
         &&
           match Atomic.get attempt.read_since with
           | Some since -> Unix.gettimeofday () -. since > timeout
           | None -> false

    method private run_callbacks : 'a. 'a Callbacks.t -> ('a -> unit) -> unit =
      fun callbacks apply -> List.iter apply (Callbacks.elements callbacks)

    method private report ~bt exn =
      let error = Lang.runtime_error_of_exception ~bt ~kind:"ffmpeg" exn in
      self#run_callbacks on_error (fun fn -> fn error)

    method private schedule ~delay attempt =
      after ~delay (fun () -> self#run_attempt attempt)

    method private start_attempt ~delay ~from =
      let attempt = new_attempt () in
      let next = `Starting attempt in
      if Atomic.compare_and_set connection_state from next then
        self#schedule ~delay attempt

    method private leave_failed failed =
      self#start_attempt ~delay:0. ~from:failed

    method private watch_failed failed failed_url =
      after ~delay:poll_delay (fun () ->
          if Atomic.get connection_state == failed then
            if self#url <> failed_url then self#leave_failed failed
            else self#watch_failed failed failed_url)

    method private open_container attempt url =
      let opts = Hashtbl.copy options in
      let configure = Ffmpeg_decoder.stream_configuration () in
      let container =
        Av.open_input ~interrupt:(self#interrupt attempt) ?format ~opts
          ~configure_audio_stream:configure.audio
          ~configure_video_stream:configure.video
          ~configure_subtitle_stream:configure.subtitle url
      in
      if Hashtbl.length opts > 0 then (
        Av.close container;
        raise
          (Leftover_options
             (Printf.sprintf "Unrecognized options: %s"
                (String.concat ","
                   (Hashtbl.fold
                      (fun k v l ->
                        Printf.sprintf "%s=%s" k (Opts.string_of_value v) :: l)
                      opts [])))));
      container

    method private connect_metadata container =
      match kind with
        | `Ffmpeg _ -> []
        | `Http http ->
            let headers =
              match
                Avutil.Options.get_string ~search_children:true
                  ~name:"icy_metadata_headers" (Av.input_obj container)
              with
                | text ->
                    List.filter_map
                      (fun line ->
                        match String.index_opt line ':' with
                          | Some i when line <> "" ->
                              Some
                                ( String.sub line 0 i,
                                  String.trim
                                    (String.sub line (i + 1)
                                       (String.length line - i - 1)) )
                          | _ -> None)
                      (List.map
                         (fun l ->
                           if String.ends_with ~suffix:"\r" l then
                             String.sub l 0 (String.length l - 1)
                           else l)
                         (String.split_on_char '\n' text))
                | exception exn ->
                    self#log#important "Error while fetching icy headers: %s"
                      (Printexc.to_string exn);
                    []
            in
            http.icy <- headers <> [];
            headers

    method private run_attempt attempt =
      let current = Atomic.get connection_state in
      match current with
        | `Starting a when a == attempt ->
            let polling = `Polling attempt in
            if Atomic.compare_and_set connection_state current polling then
              self#connect attempt polling
        | _ -> ()

    method private connect attempt polling =
      let container = ref None and loop = ref None and announced = ref false in
      let release () =
        match (!loop, !container) with
          | Some loop, _ -> ( try Decode_loop.close loop with _ -> ())
          | None, Some container -> ( try Av.close container with _ -> ())
          | None, None -> ()
      in
      let url = self#url in
      try
        let c = self#open_container attempt url in
        container := Some c;
        let options = { Input_options.format; opts = Hashtbl.create 0 } in
        let description = Ffmpeg_decoder.describe ~options ~url c in
        let ctype = self#content_type in
        if not (Decode_loop.can_produce ~ctype description) then
          raise
            (Content_mismatch
               (Printf.sprintf "url %S cannot produce content of type %s" url
                  (Frame.string_of_content_type ctype)));
        let l =
          Decode_loop.create ~decode_first_metadata:true ~ctype ~description c
        in
        loop := Some l;
        let connection =
          {
            loop = l;
            decoder_buffer = Decoder.mk_buffer ~ctype self#buffer;
            delivered = false;
          }
        in
        last_metadata <- None;
        let metadata = self#connect_metadata c in
        announced := true;
        self#run_callbacks on_connect (fun fn -> fn metadata);
        if
          not
            (Atomic.compare_and_set connection_state polling
               (`Connected (attempt, url, connection)))
        then (
          release ();
          self#run_callbacks on_disconnect (fun fn -> fn ());
          self#settle_stopping attempt)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        release ();
        if !announced then self#run_callbacks on_disconnect (fun fn -> fn ());
        if Atomic.get connection_state != polling then
          self#settle_stopping attempt
        else self#attempt_failed ~bt ~polling url exn

    method private settle_stopping attempt =
      match Atomic.get connection_state with
        | `Stopping a as current when a == attempt ->
            ignore (Atomic.compare_and_set connection_state current `Stopped)
        | _ -> ()

    method private attempt_failed ~bt ~polling url exn =
      let permanent =
        match exn with Leftover_options _ -> format <> None | _ -> false
      in
      if permanent then (
        let failed = `Failed (url, exn) in
        if Atomic.compare_and_set connection_state polling failed then (
          self#log_fault ~bt
            (Printf.sprintf "Connection failed: %s" (self#describe exn));
          self#report ~bt exn;
          self#watch_failed failed url))
      else (
        let attempt = new_attempt () in
        if Atomic.compare_and_set connection_state polling (`Starting attempt)
        then (
          self#log_fault ~bt
            (Printf.sprintf "Connection failed: %s" (self#describe exn));
          self#report ~bt exn;
          self#schedule ~delay:poll_delay attempt))

    method private describe =
      function
      | Leftover_options message | Content_mismatch message -> message
      | Avutil.Error e -> Avutil.string_of_error e
      | exn -> Printexc.to_string exn

    val mutable last_fault_log = neg_infinity

    method private log_fault ~bt message =
      if debug then
        self#log#severe "%s\n%s" message (Printexc.raw_backtrace_to_string bt)
      else (
        let now = Unix.gettimeofday () in
        if now -. last_fault_log >= Faults.fault_log_interval then (
          last_fault_log <- now;
          self#log#severe "%s" message);
        self#log#debug "%s" (Printexc.raw_backtrace_to_string bt))

    method private start =
      let rec go () =
        match Atomic.get connection_state with
          | (`Stopped | `Failed _ | `Stopping _) as from ->
              let attempt = new_attempt () in
              if
                Atomic.compare_and_set connection_state from (`Starting attempt)
              then self#schedule ~delay:0. attempt
              else go ()
          | _ -> ()
      in
      go ()

    method private stop =
      let rec go () =
        match Atomic.get connection_state with
          | (`Starting _ | `Failed _) as current ->
              if not (Atomic.compare_and_set connection_state current `Stopped)
              then go ()
          | `Polling attempt as current ->
              if
                Atomic.compare_and_set connection_state current
                  (`Stopping attempt)
              then Atomic.set attempt.abort true
              else go ()
          | `Connected (attempt, _, connection) as current ->
              if Atomic.compare_and_set connection_state current `Stopped then (
                Atomic.set attempt.abort true;
                Generator.clear self#buffer;
                self#close_connection connection)
              else go ()
          | `Stopped | `Stopping _ -> ()
      in
      go ()

    method private close_connection connection =
      (try Decode_loop.close connection.loop
       with exn ->
         self#log#severe "Error while disconnecting: %s"
           (Printexc.to_string exn));
      self#run_callbacks on_disconnect (fun fn -> fn ())

    (* io.md §2.5: the streaming thread ends the connection it moved out of
       [Connected]. *)
    method private end_connection current connection ~cause =
      let delay = if connection.delivered then 0. else poll_delay in
      let attempt = new_attempt () in
      if Atomic.compare_and_set connection_state current (`Starting attempt)
      then (
        (match cause with
          | `End_of_input -> (
              self#log#info "End of input";
              try Decode_loop.eof connection.loop connection.decoder_buffer
              with exn ->
                self#log#severe "Draining failed: %s" (Printexc.to_string exn);
                Generator.add_track_mark self#buffer)
          | `Fault (exn, bt) ->
              self#log_fault ~bt
                (Printf.sprintf "Feeding failed: %s" (self#describe exn));
              Generator.add_track_mark self#buffer;
              self#report ~bt exn);
        self#close_connection connection;
        self#schedule ~delay attempt)

    method private bound_buffer =
      let max = Frame.main_of_seconds max_buffer in
      let length = Generator.length self#buffer in
      if length > max then (
        Generator.truncate self#buffer (length - max);
        self#log#important
          "Generator max buffered length exceeded (%d < %d)! Dropping content.."
          max length;
        true)
      else false

    method private feed =
      let size = Lazy.Mutexed.force Frame.size in
      match Atomic.get connection_state with
        | `Connected (attempt, _, connection) as current -> (
            let dropped = ref false in
            try
              while
                Generator.length self#buffer < size
                && (not (Atomic.get attempt.abort))
                && not (Atomic.get shutdown)
              do
                Atomic.set attempt.read_since (Some (Unix.gettimeofday ()));
                Fun.protect
                  ~finally:(fun () -> Atomic.set attempt.read_since None)
                  (fun () ->
                    Decode_loop.decode connection.loop connection.decoder_buffer);
                if Generator.length self#buffer > 0 then
                  connection.delivered <- true;
                if self#bound_buffer then dropped := true
              done;
              dropping_cycles <- (if !dropped then dropping_cycles + 1 else 0);
              if dropping_cycles >= Faults.max_consecutive_faults then (
                dropping_cycles <- 0;
                raise
                  (Decode_loop.Source_fault
                     "Streams too far apart: buffer bound exceeded"))
            with
              | End_of_file ->
                  self#end_connection current connection ~cause:`End_of_input
              | Decode_loop.Aborted -> ()
              | exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  self#end_connection current connection
                    ~cause:(`Fault (exn, bt)))
        | _ -> ()

    method private filter_metadata m =
      let base =
        match metadata_filter with
          | Some filter -> filter m
          | None ->
              Frame.Metadata.filter
                (fun key _ -> not (String.starts_with ~prefix:"id3v2_priv" key))
                m
      in
      if not deduplicate_metadata then base
      else if Frame.Metadata.is_empty base then base
      else (
        let sorted = List.sort compare (Frame.Metadata.to_list base) in
        if Some sorted = last_metadata then Frame.Metadata.empty
        else (
          last_metadata <- Some sorted;
          base))

    method private can_generate_frame =
      super#started
      &&
        match Atomic.get connection_state with
        | `Connected _ -> true
        | _ -> Generator.length self#buffer > 0

    method private generate_frame =
      self#feed;
      let frame = Generator.slice self#buffer (Lazy.Mutexed.force Frame.size) in
      let kept = ref [] in
      let frame =
        Frame.map_metadata frame (fun (position, m) ->
            let m = self#filter_metadata m in
            if Frame.Metadata.is_empty m then None
            else (
              kept := position :: !kept;
              Some (position, m)))
      in
      if new_track_on_metadata then
        List.fold_left
          (fun frame position ->
            if Frame.has_track_mark frame position then frame
            else Frame.add_track_mark frame position)
          frame !kept
      else frame

    method remaining =
      match Atomic.get connection_state with
        | `Connected (_, _, connection) -> (
            match Decode_loop.remaining connection.loop with
              | r when r < 0 -> -1
              | r -> r + Generator.length self#buffer)
        | _ -> -1

    method! seek n =
      match (Atomic.get connection_state, kind) with
        | `Connected _, `Http { icy = true; _ } -> -1
        | `Connected (_, _, connection), _ ->
            let moved = Decode_loop.seek connection.loop n in
            Generator.clear self#buffer;
            moved
        | _ -> 0

    method abort_track = Generator.add_track_mark self#buffer
  end

type t =
  < Start_stop.active_source
  ; url : string
  ; set_url : (unit -> string) -> unit
  ; status : string
  ; register_on_connect : ((string * string) list -> unit) -> unit -> unit
  ; register_on_disconnect : (unit -> unit) -> unit -> unit
  ; register_on_error : (Runtime_error.runtime_error -> unit) -> unit -> unit >

let create ~kind ~url ~options ~format ~poll_delay ~timeout ~max_buffer
    ~metadata_filter ~deduplicate_metadata ~new_track_on_metadata ~debug
    ~trim_url ~autostart () : t =
  (new input
     ~kind ~url ~options ~format ~poll_delay ~timeout ~max_buffer
     ~metadata_filter ~deduplicate_metadata ~new_track_on_metadata ~debug
     ~trim_url ~autostart ()
    :> t)
