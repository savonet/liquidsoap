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

(** Custom classes for easy creation of output nodes. *)

open Source

let fallibility_check = ref true

let proto =
  Start_stop.output_proto
  @ [
      ( "register_telnet",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Register telnet commands for this output." );
      ( "fallible",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Allow the child source to fail, in which case the output will be \
           stopped until the source is available again." );
    ]

module Queue = Queues.Queue

(* ponytail: a constant bound on segments in a row that bring no data. It only
   has to exceed the depth of nested operators; count them if it ever does
   not. *)

let max_empty_segments = 16

(** Given abstract start stop and send methods, creates an output. Takes care of
    pulling the data out of the source, type checkings, maintains a queue of
    last ten metadata and setups standard Server commands, including start/stop.
*)

class virtual output ~output_kind ?clock ?(name = "") ~infallible
  ~register_telnet val_source autostart =
  let source = Lang.to_source val_source in
  object (self)
    initializer
      (* This should be done before the active_operator initializer attaches us
         to a clock. *)
      if !fallibility_check && infallible && source#fallible then
        raise (Error.Invalid_value (val_source, "That source is fallible.", []))

    initializer Typing.(source#frame_type <: self#frame_type)
    inherit active_operator ?clock ~name:output_kind [source]
    inherit Start_stop.base as start_stop
    method virtual private start : unit
    method virtual private stop : unit
    method virtual private send_frame : Frame.t -> unit
    method fallible = not infallible
    method! source_type : source_type = `Output (self :> Source.active)

    method private add_on_air m =
      let d = Unix.gettimeofday () in
      let m =
        Frame.Metadata.add "on_air" (Request.pretty_date (Unix.localtime d)) m
      in
      Frame.Metadata.add "on_air_timestamp" (Printf.sprintf "%.02f" d) m

    (* Metadata stuff: keep track of what was streamed. *)
    val q_length = 10
    val metadata_queue = Queue.create ()

    method private add_metadata m =
      let m = Frame.Metadata.Export.from_metadata ~cover:false m in
      Queue.push metadata_queue m;
      if Queue.length metadata_queue > q_length then
        ignore (Queue.pop metadata_queue)

    initializer
      self#on_frame (`Metadata self#add_metadata);
      if register_telnet then (
        self#register_command "start"
          (fun _ ->
            start_stop#transition_to `Started;
            "Done")
          ~descr:"Start the output.";
        self#register_command "stop"
          (fun _ ->
            start_stop#transition_to `Stopped;
            "Done")
          ~descr:"Stop the output.";
        self#register_command "status"
          (fun _ -> if start_stop#state = `Started then "on" else "off")
          ~descr:"Stop the output.";
        self#register_command "skip"
          (fun _ ->
            self#skip;
            "Done")
          ~descr:"Skip current song.";
        self#register_command "seek"
          (fun v ->
            try
              let len = self#seek (Frame.main_of_seconds (float_of_string v)) in
              Printf.sprintf "Seeked %.02f" (Frame.seconds_of_main len)
            with exn -> Printf.sprintf "Error: %s" (Printexc.to_string exn))
          ~usage:"seek <seconds>" ~descr:"Seek forward.";
        self#register_command "metadata" ~descr:"Print current metadata."
          (fun _ ->
            fst
              (Queue.fold metadata_queue
                 (fun m (s, i) ->
                   let s =
                     s
                     ^ (if s = "" then "--- " else "\n--- ")
                     ^ string_of_int i ^ " ---\n"
                     ^ Frame.Metadata.to_string
                         (Frame.Metadata.Export.to_metadata m)
                   in
                   (s, i - 1))
                 ("", Queue.length metadata_queue)));
        self#register_command "remaining"
          ~descr:"Display estimated remaining time." (fun _ ->
            let r = source#remaining in
            if r < 0 then "(undef)"
            else (
              let t = Frame.seconds_of_main r in
              Printf.sprintf "%.2f" t)))

    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method effective_source = source#effective_source

    (* Operator startup *)
    initializer
      self#on_wake_up (fun () ->
          self#clear_tick;
          (* We prefer [name] as an ID over the default, but do not overwrite
             user-defined ID. Our ID will be used for the server interface. *)
          if name <> "" then self#set_id ~force:false name;

          self#log#debug "Clock is %s." (Clock.name self#clock);

          if Frame.Fields.is_empty self#content_type then
            failwith
              (Printf.sprintf
                 "Empty content-type detected for output %s. You might want to \
                  use an expliciy type annotation!"
                 self#id);

          if not autostart then start_stop#execute_transition `Stopped);
      self#on_sleep (fun () ->
          start_stop#execute_transition `Idle;
          self#clear_tick)

    (* The output process *)
    val mutable skip = false
    method private skip = skip <- true

    method private generate_frame =
      Frame.map_metadata source#get_frame (fun (pos, m) ->
          Some (pos, self#add_on_air m))

    val on_output : (before:bool -> unit -> unit) Callbacks.t =
      Callbacks.create ()

    method register_on_output fn = Callbacks.register on_output fn
    method on_output fn = Callbacks.add on_output fn

    method private fire_on_output ~before () =
      List.iter (fun fn -> fn ~before ()) (Callbacks.elements on_output)

    (* Data gathered over the segments of the running tick. *)
    val mutable gathered = None

    (* A segment may be empty when it only states a track end, and nested
       operators may each state theirs in turn. *)
    val mutable empty_segments = 0

    (* Set once the tick's frame is sent: later segments of the tick, asked by
       another output, do not concern us. *)
    val mutable tick_done = false

    method output =
      if not tick_done then (
        self#fire_on_output ~before:true ();
        Fun.protect
          ~finally:(fun () -> self#fire_on_output ~before:false ())
          (fun () -> self#do_output))

    (* The next segment of the tick's frame, cut to the room left. *)
    method private next_segment ~room =
      if self#is_ready then
        self#get_partial_frame (fun frame -> Frame.slice frame room)
      else self#empty_frame

    (* A tick cut short (a stop, a sleep) may never run its after-tick
       callbacks: nothing of it is kept for the next one. *)
    method private clear_tick =
      tick_done <- false;
      gathered <- None;
      empty_segments <- 0

    method private finish_tick =
      tick_done <- true;
      gathered <- None;
      empty_segments <- 0;
      Clock.after_tick self#clock (fun () -> tick_done <- false)

    (* Called on every cycle where the source is unavailable or has ended.
       [`Hold] keeps the output started and skips the cycle; it also starts an
       idle output over an unavailable source. *)
    method private on_unavailable_source : [ `Stop | `Hold ] = `Stop

    method private do_output =
      let held = (not self#is_ready) && self#on_unavailable_source = `Hold in
      if (self#is_ready || held) && state = `Idle then
        start_stop#execute_transition `Started;
      (* What was gathered before a stop belongs to no frame we will send. *)
      if start_stop#state <> `Started then gathered <- None;
      if start_stop#state = `Started && not held then (
        let data = Option.value ~default:self#empty_frame gathered in
        (* The sources read on our behalf place their data at our position in
           the frame we gather. *)
        Clock.set_reading_position self#clock (Frame.position data);
        let segment =
          Fun.protect
            ~finally:(fun () -> Clock.set_reading_position self#clock 0)
            (fun () -> self#next_segment ~room:(Frame.remaining data))
        in
        let data = Frame.append data segment in
        empty_segments <-
          (if Frame.position segment = 0 then empty_segments + 1 else 0);
        if
          Frame.is_partial data
          && (match self#segment_end with
            | `Track_end | `Pause -> true
            | `Full | `Late -> false)
          && empty_segments <= max_empty_segments
        then (
          (* The source stopped where it has something to decide: what follows
             comes when we go on, in the next segment of this tick. *)
          gathered <- Some data;
          Clock.request_segment self#clock)
        else (
          self#finish_tick;

          (* Output that frame if it has some data *)
          if Frame.position data > 0 then self#send_frame data;
          if Frame.is_partial data then (
            if self#is_ready && not self#fallible then (
              self#log#critical "Infallible source produced a partial frame!";
              assert false);
            if self#on_unavailable_source = `Stop then (
              self#log#info "Source ended (no more tracks) stopping output...";
              self#execute_transition `Idle));

          if skip then (
            self#log#important "Performing user-requested skip";
            skip <- false;
            self#request_abort)))
  end

class dummy ?clock ~infallible ~autostart ~register_telnet source =
  let s = Lang.to_source source in
  object
    inherit
      output
        source autostart ?clock ~name:"dummy" ~output_kind:"output.dummy"
          ~infallible ~register_telnet

    method! private reset = ()
    method private start = ()
    method private stop = ()
    method private send_frame _ = ()
    method private self_sync = s#cached_self_sync
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.output "dummy"
    (proto @ [("", Lang.source_t return_t, None, None)])
    ~category:`Output
    ~descr:"Dummy output: computes the stream, without actually using it."
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.output_callbacks ())
    ~return_t
    (fun p ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      new dummy ~infallible ~autostart ~register_telnet (List.assoc "" p))

(** More concrete abstract-class, which takes care of the #send_frame method for
    outputs based on encoders. *)
class virtual ['a] encoded ~output_kind ?clock ~name ~infallible
  ~register_telnet ~autostart ~export_cover_metadata source =
  object (self)
    inherit
      output
        ~infallible ~output_kind ?clock ~name ~register_telnet source autostart

    method virtual private encode_metadata : Frame.Metadata.Export.t -> unit
    method virtual private encode : Frame.t -> 'a
    method virtual private send : 'a -> unit

    (* A metadata at the end of a frame labels the next frame's first piece. *)
    val mutable pending_metadata = None

    (* What the source is playing: a writer built later starts with it. *)
    method private current_metadata =
      match (Lang.to_source source)#last_metadata with
        | Some (_, m) -> m
        | None -> Frame.Metadata.empty

    method private export_metadata =
      Frame.Metadata.Export.from_metadata ~cover:export_cover_metadata

    method private initial_metadata = self#export_metadata self#current_metadata

    (* The output's own reaction to a metadata, given in full. *)
    method private on_metadata (_ : Frame.Metadata.t) = ()

    method private offer_metadata m =
      let m = self#export_metadata m in
      if not (Frame.Metadata.Export.is_empty m) then self#encode_metadata m

    (* Metadata by position, those at one position merged in frame order,
       the pending one under the frame's own at 0. *)
    method private metadata_by_position frame =
      let merge positions (position, m) =
        match positions with
          | (last, m') :: rest when last = position ->
              (position, Frame.Metadata.append m' m) :: rest
          | positions -> (position, m) :: positions
      in
      let pending =
        match pending_metadata with Some m -> [(0, m)] | None -> []
      in
      pending_metadata <- None;
      List.rev
        (List.fold_left merge []
           (pending
           @ List.stable_sort
               (fun (p, _) (p', _) -> Int.compare p p')
               (Frame.get_all_metadata frame)))

    method private send_frame frame =
      let length = Frame.position frame in
      assert (length = Lazy.Mutexed.force Frame.size || not infallible);
      let metadata = self#metadata_by_position frame in
      let send_piece start stop =
        let piece_metadata = List.assoc_opt start metadata in
        Option.iter self#on_metadata piece_metadata;
        Option.iter self#offer_metadata piece_metadata;
        self#send (self#encode (Frame.sub frame start (stop - start)))
      in
      let cuts =
        List.filter (fun p -> 0 < p && p < length) (List.map fst metadata)
      in
      let rec send_pieces start = function
        | stop :: cuts ->
            send_piece start stop;
            send_pieces stop cuts
        | [] -> if start < length then send_piece start length
      in
      send_pieces 0 cuts;
      pending_metadata <- List.assoc_opt length metadata
  end
