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
        raise (Error.Invalid_value (val_source, "That source is fallible."))

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
          (* We prefer [name] as an ID over the default, but do not overwrite
             user-defined ID. Our ID will be used for the server interface. *)
          if name <> "" then self#set_id ~force:false name;

          self#log#debug "Clock is %s." (Clock.id self#clock);

          if Frame.Fields.is_empty self#content_type then
            failwith
              (Printf.sprintf
                 "Empty content-type detected for output %s. You might want to \
                  use an expliciy type annotation!"
                 self#id);

          if not autostart then start_stop#transition_to `Stopped);
      self#on_sleep (fun () -> start_stop#transition_to `Stopped)

    (* The output process *)
    val mutable skip = false
    method private skip = skip <- true

    method private generate_frame =
      Frame.map_metadata source#get_frame (fun (pos, m) ->
          Some (pos, self#add_on_air m))

    method output =
      if self#is_ready && state = `Idle then start_stop#transition_to `Started;
      if start_stop#state = `Started then (
        let data =
          if self#is_ready then self#get_frame else self#end_of_track
        in

        (* Output that frame if it has some data *)
        if Frame.position data > 0 then self#send_frame data;
        if Frame.is_partial data then (
          if self#is_ready && not self#fallible then (
            self#log#critical "Infallible source produced a partial frame!";
            assert false);
          self#log#info "Source ended (no more tracks) stopping output...";
          self#transition_to `Idle);

        if skip then (
          self#log#important "Performing user-requested skip";
          skip <- false;
          self#abort_track))
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
    method self_sync = s#self_sync
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.output "dummy"
    (proto @ [("", Lang.source_t return_t, None, None)])
    ~category:`Output
    ~descr:"Dummy output: computes the stream, without actually using it."
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
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

    method private send_frame frame =
      let rec output_chunks frame =
        let f start stop =
          let data = self#encode (Frame.sub frame start (stop - start)) in
          self#send data;
          match Frame.get_metadata frame start with
            | None -> ()
            | Some m ->
                self#encode_metadata
                  (Frame.Metadata.Export.from_metadata
                     ~cover:export_cover_metadata m)
        in
        function
        | [] -> assert false
        | [i] -> assert (i = Lazy.force Frame.size || not infallible)
        | start :: stop :: l ->
            if start < stop then f start stop else assert (start = stop);
            output_chunks frame (stop :: l)
      in
      output_chunks frame
        (0
        :: List.sort compare
             (List.map fst (Frame.get_all_metadata frame)
             @ [Frame.position frame]))
  end
