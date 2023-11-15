(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** Abstract classes for easy creation of output nodes. *)

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

let meth = Start_stop.meth ()

(** Given abstract start stop and send methods, creates an output.  Takes care
    of pulling the data out of the source, type checkings, maintains a queue of
    last ten metadata and setups standard Server commands, including
    start/stop. *)
class virtual output ~output_kind ?(name = "") ~infallible ~register_telnet
  ~(on_start : unit -> unit) ~(on_stop : unit -> unit) val_source autostart =
  let source = Lang.to_source val_source in
  object (self)
    initializer
      (* This should be done before the active_operator initializer attaches us
         to a clock. *)
      if !fallibility_check && infallible && source#stype <> `Infallible then
        raise (Error.Invalid_value (val_source, "That source is fallible."))

    initializer Typing.(source#frame_type <: self#frame_type)
    inherit active_operator ~name:output_kind [source]
    inherit Start_stop.base ~on_start ~on_stop as start_stop
    method virtual private start : unit
    method virtual private stop : unit
    method virtual private send_frame : Frame.t -> unit
    method self_sync = source#self_sync
    method stype = if infallible then `Infallible else `Fallible

    (* Registration of Telnet commands must be delayed because some operators
       change their id at initialization time. *)
    val mutable registered_telnet = false

    method private register_telnet =
      if register_telnet && not registered_telnet then (
        registered_telnet <- true;
        (* Add a few more server controls *)
        let ns = [self#id] in
        Server.add ~ns "skip"
          (fun _ ->
            self#skip;
            "Done")
          ~descr:"Skip current song.";
        Server.add ~ns "metadata" ~descr:"Print current metadata." (fun _ ->
            let q = self#metadata_queue in
            fst
              (Queue.fold
                 (fun (s, i) m ->
                   let s =
                     s
                     ^ (if s = "" then "--- " else "\n--- ")
                     ^ string_of_int i ^ " ---\n"
                     ^ Request.string_of_metadata
                         (Frame.Metadata.Export.to_metadata m)
                   in
                   (s, i - 1))
                 ("", Queue.length q)
                 q));
        Server.add ~ns "remaining" ~descr:"Display estimated remaining time."
          (fun _ ->
            let r = source#remaining in
            if r < 0 then "(undef)"
            else (
              let t = Frame.seconds_of_main r in
              Printf.sprintf "%.2f" t)))

    method private cleanup_telnet =
      if registered_telnet then
        List.iter
          (Server.remove ~ns:[self#id])
          ["skip"; "metadata"; "remaining"];
      registered_telnet <- false

    method private can_generate_data =
      if infallible then (
        assert source#is_ready;
        true)
      else source#is_ready

    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek_source = source#seek_source

    (* Operator startup *)
    method! private wake_up activation =
      (* We prefer [name] as an ID over the default, but do not overwrite
         user-defined ID. Our ID will be used for the server interface. *)
      if name <> "" then self#set_id ~definitive:false name;

      self#log#debug "Clock is %s."
        (Source.Clock_variables.to_string self#clock);
      self#log#important "Content type is %s."
        (Frame.string_of_content_type self#content_type);

      if Frame.Fields.is_empty self#content_type then
        failwith
          (Printf.sprintf
             "Empty content-type detected for output %s. You might want to use \
              an expliciy type annotation!"
             self#id);

      (* Get our source ready. This can take a while (preparing playlists,
         etc). *)
      source#get_ready ((self :> operator) :: activation);

      if source#stype = `Infallible then
        start_stop#transition_to (if autostart then `Started else `Stopped);

      self#register_telnet

    method! private sleep =
      start_stop#transition_to `Stopped;
      source#leave (self :> operator)

    (* Metadata stuff: keep track of what was streamed. *)
    val q_length = 10
    val metadata_q = Queue.create ()

    method private add_metadata m =
      Queue.add m metadata_q;
      if Queue.length metadata_q > q_length then ignore (Queue.take metadata_q)

    method private metadata_queue = Queue.copy metadata_q

    (* The output process *)
    val mutable skip = false
    method private skip = skip <- true
    method private generate_data = source#get_data

    method private output =
      self#has_ticked;
      if self#is_ready && state <> `Stopped then
        start_stop#transition_to `Started;
      if start_stop#state = `Started then (
        let data = source#get_data in
        Printf.printf "Got frame of length: %d\n%!" (Frame.position data);
        List.iter
          (fun (_, m) ->
            self#add_metadata
              (Frame.Metadata.Export.from_metadata ~cover:false m))
          (Frame.get_all_metadata data);

        (* Output that frame if it has some data *)
        if Frame.position data > 0 then self#send_frame data;
        if Frame.is_partial data then (
          self#log#important "Source failed (no more tracks) stopping output...";
          self#transition_to `Idle))

    initializer
      self#on_after_output (fun () ->
          (* Perform skip if needed *)
          if skip then (
            self#log#important "Performing user-requested skip";
            skip <- false;
            self#abort_track))
  end

class dummy ~infallible ~on_start ~on_stop ~autostart ~register_telnet source =
  object
    inherit
      output
        source autostart ~name:"dummy" ~output_kind:"output.dummy" ~infallible
          ~on_start ~on_stop ~register_telnet

    method! private reset = ()
    method private start = ()
    method private stop = ()
    method private send_frame _ = ()
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.output "dummy"
    (proto @ [("", Lang.source_t return_t, None, None)])
    ~category:`Output
    ~descr:"Dummy output: computes the stream, without actually using it." ~meth
    ~return_t
    (fun p ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply on_start []) in
      let on_stop () = ignore (Lang.apply on_stop []) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      new dummy
        ~on_start ~on_stop ~infallible ~autostart ~register_telnet
        (List.assoc "" p))

(** More concrete abstract-class, which takes care of the #send_frame method for
    outputs based on encoders. *)
class virtual encoded ~output_kind ~name ~infallible ~on_start ~on_stop
  ~register_telnet ~autostart ~export_cover_metadata source =
  object (self)
    inherit
      output
        ~infallible ~on_start ~on_stop ~output_kind ~name ~register_telnet
          source autostart

    method virtual private insert_metadata : Frame.Metadata.Export.t -> unit
    method virtual private encode : Frame.t -> int -> int -> 'a
    method virtual private send : 'a -> unit

    method private send_frame frame =
      let rec output_chunks frame =
        let f start stop =
          let data = self#encode frame start (stop - start) in
          self#send data;
          match Frame.get_metadata frame start with
            | None -> ()
            | Some m ->
                self#insert_metadata
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
