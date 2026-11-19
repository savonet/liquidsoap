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

open Mm
open Source

(* A finite store of stream data handed to script code: the end of a track,
   or the start of the next one. *)
class consumer ~name ~clock buffer =
  object (self)
    inherit Source.source ~clock ~name ()
    method fallible = true
    method private can_generate_frame = 0 < Generator.length buffer
    method! exhausted = Generator.length buffer = 0 && self#held = 0

    method private generate_frame =
      Generator.slice buffer (Lazy.Mutexed.force Frame.size)

    method abort_track = Generator.clear buffer
    method private self_sync = (`Static, None)
    method effective_source = (self :> Source.source)
    method remaining = Generator.length buffer + self#held
  end

(* What was read of one track of the child and not delivered yet. *)
type track = {
  data : Generator.t;
  (* The child stated the end of the track and we read up to it. *)
  mutable ended : bool;
  (* The cross duration of the track, in main ticks. It governs how far ahead
     the track is read and the crossing at its end, and is left alone once
     that end is found. *)
  mutable duration : int;
  (* Every metadata read of the track, key by key, the latest value winning. *)
  mutable metadata : Frame.metadata;
  (* The last [width] of what was read of the track and has left [data]: the
     level of an ending track reaches back into what was delivered. *)
  recent : Generator.t;
}

type transition = {
  source : Source.source;
  activation : Clock.activation;
  release_callbacks : unit -> unit;
  (* The metadata of the start of the next track, when the starting source
     held some. *)
  starting_metadata : Frame.metadata option;
  (* Whether that metadata came out of the transition source. *)
  mutable announced : bool;
}

(* Put on the metadata of the starting source and taken off whatever the
   transition source delivers: seeing it come out is how we know the next
   track was announced, whatever its values. *)
let passage_key = "liq_cross_passage"

(* What was buffered, compared before and after a read to tell whether it
   brought anything. *)
type buffered = { track_count : int; length : int; last_ended : bool }

(* A transition function that raises is logged at a bounded rate and the
   crossing plays its two sources in order. *)
let script_faults = Faults.create (Log.make ["cross"])

let float_metadata key metadata =
  Option.bind (Frame.Metadata.find_opt key metadata) float_of_string_opt

(** [rms_width] is in main ticks. *)
class cross val_source ~override_duration ~duration_getter ~persist_override
  ~rms_width transition =
  let s = Lang.to_source val_source in
  object (self)
    inherit source ~name:"cross" ()
    inherit Child_support.base ~check_self_sync:true val_source
    initializer Typing.(s#frame_type <: self#frame_type)
    method fallible = true

    method! child_clock_controller =
      Some (`Other ("source", (self :> < id : string >)))

    (* The tracks buffered, the current one first. *)
    val mutable tracks : track list = []
    val mutable phase : [ `Idle | `Track | `Transition of transition ] = `Idle

    (* How the next sample delivered is entered. An entry that is owed stays
       owed until a sample is delivered. *)
    val mutable entry : [ `Track | `Continue | `As_is ] = `As_is

    (* Metadata to put on the next sample of the current track: that of a
       track the transition did not announce. *)
    val mutable owed_metadata = None

    (* The last override found, kept for the tracks that follow when
       [persist_override] is set. *)
    val mutable persisted_duration = None
    val mutable child_stated_end = false
    val mutable generated_end : segment_end = `Late

    method private resolve_duration seconds =
      let duration = Frame.main_of_seconds seconds in
      if duration < 0 then (
        self#log#important "Negative cross duration %f: using zero." seconds;
        0)
      else duration

    method cross_duration =
      Frame.seconds_of_main
        (match List.rev tracks with
          | track :: _ -> track.duration
          | [] -> self#resolve_duration (duration_getter ()))

    method private new_track =
      let duration =
        match persisted_duration with
          | Some duration -> duration
          | None -> self#resolve_duration (duration_getter ())
      in
      let track =
        {
          data = Generator.create self#content_type;
          ended = false;
          duration;
          metadata = Frame.Metadata.empty;
          recent = Generator.create self#content_type;
        }
      in
      tracks <- tracks @ [track];
      track

    method private read_metadata track metadata =
      track.metadata <- Frame.Metadata.append track.metadata metadata;
      match float_metadata override_duration metadata with
        | None -> ()
        | Some seconds ->
            let duration = self#resolve_duration seconds in
            self#log#info "Cross duration from metadata %s: %.02fs."
              override_duration seconds;
            track.duration <- duration;
            if persist_override then persisted_duration <- Some duration

    method private close_last_track =
      match List.rev tracks with
        | track :: _ when not track.ended -> track.ended <- true
        | _ -> ()

    (* Appends the data of one track to the buffer. A mark on its first sample
       closes a track left open: a child that starts a track without having
       stated the end of the previous one. *)
    method private buffer_track_data frame =
      let starts_track = List.mem 0 (Frame.track_marks frame) in
      let track =
        match List.rev tracks with
          | track :: _ when (not track.ended) && not starts_track -> track
          | track :: _ when not track.ended ->
              track.ended <- true;
              self#new_track
          | _ -> self#new_track
      in
      List.iter
        (fun (_, metadata) -> self#read_metadata track metadata)
        (Frame.get_all_metadata frame);
      Generator.append track.data frame

    (* Moves what the child output wrote into the buffer, cut where tracks
       end. *)
    method private take_intake =
      let intake = self#child_buffer in
      let rec buffer frame =
        if 0 < Frame.position frame then (
          match List.filter (fun p -> 0 < p) (Frame.track_marks frame) with
            | [] -> self#buffer_track_data frame
            | marks ->
                let next_track = List.fold_left Int.min max_int marks in
                self#buffer_track_data (Frame.sub frame 0 next_track);
                buffer (Frame.after frame next_track))
      in
      buffer (Generator.slice intake (Generator.length intake));
      if child_stated_end then (
        child_stated_end <- false;
        self#close_last_track)

    initializer
      self#on_wake_up (fun () ->
          self#child#set_on_track_end (fun () -> child_stated_end <- true))

    (* What the next track asks to have buffered for its own fade-in. *)
    method private fade_in_window track =
      let seconds key =
        Option.value ~default:0. (float_metadata key track.metadata)
      in
      Frame.main_of_seconds
        (seconds "liq_fade_in_delay" +. seconds "liq_fade_in")

    (* Whether more of the child should be read: [reserve] ahead of the current
       track's cross duration while its end is not found, then the starting
       window of the next track, and at least one sample of it. *)
    method private wants_more ~reserve =
      match tracks with
        | [] -> true
        | [current] when not current.ended ->
            Generator.length current.data < current.duration + reserve
        | [_] -> true
        | current :: next :: _ when not next.ended ->
            Generator.length next.data
            < Int.max 1 (Int.max current.duration (self#fade_in_window next))
        | _ -> false

    method private buffered =
      {
        track_count = List.length tracks;
        length =
          List.fold_left
            (fun n track -> n + Generator.length track.data)
            0 tracks;
        last_ended =
          (match List.rev tracks with track :: _ -> track.ended | [] -> false);
      }

    (* Reads the child while serving a request. Not being available ends the
       reading, and says nothing about the track. *)
    method private top_up ~reserve =
      if self#wants_more ~reserve && self#child_is_ready then (
        let before = self#buffered in
        if Generator.length self#child_buffer = 0 then self#child_tick;
        self#take_intake;
        if self#buffered <> before then self#top_up ~reserve)

    method private level frame =
      let pcm = AFrame.pcm frame in
      let count = Audio.length pcm in
      if count = 0 then neg_infinity
      else
        Audio.dB_of_lin
          (sqrt
             (Audio.squares pcm 0 count /. float count
            /. float self#audio_channels))

    (* The level of an ending track is taken over the last [rms_width] read of
       it, of which [recent] holds what has already left the buffer. *)
    method private remember track frame =
      Generator.append track.recent frame;
      let excess = Generator.length track.recent - rms_width in
      if 0 < excess then Generator.truncate track.recent excess

    method private ending_level track frame =
      let read =
        Frame.append
          (Generator.slice ~peek:true track.recent
             (Generator.length track.recent))
          frame
      in
      self#level
        (Frame.after read (Int.max 0 (Frame.position read - rms_width)))

    method private starting_level track frame =
      let rest =
        Generator.slice ~peek:true track.data (Generator.length track.data)
      in
      self#level (Frame.slice (Frame.append frame rest) rms_width)

    method private finite_source ~name frame =
      let source =
        new consumer
          ~name:(Printf.sprintf "%s.%s" self#id name)
          ~clock:self#clock
          (Generator.create ~content:frame self#content_type)
      in
      Typing.(source#frame_type <: self#frame_type);
      (source :> Source.source)

    (* A source built at run time sits in a clock of its own until it is made
       ours, and is never ready there. *)
    method private wake source =
      Typing.(source#frame_type <: self#frame_type);
      Clock.unify ~pos:self#pos source#clock self#clock;
      source#wake_up (self :> Clock.source)

    method private release_transition { source; activation; release_callbacks }
        =
      source#sleep activation;
      release_callbacks ()

    (* The crossing from [ending] to [next], at the point where nothing is left
       to play of [ending] before it. *)
    method private commit_crossing ~ending ~next ~rest =
      let ending_frame =
        Generator.slice ending.data (Generator.length ending.data)
      in
      let overlap = Frame.position ending_frame in
      let starting_frame =
        Generator.slice next.data (Int.max overlap (self#fade_in_window next))
      in
      let ending_level = self#ending_level ending ending_frame in
      let starting_level = self#starting_level next starting_frame in
      self#remember next starting_frame;
      let starting_metadata =
        match Frame.get_all_metadata starting_frame with
          | [] -> None
          | metadata ->
              Some
                (List.fold_left
                   (fun union (_, metadata) ->
                     Frame.Metadata.append union metadata)
                   Frame.Metadata.empty metadata)
      in
      let starting_frame =
        Frame.map_metadata starting_frame (fun (position, metadata) ->
            Some (position, Frame.Metadata.add passage_key "true" metadata))
      in
      let ending_source = self#finite_source ~name:"ending" ending_frame in
      let starting_source =
        self#finite_source ~name:"starting" starting_frame
      in
      self#log#important "Crossing: %.2fs at %fdB into %.2fs at %fdB."
        (Frame.seconds_of_main overlap)
        ending_level
        (Frame.seconds_of_main (Frame.position starting_frame))
        starting_level;
      tracks <- next :: rest;
      let argument source level metadata =
        ( "",
          Lang.meth Lang.unit
            [
              ("source", Lang.source source);
              ("db_level", Lang.float level);
              ("metadata", Lang.metadata metadata);
            ] )
      in
      (* The function runs at every crossing: what it registers on the sources
         it can reach, the two it is handed and our child, is released with the
         transition. *)
      let { Lang_source.release = release_callbacks; result = source } =
        Lang_source.collect_callback_releases
          [ending_source; starting_source; s] (fun () ->
            try
              let source =
                Lang.to_source
                  (Lang.apply transition
                     [
                       argument ending_source ending_level ending.metadata;
                       argument starting_source starting_level next.metadata;
                     ])
              in
              Typing.(source#frame_type <: self#frame_type);
              Clock.unify ~pos:self#pos source#clock self#clock;
              Some source
            with
            | ( Runtime_error.Runtime_error _ | Repr.Type_error _
              | Repr.Track_error _ | Clock.Conflict _ | Clock.Loop _ ) as error
            ->
              let backtrace = Printexc.get_raw_backtrace () in
              Faults.script_error script_faults
                ~kind:(Printf.sprintf "The transition function of %s" self#id)
                error backtrace;
              None)
      in
      let transition =
        match source with
          | None -> None
          | Some source ->
              let activation = self#wake source in
              if source#is_ready then Some (source, activation)
              else (
                self#log#important "The transition has nothing to deliver.";
                source#sleep activation;
                None)
      in
      let source, activation, release_callbacks =
        match transition with
          | Some (source, activation) -> (source, activation, release_callbacks)
          | None ->
              (* We stated a track end and answered ready before the function
                 ran: whatever came of it, both tracks are heard. *)
              self#log#important "Playing the two sources of the crossing.";
              release_callbacks ();
              let source =
                (new Sequence.sequence
                   ~name:(Printf.sprintf "%s.plain" self#id)
                   [ending_source; starting_source]
                  :> Source.source)
              in
              (source, self#wake source, fun () -> ())
      in
      phase <-
        `Transition
          {
            source;
            activation;
            release_callbacks;
            starting_metadata;
            announced = false;
          };
      entry <- `Track;
      Clock.signal_sync_change self#clock

    (* The transition is left for the current track. *)
    method private leave_transition transition =
      self#release_transition transition;
      phase <- `Track;
      Clock.signal_sync_change self#clock;
      let goes_on =
        match tracks with
          | current :: _ ->
              0 < Generator.length current.data || not current.ended
          | [] -> false
      in
      if goes_on then (
        if not transition.announced then
          owed_metadata <- transition.starting_metadata;
        if entry <> `Track then entry <- `Continue)
      else entry <- `Track;
      goes_on

    (* Applies the entry owed to the first sample of [frame]. Each frame goes
       through this before anything is stated about what follows it. *)
    method private enter frame =
      if Frame.position frame = 0 then frame
      else (
        let frame =
          match (entry, List.mem 0 (Frame.track_marks frame)) with
            | `Track, false -> Frame.add_track_mark frame 0
            | `Continue, true ->
                Frame.add_track_marks
                  (Frame.drop_track_marks frame)
                  (List.filter (fun p -> 0 < p) (Frame.track_marks frame))
            | _ -> frame
        in
        entry <- `As_is;
        frame)

    method private deliver_track =
      match tracks with
        | [] ->
            phase <- `Idle;
            generated_end <- `Late;
            self#empty_frame
        | current :: rest ->
            let room = Lazy.Mutexed.force Frame.size in
            let length = Generator.length current.data in
            if not current.ended then (
              let frame = Generator.slice current.data room in
              self#remember current frame;
              generated_end <-
                (if Frame.position frame < room then `Late else `Full);
              self#enter (self#announce frame))
            else (
              let next_length =
                match rest with
                  | next :: _ -> Generator.length next.data
                  | [] -> 0
              in
              (* What is played of the track before the crossing point. *)
              let head =
                Int.max 0 (length - Int.min current.duration next_length)
              in
              match rest with
                | next :: rest when head = 0 ->
                    self#commit_crossing ~ending:current ~next ~rest;
                    self#deliver
                | _ ->
                    let frame =
                      Generator.slice current.data (Int.min room head)
                    in
                    self#remember current frame;
                    if Frame.position frame = head then (
                      generated_end <- `Track_end;
                      if rest = [] then (
                        (* Nothing follows: the track played to its last
                           sample. *)
                        tracks <- [];
                        phase <- `Idle))
                    else generated_end <- `Full;
                    self#enter (self#announce frame))

    (* Puts the metadata owed on the first sample of the current track
       delivered after a transition. What the sample carries wins. *)
    method private announce frame =
      match owed_metadata with
        | Some metadata when 0 < Frame.position frame ->
            owed_metadata <- None;
            let carried =
              Option.value ~default:Frame.Metadata.empty
                (Frame.get_metadata frame 0)
            in
            Frame.add_metadata frame 0 (Frame.Metadata.append metadata carried)
        | _ -> frame

    method private deliver_transition transition =
      let source = transition.source in
      if source#is_ready then (
        let frame =
          Frame.map_metadata source#get_frame (fun (position, metadata) ->
              if Frame.Metadata.mem passage_key metadata then (
                transition.announced <- true;
                let metadata = Frame.Metadata.remove passage_key metadata in
                if Frame.Metadata.is_empty metadata then None
                else Some (position, metadata))
              else Some (position, metadata))
        in
        (* What is owed applies to this frame; what is stated below applies to
           what follows it. *)
        let frame = self#enter frame in
        let stated = source#segment_end in
        if source#exhausted then
          generated_end <-
            (if self#leave_transition transition then `Pause else `Track_end)
        else (
          generated_end <- stated;
          if stated = `Track_end then entry <- `Track);
        frame)
      else if source#exhausted || self#track_can_deliver then (
        ignore (self#leave_transition transition);
        self#deliver)
      else (
        generated_end <- `Late;
        self#empty_frame)

    method private deliver =
      match phase with
        | `Idle -> (
            match tracks with
              | [] ->
                  generated_end <- `Late;
                  self#empty_frame
              | _ ->
                  phase <- `Track;
                  entry <- `Track;
                  self#deliver_track)
        | `Track -> self#deliver_track
        | `Transition transition -> self#deliver_transition transition

    method private generate_frame =
      let reserve =
        match phase with
          | `Transition _ -> 0
          | `Idle | `Track -> Lazy.Mutexed.force Frame.size
      in
      self#top_up ~reserve;
      self#deliver

    method private frame_end (_ : Frame.t) = generated_end

    (* Whether the current track, or what follows it, has something to
       deliver: data we hold, a crossing that is decided, or a child that
       delivers when read. Reads nothing. *)
    method private track_can_deliver =
      match tracks with
        | [] -> self#child_is_ready
        | current :: rest ->
            0 < Generator.length current.data
            || rest <> [] || self#child_is_ready

    method private can_generate_frame =
      match phase with
        | `Idle | `Track -> self#track_can_deliver
        | `Transition { source } -> source#is_ready || self#track_can_deliver

    (* The time to the next mark we will put. *)
    method remaining =
      let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
      let track_remaining () =
        match tracks with
          | [] -> self#child#remaining
          | current :: rest when current.ended ->
              let next_length =
                match rest with
                  | next :: _ -> Generator.length next.data
                  | [] -> 0
              in
              Int.max 0
                (Generator.length current.data
                - Int.min current.duration next_length)
          | current :: _ -> (
              match Generator.length current.data + self#child#remaining with
                | -1 -> -1
                | total -> Int.max 0 (total - current.duration))
      in
      match phase with
        | `Idle | `Track -> track_remaining ()
        | `Transition { source } -> source#remaining + track_remaining ()

    method effective_source = (self :> Source.source)

    (* Our tracks are those of our child. *)
    method! resolved_composition =
      match self#composition with
        | (`File | `Live) as tag -> tag
        | `Passthrough -> s#resolved_composition

    (* The child's sync source paces the child clock, not ours. *)
    method private self_sync =
      ( `Dynamic,
        match phase with
          | `Transition { source } -> snd source#cached_self_sync
          | `Idle | `Track -> None )

    method! exhausted =
      self#held = 0 && self#child#exhausted
      && Generator.length self#child_buffer = 0
      && List.for_all (fun track -> Generator.length track.data = 0) tracks
      && match phase with `Transition _ -> false | `Idle | `Track -> true

    method private reset =
      (match phase with
        | `Transition transition -> self#release_transition transition
        | `Idle | `Track -> ());
      phase <- `Idle;
      tracks <- [];
      owed_metadata <- None;
      child_stated_end <- false

    initializer
      self#on_wake_up (fun () ->
          self#reset;
          entry <- `As_is;
          persisted_duration <- None);
      self#on_sleep (fun () -> self#reset)

    (* Carried out on the streaming thread, between two cycles. Everything of
       the track being heard goes. The child is aborted only while it is still
       on that track: once its end was found the child is on the next one,
       which is kept. *)
    method abort_track =
      match phase with
        | `Idle -> ()
        | `Track | `Transition _ -> (
            let kept =
              match tracks with
                | current :: rest when current.ended -> Some rest
                | _ -> None
            in
            self#reset;
            self#discard_held;
            entry <- `Track;
            match kept with
              | Some rest -> tracks <- rest
              | None ->
                  Generator.clear self#child_buffer;
                  self#child#abort_track)
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  let transition_arg =
    Lang.method_t Lang.unit_t
      [
        ("source", ([], Lang.source_t frame_t), "Source");
        ("db_level", ([], Lang.float_t), "dB level of the source.");
        ("metadata", ([], Lang.metadata_t), "Metadata of the source.");
      ]
  in
  Lang.add_operator "cross"
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 5.),
        Some
          "Duration (in seconds) of buffered data from the end and start of \
           each track that is used to compute the transition between tracks." );
      ( "override_duration",
        Lang.string_t,
        Some (Lang.string "liq_cross_duration"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the 'duration' parameter for current track." );
      ( "persist_override",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Keep duration override on track change." );
      ( "width",
        Lang.float_t,
        Some (Lang.float 2.),
        Some "Width of the power computation window." );
      ( "",
        Lang.fun_t
          [(false, "", transition_arg); (false, "", transition_arg)]
          (Lang.source_t frame_t),
        None,
        Some
          "Transition function, composing from the end of a track and the next \
           track. The sources corresponding to the two tracks are decorated \
           with fields indicating the power of the signal before and after the \
           transition (`power`), and the metadata (`metadata`)." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Fade
    ~meth:
      [
        {
          name = "cross_duration";
          scheme = Lang.([], fun_t [] float_t);
          descr = "Get the current crossfade duration.";
          value =
            (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#cross_duration));
        };
      ]
    ~descr:
      "Cross operator, allowing the composition of the _n_ last seconds of a \
       track with the beginning of the next track, using a transition function \
       depending on the relative power of the signal before and after the end \
       of track."
    (fun p ->
      let duration_getter = Lang.to_float_getter (List.assoc "duration" p) in
      let override_duration =
        Lang.to_string (List.assoc "override_duration" p)
      in
      let persist_override = Lang.to_bool (List.assoc "persist_override" p) in
      let width = List.assoc "width" p in
      if Lang.to_float width <= 0. then
        raise
          (Error.Invalid_value
             (width, "The width of the level analysis must be positive.", []));
      let rms_width = Frame.main_of_seconds (Lang.to_float width) in
      let transition = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      new cross
        source transition ~duration_getter ~rms_width ~override_duration
        ~persist_override)
