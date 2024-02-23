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

open Mm
open Source

class consumer buffer =
  object (self)
    inherit Source.source ~name:"buffer" ()
    method stype = `Fallible
    method private can_generate_frame = 0 < Generator.length buffer

    method private generate_frame =
      Generator.slice buffer (Lazy.force Frame.size)

    method abort_track = Generator.clear buffer
    method self_sync = (`Static, false)
    method seek_source = (self :> Source.source)
    method remaining = Generator.length buffer
  end

let finalise_child_clock child_clock source =
  Clock.forget source#clock child_clock

(** [rms_width] is in samples.
  * [cross_length] is in ticks (like #remaining estimations) and must be at least one frame. *)
class cross val_source ~duration_getter ~override_duration ~persist_override
  ~rms_width transition =
  let s = Lang.to_source val_source in
  let original_duration_getter = duration_getter in
  object (self)
    inherit source ~name:"cross" () as super

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive:(fun () -> false)
        ()

    inherit! Child_support.base ~check_self_sync:true [val_source]
    initializer Typing.(s#frame_type <: self#frame_type)
    method stype = `Fallible

    (* This is complicated. crossfade should never be used with [self_sync]
     * sources but we do not have a static way of knowing it at the moment.
     * Going with the same choice as above for now. *)
    method self_sync = s#self_sync
    val mutable cross_length = Lazy.force Frame.size
    val mutable rejected_cross_length = None
    val mutable duration_getter = original_duration_getter
    method cross_duration = duration_getter ()

    method set_cross_length =
      let new_cross_length = self#cross_duration in
      let main_new_cross_length = Frame.main_of_seconds new_cross_length in

      if main_new_cross_length <> cross_length then
        if new_cross_length <= 0. then (
          if rejected_cross_length <> Some new_cross_length then (
            self#log#critical "Invalid cross duration: %.2f <= 0.!"
              new_cross_length;
            rejected_cross_length <- Some new_cross_length);
          cross_length <- Lazy.force Frame.size)
        else (
          let main_new_cross_length =
            max (Lazy.force Frame.size) main_new_cross_length
          in
          self#log#info "Setting crossfade duration to %.2fs"
            (Frame.seconds_of_main main_new_cross_length);
          rejected_cross_length <- None;
          cross_length <- main_new_cross_length)

    initializer self#set_cross_length

    (* We need to store the end of a track, and compute the power of the signal
     * before the end of track. For doing so we need to remember a sliding window
     * of samples, maintain the sum of squares, and the number of samples in that
     * sum. The sliding window is necessary because of possibly inaccurate
     * remaining time estimaton. *)
    val mutable gen_before = Generator.create Frame.Fields.empty
    val mutable rms_before = 0.
    val mutable rmsi_before = 0
    val mutable mem_before = Array.make rms_width 0.
    val mutable mem_i = 0
    val mutable before_metadata = None

    (* Same for the new track. No need for a sliding window here. *)
    val mutable gen_after = Generator.create Frame.Fields.empty
    val mutable rms_after = 0.
    val mutable rmsi_after = 0
    val mutable after_metadata = None

    method private reset_analysis =
      gen_before <- Generator.create self#content_type;
      gen_after <- Generator.create self#content_type;
      rms_before <- 0.;
      rmsi_before <- 0;
      mem_i <- 0;
      Array.iteri (fun i _ -> mem_before.(i) <- 0.) mem_before;
      rms_after <- 0.;
      rmsi_after <- 0;
      before_metadata <- after_metadata;
      after_metadata <- None

    (* The played source. We _need_ exclusive control on that source,
     * since we are going to pull data from it at a higher rate around
     * track limits. *)
    val source = s

    method private prepare_source s =
      let s = (s :> source) in
      s#get_ready [(self :> source)];
      Clock.unify ~pos:self#pos source#clock s#clock;
      self#set_cross_length

    method! private wake_up a =
      self#reset_analysis;
      super#wake_up a;
      source#get_ready [(self :> source)];
      Lang.iter_sources (fun s -> s#get_ready [(self :> source)]) transition

    val mutable status
        : [ `Idle | `Before of Source.source | `After of Source.source ] =
      `Idle

    method private leave_status =
      match status with
        | `Idle -> ()
        | `Before s | `After s -> s#leave (self :> source)

    method! private sleep =
      source#leave (self :> source);
      s#leave (self :> source);
      Lang.iter_sources (fun s -> s#leave (self :> source)) transition;
      self#leave_status

    method private child_get ~is_first source =
      let frame = ref self#empty_frame in
      self#child_on_output (fun () ->
          frame :=
            source#get_partial_frame (fun f ->
                match self#split_frame f with
                  | buf, Some _ when Frame.position buf = 0 && is_first -> f
                  | buf, _ -> buf));
      !frame

    method private append mode buf_frame =
      let l = Frame.get_all_metadata buf_frame in
      List.iter
        (fun (_, m) ->
          match Frame.Metadata.find_opt override_duration m with
            | None -> ()
            | Some v -> (
                try
                  self#log#info "Overriding crossfade duration from metadata %s"
                    override_duration;
                  let l = float_of_string v in
                  duration_getter <- (fun () -> l)
                with _ -> ()))
        l;
      (match (List.rev l, mode) with
        | (_, m) :: _, `Before -> before_metadata <- Some m
        | (_, m) :: _, `After -> after_metadata <- Some m
        | _ -> ());
      match mode with
        | `Before -> Generator.append gen_before buf_frame
        | `After -> Generator.append gen_after buf_frame

    method private prepare_before =
      self#log#info "Buffering end of track...";
      let before = new consumer gen_before in
      Typing.(before#frame_type <: self#frame_type);
      self#prepare_source before;
      self#leave_status;
      status <- `Before (before :> Source.source);
      self#buffer_before ~is_first:true ();
      before

    method private get_source ~reselect () =
      match status with
        | `Idle when source#is_ready -> Some self#prepare_before
        | `Idle -> None
        | `Before _ -> (
            self#buffer_before ~is_first:false ();
            match status with
              | `Idle -> assert false
              | `Before before_source -> Some before_source
              | `After after_source -> Some after_source)
        | `After after_source
          when self#can_reselect
                 ~reselect:(match reselect with `Force -> `Ok | _ -> reselect)
                 after_source ->
            Some after_source
        | `After _ -> Some self#prepare_before

    method private buffer_before ~is_first () =
      if Generator.length gen_before < cross_length && source#is_ready then (
        let buf_frame = self#child_get ~is_first source in
        self#append `Before buf_frame;
        (* Analyze them *)
        let pcm = AFrame.pcm buf_frame in
        let len = Audio.length pcm in
        let squares = Audio.squares pcm 0 len in
        rms_before <- rms_before -. mem_before.(mem_i) +. squares;
        mem_before.(mem_i) <- squares;
        mem_i <- (mem_i + len) mod rms_width;
        rmsi_before <- min rms_width (rmsi_before + len);

        (* Should we buffer more or are we done ? *)
        if Frame.is_partial buf_frame then (
          if not persist_override then
            duration_getter <- original_duration_getter;
          self#analyze_after)
        else self#buffer_before ~is_first:false ())

    (* Analyze the beginning of a new track. *)
    method private analyze_after =
      let rec f ~is_first () =
        if
          Generator.length gen_after < Generator.length gen_before
          && source#is_ready
        then (
          let buf_frame = self#child_get ~is_first source in
          self#append `After buf_frame;
          if Generator.length gen_after <= rms_width then (
            let pcm = AFrame.pcm buf_frame in
            let len = Audio.length pcm in
            let squares = Audio.squares pcm 0 len in
            rms_after <- rms_after +. squares;
            rmsi_after <- rmsi_after + len);
          if Frame.is_partial buf_frame then
            self#log#critical
              "End of track reached while buffering next track data, crossfade \
               duration is longer than the track's duration. Make sure to \
               adjust the crossfade duration to avoid issues."
          else f ~is_first:false ())
      in
      f ~is_first:true ();
      self#create_after

    (* Sum up analysis and build the transition *)
    method private create_after =
      let db_after =
        Audio.dB_of_lin
          (sqrt (rms_after /. float rmsi_after /. float self#audio_channels))
      in
      let db_before =
        Audio.dB_of_lin
          (sqrt (rms_before /. float rmsi_before /. float self#audio_channels))
      in
      let buffered_before = Generator.length gen_before in
      let buffered_after = Generator.length gen_after in
      let buffered = min buffered_after buffered_before in
      let after =
        let metadata = function None -> Frame.Metadata.empty | Some m -> m in
        let before_metadata = metadata before_metadata in
        let after_metadata = metadata after_metadata in
        let before_head =
          if buffered < buffered_before then (
            let head =
              Generator.slice gen_before (buffered_before - buffered)
            in
            let head_gen =
              Generator.create ~content:head (Generator.content_type gen_before)
            in
            let s = new consumer head_gen in
            s#set_id (self#id ^ "_before_head");
            Typing.(s#frame_type <: self#frame_type);
            Some s)
          else None
        in
        let before = new consumer gen_before in
        Typing.(before#frame_type <: self#frame_type);
        let before = new Insert_metadata.replay before_metadata before in
        Typing.(before#frame_type <: self#frame_type);
        before#set_id (self#id ^ "_before");
        let after_tail =
          if buffered < buffered_after then (
            let head = Generator.slice gen_after buffered in
            let head_gen =
              Generator.create ~content:head (Generator.content_type gen_after)
            in
            let tail_gen = gen_after in
            gen_after <- head_gen;
            let s = new consumer tail_gen in
            Typing.(s#frame_type <: self#frame_type);
            s#set_id (self#id ^ "_after_tail");
            Some s)
          else None
        in
        let after = new consumer gen_after in
        Typing.(after#frame_type <: self#frame_type);
        let after = new Insert_metadata.replay after_metadata after in
        Typing.(after#frame_type <: self#frame_type);
        before#set_id (self#id ^ "_before");
        after#set_id (self#id ^ "_after");
        self#log#important "Analysis: %fdB / %fdB (%.2fs / %.2fs)" db_before
          db_after
          (Frame.seconds_of_main buffered_before)
          (Frame.seconds_of_main buffered_after);
        self#log#important
          "Computing crossfade duration over overlapping %.2fs buffered data \
           at start and end."
          (Frame.seconds_of_main buffered);
        let compound =
          let params =
            [
              ( "",
                Lang.meth Lang.unit
                  [
                    ("source", Lang.source before);
                    ("db_level", Lang.float db_before);
                    ( "expected_duration",
                      Lang.float (Frame.seconds_of_main cross_length) );
                    ( "buffered",
                      Lang.float (Frame.seconds_of_main buffered_before) );
                    ("metadata", Lang.metadata before_metadata);
                  ] );
              ( "",
                Lang.meth Lang.unit
                  [
                    ("source", Lang.source after);
                    ("db_level", Lang.float db_after);
                    ( "expected_duration",
                      Lang.float (Frame.seconds_of_main cross_length) );
                    ( "buffered",
                      Lang.float (Frame.seconds_of_main buffered_after) );
                    ("metadata", Lang.metadata after_metadata);
                  ] );
            ]
          in
          Lang.to_source (Lang.apply transition params)
        in
        Typing.(compound#frame_type <: self#frame_type);
        let compound =
          match (before_head, after_tail) with
            | None, None -> compound
            | Some s, None ->
                (new Sequence.sequence ~merge:true [s; compound]
                  :> Source.source)
            | None, Some s ->
                (new Sequence.sequence ~single_track:false [compound; s]
                  :> Source.source)
            | Some _, Some _ -> assert false
        in
        Clock.unify ~pos:self#pos compound#clock s#clock;
        Typing.(compound#frame_type <: self#frame_type);
        Clock.collect ();
        compound
      in
      self#prepare_source after;
      self#reset_analysis;
      self#leave_status;
      status <- `After after

    method remaining =
      match status with
        | `Idle -> source#remaining
        | `Before s -> (
            match (s#remaining, source#remaining) with
              | -1, _ | _, -1 -> -1
              | r, r' -> r + r')
        | `After s -> s#remaining

    method seek_source =
      match status with
        | `Idle -> source#seek_source
        | `Before s | `After s -> s#seek_source

    method abort_track =
      match status with
        | `Idle -> source#abort_track
        | `Before s | `After s ->
            source#abort_track;
            status <- `After s;
            ignore self#prepare_before
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
        ("expected_duration", ([], Lang.float_t), "Expected buffered duration.");
        ("buffered", ([], Lang.float_t), "Buffered duration.");
        ("metadata", ([], Lang.metadata_t), "Metadata of the source.");
      ]
  in
  Lang.add_operator "cross"
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 5.),
        Some
          "Duration (in seconds) of buffered data from each track that is used \
           to compute the transition between tracks." );
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
        ( "cross_duration",
          Lang.([], fun_t [] float_t),
          "Get the current crossfade duration.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#cross_duration) );
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
      let rms_width = Lang.to_float (List.assoc "width" p) in
      let rms_width = Frame.audio_of_seconds rms_width in
      let transition = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      new cross
        source transition ~duration_getter ~rms_width ~override_duration
        ~persist_override)
