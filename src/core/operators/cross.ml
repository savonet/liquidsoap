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

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "crossfade") "Crossfade settings"

let conf_assume_autocue =
  Dtools.Conf.bool
    ~p:(conf#plug "assume_autocue")
    ~d:false
    "Assume autocue when all 4 cue in/out and fade in/out metadata override \
     are present."

class consumer ~clock buffer =
  object (self)
    inherit Source.source ~clock ~name:"cross.buffer" ()
    method fallible = true
    method private can_generate_frame = 0 < Generator.length buffer

    method private generate_frame =
      Generator.slice buffer (Lazy.force Frame.size)

    method abort_track = Generator.clear buffer
    method self_sync = (`Static, None)
    method seek_source = (self :> Source.source)
    method remaining = Generator.length buffer
  end

(** [rms_width] is in samples. [cross_length] is in ticks (like #remaining
    estimations) and must be at least one frame. *)
class cross val_source ~end_duration_getter ~override_end_duration
  ~override_duration ~start_duration_getter ~override_start_duration
  ~override_max_start_duration ~persist_override ~rms_width ~assume_autocue
  transition =
  let s = Lang.to_source val_source in
  let original_end_duration_getter = end_duration_getter in
  let original_start_duration_getter = start_duration_getter in
  object (self)
    inherit source ~name:"cross" ()

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> false)
        ~track_sensitive:(fun () -> false)
        ()

    inherit Child_support.base ~check_self_sync:true [val_source]
    initializer Typing.(s#frame_type <: self#frame_type)
    method fallible = true

    (* This is complicated. crossfade should never be used with [self_sync]
     * sources but we do not have a static way of knowing it at the moment.
     * Going with the same choice as above for now. *)
    method self_sync = s#self_sync
    val mutable end_duration_getter = end_duration_getter
    val mutable start_duration_getter = start_duration_getter
    val mutable max_start_duration = None
    val mutable end_main_duration = 0
    val mutable max_start_main_duration = None
    val mutable start_main_duration = 0
    method end_duration = Frame.seconds_of_main end_main_duration
    method start_duration = Frame.seconds_of_main start_main_duration

    method set_end_main_duration =
      let end_duration = end_duration_getter () in
      let _end_main_duration = Frame.main_of_seconds end_duration in
      let frame_size = Lazy.force Frame.size in

      end_main_duration <-
        (if _end_main_duration < 0 then (
           self#log#important
             "Cannot set crossfade end duration to negative value %f!"
             end_duration;
           frame_size
           (* Accept zero as simplify disabled crossfade. Set to frame_size. *))
         else if _end_main_duration = 0 then frame_size
           (* For any non-zero too short value, warn the user. *)
         else if _end_main_duration < frame_size then (
           self#log#important
             "Cannot set crossfade end duration to less than the frame size!";
           frame_size)
         else _end_main_duration)

    method set_start_main_duration =
      let start_duration = start_duration_getter () in
      let _start_main_duration = Frame.main_of_seconds start_duration in
      let frame_size = Lazy.force Frame.size in

      start_main_duration <-
        (if _start_main_duration < 0 then (
           self#log#important
             "Cannot set crossfade start duration to negative value %f!"
             start_duration;
           frame_size
           (* Accept zero as simplify disabled crossfade. Set to frame_size. *))
         else if _start_main_duration = 0 then frame_size
           (* For any non-zero too short value, warn the user. *)
         else if _start_main_duration < frame_size then (
           self#log#important
             "Cannot set crossfade start duration to less than the frame size!";
           frame_size)
         else _start_main_duration);

      max_start_main_duration <-
        (match max_start_duration with
          | None -> None
          | Some max_start_duration ->
              let _max_start_main_duration =
                Frame.main_of_seconds max_start_duration
              in
              if _max_start_main_duration < 0 then (
                self#log#important
                  "Cannot set crossfade max start duration to negative value \
                   %f!"
                  max_start_duration;
                None)
              else if _max_start_main_duration < frame_size then (
                self#log#important
                  "Cannot set crossfade max start duration to less than the \
                   frame size!";
                None)
              else Some _max_start_main_duration)

    method reset_duration =
      end_duration_getter <- original_end_duration_getter;
      start_duration_getter <- original_start_duration_getter;
      max_start_duration <- None;
      self#set_end_main_duration;
      self#set_start_main_duration

    initializer self#reset_duration

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

    method private autocue_enabled mode =
      let metadata, set_metadata =
        match mode with
          | `Before -> (before_metadata, fun m -> before_metadata <- Some m)
          | `After -> (after_metadata, fun m -> after_metadata <- Some m)
      in
      match metadata with
        | Some h -> (
            let has_metadata =
              List.for_all
                (fun v -> Frame.Metadata.mem v h)
                ["liq_cue_in"; "liq_cue_out"; "liq_fade_in"; "liq_fade_out"]
            in
            let has_marker = Frame.Metadata.mem "liq_autocue" h in
            match (has_marker, has_metadata, assume_autocue) with
              | true, true, _ -> true
              | true, false, _ ->
                  self#log#critical
                    "`\"liq_autocue\"` metadata is present but some of the cue \
                     in/out and fade in/out metadata are missing!";
                  false
              | false, true, true ->
                  self#log#info "Assuming autocue";
                  set_metadata (Frame.Metadata.add "liq_autocue" "assumed" h);
                  true
              | _ -> false)
        | None -> false

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
    method private prepare_source s = s#wake_up (self :> Clock.source)

    initializer
      self#on_wake_up (fun () ->
          source#wake_up (self :> Clock.source);
          self#reset_analysis)

    val mutable status
        : [ `Idle | `Before of Source.source | `After of Source.source ] =
      `Idle

    method set_status v =
      (match status with
        | `Idle -> ()
        | `Before s | `After s -> s#sleep (self :> Clock.source));
      status <- v

    method! child_clock_controller = Some (`Other (self :> < id : string >))

    method private child_get ~is_first source =
      let frame = ref self#empty_frame in
      self#on_child_tick (fun () ->
          if source#is_ready then
            frame :=
              source#get_partial_frame (fun f ->
                  match self#split_frame f with
                    | buf, Some _ when Frame.position buf = 0 && is_first -> f
                    | buf, _ -> buf));
      !frame

    method private process_override_metadata m =
      (match Frame.Metadata.find_opt override_duration m with
        | None -> ()
        | Some v -> (
            try
              self#log#info
                "Overriding crossfade start and end duration from metadata %s"
                override_duration;
              let l = float_of_string v in
              end_duration_getter <- (fun () -> l);
              start_duration_getter <- (fun () -> l);
              self#set_end_main_duration;
              self#set_start_main_duration
            with _ -> ()));
      (match Frame.Metadata.find_opt override_end_duration m with
        | None -> ()
        | Some v -> (
            try
              self#log#info "Overriding crossfade end duration from metadata %s"
                override_end_duration;
              let l = float_of_string v in
              end_duration_getter <- (fun () -> l);
              self#set_end_main_duration
            with _ -> ()));
      (match Frame.Metadata.find_opt override_end_duration m with
        | None -> ()
        | Some v -> (
            try
              self#log#info "Overriding crossfade end duration from metadata %s"
                override_end_duration;
              let l = float_of_string v in
              end_duration_getter <- (fun () -> l);
              self#set_end_main_duration
            with _ -> ()));
      (match Frame.Metadata.find_opt override_max_start_duration m with
        | None -> ()
        | Some v -> (
            try
              self#log#info
                "Overriding crossfade max start duration from metadata %s"
                override_max_start_duration;
              let l = float_of_string v in
              max_start_duration <- Some l
            with _ -> ()));
      match Frame.Metadata.find_opt override_start_duration m with
        | None -> ()
        | Some v -> (
            try
              self#log#info
                "Overriding crossfade start duration from metadata %s"
                override_start_duration;
              let l = float_of_string v in
              start_duration_getter <- (fun () -> l);
              self#set_start_main_duration
            with _ -> ())

    initializer self#on_frame (`Metadata self#process_override_metadata)

    method private append mode buf_frame =
      let l = Frame.get_all_metadata buf_frame in
      List.iter (fun (_, m) -> self#process_override_metadata m) l;
      (match (List.rev l, mode) with
        | (_, m) :: _, `Before -> before_metadata <- Some m
        | (_, m) :: _, `After -> after_metadata <- Some m
        | _ -> ());
      match mode with
        | `Before -> Generator.append gen_before buf_frame
        | `After -> Generator.append gen_after buf_frame

    method private prepare_before =
      self#log#info "Buffering end of track...";
      let before = new consumer ~clock:source#clock gen_before in
      Typing.(before#frame_type <: self#frame_type);
      self#prepare_source before;
      self#set_status (`Before (before :> Source.source));
      self#buffer_before ~is_first:true ();
      match status with
        | `After s | `Before s -> if s#is_ready then Some s else None
        | _ -> assert false

    method private get_source ~reselect () =
      let reselect = match reselect with `Force -> `Ok | _ -> reselect in
      match status with
        | `Idle when source#is_ready -> self#prepare_before
        | `Idle -> None
        | `Before _ -> (
            self#buffer_before ~is_first:false ();
            match status with
              | `Idle -> assert false
              | `Before before_source
                when self#can_reselect ~reselect before_source ->
                  Some before_source
              | `Before _ -> None
              | `After after_source -> Some after_source)
        | `After after_source when self#can_reselect ~reselect after_source ->
            Some after_source
        | `After _ -> self#prepare_before

    method private buffer_before ~is_first () =
      if Generator.length gen_before < end_main_duration && source#is_ready then (
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
          if not persist_override then self#reset_duration;
          self#analyze_after)
        else self#buffer_before ~is_first:false ())

    method private expected_start_duration =
      let max_start_main_duration =
        Option.value ~default:start_main_duration max_start_main_duration
      in
      if start_main_duration < Generator.length gen_before then
        min (Generator.length gen_before) max_start_main_duration
      else start_main_duration

    (* Analyze the beginning of a new track. *)
    method private analyze_after =
      let rec f ~is_first () =
        let expected_start_duration = self#expected_start_duration in
        if
          Generator.length gen_after < expected_start_duration
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
          if Frame.is_partial buf_frame then (
            if not persist_override then self#reset_duration;
            self#log#critical
              "End of track reached while buffering next track data, crossfade \
               duration is longer than the track's duration. Make sure to \
               adjust the crossfade duration to avoid issues.";
            self#create_after)
          else f ~is_first:false ())
        else self#create_after
      in
      f ~is_first:true ()

    method private append_before_metadata lbl value =
      before_metadata <-
        Some
          (Frame.Metadata.add lbl value
             (Option.value ~default:Frame.Metadata.empty before_metadata))

    method private append_after_metadata lbl value =
      after_metadata <-
        Some
          (Frame.Metadata.add lbl value
             (Option.value ~default:Frame.Metadata.empty after_metadata))

    method private autocue_adjustements ~before_autocue ~after_autocue
        ~buffered_before ~buffered_after () =
      let before_metadata =
        Option.value ~default:Frame.Metadata.empty before_metadata
      in
      let extra_cross_duration = buffered_before - buffered_after in
      if after_autocue then
        if before_autocue && 0 < extra_cross_duration then (
          let new_cross_duration = buffered_before - extra_cross_duration in
          Generator.keep gen_before new_cross_duration;
          let new_cross_duration = Frame.seconds_of_main new_cross_duration in
          let extra_cross_duration =
            Frame.seconds_of_main extra_cross_duration
          in
          self#log#info
            "Shortening ending track by %.2f to match the starting track's \
             buffer."
            extra_cross_duration;
          let fade_out =
            float_of_string (Frame.Metadata.find "liq_fade_out" before_metadata)
          in
          let fade_out = min new_cross_duration fade_out in
          let fade_out_delay = max (new_cross_duration -. fade_out) 0. in
          self#append_before_metadata "liq_fade_out" (string_of_float fade_out);
          self#append_before_metadata "liq_fade_out_delay"
            (string_of_float fade_out_delay))

    (* Sum up analysis and build the transition *)
    method private create_after =
      let before_autocue = self#autocue_enabled `Before in
      let after_autocue = self#autocue_enabled `After in
      let buffered_before = Generator.length gen_before in
      let buffered_after = Generator.length gen_after in
      let buffered = min buffered_before buffered_after in
      let db_after =
        Audio.dB_of_lin
          (sqrt (rms_after /. float rmsi_after /. float self#audio_channels))
      in
      let db_before =
        Audio.dB_of_lin
          (sqrt (rms_before /. float rmsi_before /. float self#audio_channels))
      in
      self#autocue_adjustements ~before_autocue ~after_autocue ~buffered_before
        ~buffered_after ();
      let compound =
        let metadata = function None -> Frame.Metadata.empty | Some m -> m in
        let before_metadata = metadata before_metadata in
        let after_metadata = metadata after_metadata in
        let before_head =
          if (not after_autocue) && buffered < buffered_before then (
            let head =
              Generator.slice gen_before (buffered_before - buffered)
            in
            let head_gen =
              Generator.create ~content:head (Generator.content_type gen_before)
            in
            let s = new consumer ~clock:source#clock head_gen in
            s#set_id (self#id ^ "_before_head");
            Typing.(s#frame_type <: self#frame_type);
            Some s)
          else None
        in
        let before =
          new Replay_metadata.replay
            before_metadata
            (new consumer ~clock:source#clock gen_before)
        in
        Typing.(before#frame_type <: self#frame_type);
        let after_tail =
          if (not after_autocue) && buffered < buffered_after then (
            let head = Generator.slice gen_after buffered in
            let head_gen =
              Generator.create ~content:head (Generator.content_type gen_after)
            in
            let tail_gen = gen_after in
            gen_after <- head_gen;
            let s = new consumer ~clock:source#clock tail_gen in
            Typing.(s#frame_type <: self#frame_type);
            s#set_id (self#id ^ "_after_tail");
            Some s)
          else None
        in
        let after =
          new Replay_metadata.replay
            after_metadata
            (new consumer ~clock:source#clock gen_after)
        in
        Typing.(after#frame_type <: self#frame_type);
        before#set_id (self#id ^ "_before");
        after#set_id (self#id ^ "_after");
        self#log#important "Analysis: %fdB / %fdB (%.2fs / %.2fs)" db_before
          db_after
          (Frame.seconds_of_main buffered_before)
          (Frame.seconds_of_main buffered_after);
        let compound =
          let params =
            [
              ( "",
                Lang.meth Lang.unit
                  [
                    ("source", Lang.source before);
                    ("db_level", Lang.float db_before);
                    ("metadata", Lang.metadata before_metadata);
                  ] );
              ( "",
                Lang.meth Lang.unit
                  [
                    ("source", Lang.source after);
                    ("db_level", Lang.float db_after);
                    ("metadata", Lang.metadata after_metadata);
                  ] );
            ]
          in
          Lang.to_source (Lang.apply transition params)
        in
        Typing.(compound#frame_type <: self#frame_type);
        let compound =
          match (before_head, after_tail) with
            | None, None -> (compound :> Source.source)
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
        compound
      in
      self#prepare_source compound;
      self#reset_analysis;
      self#set_status (`After compound)

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
        ("metadata", ([], Lang.metadata_t), "Metadata of the source.");
      ]
  in
  Lang.add_operator "cross"
    [
      ( "start_duration",
        Lang.nullable_t (Lang.getter_t Lang.float_t),
        Some Lang.null,
        Some
          "Duration (in seconds) of buffered data from the start of each track \
           that is used to compute the transition between tracks." );
      ( "end_duration",
        Lang.nullable_t (Lang.getter_t Lang.float_t),
        Some Lang.null,
        Some
          "Duration (in seconds) of buffered data from the end of each track \
           that is used to compute the transition between tracks." );
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 5.),
        Some
          "Duration (in seconds) of buffered data from the end and start of \
           each track that is used to compute the transition between tracks." );
      ( "assume_autocue",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Assume that a track has autocue enabled when all four cue in/out \
           and fade in/out override metadata are present. Defaults to \
           `settings.crossfade.assume_autocue` when `null`." );
      ( "override_start_duration",
        Lang.string_t,
        Some (Lang.string "liq_cross_start_duration"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the 'start_duration' parameter for current track." );
      ( "override_max_start_duration",
        Lang.string_t,
        Some (Lang.string "liq_cross_max_start_duration"),
        Some
          "Metadata field which, if present and containing a float, informs \
           the crossfade of the maximum start duration. When not present, it \
           is assumed to be `0.`." );
      ( "override_end_duration",
        Lang.string_t,
        Some (Lang.string "liq_cross_end_duration"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the 'end_duration' parameter for current track." );
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
          name = "start_duration";
          scheme = Lang.([], fun_t [] float_t);
          descr = "Get the current crossfade start duration.";
          value =
            (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#start_duration));
        };
        {
          name = "end_duration";
          scheme = Lang.([], fun_t [] float_t);
          descr = "Get the current crossfade end duration.";
          value =
            (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#end_duration));
        };
      ]
    ~descr:
      "Cross operator, allowing the composition of the _n_ last seconds of a \
       track with the beginning of the next track, using a transition function \
       depending on the relative power of the signal before and after the end \
       of track."
    (fun p ->
      let assume_autocue =
        Lang.to_valued_option Lang.to_bool (List.assoc "assume_autocue" p)
      in
      let assume_autocue =
        Option.value ~default:conf_assume_autocue#get assume_autocue
      in
      let start_duration_getter =
        Lang.to_valued_option Lang.to_float_getter
          (List.assoc "start_duration" p)
      in
      let end_duration_getter =
        Lang.to_valued_option Lang.to_float_getter (List.assoc "end_duration" p)
      in
      let duration_getter = Lang.to_float_getter (List.assoc "duration" p) in
      let start_duration_getter =
        Option.value ~default:duration_getter start_duration_getter
      in
      let end_duration_getter =
        Option.value ~default:duration_getter end_duration_getter
      in
      let override_start_duration =
        Lang.to_string (List.assoc "override_start_duration" p)
      in
      let override_max_start_duration =
        Lang.to_string (List.assoc "override_max_start_duration" p)
      in
      let override_end_duration =
        Lang.to_string (List.assoc "override_end_duration" p)
      in
      let override_duration =
        Lang.to_string (List.assoc "override_duration" p)
      in
      let persist_override = Lang.to_bool (List.assoc "persist_override" p) in
      let rms_width = Lang.to_float (List.assoc "width" p) in
      let rms_width = Frame.audio_of_seconds rms_width in
      let transition = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      new cross
        source transition ~start_duration_getter ~end_duration_getter ~rms_width
        ~override_start_duration ~override_max_start_duration
        ~override_end_duration ~override_duration ~persist_override
        ~assume_autocue)
