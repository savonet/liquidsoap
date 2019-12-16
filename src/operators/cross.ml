(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Source
module Generator = Generator.From_frames
module Generated = Generated.Make (Generator)

let finalise_slave_clock slave_clock source =
  Clock.forget source#clock slave_clock

(** [rms_width] and [minimum_length] are all in samples.
  * [cross_length] is in ticks (like #remaining estimations).
  * We are assuming a fixed audio kind -- at least for now. *)
class cross ~kind (s : source) ~cross_length ~override_duration ~rms_width
  ~minimum_length ~conservative ~active transition =
  let channels = float (Frame.type_of_kind kind).Frame.audio in
  object (self)
    inherit source ~name:"cross" kind as super

    (* This actually depends on [f], we have to trust the user here. *)
    method stype = s#stype

    (* This is complicated. crossfade should never be used with [self_sync]
     * sources but we do not have a static way of knowing it at the moment.
     * Going with the same choice as above for now. *)
    method self_sync = s#self_sync

    val mutable cross_length = cross_length

    (* We need to store the end of a track, and compute the power of the signal
     * before the end of track. For doing so we need to remember a sliding window
     * of samples, maintain the sum of squares, and the number of samples in that
     * sum. The sliding window is necessary because of possibly inaccurate
     * remaining time estimaton. *)
    val mutable gen_before = Generator.create ()

    val mutable rms_before = 0.

    val mutable rmsi_before = 0

    val mutable mem_before = Array.make rms_width 0.

    val mutable mem_i = 0

    val mutable before_metadata = None

    (* Same for the new track. No need for a sliding window here. *)
    val mutable gen_after = Generator.create ()

    val mutable rms_after = 0.

    val mutable rmsi_after = 0

    val mutable after_metadata = None

    (* An audio frame for intermediate computations.
     * It is used to buffer the end and beginnings of tracks.
     * Its past metadata should mimick that of the main stream in order
     * to avoid metadata duplication. *)
    val buf_frame = Frame.create kind

    method private reset_analysis =
      gen_before <- Generator.create ();
      gen_after <- Generator.create ();
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

    (* Give a default value for the transition source. *)
    val mutable transition_source = None

    val mutable pending_after = Generator.create ()

    method private prepare_transition_source s =
      let s = (s :> source) in
      s#get_ready ~dynamic:true [(self :> source)];
      Clock.unify source#clock s#clock;
      transition_source <- Some s

    method cleanup_transition_source =
      match transition_source with
        | None -> ()
        | Some s ->
            s#leave ~dynamic:true (self :> source);
            transition_source <- None

    method private wake_up _ =
      source#get_ready ~dynamic:true [(self :> source)];
      source#get_ready [(self :> source)];
      Lang.iter_sources
        (fun s -> s#get_ready ~dynamic:true [(self :> source)])
        transition

    method private sleep =
      source#leave (self :> source);
      s#leave ~dynamic:true (self :> source);
      Lang.iter_sources
        (fun s -> s#leave ~dynamic:true (self :> source))
        transition;
      self#cleanup_transition_source

    method private set_clock =
      let slave_clock = Clock.create_known (new Clock.clock self#id) in
      (* Our external clock should stricly contain the slave clock. *)
      Clock.unify self#clock
        (Clock.create_unknown ~sources:[] ~sub_clocks:[slave_clock]);

      (* The source must belong to our clock, since we need occasional
       * control on its flow (to fold an end of track over a beginning).
       *
       * To be safe, also require that the transition's sources belong
       * to that same clock, since we (dynamically) activate them as well.
       * The activation mechanism is designed for a single thread,
       * which multiples clocks wouldn't ensure. *)
      Clock.unify slave_clock s#clock;
      Lang.iter_sources (fun s -> Clock.unify slave_clock s#clock) transition;

      (* Make sure the slave clock can be garbage collected, cf. cue_cut(). *)
      Gc.finalise (finalise_slave_clock slave_clock) self

    val mutable master_time = 0

    val mutable last_slave_tick = 0

    (* in master time *)

    (* Indicate that the source should be managed by relaying master ticks,
     * and it has been used during the current tick, so it should be ticked
     * even if we're not in active mode. *)
    val mutable needs_tick = false

    val mutable status = `Idle

    method private slave_tick =
      (Clock.get source#clock)#end_tick;
      source#after_output;
      Frame.advance buf_frame;
      needs_tick <- false;
      last_slave_tick <- (Clock.get self#clock)#get_tick

    method after_output =
      super#after_output;
      if needs_tick then self#slave_tick;
      let master_clock = Clock.get self#clock in
      (* Is it really a new tick? *)
      if master_time <> master_clock#get_tick then (
        (* Did the slave clock tick during this instant? *)
        if active && last_slave_tick <> master_time then (
          self#slave_tick;
          last_slave_tick <- master_time );
        master_time <- master_clock#get_tick )

    method private save_last_metadata mode buf_frame =
      let compare x y = -compare (fst x) (fst y) in
      let l = List.sort compare (AFrame.get_all_metadata buf_frame) in
      match (l, mode) with
        | (_, m) :: _, `Before -> before_metadata <- Some m
        | (_, m) :: _, `After -> after_metadata <- Some m
        | _ -> ()

    method private update_cross_length frame pos =
      List.iter
        (fun (p, m) ->
          if p >= pos then (
            match Utils.hashtbl_get m override_duration with
              | None -> ()
              | Some v -> (
                  try
                    let l = float_of_string v in
                    self#log#info "Setting crossfade duration to %.2fs" l;
                    cross_length <- Frame.master_of_seconds l
                  with _ -> () ) ))
        (Frame.get_all_metadata frame)

    method private get_frame frame =
      match status with
        | `Idle ->
            let rem = if conservative then 0 else source#remaining in
            if rem < 0 || rem > cross_length then (
              let p = Frame.position frame in
              source#get frame;
              self#save_last_metadata `Before frame;
              self#update_cross_length frame p;
              needs_tick <- true )
            else (
              self#log#info "Buffering end of track...";
              status <- `Before;
              Frame.set_breaks buf_frame [Frame.position frame];
              Frame.set_all_metadata buf_frame
                ( match Frame.get_past_metadata frame with
                  | Some x -> [(-1, x)]
                  | None -> [] );
              self#buffering cross_length;
              if status <> `Limit then
                self#log#info "More buffering will be needed.";
              self#get_frame frame )
        | `Before ->
            (* We started buffering but the track didn't end.
             * Play the beginning of the buffer while filling it more. *)
            let len = Generator.length gen_before in
            if len <= cross_length then self#buffering (cross_length - len);
            Generator.fill gen_before frame
        | `Limit ->
            (* The track finished.
             * We compute rms_after and launch the transition. *)
            if source#is_ready then self#analyze_after;
            self#slave_tick;
            self#create_transition;

            (* Check if the new source is ready *)
            if (Utils.get_some transition_source)#is_ready then
              self#get_frame frame
            else
              (* If not, finish this track, which requires our callers
               * to wait that we become ready again. *)
              Frame.add_break frame (Frame.position frame)
        | `After ->
            (Utils.get_some transition_source)#get frame;
            needs_tick <- true;
            if Generator.length pending_after = 0 && Frame.is_partial frame then (
              status <- `Idle;
              self#cleanup_transition_source;

              (* If underlying source if ready, try to continue filling up the frame
               * using it. Each call to [get_frame] must add exactly one break so
               * call it again and then remove the intermediate break that was just
               * just added. *)
              if source#is_ready then (
                self#get_frame frame;
                Frame.set_breaks frame
                  ( match Frame.breaks frame with
                    | b :: _ :: l -> b :: l
                    | _ -> assert false ) ) )

    (* [bufferize n] stores at most [n+d] samples from [s] in [gen_before],
     * where [d=AFrame.size-1]. *)
    method private buffering n =
      (* For the first call, the position is the old position in the master
       * frame. After that it'll always be 0. *)
      if not (Frame.is_partial buf_frame) then self#slave_tick;
      let start = Frame.position buf_frame in
      let stop =
        source#get buf_frame;
        Frame.position buf_frame
      in
      self#save_last_metadata `Before buf_frame;
      self#update_cross_length buf_frame start;
      let content =
        let end_pos, c = Frame.content buf_frame start in
        assert (end_pos >= stop);
        c
      in
      Generator.feed gen_before
        ~metadata:(Frame.get_all_metadata buf_frame)
        content start (stop - start);

      (* Analyze them *)
      let pcm = content.Frame.audio in
      for i = start to stop - 1 do
        let squares =
          Array.fold_left
            (fun squares track ->
              let x = track.{i} in
              squares +. (x *. x))
            0. pcm
        in
        rms_before <- rms_before -. mem_before.(mem_i) +. squares;
        mem_before.(mem_i) <- squares;
        mem_i <- (mem_i + 1) mod rms_width;
        rmsi_before <- min rms_width (rmsi_before + 1)
      done;

      (* Should we buffer more or are we done ? *)
      if AFrame.is_partial buf_frame then (
        Generator.add_break gen_before;
        status <- `Limit )
      else if n > 0 then self#buffering (n - stop + start)

    (* Analyze the beginning of a new track. *)
    method private analyze_after =
      let before_len = Generator.length gen_before in
      let rec f () =
        let start = Frame.position buf_frame in
        let stop =
          source#get buf_frame;
          Frame.position buf_frame
        in
        let content =
          let end_pos, c = Frame.content buf_frame start in
          assert (end_pos >= stop);
          c
        in
        Generator.feed gen_after
          ~metadata:(Frame.get_all_metadata buf_frame)
          content start (stop - start);
        let after_len = Generator.length gen_after in
        if after_len <= rms_width then (
          let pcm = content.Frame.audio in
          for i = start to stop - 1 do
            let squares =
              Array.fold_left
                (fun squares track ->
                  let x = track.{i} in
                  squares +. (x *. x))
                0. pcm
            in
            rms_after <- rms_after +. squares;
            rmsi_after <- rmsi_after + 1
          done );
        self#save_last_metadata `After buf_frame;
        self#update_cross_length buf_frame start;
        if not (AFrame.is_partial buf_frame) then (
          self#slave_tick;
          if after_len < before_len then f () )
      in
      f ()

    (* Sum up analysis and build the transition *)
    method private create_transition =
      let db_after =
        Audio.dB_of_lin (sqrt (rms_after /. float rmsi_after /. channels))
      in
      let db_before =
        Audio.dB_of_lin (sqrt (rms_before /. float rmsi_before /. channels))
      in
      let compound =
        Clock.collect_after (fun () ->
            let metadata = function None -> Hashtbl.create 0 | Some m -> m in
            let before_metadata = metadata before_metadata in
            let after_metadata = metadata after_metadata in
            let before =
              new Insert_metadata.replay
                ~kind before_metadata
                (new Generated.consumer ~kind gen_before)
            in
            let after =
              new Insert_metadata.replay
                ~kind after_metadata
                (new Generated.consumer ~kind gen_after)
            in
            let () =
              before#set_id (self#id ^ "_before");
              after#set_id (self#id ^ "_after")
            in
            let f a b =
              let params =
                [
                  ("", Lang.float db_before);
                  ("", Lang.float db_after);
                  ("", Lang.metadata before_metadata);
                  ("", Lang.metadata after_metadata);
                  ("", Lang.source a);
                  ("", Lang.source b);
                ]
              in
              let t = Lang.source_t (Lang.kind_type_of_frame_kind kind) in
              Lang.to_source (Lang.apply ~t transition params)
            in
            let compound =
              self#log#important "Analysis: %fdB / %fdB (%.2fs / %.2fs)"
                db_before db_after
                (Frame.seconds_of_master (Generator.length gen_before))
                (Frame.seconds_of_master (Generator.length gen_after));
              if
                Frame.master_of_audio minimum_length
                < Generator.length gen_after
              then f before after
              else (
                self#log#important "Not enough data for crossing.";
                (new Sequence.sequence ~kind [before; after] :> source) )
            in
            Clock.unify compound#clock s#clock;
            compound)
      in
      compound#get_ready ~dynamic:true [(self :> source)];
      self#cleanup_transition_source;
      self#prepare_transition_source compound;
      pending_after <- gen_after;
      status <- `After;
      self#reset_analysis

    method remaining =
      match status with
        | `Before | `Idle -> source#remaining
        | `Limit -> -1
        | `After -> (Utils.get_some transition_source)#remaining

    method is_ready =
      match status with
        | `Idle | `Before -> source#is_ready
        | `Limit -> true
        | `After -> (Utils.get_some transition_source)#is_ready

    method abort_track =
      if status = `After && (Utils.get_some transition_source)#is_ready then
        (Utils.get_some transition_source)#abort_track
      else source#abort_track
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "cross"
    [
      ( "duration",
        Lang.float_t,
        Some (Lang.float 5.),
        Some
          "Duration (in seconds) of buffered data from each track that is used \
           to compute the transision between tracks." );
      ( "override_duration",
        Lang.string_t,
        Some (Lang.string "liq_cross_duration"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the 'duration' parameter for current track." );
      ( "minimum",
        Lang.float_t,
        Some (Lang.float (-1.)),
        Some
          "Minimum duration (in sec.) for a cross: If the track ends without \
           any warning (e.g. in case of skip) there may not be enough data for \
           a decent composition. Set to 0. to avoid having transitions after \
           skips, or more to avoid transitions on short tracks. With a \
           negative default, transitions always occur." );
      ( "width",
        Lang.float_t,
        Some (Lang.float 2.),
        Some "Width of the power computation window." );
      ( "conservative",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Do not trust remaining time estimations, always buffering data in \
           advance. This avoids being tricked by skips, either manual or \
           caused by skip_blank()." );
      ( "active",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "The active behavior is to keep ticking the child's clock when the \
           operator is not streaming. Otherwise the child's clock is strictly \
           based on what is streamed off the child source, which results in \
           time-dependent active sources to be frozen when that source is \
           stopped." );
      ( "",
        Lang.fun_t
          [
            (false, "", Lang.float_t);
            (false, "", Lang.float_t);
            (false, "", Lang.metadata_t);
            (false, "", Lang.metadata_t);
            (false, "", Lang.source_t k);
            (false, "", Lang.source_t k);
          ]
          (Lang.source_t k),
        None,
        Some
          "Transition function, composing from the end of a track and the next \
           track. It also takes the power of the signal before and after the \
           transition, and the metadata." );
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:
      "Cross operator, allowing the composition of the N last seconds of a \
       track with the beginning of the next track, using a transition function \
       depending on the relative power of the signal before and after the end \
       of track."
    (fun p kind ->
      let duration = Lang.to_float (List.assoc "duration" p) in
      let override_duration =
        Lang.to_string (List.assoc "override_duration" p)
      in
      let cross_length = Frame.master_of_seconds duration in
      let minimum = Lang.to_float (List.assoc "minimum" p) in
      let minimum_length = Frame.audio_of_seconds minimum in
      let rms_width = Lang.to_float (List.assoc "width" p) in
      let rms_width = Frame.audio_of_seconds rms_width in
      let transition = Lang.assoc "" 1 p in
      let conservative = Lang.to_bool (List.assoc "conservative" p) in
      let active = Lang.to_bool (List.assoc "active" p) in
      let source = Lang.to_source (Lang.assoc "" 2 p) in
      let c =
        new cross
          ~kind source transition ~conservative ~active ~cross_length ~rms_width
          ~minimum_length ~override_duration
      in
      (c :> source))
