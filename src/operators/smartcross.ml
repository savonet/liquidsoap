(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2014 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

module Generator = Generator.From_frames
module Generated = Generated.Make(Generator)

(** [rms_width], [inhibit] and [minimum_length] are all in samples.
  * [cross_length] is in ticks (like #remaining estimations).
  * We are assuming a fixed audio kind -- at least for now. *)
class cross ~kind (s:source)
            ~cross_length ~rms_width ~inhibit ~minimum_length
            ~conservative ~active transition =
  let channels = float (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit source ~name:"smart_cross" kind as super

  (* This actually depends on [f], we have to trust the user here. *)
  method stype = s#stype

  (* We need to store the end of a track, and compute the power of the signal
   * before the end of track. For doing so we need to remember a sliding window
   * of samples, maintain the sum of squares, and the number of samples in that
   * sum. The sliding window is necessary because of possibly inaccurate
   * remaining time estimaton. *)
  val mutable gen_before = Generator.create ()
  val mutable rms_before = 0.
  val mutable rmsi_before = 0
  val mutable mem_before = Array.create rms_width 0.
  val mutable mem_i = 0
  val mutable before_metadata = None

  (* Same for the new track. No need for a sliding window here. *)
  val mutable gen_after  = Generator.create ()
  val mutable rms_after  = 0.
  val mutable rmsi_after = 0
  val mutable after_metadata = None

  (* An audio frame for intermediate computations.
   * It is used to buffer the end and beginnings of tracks.
   * Its past metadata should mimick that of the main stream in order
   * to avoid metadata duplication. *)
  val buf_frame = Frame.create kind

  method private reset_analysis =
    gen_before <- Generator.create () ;
    gen_after  <- Generator.create () ;
    rms_before <- 0. ; rmsi_before <- 0 ; mem_i <- 0 ;
    Array.iteri (fun i _ -> mem_before.(i) <- 0.) mem_before ;
    rms_after <- 0. ; rmsi_after <- 0 ;
    before_metadata <- after_metadata ;
    after_metadata <- None

  (* The played source: [s] at first, then a combination of tracks by [f].
   * We _need_ exclusive control on that source, since we are going to
   * pull data from it at a higher rate around track limits. *)
  val mutable source = s

  method private wake_up _ =
    s#get_ready ~dynamic:true [(self:>source)] ;
    source <- s ;
    source#get_ready [(self:>source)] ;
    Lang.iter_sources
      (fun s -> s#get_ready ~dynamic:true [(self:>source)])
      transition

  method private sleep =
    source#leave (self:>source) ;
    s#leave ~dynamic:true (self:>source) ;
    Lang.iter_sources
      (fun s -> s#leave ~dynamic:true (self:>source))
      transition

  (** See cross.ml for the details of clock management. *)

  method private set_clock =
    let slave_clock = Clock.create_known (new Clock.clock self#id) in
    (* Our external clock should stricly contain the slave clock. *)
    Clock.unify
      self#clock
      (Clock.create_unknown ~sources:[] ~sub_clocks:[slave_clock]) ;
    (* The source must belong to our clock, since we need occasional
     * control on its flow (to fold an end of track over a beginning).
     *
     * To be safe, also require that the transition's sources belong
     * to that same clock, since we (dynamically) activate them as well.
     * The activation mechanism is designed for a single thread,
     * which multiples clocks wouldn't ensure. *)
    Clock.unify slave_clock s#clock ;
    Lang.iter_sources
      (fun s -> Clock.unify slave_clock s#clock)
      transition

  val mutable master_time = 0
  val mutable last_slave_tick = 0 (* in master time *)

  (* Indicate that the source should be managed by relaying master ticks,
   * and it has been used during the current tick, so it should be ticked
   * even if we're not in active mode. *)
  val mutable needs_tick = false

  method private slave_tick =
    (Clock.get source#clock)#end_tick ;
    source#after_output ;
    Frame.advance buf_frame ;
    needs_tick <- false ;
    last_slave_tick <- (Clock.get self#clock)#get_tick

  method after_output =
    super#after_output ;
    if needs_tick then self#slave_tick ;
    let master_clock = Clock.get self#clock in
      (* Is it really a new tick? *)
      if master_time <> master_clock#get_tick then begin
        (* Did the slave clock tick during this instant? *)
        if active && last_slave_tick <> master_time then begin
          self#slave_tick ;
          last_slave_tick <- master_time
        end ;
        master_time <- master_clock#get_tick
      end

  val mutable status = `Idle

  method private get_frame frame =
    match status with
      | `Idle ->
          let rem = if conservative then 0 else source#remaining in
            if rem < 0 || rem > cross_length then begin
              source#get frame ;
              needs_tick <- true
            end else begin
              self#log#f 4 "Buffering end of track..." ;
              status <- `Before ;
              Frame.set_breaks buf_frame [Frame.position frame] ;
              Frame.set_all_metadata buf_frame
                (match Frame.get_past_metadata frame with
                   | Some x -> [-1,x] | None -> []) ;
              self#buffering cross_length ;
              if status <> `Limit then
                self#log#f 4 "More buffering will be needed." ;
              self#get_frame frame
            end
      | `Before ->
          (* We started buffering but the track didn't end.
           * Play the beginning of the buffer while filling it more. *)
          self#buffering 0 ;
          Generator.fill gen_before frame
      | `Limit ->
          (* The track finished.
           * We compute rms_after and launch the transition. *)
          if source#is_ready then self#analyze_after ;
          self#slave_tick ;
          self#create_transition ;
          (* Check if the new source is ready *)
          if source#is_ready then
            self#get_frame frame
          else
            (* If not, finish this track, which requires our callers
             * to wait that we become ready again. *)
            Frame.add_break frame (Frame.position frame)
      | `After size ->
          (* The work is done, we are now playing the transition.
           * We have to keep playing it without checking for a new track limit
           * for [size] samples. *)
          if size<=0 then begin
            status <- `Idle ;
            self#get_frame frame
          end else
            let position = AFrame.position frame in
              source#get frame ;
              needs_tick <- true ;
              status <- `After (size-(AFrame.position frame)+position)

  (* [bufferize n] stores at most [n+d] samples from [s] in [gen_before],
   * where [d=AFrame.size-1]. *)
  method private buffering n =
    (* For the first call, the position is the old position in the master
     * frame. After that it'll always be 0. *)
    if not (Frame.is_partial buf_frame) then self#slave_tick ;
    let start = Frame.position buf_frame in
    let stop = source#get buf_frame ; Frame.position buf_frame in
    let content =
      let end_pos,c = Frame.content buf_frame start in
        assert (end_pos>=stop) ;
        c
    in
    Generator.feed gen_before
      ~metadata:(Frame.get_all_metadata buf_frame)
      content start (stop-start) ;
    (* Analyze them *)
    let pcm = content.Frame.audio in
    for i = start to stop - 1 do
      let squares =
        Array.fold_left
          (fun squares track -> squares +. track.(i)*.track.(i))
          0.
          pcm
      in
        rms_before <- rms_before -. mem_before.(mem_i) +. squares ;
        mem_before.(mem_i) <- squares ;
        mem_i <- (mem_i + 1) mod rms_width ;
        rmsi_before <- min rms_width (rmsi_before + 1)
    done ;
    (* Should we buffer more or are we done ? *)
    if AFrame.is_partial buf_frame then begin
      Generator.add_break gen_before ;
      status <- `Limit
    end else
      if n>0 then self#buffering (n - AFrame.position buf_frame)

  (* Analyze the beginning of a new track. *)
  method private analyze_after =
    let todo = ref rms_width in
    let first = ref true in
      while !todo > 0 do

        if not (Frame.is_partial buf_frame) then self#slave_tick ;
        let start = Frame.position buf_frame in
        let stop = source#get buf_frame ; Frame.position buf_frame in
        let content =
          let end_pos,c = Frame.content buf_frame start in
            assert (end_pos>=stop) ;
            c
        in

        Generator.feed gen_after
          ~metadata:(Frame.get_all_metadata buf_frame)
          content start (stop-start) ;

        if !first then begin
          after_metadata <- AFrame.get_metadata buf_frame start ;
          first := false
        end ;

        let pcm = content.Frame.audio in
        for i = start to stop - 1 do
          let squares =
            Array.fold_left
              (fun squares track -> squares +. track.(i)*.track.(i))
              0.
              pcm
          in
            rms_after <- rms_after +. squares ;
            rmsi_after <- rmsi_after + 1
        done ;
        todo := !todo - AFrame.position buf_frame ;
        (* Not sure how to handle end-of-track at this point... *)
        if AFrame.is_partial buf_frame then todo := 0
      done

  (* Sum up analysis and build the transition *)
  method private create_transition =
    let db_after  =
      Audio.dB_of_lin (sqrt (rms_after  /. float rmsi_after /. channels))
    in
    let db_before =
      Audio.dB_of_lin (sqrt (rms_before /. float rmsi_before /. channels))
    in
    let compound =
      Clock.collect_after
        (fun () ->
           let before = new Generated.consumer ~kind gen_before in
           let after =
             let beginning =
               new Generated.consumer ~kind gen_after
             in
               new Sequence.sequence ~kind ~merge:true [beginning;s]
           in
           let () =
             before#set_id (self#id ^ "_before") ;
             after#set_id (self#id ^ "_after")
           in
           let metadata = function
             | None ->
                 Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) []
             | Some m -> Lang.metadata m
           in
           let f a b =
             let params =
               [ "", Lang.float db_before ;
                 "", Lang.float db_after ;
                 "", metadata before_metadata ;
                 "", metadata after_metadata ;
                 "", Lang.source a ;
                 "", Lang.source b ]
             in
             let t = Lang.source_t (Lang.kind_type_of_frame_kind kind) in
               Lang.to_source (Lang.apply ~t transition params)
           in
           let compound =
             self#log#f 3 "Analysis: %fdB / %fdB (%.2fs / %.2fs)"
               db_before db_after
               (Frame.seconds_of_master (Generator.length gen_before))
               (Frame.seconds_of_master (Generator.length gen_after)) ;
             if Generator.length gen_before >
                  Frame.master_of_audio minimum_length then
               f before after
             else begin
               self#log#f 3 "Not enough data for crossing." ;
               ((new Sequence.sequence ~kind [before;after]):>source)
             end
           in
             Clock.unify compound#clock s#clock ;
             compound)
    in
      compound#get_ready [(self:>source)] ;
      source#leave (self:>source) ;
      source <- compound ;
      status <- `After inhibit ;
      self#reset_analysis

  method remaining =
    match status with
      | `Idle | `After _ -> source#remaining
      | `Limit -> 0 (* TODO -1? *)
      | `Before ->
          let rem = source#remaining in
            if rem<0 then -1 else
              source#remaining +
              Generator.length gen_before

  (** Contrary to cross.ml, the transition is only created (and stored in
    * the source instance variable) after that status has moved from `Limit to
    * `After. If is_ready becomes false at this point, source.ml will end the
    * track before that the transition (or bare end of track) gets a chance
    * to be played. *)
  method is_ready = source#is_ready || status = `Limit

  method abort_track = source#abort_track

end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  Lang.add_operator "smart_cross"
    [ "duration", Lang.float_t, Some (Lang.float 5.),
      Some "Duration in seconds of the crossed end of track." ;

      "inhibit", Lang.float_t, Some (Lang.float (-1.)),
      Some "Minimum delay between two transitions. It is useful in order to \
            avoid that a transition is triggered on top of another when \
            an end-of-track occurs in the first one. Negative values mean \
            <code>duration+1</code>. \
            Warning: zero inhibition can cause infinite loops." ;

      "minimum", Lang.float_t, (Some (Lang.float (-1.))),
      Some "Minimum duration (in sec.) for a cross: \
            If the track ends without any warning (e.g. in case of skip) \
            there may not be enough data for a decent composition. \
            Set to 0. to avoid having transitions after skips, \
            or more to avoid transitions on short tracks. \
            With the negative default, transitions always occur." ;

      "width", Lang.float_t, Some (Lang.float 1.),
      Some "Width of the power computation window." ;

      "conservative", Lang.bool_t, Some (Lang.bool false),
      Some "Do not trust remaining time estimations, always buffering \
            data in advance. This avoids being tricked by skips, either \
            manual or caused by skip_blank()." ;

      "active", Lang.bool_t, Some (Lang.bool false),
      Some "The active behavior is to keep ticking the child's clock \
            when the operator is not streaming. Otherwise the child's clock \
            is strictly based on what is streamed off the child source, \
            which results in time-dependent active sources to be frozen \
            when that source is stopped." ;

      "",
      Lang.fun_t
        [false,"",Lang.float_t; false,"",Lang.float_t;
         false,"",Lang.metadata_t; false,"",Lang.metadata_t;
         false,"",Lang.source_t k; false,"",Lang.source_t k]
        (Lang.source_t k),
      None,
      Some "Transition function, composing from the end of a track \
            and the next track. It also takes the power of the signal before \
            and after the transition, and the metadata." ;

      "", Lang.source_t k, None, None

    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Cross operator, allowing the composition of \
            the N last seconds of a track with the beginning of \
            the next track, using a transition function depending on \
            the relative power of the signal before and after \
            the end of track."
    (fun p kind ->
       let duration = Lang.to_float (List.assoc "duration" p) in
       let cross_length = Frame.master_of_seconds duration in

       let minimum = Lang.to_float (List.assoc "minimum" p) in
       let minimum_length = Frame.audio_of_seconds minimum in

       let inhibit = Lang.to_float (List.assoc "inhibit" p) in
       let inhibit = if inhibit < 0. then duration +. 1. else inhibit in
       let inhibit = Frame.audio_of_seconds inhibit in

       let rms_width = Lang.to_float (List.assoc "width" p) in
       let rms_width = Frame.audio_of_seconds rms_width in

       let transition = Lang.assoc "" 1 p in

       let conservative = Lang.to_bool (List.assoc "conservative" p) in
       let active = Lang.to_bool (List.assoc "active" p) in

       let source = Lang.to_source (Lang.assoc "" 2 p) in
         new cross ~kind source transition ~conservative ~active
               ~cross_length ~rms_width ~inhibit ~minimum_length)
