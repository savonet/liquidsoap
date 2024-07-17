(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

module Generator = Float_pcm.Generator
module Generated = Generated.From_Float_pcm_Generator

(** [rms_width], [inhibit] and [minimum_length] are all in samples.
  * [cross_length] is in ticks (like #remaining estimations). *)
class cross s ~cross_length ~rms_width ~inhibit ~minimum_length
              ~conservative transition =
object (self)
  (* We declare that [s] is our source but we'll actually
   * have to overrid some of the children maintainance operations
   * since we forge our child. *)
  inherit operator [s] as super

  (* This actually depends on [f], we have to trust the user here. *)
  method stype = s#stype

  (* The played source will be [s] at first,
   * but can become a combination of tracks by [f]. *)
  val mutable source = s

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

  method private reset_analysis =
    gen_before <- Generator.create () ;
    gen_after  <- Generator.create () ;
    rms_before <- 0. ; rmsi_before <- 0 ; mem_i <- 0 ;
    Array.iteri (fun i _ -> mem_before.(i) <- 0.) mem_before ;
    rms_after <- 0. ; rmsi_after <- 0 ;
    before_metadata <- after_metadata ;
    after_metadata <- None

  (* Activation record for our chilren sources. *)
  val mutable activation = []

  method wake_up activator =
    activation <- (self:>source)::activator ;
    s#get_ready ~dynamic:true activation ;
    source <- s ;
    source#get_ready activation ;
    Lang.iter_sources
      (fun s -> s#get_ready ~dynamic:true activation)
      transition

  method sleep =
    s#leave ~dynamic:true (self:>source) ;
    Lang.iter_sources
      (fun s -> s#leave ~dynamic:true (self:>source))
      transition

  val mutable status = `Idle

  method private get_frame ab =
    (* TODO check that [s] doesn't cache. *)
    match status with
      | `Idle ->
          (* TODO 0 is often answered just before the beginning of a track
           * I'd prefer -1 if possible. Otherwise add more info in the state.
           * Or simply don't bother about that harmless transition. *)
          let rem = if conservative then 0 else source#remaining in
            if rem < 0 || rem > cross_length then
              source#get ab
            else begin
              self#log#f 4 "Buffering end of track..." ;
              status <- `Before ;
              self#buffering cross_length ;
              begin match status with
                | `Limit -> ()
                | _ -> self#log#f 4 "More buffering will be needed."
              end ;
              self#get_frame ab
            end
      | `Before ->
          (* We started buffering but the track didn't end.
           * Play the beginning of the buffer while filling it more. *)
          self#buffering 0 ;
          AFrame.fill_frame gen_before ab
      | `Limit ->
          (* The track finished.
           * We compute rms_after and launch the transition. *)
          if source#is_ready then self#analyze_after ;
          self#create_transition ;
	  (* Check if the new source is ready *)
          if source#is_ready then 
	    self#get_frame ab
	  else
            Frame.add_break ab (Frame.position ab)	    
      | `After size ->
          (* The work is done, we are now playing the transition.
           * We have to keep playing it without checking for a new track limit
           * for [size] samples. *)
          if size<=0 then begin
            status <- `Idle ;
            self#get_frame ab
          end else
            let position = AFrame.position ab in
              source#get ab ;
              status <- `After (size-(AFrame.position ab)+position)

  (* An audio frame for intermediate computations. *)
  val b = Fmt.create_frame ()

  (* [bufferize n] stores at most [n+d] samples from [s] in [gen_before],
   * where [d=AFrame.size-1]. *)
  method private buffering n =
    (* Get audio samples *)
    AFrame.clear b ;
    assert (source#is_ready) ;
    source#get b ;
    (* Store them.
     * TODO Store the metadata too! Currently, all metadata is lost if the
     * smartcross() is conservative. *)
    Generator.feed gen_before
      (let m = AFrame.position b in
       let a = AFrame.get_float_pcm b in
         (* We _have to_ copy the arrays before passing to Gen. *)
         Array.map (fun a -> Array.sub a 0 m) a) ;
    (* Analyze them *)
    for i = 0 to AFrame.position b - 1 do
      let squares =
        Array.fold_left
          (fun squares track -> squares +. track.(i)*.track.(i))
          0.
          (AFrame.get_float_pcm b)
      in
        rms_before <- rms_before -. mem_before.(mem_i) +. squares ;
        mem_before.(mem_i) <- squares ;
        mem_i <- (mem_i + 1) mod rms_width ;
        rmsi_before <- min rms_width (rmsi_before + 1)
    done ;
    (* Should we buffer more or are we done ? *)
    if AFrame.is_partial b then
      status <- `Limit
    else
      if n>0 then begin
        (* Call after_output so that the source gets ready for more #get. *)
        source#after_output ;
        self#buffering (n - AFrame.position b)
      end

  (* Analyze the beginning of a new track. *)
  method private analyze_after =
    let todo = ref rms_width in
    let first = ref true in
      while !todo > 0 do
        AFrame.clear b ;
        assert (source#is_ready) ;
        source#get b ;
        source#after_output ;
        if !first then after_metadata <- AFrame.get_metadata b 0 ;
        first := false ;
        Generator.feed gen_after
          (let m = AFrame.position b in
           let a = AFrame.get_float_pcm b in
             (* We _have to_ copy the arrays before passing to Gen. *)
             Array.map (fun a -> Array.sub a 0 m) a) ;
        for i = 0 to AFrame.position b - 1 do
          let squares =
            Array.fold_left
              (fun squares track -> squares +. track.(i)*.track.(i))
              0.
            (AFrame.get_float_pcm b)
          in
            rms_after <- rms_after +. squares ;
            rmsi_after <- rmsi_after + 1
        done ;
        todo := !todo - AFrame.position b ;
        (* Not sure how to handle end-of-track at this point... *)
        if AFrame.is_partial b then todo := 0
      done

  val chans = float (Fmt.channels())

  (* Sum up analysis and build the transition *)
  method private create_transition =
    let chans = float (Fmt.channels()) in
    let db_after  =
      Sutils.dB_of_lin (sqrt (rms_after  /. float rmsi_after /. chans))
    in
    let db_before =
      Sutils.dB_of_lin (sqrt (rms_before /. float rmsi_before /. chans))
    in
    let before = ((new Generated.consumer gen_before):>source) in
    let after  =
      let metadata = 
        match after_metadata with
	  | None -> None
	  | Some m -> Some (0,m)
      in
      let beginning =
        ((new Generated.consumer ~metadata gen_after):>source)
      in
        ((new Sequence.sequence ~merge:true [beginning;s]):>source)
    in
    let metadata =
      function None -> Lang.list [] | Some m -> Lang.metadata m
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
        Lang.to_source (Lang.apply transition params)
    in
    let compound =
      self#log#f 3 "Analysis: %fdB / %fdB (%.2fs / %.2fs)"
           db_before db_after
           (Fmt.seconds_of_samples (Generator.length gen_before))
           (Fmt.seconds_of_samples (Generator.length gen_after)) ;
      if Generator.length gen_before > minimum_length then
        f before after
      else begin
        self#log#f 3 "Not enough data for crossing." ;
        ((new Sequence.sequence [before;after]):>source)
      end
    in
      source#leave (self:>source) ;
      compound#get_ready activation ;
      source <- compound ;
      status <- `After inhibit ;
      self#reset_analysis

  method remaining =
    match status with
      | `Idle | `After _ -> source#remaining
      | `Limit -> 0
      | `Before ->
          source#remaining +
          Generator.length gen_before * Fmt.ticks_per_sample ()

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method after_output =
    self#advance ;
    source#after_output (* TODO cross doesn't do the same ! *)

end

let transition_t =
  Lang.fun_t [false,"",Lang.source_t;false,"",Lang.source_t] Lang.source_t

let condition_t =
  Lang.fun_t [false,"",Lang.int_t;false,"",Lang.int_t] Lang.bool_t

let () =
  Lang.add_operator "smart_cross"
    [ "duration", Lang.float_t, Some (Lang.float 5.),
      Some "Duration in seconds of the crossed end of track." ;

      "inhibit", Lang.float_t, Some (Lang.float (-1.)),
      Some ("Minimum delay between two transitions. It is useful in order to "^
            "avoid that a transition is triggered on top of another when "^
            "an end-of-track occurs in the first one. Negative values mean "^
            "'same as duration'. "^
            "Warning: zero inhibition can cause infinite loops.") ;

      "minimum", Lang.float_t, (Some (Lang.float (-1.))),
      Some ("Minimum duration (in sec.) for a cross: "^
            "If the track ends without any warning (e.g. in case of skip) "^
            "there may not be enough data for a decent composition. "^
            "Set to 0. to avoid having transitions after skips, "^
            "or more to avoid transitions on short tracks. "^
            "With the negative default, transitions always occur.") ;

      "width", Lang.float_t, Some (Lang.float 1.),
      Some "Width of the power computation window." ;

      "conservative", Lang.bool_t, Some (Lang.bool false),
      Some ("Do not trust remaining time estimations, always buffering "^
            "data in advance. This avoids being tricked by skips, either "^
            "manual or caused by skip_blank().") ;

      "",
      Lang.fun_t
        [false,"",Lang.float_t;false,"",Lang.float_t;
         false,"",Lang.metadata_t;false,"",Lang.metadata_t;
         false,"",Lang.source_t;false,"",Lang.source_t]
        Lang.source_t,
      None,
      Some ("Transition function, composing from the end of a track "^
            "and the next track. It also takes the power of the signal before "^
            "and after the transition, and the metadata.") ;

      "",Lang.source_t,None,None

    ]
    ~category:Lang.SoundProcessing
    ~descr:("Cross operator, allowing the composition of "^
            "the N last seconds of a track with the beginning of "^
            "the next track, using a transition function depending on "^
            "the relative power of the signal before and after "^
            "the end of track.")
    (fun p ->
       let duration = Lang.to_float (List.assoc "duration" p) in
       let cross_length = Fmt.ticks_of_seconds duration in

       let minimum = Lang.to_float (List.assoc "minimum" p) in
       let minimum_length = Fmt.samples_of_seconds minimum in

       let inhibit = Lang.to_float (List.assoc "inhibit" p) in
       let inhibit = if inhibit < 0. then duration else inhibit in
       let inhibit = Fmt.samples_of_seconds inhibit in

       let rms_width = Lang.to_float (List.assoc "width" p) in
       let rms_width = Fmt.samples_of_seconds rms_width in

       let transition = Lang.assoc "" 1 p in

       let conservative = Lang.to_bool (List.assoc "conservative" p) in

       let source = Lang.to_source (Lang.assoc "" 2 p) in
         ((new cross source transition ~conservative
             ~cross_length ~rms_width ~inhibit ~minimum_length):>source))
