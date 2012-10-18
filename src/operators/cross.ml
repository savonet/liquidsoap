(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2012 Savonet team

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

(* Common effects like cross-fading can be split into two parts: crossing,
 * and fading. Here we implement crossing, not caring about fading:
 * a arbitrary transition function is passed, taking care of the combination.
 *
 * A buffer is needed to store the end of a track before combining it with the
 * next track. We could always have a full buffer, but this would involve
 * copying all the time. Instead, we try to fill the buffer only when getting
 * close to the end of track. The problem then is to cope with tracks which are
 * longer than expected, i.e. which end doesn't really fit in the buffer.
 *
 * This operator works with any type of stream.
 * All three parameters are durations in ticks. *)
class cross ~kind (s:source)
        ?(meta="liq_start_next") ~cross_length
        ~conservative ~active ~inhibit ~minimum_length f =
object (self)
  inherit source ~name:"cross" kind as super

  method stype = s#stype (* This should actually depend on [f]. *)

  (* The played source will be [s] at first, but can become a combination of
   * tracks by [f]. See the doc for the corresponding field, as well as
   * the #source_get in Smartcross. *)
  val mutable source = s

  method private wake_up activation =
    super#wake_up activation ;
    s#get_ready ~dynamic:true [(self:>source)] ;
    source <- s ;
    source#get_ready [(self:>source)] ;
    Lang.iter_sources (fun s -> s#get_ready ~dynamic:true [(self:>source)]) f

  method private sleep =
    source#leave (self:>source) ;
    s#leave ~dynamic:true (self:>source) ;
    Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>source)) f

  val default_cross_length = cross_length
  (* Cross-length for the current track, in ticks. *)
  val mutable cur_cross_length = None

  (** Idle: no buffering, can start buffering anytime,
    * Started: currently buffering and replaying extra buffered data,
    * After: buffering finished, grace time before going back to Idle. *)
  val mutable status = `Idle

  method remaining =
    match status with
      | `Idle | `After _ -> source#remaining
      | `Started b ->
          let rem = source#remaining in
            if rem<0 then rem else rem + Generator.length b

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  (** This operator sets up a slave clock like resample. It differs
    * in that the source is sometimes used to directly fill the
    * master frame, and sometimes used to fill an intermediate
    * slave frame that is then added to a buffer. A simple situation
    * à la resample can be restored if we make sure that the
    * position in the slave frame is the same as the position
    * of the master frame when we switch from one to the
    * other. Finally, we also have to relay some master ticks
    * as slave ticks when the slave frame is unused.
    *
    * The normal situation is like that (number are slave ticks):
    *   Master 11111|22222|        444|55555|66
    *   Slave             |33333|44
    * At the end of tick 2, we realize that the end of track is getting close
    * so we start buffering it. In the middle of 4 we reach the end of
    * track, and pass the buffer and source to the transition function.
    *
    * Technically it is possible that buffering doesn't start at the
    * beginning of a frame: we trigger it at the beginning of #get_frame,
    * which could be in the middle of a frame. For example, after an
    * end of track, #get_frame could be anywhere, and we might buffer
    * already if we're conservative.
    *
    * For the switch from slave frame to master frame, we need to
    * break synchronization: the buffering ended at position P
    * in the slave frame, but when we pass the buffer and the
    * source to the transition, everything is at position 0.
    * Thus, an active mode would interact badly with caching.
    *
    * So: no extra tick when moving from the master frame to the
    * slave frame (i.e. when starting buffering) and a slave tick
    * after each filling of the slave frame, and a slave tick
    * at the end of buffering. *)

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
      f

  (* Intermediate for buffering the source's stream. *)
  val buf_frame = Frame.create kind
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

  (* In order to get the cross_length metadata we parse all metadata
   * in the stream. It is not enough to look at the first frame
   * that we obtain, since the transition might delay the metadata.
   * We could get the past metadata after the transition "grace time"
   * passes but this is not super safe as new metadata might have
   * been inserted in the meantime, hiding the interesting one. *)
  method private update_cross_length ab pos =
    match cur_cross_length with
      | Some _ -> ()
      | None ->
          List.iter
            (fun (p,m) ->
               if p>=pos then
                 match Utils.hashtbl_get m meta with
                   | None -> ()
                   | Some v ->
                       try
                         let l = float_of_string v in
                           cur_cross_length <-
                             Some (Frame.master_of_seconds l)
                       with _ -> ())
            (Frame.get_all_metadata ab)

  method private get_frame ab =
    let p = Frame.position ab in
    match status with
      | `Idle ->
          let cross_length =
            match cur_cross_length with
              | Some c -> c | None -> default_cross_length
          in
          let rem = if conservative then 0 else source#remaining in
            if rem < 0 || rem > cross_length then begin
              source#get ab ;
              needs_tick <- true ;
              self#update_cross_length ab p
            end else begin
              self#log#f 4 "Buffering end of track..." ;
              let buffer = Generator.create () in
                Frame.set_breaks buf_frame [Frame.position ab] ;
                Frame.set_all_metadata buf_frame
                  (match Frame.get_past_metadata ab with
                     | Some x -> [-1,x] | None -> []) ;
                status <- `Started buffer ;
                cur_cross_length <- None ;
                self#buffering buffer cross_length ;
                (* We may be in `After or `Started state. *)
                begin match status with
                  | `Started _ -> self#log#f 4 "More buffering will be needed."
                  | _ -> ()
                end ;
                self#get_frame ab
            end
      | `Started buffer ->
          (* We started buffering but the track didn't end.
           * Play the beginning of the buffer while filling it more. *)
          self#buffering buffer 0 ;
          Generator.fill buffer ab
      | `After size ->
          (* After the buffering phase, [source] has become the result
           * of the transition. We play it without possibly crossing
           * new tracks until some delay has passed (given by [size]). *)
          if size<=0 then begin
            status <- `Idle ;
            self#get_frame ab
          end else
            let position = Frame.position ab in
              source#get ab ;
              needs_tick <- true ;
              status <- `After (size - Frame.position ab + position) ;
              (* Try to catch a [liq_start_next] value. *)
              self#update_cross_length ab p

  (* Try to store [n] ticks of data from [s] in [buffer],
   * setup the crossing when the end of track is reached,
   * otherwise remain in `Started (buffering) mode. *)
  method private buffering buffer n =
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
    Generator.feed buffer
      ~metadata:(Frame.get_all_metadata buf_frame)
      content start (stop-start) ;
    if Frame.is_partial buf_frame then
      (* As for Switch's transitions, we avoid stacking compositions
       * because this would lead to huge sources, never simplified.
       * We compose the end of a track with the original source [s] instead of
       * the composed [source]. *)
      let s =
        (* TODO this is questionable: if the source is caching
         *   the transition won't see the beginning of the next track *)
        self#slave_tick ;
        Clock.collect_after
          (fun () ->
             let s =
               (* TODO if end_of_track isn't ready [source] isn't going
                *   to be ready either, and it'll crash *)
               if not s#is_ready then self#log#f 3 "No next track ready yet." ;
               let end_of_track = new Generated.consumer ~kind buffer in
                 if Generator.length buffer > minimum_length then
                   let t = Lang.source_t (Lang.kind_type_of_frame_kind kind) in
                     Lang.to_source
                       (Lang.apply ~t f ["",Lang.source end_of_track;
                                         "",Lang.source s])
                 else begin
                   self#log#f 4 "Not enough data for crossing." ;
                   new Sequence.sequence ~kind [end_of_track; s]
                 end
             in
               (* This might seem useless by construction of [source]
                * but is actually useful if [f] discards [s]. *)
               Clock.unify s#clock source#clock ;
               s)
      in
        (* [source] almost always contains [s], it's better to have it
         * activated twice for a split second than to have it stop
         * and restart completely: get_ready before leave. *)
        s#get_ready [(self:>source)] ;
        source#leave (self:>source) ;
        source <- s ;
        status <- `After inhibit
    else
      if n>0 then self#buffering buffer (n - Frame.position buf_frame)

end

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "cross"
    [ "duration", Lang.float_t, Some (Lang.float 5.),
      Some
        "Duration in seconds of the crossed end of track. \
         This value can be changed on a per-file basis using \
         a special metadata field." ;

      "override", Lang.string_t, Some (Lang.string "liq_start_next"),
      Some "Metadata field which, if present and containing a float, \
            overrides the 'duration' parameter for current track." ;

      "inhibit", Lang.float_t, Some (Lang.float (-1.)),
      Some  "Minimum delay between two transitions. It is useful in order to \
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
      Lang.fun_t [false,"",Lang.source_t k;
                  false,"",Lang.source_t k] (Lang.source_t k),
      None,
      Some "Composition of an end of track and the next track." ;

      "",Lang.source_t k,None,None

    ]
    ~category:Lang.SoundProcessing
    ~descr:"Generic cross operator, allowing the composition of \
            the N last seconds of a track with the beginning of \
            the next track."
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let duration = Lang.to_float (List.assoc "duration" p) in
       let cross_length = Frame.master_of_seconds duration in
       let meta = Lang.to_string (List.assoc "override" p) in
       let minimum = Lang.to_float (List.assoc "minimum" p) in
       let minimum_length = Frame.master_of_seconds minimum in

       let inhibit = Lang.to_float (List.assoc "inhibit" p) in
       let inhibit = if inhibit < 0. then duration +. 1. else inhibit in
       let inhibit = Frame.master_of_seconds inhibit in

       let conservative = Lang.to_bool (List.assoc "conservative" p) in
       let active = Lang.to_bool (List.assoc "active" p) in

       let f = Lang.assoc "" 1 p in
       let source = Lang.to_source (Lang.assoc "" 2 p) in
         new cross source ~kind ~meta ~cross_length
               ~inhibit ~active ~conservative ~minimum_length f)
