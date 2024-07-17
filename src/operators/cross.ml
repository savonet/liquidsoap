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

(* Do not care about fading at first, not even about adding the last frames of a
 * track to the beginning of the next one: a generic function will be used for
 * combination -- a transition function like in Switch.
 * A buffer is needed to store the end of a track before combining it with the
 * next track. We could always have a full buffer, but this would involve
 * copying all the time. Instead, we try to fill the buffer only when getting
 * close to the end of track. The problem then is to cope with tracks which are
 * longer than expected, i.e. which end doesn't really fit in the buffer.
 *
 * All three parameters are durations in ticks.
 * This operator is data-dependent only because it uses Generator, which is
 * specialized for audio frames. Otherwise it should be generic. That's why I
 * stick to the Frame module here, and ticks units. *)
class cross s ~cross_length ~inhibit ~minimum_length f =
object (self)
  inherit operator [s] as super

  method stype = s#stype (* This actually depends on [f]. *)

  (* The played source will be [s] at first, but can become a combination of
   * tracks by [f]. *)
  val mutable source = s

  val mutable activation = []

  method wake_up activator =
    activation <- (self:>source)::activator ;
    s#get_ready ~dynamic:true activation ;
    source <- s ;
    source#get_ready activation ;
    Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f

  method sleep =
    s#leave ~dynamic:true (self:>source) ;
    Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>source)) f

  val default_cross_length = cross_length
  (* Cross-length for the current track in ticks. *)
  val mutable cur_cross_length = None

  (* It is tricky to get the cross_length.
   * The metadata 'liq_start_next' can come through the transition function,
   * from which other metadata comes.
   * Our solution is to scan every metadata packet until we found a value,
   * and reset the value when a transition starts.
   * All that would probably be simpler if we had persistent metadatas:
   * remember the latest metadata in the stream, stored in [Frame.t]s. *)
  method update_cross_length ab pos =
    match cur_cross_length with
      | Some _ -> ()
      | None ->
          List.iter
            (fun (p,m) ->
               if p>=pos then
                 match Utils.hashtbl_get m "liq_start_next" with
                   | None -> ()
                   | Some v ->
                       try
                         let l = float_of_string v in
                           cur_cross_length <-
                             Some (Fmt.ticks_of_seconds l)
                       with _ -> ())
            (Frame.get_all_metadata ab)

  val mutable status = `Idle

  method private get_frame ab =
    (* TODO check that [s] doesn't cache. *)
    let p = Frame.position ab in
    match status with
      | `Idle ->
          let cross_length =
            match cur_cross_length with
              | Some c -> c | None -> default_cross_length
          in
          (* TODO 0 is often answered just before the beginning of a track
           * I'd prefer -1 if possible. Otherwise add more info in the state.
           * Or simply don't bother about that harmless transition. *)
          let rem = source#remaining in
            if rem < 0 || rem > cross_length then begin
              source#get ab ;
              self#update_cross_length ab p
            end else begin
              self#log 4 "Buffering end of track..." ;
              let buffer = Generator.create () in
                status <- `Started buffer ;
                cur_cross_length <- None ;
                self#buffering buffer cross_length ;
                begin match status with
                  | `Stopped _ -> ()
                  | _ -> self#log 4 "More buffering will be needed."
                end ;
                self#get_frame ab ;
            end
      | `Started buffer ->
          (* We started buffering but the track didn't end.
           * Play the beginning of the buffer while filling it more. *)
          self#buffering buffer 0 ;
          AFrame.fill_frame buffer ab
      | `Stopped (s,size) ->
          (* The buffering stopped and we got [s], combination of the buffered
           * end of track and the new track. It will be played without bothering
           * about the possible close end-of-track for [size] bytes. Then it
           * will replace the normal source, allowing it to be crossed. *)
          if size<=0 then begin
            source <- s ;
            status <- `Idle ;
            self#get_frame ab
          end else
            let position = Frame.position ab in
              s#get ab ;
              status <- `Stopped (s,size-(Frame.position ab)+position) ;
              (* Try to catch a liq_start_next value.. *)
              self#update_cross_length ab p

  (* An audio frame for intermediate computations. *)
  val b = Fmt.create_frame ()

  (* [bufferize n] stores at most [n+d] frames from [s] in [buffers],
   * where [d=Frame.size-1]. *)
  method private buffering buffer n =
    AFrame.clear b ;
    source#get b ;
    Generator.feed buffer
      (let m = AFrame.position b in
       let a = AFrame.get_float_pcm b in
         (* Always copy the tracks
          * because they are not copied by the Generator. *)
         Array.map (fun a -> Array.sub a 0 m) a) ;
    if AFrame.is_partial b then
      (* As for Switch's transitions, we avoid stacking compositions
       * because this would lead to huge sources, never simplified.
       * We compose the end of a track with the original source [s] instead of
       * the composed [source]. *)
      let s =
        if Generator.length buffer > minimum_length then
          Lang.to_source
            (Lang.apply f ["",Lang.source (new Generated.consumer buffer);
                           "",Lang.source s])
        else begin
          self#log 4 "Not enough data for crossing." ;
          ((new Sequence.sequence [new Generated.consumer buffer ; s]):>source)
        end
      in
        source#leave (self:>source) ;
        s#get_ready activation ;
        status <- `Stopped (s, inhibit)
    else
      if n>0 then self#buffering buffer (n - Frame.position b)

  method remaining =
    match status with
      | `Idle -> source#remaining
      | `Started b ->
          source#remaining +
          Generator.length b * Fmt.ticks_per_sample ()
      | `Stopped (s,_) -> s#remaining

  method is_ready =
    match status with
      | `Idle -> source#is_ready
      | `Started b -> true
      | `Stopped (s,_) -> s#is_ready

  method abort_track =
    match status with
      | `Stopped (s,_) -> s#abort_track
      | _ -> source#abort_track

  method after_output =
    self#clear_cache ;
    match status with
      | `Stopped (s,_) -> s#after_output
      | _ -> source#after_output

end

let () =
  Lang.add_operator "cross"
    [ "duration", Lang.float_t, Some (Lang.float 5.),
      Some
        ("Duration in seconds of the crossed end of track. "^
         "This value can be set on a per-file basis using the metadata field "^
         "'liq_start_next' (float in seconds).") ;

      "inhibit", Lang.float_t, Some (Lang.float (-1.)),
      Some ("Minimum delay between two transitions. It is useful in order to "^
            "avoid that a transition is triggered on top of another when "^
            "an end-of-track occurs in the first one. Negative values mean "^
            "'same as duration'.") ;

      "minimum", Lang.float_t, (Some (Lang.float (-1.))),
      Some ("Minimum duration (in sec.) for a cross: "^
            "If the track ends without any warning (e.g. in case of skip) "^
            "there may not be enough data for a decent composition. "^
            "Set to 0. to avoid having transitions after skips, "^
            "or more to avoid transitions on short tracks. "^
            "With the negative default, transitions always occur.") ;

      "",
      Lang.fun_t [false,"",Lang.source_t;false,"",Lang.source_t] Lang.source_t,
      None,
      Some "Composition of an end of track and the next track." ;

      "",Lang.source_t,None,None

    ]
    ~category:Lang.SoundProcessing
    ~descr:("Generic cross operator, allowing the composition of "^
            "the N last seconds of a track with the beginning of "^
            "the next track.")
    (fun p ->
       let duration = Lang.to_float (List.assoc "duration" p) in
       let cross_length = Fmt.ticks_of_seconds duration in

       let minimum = Lang.to_float (List.assoc "minimum" p) in
       let minimum_length = Fmt.ticks_of_seconds minimum in

       let inhibit = Lang.to_float (List.assoc "inhibit" p) in
       let inhibit = if inhibit < 0. then duration else inhibit in
       let inhibit = Fmt.ticks_of_seconds inhibit in

       let f = Lang.assoc "" 1 p in
       let source = Lang.to_source (Lang.assoc "" 2 p) in
         ((new cross source ~cross_length ~inhibit ~minimum_length f):>source))
