(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2009 Savonet team

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
class cross s ~kind ?(meta="liq_start_next") ~cross_length
              ~conservative ~inhibit ~minimum_length f =
object (self)
  inherit operator ~name:"cross" kind [s] as super

  method stype = s#stype (* This actually depends on [f]. *)

  (* The played source will be [s] at first, but can become a combination of
   * tracks by [f]. See the doc for the corresponding field, as well as
   * the #source_get in Smartcross. *)
  val mutable source = s

  method private source_get ab =
    source#get ab ;
    source#after_output

  val mutable activation = []

  method private wake_up activator =
    activation <- (self:>source)::activator ;
    s#get_ready ~dynamic:true activation ;
    source <- s ;
    source#get_ready activation ;
    Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f

  method private sleep =
    s#leave ~dynamic:true (self:>source) ;
    Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>source)) f

  val default_cross_length = cross_length
  (* Cross-length for the current track in ticks. *)
  val mutable cur_cross_length = None

  (* An audio frame for intermediate computations. *)
  val buf_frame = Frame.create kind

  (* It is tricky to get the cross_length.
   * The metadata meta can come through the transition function,
   * from which other metadata comes.
   * Our solution is to scan every metadata packet until we found a value,
   * and reset the value when a transition starts.
   * All that would probably be simpler if we had persistent metadatas:
   * remember the latest metadata in the stream, stored in [Frame.t]s. *)
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

  val mutable status = `Idle

  method private get_frame ab =
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
          let rem = if conservative then 0 else source#remaining in
            if rem < 0 || rem > cross_length then begin
              self#source_get ab ;
              self#update_cross_length ab p
            end else begin
              self#log#f 4 "Buffering end of track..." ;
              let buffer = Generator.create () in
                Frame.set_all_metadata buf_frame
                  (match Frame.get_past_metadata ab with
                     | Some x -> [-1,x] | None -> []) ;
                status <- `Started buffer ;
                cur_cross_length <- None ;
                self#buffering buffer cross_length ;
                begin match status with
                  | `Started _ -> self#log#f 4 "More buffering will be needed."
                  | _ -> ()
                end ;
                self#get_frame ab ;
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
              self#source_get ab ;
              status <- `After (size - Frame.position ab + position) ;
              (* Try to catch a [liq_start_next] value. *)
              self#update_cross_length ab p

  (* Try to store [n] ticks of data from [s] in [buffer],
   * setup the crossing when the end of track is reached,
   * otherwise remain in `Started (buffering) mode. *)
  method private buffering buffer n =
    Frame.advance buf_frame ;
    self#source_get buf_frame ;
    Generator.feed_from_frame buffer buf_frame;
    if Frame.is_partial buf_frame then
      (* As for Switch's transitions, we avoid stacking compositions
       * because this would lead to huge sources, never simplified.
       * We compose the end of a track with the original source [s] instead of
       * the composed [source]. *)
      let s =
        if not s#is_ready then self#log#f 3 "No ready track yet." ;
        let end_of_track = new Generated.consumer ~kind buffer in
          if Generator.length buffer > minimum_length then
            Lang.to_source
              (Lang.apply f ["",Lang.source end_of_track;
                             "",Lang.source s])
          else begin
            self#log#f 4 "Not enough data for crossing." ;
            new Sequence.sequence ~kind [end_of_track; s]
          end
      in
        source#leave (self:>source) ;
        s#get_ready activation ;
        source <- s ;
        status <- `After inhibit
    else
      if n>0 then self#buffering buffer (n - Frame.position buf_frame)

  method remaining =
    match status with
      | `Idle | `After _ -> source#remaining
      | `Started b ->
          let rem = source#remaining in
            if rem<0 then rem else rem + Generator.length b

  method is_ready = source#is_ready

  method abort_track = source#abort_track

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

       let f = Lang.assoc "" 1 p in
       let source = Lang.to_source (Lang.assoc "" 2 p) in
         new cross source ~kind ~meta ~cross_length
               ~inhibit ~conservative ~minimum_length f)
