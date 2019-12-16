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

(* The [cue_cut] class is able to skip over the beginning and end
 * of a track according to cue points.
 * This involves quite a bit of trickery involving clocks, #seek as well
 * as reverting frame contents. Even more trickery would be needed to
 * implement a [cue_split] operator that splits tracks according to
 * cue points: in particular, the frame manipulation would get nasty,
 * involving storing chunks that have been fetched too early, replaying
 * them later, glued with new content. *)

(* We use ticks for precision, but store them as Int64 to allow
 * long durations. This should eventually be generalized to all of
 * liquidsoap, removing limitations such as the duration passed to
 * #seek or returned by #remaining.
 * We introduce a few notations to make this comfortable. *)

let ( -- ) = Int64.sub
let ( ++ ) = Int64.add

class cue_cut ~kind ~m_cue_in ~m_cue_out (source : Source.source) =
  object (self)
    inherit source ~name:"cue_cut" kind as super

    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    (** For each track, the state may be:
    *  - None (undefined) if no track has started
    *  - Some None (defined, useless)
    *    if the track should be played until its end
    *  - Some (Some (elapsed,cue_out))
    *    if a cue_out point has been set,
    *    where both positions (current and end position) are given
    *    relative to the beginning of the track (not relative to cue_in).
    * There is no need to store cue_in point information as it is
    * performed immediately. *)
    val mutable track_state = None

    method remaining =
      let source_remaining = Int64.of_int source#remaining in
      Int64.to_int
        ( match track_state with
          | None | Some None -> source_remaining
          | Some (Some (elapsed, cue_out)) ->
              let target = cue_out -- elapsed in
              if source_remaining = -1L then target
              else min source_remaining target )

    (* Management of the source has to be done fully manually:
     * we don't use the default mechanisms because we want complete
     * control over the source's clock. See cross.ml for details. *)
    method private wake_up activation =
      super#wake_up activation;
      source#get_ready [(self :> source)]

    method private sleep = source#leave (self :> source)

    method private set_clock =
      let slave_clock = Clock.create_known (new Clock.clock self#id) in
      (* Our external clock should stricly contain the slave clock. *)
      Clock.unify self#clock
        (Clock.create_unknown ~sources:[] ~sub_clocks:[slave_clock]);

      (* The source must belong to our clock, since we need occasional
       * control on its flow (to seek at the beginning of a track). *)
      Clock.unify slave_clock source#clock;

      (* When this source disappears the slave clock becomes useless.
       * To allow for its collection, remove references to it as a subclock
       * of the master clock. *)
      Gc.finalise (fun self -> Clock.forget self#clock slave_clock) self

    (* The slave clock will tick just like the master clock, except one
     * extra tick at the beginning of each track where data has to
     * be skipped. *)
    method private slave_tick =
      (Clock.get source#clock)#end_tick;
      source#after_output

    method after_output =
      super#after_output;
      self#slave_tick

    method private get_cue_points buf pos =
      match Frame.get_metadata buf pos with
        | None -> (None, None)
        | Some table ->
            let get key =
              try
                let content = Hashtbl.find table key in
                try
                  Some
                    (Int64.of_float
                       ( float_of_string content
                       *. float (Lazy.force Frame.master_rate) ))
                with _ ->
                  self#log#severe "Ill-formed metadata %s=%S!" key content;
                  None
              with Not_found -> None
            in
            let cue_in = get m_cue_in in
            let cue_out = get m_cue_out in
            (* Sanity checks
             * We ignore invalid values rather than setting
             * cue-out = cue-in since this would result in empty
             * tracks and potential loops. *)
            let cue_in =
              match cue_in with
                | Some i when i <= 0L ->
                    if i < 0L then
                      self#log#severe "Ignoring negative cue-in point.";
                    None
                | i -> i
            in
            let cue_out =
              match (cue_in, cue_out) with
                | Some i, Some o when o < i ->
                    self#log#severe
                      "Ignoring cue-out point before cue-in. Note that cue-out \
                       should be given relative to the beginning of the file.";
                    None
                | None, Some o when o < 0L ->
                    self#log#severe "Ignoring negative cue-out point.";
                    None
                | _, cue_out -> cue_out
            in
            (cue_in, cue_out)

    method private cue_in ~buf ~breaks ~delta_pos ~out_pos ~pos seek_time =
      self#log#important "Cueing in...";
      let seek_pos = Int64.to_int seek_time - delta_pos in
      let seeked_pos = source#seek seek_pos in
      (* Set back original breaks. *)
      Frame.set_breaks buf breaks;

      (* Before pulling new data to fill-in the frame,
       * we need to tick the slave clock otherwise we might get
       * the same old (cached) data. *)
      self#slave_tick;
      source#get buf;
      let new_pos = Frame.position buf in
      ( Int64.of_int (delta_pos + seeked_pos + new_pos - pos),
        if seeked_pos = seek_pos then out_pos
        else (
          let position = Int64.of_int (delta_pos + seeked_pos) in
          match out_pos with
            | Some o when position > o ->
                self#log#important
                  "Initial seek reached %i ticks past cue-out point!"
                  (Int64.to_int (position -- o));
                Some position
            | _ ->
                if seeked_pos = 0 then
                  self#log#severe "Could not seek to cue point!";
                self#log#info "Seeked %i ticks instead of %i." seeked_pos
                  seek_pos;
                out_pos ) )

    method private cue_out ~buf ~elapsed ~pos out_pos =
      self#log#important "Cueing out...";

      (* If not already an end of track, notify the source to end the track
       * and do one more #get to consume any remaining data. *)
      if not (Frame.is_partial buf) then (
        source#abort_track;
        self#slave_tick;
        source#get (Frame.create kind) );

      (* Quantify in the previous #get
       * - the amount of [extra] data past the cue point, to be dropped;
       * - the amount of [remaining] data, that should be left. *)
      let new_pos = Frame.position buf in
      let extra = Int64.to_int (elapsed -- out_pos) in
      let remaining = new_pos - pos - extra in
      (* We know that [extra>0] so [remaining] is strictly less
       * than the total amount of data from the last #get, ie.,
       * we are really cutting data out and setting an end-of-track
       * break.
       * It may be that [remaining=0]. This will happen after a
       * #get where [elapsed = out_pos] where no cutting will be
       * performed. This is important because cutting wouldn't
       * yield a partial frame, ie., and end of track. In other
       * words, we delay (as usual) end-of-tracks from the end to
       * the beginning of frames.
       * We enforce that [remaining>=0] by checking for each
       * frame that there isn't too much extra data. This also relies
       * on the careful checks done on cue_in, out_pos, and the
       * correction of out_pos after abusive cue_in. *)
      assert (remaining >= 0);
      Frame.set_breaks buf (Utils.remove_one (( = ) new_pos) (Frame.breaks buf));
      Frame.add_break buf (pos + remaining);
      track_state <- None

    method private get_frame buf =
      let breaks = Frame.breaks buf in
      let pos = Frame.position buf in
      source#get buf;
      let new_pos = Frame.position buf in
      let delta_pos = new_pos - pos in
      let in_track_state =
        (* Compute track state after the #get *)
        match track_state with
          | Some None -> None
          | Some (Some (e, o)) -> Some (e ++ Int64.of_int delta_pos, o)
          | None -> (
              (* New track: get the cue point information from metadata *)
              let in_pos, out_pos = self#get_cue_points buf pos in
              (* Perform cue_in if required, adjusting out_pos if needed *)
              let elapsed, out_pos =
                match in_pos with
                  | None | Some 0L -> (Int64.of_int delta_pos, out_pos)
                  | Some seek_time ->
                      self#cue_in ~buf ~breaks ~delta_pos ~out_pos ~pos
                        seek_time
              in
              match out_pos with None -> None | Some pos -> Some (elapsed, pos)
              )
      in
      track_state <- Some in_track_state;

      (* Perform cue-out if needed *)
      match in_track_state with
        | Some (elapsed, out_pos) when elapsed > out_pos ->
            self#cue_out ~buf ~elapsed ~pos out_pos
        | _ ->
            if Frame.is_partial buf then (
              if in_track_state <> None then
                self#log#important "End of track before cue-out point.";
              track_state <- None )
  end

let () =
  let kind = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "cue_cut" ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:
      "Start track after a cue in point and stop it at cue out point. The cue \
       points are given as metadata, in seconds from the begining of tracks."
    [
      ( "cue_in_metadata",
        Lang.string_t,
        Some (Lang.string "liq_cue_in"),
        Some "Metadata for cue in points." );
      ( "cue_out_metadata",
        Lang.string_t,
        Some (Lang.string "liq_cue_out"),
        Some "Metadata for cue out points." );
      ("", Lang.source_t kind, None, None);
    ]
    (fun p kind ->
      let m_cue_in = Lang.to_string (Lang.assoc "cue_in_metadata" 1 p) in
      let m_cue_out = Lang.to_string (Lang.assoc "cue_out_metadata" 1 p) in
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      new cue_cut ~kind ~m_cue_in ~m_cue_out s)
