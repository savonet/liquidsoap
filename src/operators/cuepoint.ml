(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

let (--) = Int64.sub
let (++) = Int64.add

class cue_cut ~kind ~m_cue_in ~m_cue_out (source:Source.source) =
object (self)
  inherit source ~name:"cue_cut" kind as super

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  (** Number of ticks from the beginning. *)
  val mutable elapsed = 0L
  (** Are we before the cue-in point?
    * This holds iff no track has started. *)
  val mutable before = true
  (** Cue points in ticks, cue_out given relative to beginning. *)
  val mutable cue_in = None
  val mutable cue_out = None

  method private reset =
    elapsed <- 0L;
    before <- true;
    cue_in <- None;
    cue_out <- None

  method remaining =
   let source_remaining = Int64.of_int source#remaining in
     Int64.to_int
       (match cue_out with
          | None -> source_remaining
          | Some time ->
              let target = time -- elapsed in
                if source_remaining = -1L then target else
                  min source_remaining target)

  (* Management of the source has to be done fully manually:
   * we don't use the default mechanisms because we want complete
   * control over the source's clock. See cross.ml for details. *)

  method private wake_up activation =
    super#wake_up activation ;
    source#get_ready [(self:>source)]

  method private sleep =
    source#leave (self:>source)

  method set_clock =
    let slave_clock = Clock.create_known (new Clock.clock self#id) in
    (* Our external clock should stricly contain the slave clock. *)
    Clock.unify
      self#clock
      (Clock.create_unknown ~sources:[] ~sub_clocks:[slave_clock]) ;
    (* The source must belong to our clock, since we need occasional
     * control on its flow (to seek at the beginning of a track). *)
    Clock.unify slave_clock source#clock

  (* The slave clock will tick just like the master clock, except one
   * extra tick at the beginning of each track where data has to
   * be skipped. *)
  method private slave_tick =
    (Clock.get source#clock)#end_tick ;
    source#after_output

  method after_output =
    super#after_output ;
    self#slave_tick

  method private get_frame buf =
    let breaks = Frame.breaks buf in
    let pos = Frame.position buf in
    source#get buf;
    let new_pos = Frame.position buf in
    let delta_pos = new_pos - pos in
    let meta = Frame.get_all_metadata buf in
    (* Scan all metadata for cueing points. *)
    List.iter
      (fun (p,m) ->
         assert (p < new_pos) ;
         if pos <= p then
           List.iter
             (fun (meta,f) ->
                try
                  begin
                    let content = Hashtbl.find m meta in
                      try
                        f (Int64.of_float
                             (float_of_string content *.
                              float (Lazy.force Frame.master_rate)))
                      with
                        | _ -> self#log#f 2
                                 "Ill-formed metadata %s=%S!" meta content
                  end
                with Not_found -> ())
             [(m_cue_in,(fun x -> cue_in <- Some x));
              (m_cue_out,(fun x -> cue_out <- Some x))])
      meta ;
    begin
     match before,cue_in with
       | false,_
       | true,None ->
           before <- false ;
           elapsed <- elapsed ++ Int64.of_int delta_pos
       | true,Some seek_time ->
           self#log#f 3 "Cueing in.";
           before <- false;
           let seek_pos = Int64.to_int seek_time - delta_pos in
           let seeked_pos = source#seek seek_pos in
           if seeked_pos <> seek_pos then
             self#log#f 4 "Seeked %i ticks instead of %i." seeked_pos seek_pos;
           (* Set back original breaks. *)
           Frame.set_breaks buf breaks;
           (* Before pulling new data to fill-in the frame,
            * we need to tick the slave clock otherwise we might get
            * the same old (cached) data. *)
           self#slave_tick ;
           source#get buf ;
           let new_pos = Frame.position buf in
           assert (elapsed = 0L) ;
           elapsed <- Int64.of_int (delta_pos + seeked_pos + new_pos - pos)
    end ;
    match cue_out with
      | Some t when elapsed > t ->
          self#log#f 3 "Cueing out." ;
          let new_pos = Frame.position buf in
          let extra = Int64.to_int (elapsed -- t) in
            if not (Frame.is_partial buf) then self#abort_track ;
            assert (extra < new_pos - pos) ;
            Frame.set_breaks buf
              (Utils.remove_one ((=) new_pos) (Frame.breaks buf)) ;
            Frame.add_break buf (pos + extra) ;
            self#slave_tick ;
            self#reset
      | _ ->
          if Frame.is_partial buf then self#reset
end

let () =
  let kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "cue_cut"
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:"Start track after a cue in point and stop it at cue out point. \
            The cue points are given as metadata, in seconds \
            from the begining of tracks."
    [
      "cue_in_metadata", Lang.string_t, Some (Lang.string "liq_cue_in"),
      Some "Metadata for cue in points.";
      "cue_out_metadata", Lang.string_t, Some (Lang.string "liq_cue_out"),
      Some "Metadata for cue out points.";
      "", Lang.source_t kind, None, None
    ]
    (fun p kind ->
      let m_cue_in = Lang.to_string (Lang.assoc "cue_in_metadata" 1 p) in
      let m_cue_out = Lang.to_string (Lang.assoc "cue_out_metadata" 1 p) in
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      new cue_cut ~kind ~m_cue_in ~m_cue_out s)
