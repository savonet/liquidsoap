(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Play multiple sources at the same time, and perform weighted mix *)

open Types

let max a b = if b = -1 || a = -1 then -1 else max a b

class add ?(renorm=true) (sources: (int*source) list) =
object (self)
  inherit operator (List.map snd sources) as super

  (* We want the sources at the beginning of the list to
   * have their metadatas copied to the output stream, so direction
   * matters. The algo in get_frame reverses the list in the fold_left. *)
  val sources = List.rev sources

  method stype =
    if List.exists (fun (_,s) -> s#stype = Infallible) sources then
      Infallible
    else
      Fallible

  method remaining =
    List.fold_left max 0
      (List.map
         (fun (_,s) -> s#remaining)
         (List.filter (fun (_,s) -> s#is_ready) sources))

  method abort_track = List.iter (fun (_,s) -> s#abort_track) sources

  method is_ready = List.exists (fun (_,s) -> s#is_ready) sources

  (* We fill the buffer as much as possible, removing any break.
   * Every ready source is asked for as much data as possible, by asking
   * it to fill the intermediate [tmp] buffer. Then that
   * data is added to the main buffer [buf], with some amplitude change.
   * At the very end, if no source has been able to fill [buf] completely,
   * a break is set.
   * The first source is asked to write directly on [buf], which avoids
   * copies when only one source is available, which happens for most of the
   * frames.
   * Only the first source's metadatas/breaks are kept.
   * Normally, all active sources are proposed to fill the buffer as much as
   * wanted, even if they end a track -- this is quite needed. There is an
   * exception when there is only one active source, then the end of tracks
   * are not hidden anymore, which is happy for transitions, for example. *)

  val tmp = Mixer.Buffer.create ()

  method get_frame buf =

    (* Compute the list of ready sources, and their total weight *)
    let weight,sources =
      List.fold_left
        (fun (t,l) (w,s) -> if s#is_ready then (w+t),((w,s)::l) else t,l)
        (0,[]) sources
    in
    let weight = float weight in

    (* Sum contributions *)
    let offset = Mixer.Buffer.position buf in
    let first = ref true in
    let end_offset =
      Mixer.Buffer.blankify buf offset (Mixer.Buffer.size-offset) ;
      List.fold_left
        (fun end_offset (w,s) ->
           let buffer =
             if !first then buf else begin
               Mixer.Buffer.free tmp ;
               Mixer.Buffer.set_breaks tmp [offset] ;
               tmp
             end
           in
           let c = (float w)/.weight in
           let b = ref (0::(Mixer.Buffer.breaks buffer)) in

             (* Fill until full, but take care of fallible sources *)
             while Mixer.Buffer.is_partial buffer &&
                   !b <> Mixer.Buffer.breaks buffer do
               assert (not (Mixer.Buffer.ill ~base:offset buffer)) ;
               b := Mixer.Buffer.breaks buffer ;
               if s#is_ready then s#get buffer ;
               if List.length sources = 1 then b := Mixer.Buffer.breaks buffer
             done ;

             let already = Mixer.Buffer.position buffer in
               if renorm then
                 Mixer.Buffer.change_volume buffer offset (already-offset) c ;
               if not !first then
                 Mixer.Buffer.add buf offset tmp offset (already-offset) ;
               first := false ;
               max end_offset already)
        offset sources
    in
      (* If the other sources have filled more than the first one we must
       * add one mark in the Mixer.Buffer. *)
      if end_offset > Mixer.Buffer.position buf then
        Mixer.Buffer.add_break buf end_offset

end

let register_add =

  Lang.add_operator "add"
    ~descr:"Add sources, with normalization"
    [ "normalize", Lang.bool_t, Some (Lang.bool true), None ;
      "weights", Lang.list_t Lang.int_t, Some (Lang.list []),
      Some ("Relative weight of the sources in the sum. " ^
            "The empty list stands for the homogeneous distribution.") ;
      "", Lang.list_t Lang.source_t, None, None ]
    (fun p ->
       let sources = Lang.to_source_list (List.assoc "" p) in
       let weights =
         List.map Lang.to_int (Lang.to_list (List.assoc "weights" p))
       in
       let weights =
         if weights = [] then
           Utils.make_list (List.length sources) 1
         else
           weights
       in
       let renorm = Lang.to_bool (List.assoc "normalize" p) in
         if List.length weights <> List.length sources then
           raise
             (Lang.Invalid_value
                ((List.assoc "weights" p),
                 "there should be as many weights as sources")) ;
         ((new add ~renorm
             (List.map2 (fun w s -> (w,s)) weights sources)):>source))
