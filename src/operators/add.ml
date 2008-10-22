(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

open Source

let max a b = if b = -1 || a = -1 then -1 else max a b

let tile_pos n =
  let vert l x y x' y' =
    if l = 0 then [||] else
      let dx = (x' - x) / l in
      let x = ref (x-dx) in
        Array.init l (fun i -> x := !x + dx; !x, y, dx, (y'-y))
  in
  let x' = Fmt.video_width () in
  let y' = Fmt.video_height () in
  let horiz m n =
    Array.append (vert m 0 0 x' (y'/2)) (vert n 0 (y'/2) x' y')
  in
    horiz (n/2) (n-n/2)

class add ?(renorm=true) (sources: (int*source) list) video_init video_loop =
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

  val tmp = Frame.make ()

  method get_frame buf =

    (* Compute the list of ready sources, and their total weight *)
    let weight,sources =
      List.fold_left
        (fun (t,l) (w,s) -> if s#is_ready then (w+t),((w,s)::l) else t,l)
        (0,[]) sources
    in
    let weight = float weight in

    (* Sum contributions *)
    let offset = AFrame.position buf in
    let voffset = VFrame.position buf in
    let first = ref true in
    let nbuf = ref 0 in
    let end_offset =
      List.fold_left
        (fun end_offset (w,s) ->
           let buffer =
             if !first then buf else begin
               AFrame.clear tmp ;
               AFrame.set_breaks tmp [offset] ;
               tmp
             end
           in
           let c = (float w)/.weight in

             if List.length sources = 1 then
               (* if s#is_ready SHOULDN'T BE NEEDED *)
               s#get buffer
             else
               (* If there is more than one source we fill greedily. *)
               while AFrame.is_partial buffer && s#is_ready do
                 s#get buffer
               done ;

             let already = AFrame.position buffer in
             let valready = VFrame.position buffer in
               if c<>1. && renorm then
                 Float_pcm.multiply
                   (AFrame.get_float_pcm buffer) offset (already-offset) c ;
               if not !first then
                 (
                   Float_pcm.add
                     (AFrame.get_float_pcm buf) offset
                     (AFrame.get_float_pcm tmp) offset
                     (already-offset) ;
                   incr nbuf;
                   let vbuf = VFrame.get_rgb buf in
                   let vtmp = VFrame.get_rgb tmp in
                     for c = 0 to Array.length vbuf - 1 do
                       for i = voffset to valready - 1 do
                         video_loop !nbuf vbuf.(c).(i) vtmp.(c).(i)
                       done
                     done
                 )
               else
                 (
                   let vbuf = VFrame.get_rgb buf in
                     for c = 0 to Array.length vbuf - 1 do
                       for i = voffset to valready - 1 do
                         video_init vbuf.(c).(i)
                       done
                     done
                 );
               first := false ;
               max end_offset already)
        offset sources
    in
      (* If the other sources have filled more than the first one we must
       * add one mark in the Mixer.Buffer. *)
      if end_offset > AFrame.position buf then
        AFrame.add_break buf end_offset

end

let () =
  Lang.add_operator "add"
    ~category:Lang.SoundProcessing
    ~descr:"Mix sources, with optional normalization. \
           Only relay metadata from the first source that is effectively \
           summed."
    [ "normalize", Lang.bool_t, Some (Lang.bool true), None ;
      "weights", Lang.list_t Lang.int_t, Some (Lang.list []),
      Some "Relative weight of the sources in the sum. \
            The empty list stands for the homogeneous distribution." ;
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
         ((new add
             ~renorm
             (List.map2 (fun w s -> (w,s)) weights sources)
             (fun _ -> ())
             (fun _ buf tmp -> RGB.add buf tmp)
         ):>source))

let () =
  Lang.add_operator "video.tile"
    ~category:Lang.VideoProcessing
    ~descr:"Tile sources (same as add but produces tiles of videos)."
    [
      "normalize", Lang.bool_t, Some (Lang.bool true), None ;
      "weights", Lang.list_t Lang.int_t, Some (Lang.list []),
      Some "Relative weight of the sources in the sum. \
            The empty list stands for the homogeneous distribution." ;
      "proportional", Lang.bool_t, Some (Lang.bool true), Some "Scale preserving the proportions.";
      "", Lang.list_t Lang.source_t, None, None
    ]
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
       let proportional = Lang.to_bool (List.assoc "proportional" p) in
       let tp = tile_pos (List.length sources) in
       let video_loop n buf tmp =
         let x, y, w, h = tp.(n) in
         let x, y, w, h =
           if proportional then
             (
               let sw, sh = RGB.get_width buf, RGB.get_height buf in
                 if w * sh < sw * h then
                   let h' = sh * w / sw in
                     x, y+(h-h')/2, w, h'
                 else
                   let w' = sw * h / sh in
                     x+(w-w')/2, y, w', h
             )
           else
             x, y, w, h
         in
           RGB.blit_off ~blank:false tmp ~w ~h buf x y
       in
       let video_init buf = video_loop 0 buf buf in
         if List.length weights <> List.length sources then
           raise
             (Lang.Invalid_value
                ((List.assoc "weights" p),
                 "there should be as many weights as sources")) ;
         ((new add
             ~renorm
             (List.map2 (fun w s -> (w,s)) weights sources)
             video_init
             video_loop
         ):>source))
