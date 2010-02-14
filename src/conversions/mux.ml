(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Muxing takes a master and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the master has no channel of that kind, anything for the others.
  *
  * There are several possible modes for muxing:
  *  - Master: the auxiliary source should be infallible, and has to
  *    fill in exactly the data zone that the master produces.
  *    Track information for the aux source is lost.
  *    In the future we might want to require exclusivity on the auxiliary
  *    source because this mode requires doing tricks to it, and sharing
  *    would have funny effects.
  *  - Auxiliary: same with exchanged roles.
  *  - Symmetric: The sources have a symmetric role, we loose all track
  *    information, filling as much as possible. If one of the sources
  *    is not ready anymore, extra data from the other is dropped. *)
type mode = Master | Auxiliary | Symmetric

class mux ~kind ~mode ~master ~master_layer ~aux ~aux_layer mux_content =
object (self)

  inherit Source.operator ~name:"mux" kind [master;aux]

  method stype =
    if master#stype = Source.Infallible && aux#stype = Source.Infallible then
      Source.Infallible
    else
      Source.Fallible

  method is_ready = master#is_ready && aux#is_ready
  method abort_track = master#abort_track ; aux#abort_track
  method remaining =
    let master = master#remaining in
    let aux = aux#remaining in
      if master = -1 && aux = -1 then -1 else min master aux

  method private get_frame frame =
    match mode with
      | Symmetric ->
          (* We get as much info from one source, save the content,
           * repeat the operation for the other, then merge the contents.
           * TODO A more fine-grained loop could minimize dropping
           *
           * We forcibly re-use channels from the current content type,
           * which will trivialize blit in most cases (inside a track,
           * the content doesn't change). *)
          let pos = Frame.position frame in
          let _,content = Frame.content frame pos in
          let get adapt_layer s =
            let breaks = Frame.breaks frame in
              (* Commenting out the following line suffices to disable
               * the optimization. *)
              Frame.set_content_unsafe frame pos (adapt_layer content) ;
              while s#is_ready && Frame.is_partial frame do
                s#get frame
              done ;
              let p,c = Frame.content frame pos in
              let end_pos = Frame.position frame in
                assert (p >= Frame.position frame) ;
                Frame.set_breaks frame breaks ;
                c, end_pos
          in
          let master,end_master = get master_layer master in
          let aux,end_aux = get aux_layer aux in
          let end_pos = min end_master end_aux in
          let new_content = mux_content master aux in
          let dest =
            (* The hope here is that the initial content had the right
             * type so it will be re-used and the blit will be trivial.
             * Otherwise, if the old content didn't reflect the actual
             * content type of our sources, we'll get a new layer and
             * the blit will have an effect. This should rarely happen,
             * so we don't bother to optimize that case too. *)
            Frame.content_of_type frame pos (Frame.type_of_content new_content)
          in
            Frame.blit_content new_content pos dest pos (end_pos-pos) ;
            Frame.add_break frame end_pos
      | _ ->
          failwith "Not yet implemented" (* TODO *)

end

let () =
  let master_t =
    Lang.frame_kind_t
      ~audio:(Lang.univ_t 1)
      ~video:Lang.zero_t
      ~midi:(Lang.univ_t 3)
  in
  let aux_t =
    Lang.frame_kind_t
      ~audio:Lang.zero_t
      ~video:(Lang.univ_t 2)
      ~midi:Lang.zero_t
  in
  let out_t =
    Lang.frame_kind_t
      ~audio:(Lang.univ_t 1)
      ~video:(Lang.univ_t 2)
      ~midi:(Lang.univ_t 3)
  in
    Lang.add_operator "mux_video"
      ~category:Lang.Conversions
      ~descr:"Add video channnels to a stream."
      ~kind:(Lang.Unconstrained out_t)
      [
        "video", Lang.source_t aux_t, None, None ;
        "", Lang.source_t master_t, None, None ;
      ]
      (fun p kind ->
         let master = Lang.to_source (List.assoc "" p) in
         let master_layer c = { c with Frame.video = [||] } in
         let aux = Lang.to_source (List.assoc "video" p) in
         let aux_layer c = { c with Frame.audio = [||] ; midi = [||] } in
         let mux_content master aux =
           { master with Frame.video = aux.Frame.video }
         in
         let mode = Symmetric in
           new mux ~kind ~mode
             ~master ~aux ~master_layer ~aux_layer mux_content)

let () =
  let master_t =
    Lang.frame_kind_t
      ~audio:Lang.zero_t
      ~video:(Lang.univ_t 2)
      ~midi:(Lang.univ_t 3)
  in
  let aux_t =
    Lang.frame_kind_t
      ~audio:(Lang.univ_t 1)
      ~video:Lang.zero_t
      ~midi:Lang.zero_t
  in
  let out_t =
    Lang.frame_kind_t
      ~audio:(Lang.univ_t 1)
      ~video:(Lang.univ_t 2)
      ~midi:(Lang.univ_t 3)
  in
    Lang.add_operator "mux_audio"
      ~category:Lang.Conversions
      ~descr:"Add audio channnels to a stream."
      ~kind:(Lang.Unconstrained out_t)
      [
        "audio", Lang.source_t aux_t, None, None ;
        "", Lang.source_t master_t, None, None ;
      ]
      (fun p kind ->
         let master = Lang.to_source (List.assoc "" p) in
         let master_layer c = { c with Frame.audio = [||] } in
         let aux = Lang.to_source (List.assoc "audio" p) in
         let aux_layer c = { c with Frame.video = [||] ; midi = [||] } in
         let mux_content master aux =
           { master with Frame.audio = aux.Frame.audio }
         in
         let mode = Symmetric in
           new mux ~kind ~mode
             ~master ~aux ~master_layer ~aux_layer mux_content)
