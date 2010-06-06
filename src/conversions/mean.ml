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

open Source

class mean ~kind source =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method private get_frame frame =
    let start = Frame.position frame in
    let src =
      source#get frame ;
      let layer_end,src = Frame.content frame start in
        assert (layer_end = Lazy.force Frame.size) ;
        src
    in
    let len = Frame.position frame - start in
    let src_type = Frame.type_of_content src in
    let dst_type = { src_type with Frame.audio = 1 } in
    let dst = Frame.content_of_type frame start dst_type in
      (* Copy midi and video channels. This should be avoided eventually. *)
      for i = 0 to Array.length src.Frame.video - 1 do
        let (!) = Frame.video_of_master in
          for j = 0 to !len-1 do
            RGB.blit_fast
              src.Frame.video.(i).(!start+j)
              dst.Frame.video.(i).(!start+j)
          done
      done ;
      for i = 0 to Array.length src.Frame.midi - 1 do
        Midi.blit
          src.Frame.midi.(i) start
          dst.Frame.midi.(i) start
          len
      done ;
      (* Fill the unique audio channel. *)
      let (!) = Frame.audio_of_master in
      let channels = float (Array.length dst.Frame.audio) in
        if channels>0. then
          for i = !start to !(start+len) - 1 do
            dst.Frame.audio.(0).(i) <-
              Array.fold_left (fun m b -> m +. b.(i)) 0. src.Frame.audio
              /. channels
          done
end

let () =
  let in_kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  let out_kind =
    let { Frame.audio=a;video=v;midi=m } = Lang.of_frame_kind_t in_kind in
      Lang.frame_kind_t (Lang.succ_t Lang.zero_t) v m
  in
    Lang.add_operator "mean"
      [ "", Lang.source_t in_kind, None, None ]
      ~kind:(Lang.Unconstrained out_kind)
      ~category:Lang.SoundProcessing
      ~descr:"Transform all audio channels into one by taking their mean."
      (fun p kind ->
         let s = Lang.to_source (Lang.assoc "" 1 p) in
           new mean ~kind s)
