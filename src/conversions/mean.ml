(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

class mean ~kind (source:source) =
  let ctype = Frame.type_of_kind kind in
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = AFrame.position buf in
    let buffer = source#get buf ; AFrame.content buf offset in
    let dst = Frame.content_of_type buf (Frame.master_of_audio offset) ctype in
    let dst = dst.Frame.audio.(0) in
      for i = offset to AFrame.position buf - 1 do
        dst.(i) <- 
          Array.fold_left (fun m b -> m +. b.(i)) 0. buffer
          /. float (Array.length buffer)
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
