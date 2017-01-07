(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

class clip ~kind (source:source) =
object
  inherit operator ~name:"clip" kind [source]

  method stype = source#stype
  method remaining = source#remaining
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.content buf offset in
      let position = AFrame.position buf in
      Audio.clip b offset (position - offset)
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "clip"
  [ "", Lang.source_t k, None, None ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Clip sound."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new clip ~kind src)
