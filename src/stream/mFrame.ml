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

include Frame

let size () = Lazy.force Frame.size

let position t = position t

let content b pos =
  let stop,content = content b pos in
    assert (stop = size ()) ;
    content.midi

let content_of_type ~channels b pos =
  let ctype = { Frame. audio = 0 ; video = 0 ; midi = channels } in
  let content = Frame.content_of_type b pos ctype in
    content.Frame.midi

let set_events f e = ()

(* TODO this is ugly *)
let clear f =
  Array.iter (fun t -> t := []) (content f 0)
