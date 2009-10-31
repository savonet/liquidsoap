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

include Frame

let vot = Frame.video_of_master
let tov = Frame.master_of_video

let size _ = vot (Lazy.force size)
let position t = vot (position t)
let add_break t i = add_break t (tov i)

let set_metadata t i m = set_metadata t (tov i) m
let get_metadata t i = get_metadata t (tov i)

let get_rgb b pos =
  let content = content_of_type b (tov pos) (type_of_kind b.content_kind) in
    content.video
