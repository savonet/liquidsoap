(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Frame

type t = Frame.t

let vot ?round x =
  match round with
    | None | Some `Down -> Frame.video_of_main x
    | Some `Up -> Frame.video_of_main (x + Lazy.force Frame.video_rate - 1)

let content ?(field = Frame.Fields.video) b =
  try Frame.get b field with Not_found -> raise Content.Invalid

let data ?field b = Content.Video.get_data (content ?field b)
let position t = vot (position t)
