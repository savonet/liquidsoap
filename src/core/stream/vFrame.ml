(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let tov = Frame.main_of_video

let vot ?round x =
  match round with
    | None | Some `Down -> Frame.video_of_main x
    | Some `Up -> Frame.video_of_main (x + Multicore.force Frame.video_rate - 1)

let content b = try Frame.video b with Not_found -> raise Content.Invalid
let data b = Content.Video.get_data (content b)
let size _ = vot (Multicore.force size)
let next_sample_position t = vot ~round:`Up (Frame.position t)
let add_break t i = add_break t (tov i)
let is_partial t = is_partial t
let position t = vot (position t)

let get_content frame source =
  let p0 = Frame.position frame in
  let p1 =
    source#get frame;
    Frame.position frame
  in
  let v0 = vot ~round:`Up p0 in
  let v1 = vot ~round:`Down p1 in
  if v0 < v1 then Some (content frame, v0, v1 - v0) else None
