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

type t = int*int*RGB.t

let new_converter w h =
  let f = RGB.create w h in
  (w,h,f)

let yuv_to_rgb (w,h,g) f yuv =
  if w <> Fmt.video_width () &&
     h <> Fmt.video_height () then
    begin
      RGB.of_YUV420 yuv g;
      RGB.proportional_scale f g
    end
  else
    RGB.of_YUV420 yuv f

let rgb_to_yuv (w,h,g) f yuv =
  if w <> Fmt.video_width () &&
     h <> Fmt.video_height () then
    begin
      RGB.proportional_scale g f;
      RGB.to_YUV420 g yuv
    end
  else
    RGB.to_YUV420 f yuv
