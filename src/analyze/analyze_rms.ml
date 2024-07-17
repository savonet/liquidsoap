(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class rms ~window_length ~update source =
object (self)
  inherit Source.operator [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  val window = Array.create window_length 0.
  val mutable pos = 0
  val mutable rms = 0.

  method get_frame ab =
    let buf = AFrame.get_float_pcm ab in
    let p0 = AFrame.position ab in
    let p1 = source#get ab ; AFrame.position ab in
      for i = p0 to p1-1 do
        let m =
          Array.fold_left
            (fun m channel -> m +. channel.(i) *. channel.(i))
            0.
            buf
        in
        let m = m /. float (Array.length buf) in
          rms <- rms -. window.(pos) +. m ;
          window.(pos) <- m ;
          pos <- pos + 1 mod window_length
      done ;
      update (sqrt (rms /. float window_length))
end
