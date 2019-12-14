(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

class rms ~kind ~window_length ~update source =
  object
    inherit Source.operator ~name:"rms" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method remaining = source#remaining

    method self_sync = source#self_sync

    val window = Array.make window_length 0.

    val mutable pos = 0

    val mutable rms = 0.

    method get_frame ab =
      let p0 = AFrame.position ab in
      let p1 = source#get ab ; AFrame.position ab in
      let buf = AFrame.content ab p0 in
      for i = p0 to p1 - 1 do
        let m =
          Array.fold_left
            (fun m channel ->
              let x = channel.{i} in
              m +. (x *. x))
            0. buf
        in
        let m = m /. float (Array.length buf) in
        rms <- rms -. window.(pos) +. m ;
        window.(pos) <- m ;
        pos <- pos + (1 mod window_length)
      done ;
      update (sqrt (rms /. float window_length))
  end
