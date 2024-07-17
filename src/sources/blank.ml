(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

class blank duration =
  let nb_frames = int_of_float (duration /. Mixer.Buffer.length) in
object
  inherit source
  method stype = Infallible
  method is_ready = true

  val mutable remaining = nb_frames
  method remaining = remaining

  val mutable must_fail = false
  method abort_track =
    must_fail <- true ;
    remaining <- 0

  method get_frame ab =
    if must_fail then begin
      must_fail <- false ;
      remaining <- nb_frames ;
      Mixer.Buffer.add_break ab (Mixer.Buffer.position ab)
    end else
      let position = Mixer.Buffer.position ab in
      let size = Mixer.Buffer.size in
        remaining <- remaining - 1 ;
        if remaining = 0 then must_fail <- true ;
        Mixer.Buffer.blankify ab position (size-position) ;
        Mixer.Buffer.add_break ab size
end

let _ =
  Lang.add_operator
    "blank"
    ~descr:"This source is not very noisy :)"
    [ "duration", Lang.float_t, Some (Lang.float 0.),
      Some "Duration of blank tracks, default means forever." ]
    (fun p ->
       let d = Lang.to_float (List.assoc "duration" p) in
         ((new blank d):>source))

class empty =
object
  inherit source
  method stype = Fallible
  method is_ready = false
  method remaining = 0
  method abort_track = ()
  method get_frame ab = assert false
end

let empty = ((new empty):>source)
