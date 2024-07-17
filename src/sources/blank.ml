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
  let nb_samples =
    if duration = 0. then -1 else Fmt.samples_of_seconds duration
  in
object
  inherit source
  method stype = Infallible
  method is_ready = true

  (** Remaining number of samples, -1 for infinity. *)
  val mutable remaining = nb_samples
  method remaining = Fmt.ticks_of_samples remaining

  method abort_track = remaining <- 0

  method get_frame ab =
    let position = AFrame.position ab in
    let size =
      if remaining < 0 then
        AFrame.size ab
      else
        min (position + remaining) (AFrame.size ab)
    in
      Float_pcm.blankify (AFrame.get_float_pcm ab) position (size-position) ;
      AFrame.add_break ab size ;
      if remaining = 0 then
        remaining <- nb_samples
      else if remaining > 0 then
        remaining <- remaining - size + position
end

let () =
  Lang.add_operator
    "blank"
    ~category:Lang.Input
    ~descr:"Produce silence."
    [ "duration", Lang.float_t, Some (Lang.float 0.),
      Some "Duration of blank tracks in seconds, default means forever." ]
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
