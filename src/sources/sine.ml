(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

(** Generate a sine *)

open Source

class sine ~kind freq duration =
  let period = int_of_float (float (Lazy.force Frame.audio_rate) /. freq) in
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)

  inherit Synthesized.source ~name:"sine" kind duration

  val mutable pos = 0

  method private synthesize frame off len =
    let off = Frame.audio_of_master off in
    let len = Frame.audio_of_master len in
    let b = AFrame.content_of_type ~channels frame off in
    let write i x =
      for c = 0 to Array.length b - 1 do
        b.(c).(i) <- x
      done
    in
      for i = off to off+len-1 do
        write i (sin (float pos /. float period *. 2. *. 3.1416));
        pos <- pos + 1;
        if pos >= period then pos <- pos - period;
      done

end

let () =
  Lang.add_operator "sine"
    ~category:Lang.Input
    ~descr:"Generate a sine wave."
    ~kind:Lang.audio_any
    [ "duration", Lang.float_t, Some (Lang.float 0.), None ;
      "", Lang.float_t, Some (Lang.float 440.), Some "Frequency of the sine." ]
    (fun p kind ->
       (new sine ~kind
          (Lang.to_float (List.assoc "" p))
          (Lang.to_float (List.assoc "duration" p)) :> source))
