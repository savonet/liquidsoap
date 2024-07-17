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

(** Substract one source from another. *)

(* TODO: cleanly handle tracks of with different sizes *)

open Source

class substract (y:source) (x:source) =
object (self)
  inherit operator [y; x] as super

  method stype =
    if x#stype = Infallible && y#stype = Infallible then
      Infallible
    else
      Fallible

  method remaining =
    min x#remaining y#remaining

  method abort_track =
    x#abort_track;
    y#abort_track

  method is_ready =
    x#is_ready && y#is_ready

  val tmp = Fmt.create_frame ()

  method get_frame buf =
    (* Sum contributions *)
    let offset = AFrame.position buf in
      y#get buf;
      x#get tmp;
      let position = AFrame.position buf in
        AFrame.substract buf offset tmp offset (position - offset)
end

let () =
  Lang.add_operator "substract"
    ~category:Lang.SoundProcessing
    ~descr:("Compute the difference y-x of two sources y and x.")
    [ "", Lang.source_t, None, Some "y";
      "", Lang.source_t, None, Some "x"; ]
    (fun p ->
       let y = Lang.to_source (Lang.assoc "" 1 p) in
       let x = Lang.to_source (Lang.assoc "" 2 p) in
         ((new substract y x):>source))
