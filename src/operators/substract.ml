(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

(** Substract one source from another. *)

(* TODO: cleanly handle tracks of with different sizes *)

open Source

class substract ~kind (y:source) (x:source) =
object (self)
  inherit operator kind [y; x] as super

  method stype =
    if x#stype = Infallible && y#stype = Infallible then
      Infallible
    else
      Fallible

  method remaining =
    let x = x#remaining in
    let y = y#remaining in
      if x<0 then y else
        if y<0 then x else
          min x y

  method abort_track =
    x#abort_track;
    y#abort_track

  method is_ready =
    x#is_ready && y#is_ready

  val tmp = Frame.create kind

  method private get_frame buf =
    (* Sum contributions *)
    let offset = AFrame.position buf in
      y#get buf;
      x#get tmp;
      let position = AFrame.position buf in
        AFrame.substract buf offset tmp offset (position - offset)
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "substract"
    ~category:Lang.SoundProcessing
    ~descr:"Compute the difference y-x of two sources y and x. \
            The metadata and breaks from x are lost."
    ~flags:[Lang.Hidden] (* TODO handle end of tracks, cf. add() *)
    [ "", Lang.source_t k, None, Some "y";
      "", Lang.source_t k, None, Some "x"; ]
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let y = Lang.to_source (Lang.assoc "" 1 p) in
       let x = Lang.to_source (Lang.assoc "" 2 p) in
         new substract ~kind y x)
