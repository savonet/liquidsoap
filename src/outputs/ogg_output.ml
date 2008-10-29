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

 (** Abstract class for ogg outputs. *)

class virtual base = 
object(self)

  val virtual mutable encoder : Ogg_encoder.t option

  method virtual id : string

  method output_start = 
    encoder <- Some (Ogg_encoder.create self#id)

  method end_of_stream = 
    let enc = Utils.get_some encoder in
    Ogg_encoder.end_of_stream enc;
    Ogg_encoder.flush enc

  method output_stop =
    let enc = Utils.get_some encoder in
    assert(!(enc.Ogg_encoder.eos));
    encoder <- None
end
