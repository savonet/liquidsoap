(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** Convert a value into a channel number, checking that it actually exists. *)
let to_chan v =
  Lang.to_int v

(** Convert delta-times to ticks. *)
let ticks_of_delta division tempo delta =
  match division with
    | Midi.Ticks_per_quarter tpq ->
        (* These computations sometimes overflow on 32 bits. *)
        let tpq = Int64.of_int tpq in
        let tempo = Int64.of_int tempo in
        let tps = Int64.of_int (Lazy.force Frame.size) in
        let ten = Int64.of_int 1000000 in
        let delta = Int64.of_int delta in
        let ( * ) = Int64.mul in
        let ( / ) = Int64.div in
          Int64.to_int ((((delta * tempo) / tpq) * tps) / ten)
    | Midi.SMPTE (fps,res) ->
        (delta * Lazy.force Frame.size) / (fps * res)
