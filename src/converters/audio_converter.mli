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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** External audio conversion utilities *)

(* TODO: is it the right place for this ? *)
val audio_conf : Dtools.Conf.ut
val converter_conf : Dtools.Conf.ut

module Samplerate : sig
  exception Invalid_data

  type converter = float -> Frame.audio_t -> Frame.audio_t
  type converter_plug = unit -> converter
  type t

  val samplerate_conf : Dtools.Conf.ut
  val converters : converter_plug Plug.plug

  (** [create chan_nb] creates a converter. *)
  val create : int -> t

  (** [resample converter ratio data]: converts input data at given
      ratio. Raises [Invalid_data] if number of channels do not match the number
      passed at [create]. *)
  val resample : t -> float -> Frame.audio_t array -> Frame.audio_t array
end
