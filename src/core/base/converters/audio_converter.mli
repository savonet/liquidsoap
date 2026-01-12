(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** External audio conversion utilities *)

(* TODO: is it the right place for this ? *)
val audio_conf : Dtools.Conf.ut
val converter_conf : Dtools.Conf.ut

module Samplerate : sig
  exception Invalid_data

  type converter =
    float -> Content_audio.data -> int -> int -> Content_audio.data * int * int

  type converter_plug = int -> converter
  type t

  val samplerate_conf : Dtools.Conf.ut
  val converters : converter_plug Plug.t

  (** [create chan_nb] creates a converter. *)
  val create : int -> t

  (** [resample converter ratio data]: converts input data at given ratio.
      Raises [Invalid_data] if number of channels do not match the number passed
      at [create]. *)
  val resample :
    t ->
    float ->
    Content_audio.data ->
    int ->
    int ->
    Content_audio.data * int * int
end

module Channel_layout : sig
  exception Unsupported
  exception Invalid_data

  type layout = [ `Mono | `Stereo | `Five_point_one ]
  type converter = layout -> layout -> Content_audio.data -> Content_audio.data
  type t

  val channels_of_layout : layout -> int
  val layout_of_channels : int -> layout
  val channel_layout_conf : Dtools.Conf.ut
  val converters : converter Plug.t

  (** [create src dst] creates a converter. *)
  val create : layout -> layout -> t

  (** [convert converter data]: converts input data to the destination layout.
      Raises [Invalid_data] if input layout does not match the layout passed as
      [create]. *)
  val convert : t -> Content_audio.data -> Content_audio.data
end
