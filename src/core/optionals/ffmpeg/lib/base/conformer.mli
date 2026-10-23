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

(** Raw content conformers (content.md §9, §14), bound to a track's format: a
    frame whose parameters are compatible with the format's is merged into it
    and passed on, so the first frame decides what the format left open; a later
    frame in another format is converted to the format's (resampled, or fitted),
    never ending the track. Every producer of raw content delivers through one.
    [convert] raises [Avutil.Error] or [Failure] on a frame the conversion
    rejects, an item fault of the producer. *)

module Audio : sig
  type t

  val create : log:Log.t -> Content.format -> t

  (** [deliver] receives each frame with the time base of its timestamps. *)
  val convert :
    t ->
    time_base:Avutil.rational ->
    (Avutil.rational -> Avutil.audio Avutil.frame -> unit) ->
    Avutil.audio Avutil.frame ->
    unit

  (** Drains the resampler, when one converts. *)
  val flush :
    t -> (Avutil.rational -> Avutil.audio Avutil.frame -> unit) -> unit
end

module Video : sig
  type t

  val create : log:Log.t -> Content.format -> t

  val convert :
    t ->
    (Avutil.video Avutil.frame -> unit) ->
    Avutil.video Avutil.frame ->
    unit
end
