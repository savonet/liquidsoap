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

val conf : Dtools.Conf.ut
val conf_max_latency : float Dtools.Conf.t

(** The core loop, reading from the scheduler and controlling output. *)
val start : unit -> unit
val running : unit -> bool

(** Set [shutdown] to false to stop the loop *)
val shutdown : bool ref

(** Forces the shutdown of the scheduler, in case the root's thread died. *)
val force_sleep : unit -> unit

val uptime : unit -> float
val delay : unit -> float
