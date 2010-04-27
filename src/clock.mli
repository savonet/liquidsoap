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

class clock : string -> Source.clock

class wallclock : ?sync:bool -> string -> clock

val running : unit -> bool
val set_running : unit -> unit

val collect_after : (unit -> 'a) -> 'a
val stop : unit -> unit

type clock_variable = Source.clock_variable
val to_string      : clock_variable -> string
val create_unknown : sources:(Source.active_source list) ->
                     sub_clocks:(clock_variable list) ->
                     clock_variable
val create_known : clock -> clock_variable
val unify : clock_variable -> clock_variable -> unit
val get : clock_variable -> Source.clock
