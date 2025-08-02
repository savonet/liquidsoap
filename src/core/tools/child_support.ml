(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Utility for operators that need to control child source clocks. See
    [clock.mli] for a more detailed description. *)

class virtual base ~check_self_sync children_val =
  let children = List.map Lang.to_source children_val in
  object (self)
    initializer
      if check_self_sync then
        List.iter
          (fun c ->
            if (Lang.to_source c)#self_sync <> (`Static, None) then
              raise
                (Error.Invalid_value
                   ( c,
                     "This source may control its own latency and cannot be \
                      used with this operator." )))
          children_val

    method virtual id : string
    method virtual clock : Clock.t
    method virtual pos : Pos.Option.t
    method virtual on_before_streaming_cycle : (unit -> unit) -> unit
    val mutable child_clock = None

    initializer
      child_clock <-
        Some
          (Clock.create_sub_clock
             ~id:(Clock.id self#clock ^ ".child")
             self#clock);

      self#on_before_streaming_cycle (fun () ->
          if not (Clock.started self#child_clock) then
            Clock.start self#child_clock;
          Clock.activate_pending_sources self#child_clock)

    method child_clock =
      match child_clock with Some c -> c | None -> assert false

    method virtual log : Log.t

    initializer
      List.iter
        (fun s -> Clock.unify ~pos:self#pos self#child_clock s#clock)
        children

    method child_tick = Clock.tick self#child_clock

    method on_child_tick fn =
      Clock.on_tick self#child_clock fn;
      self#child_tick
  end
