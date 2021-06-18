(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** Utility for operators that need to control child source clocks. *)

let finalise_child_clock child_clock source =
  Clock.forget source#clock child_clock

class virtual base children_val =
  let children = List.map Lang.to_source children_val in
  object (self)
    initializer
    List.iter
      (fun c ->
        if (Lang.to_source c)#self_sync <> (`Static, false) then
          raise
            (Lang_errors.Invalid_value
               ( c,
                 "This source may control its own latency and cannot be used \
                  with this operator." )))
      children_val

    val mutable child_clock = None

    val mutable needs_tick = true

    method virtual id : string

    method virtual clock : Source.clock_variable

    method private child_clock = Option.get child_clock

    method private set_clock =
      child_clock <-
        Some
          (Clock.create_known
             (new Clock.clock ~start:false (Printf.sprintf "%s.child" self#id)));

      Clock.unify self#clock
        (Clock.create_unknown ~sources:[] ~sub_clocks:[self#child_clock]);

      List.iter (fun c -> Clock.unify self#child_clock c#clock) children;

      Gc.finalise (finalise_child_clock self#child_clock) self

    method private child_tick =
      (Clock.get self#child_clock)#end_tick;
      needs_tick <- false

    method after_output =
      if needs_tick then self#child_tick;
      needs_tick <- true
  end
