(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

(** Utility for operators that need to control child source clocks. See [clock.mli]
    for a more detailed description. *)

let finalise_child_clock child_clock source =
  Clock.forget source#clock child_clock

class virtual base ?(create_known_clock = true) ~check_self_sync children_val =
  let children = List.map Lang.to_source children_val in
  let create_child_clock id =
    if create_known_clock then
      Clock.create_known
        (Clock.clock ~start:false (Printf.sprintf "%s.child" id))
    else Clock.create_unknown ~start:false ~sources:[] ~sub_clocks:[] ()
  in
  object (self)
    initializer
      if check_self_sync then
        List.iter
          (fun c ->
            if (Lang.to_source c)#self_sync <> (`Static, false) then
              raise
                (Error.Invalid_value
                   ( c,
                     "This source may control its own latency and cannot be \
                      used with this operator." )))
          children_val

    val mutable child_clock = None

    (* If [true] during [#after_output], issue a [#end_tick] call
       on the child clock, which makes it perform a whole streaming
       loop. *)
    val mutable needs_tick = true
    method virtual id : string
    method virtual clock : Source.clock_variable
    method private child_clock = Option.get child_clock
    method virtual pos : Pos.Option.t

    method private set_clock =
      child_clock <- Some (create_child_clock self#id);

      Clock.unify ~pos:self#pos self#clock
        (Clock.create_unknown ~sources:[] ~sub_clocks:[self#child_clock] ());

      List.iter
        (fun c -> Clock.unify ~pos:c#pos self#child_clock c#clock)
        children;

      Gc.finalise (finalise_child_clock self#child_clock) self

    method private child_tick =
      (Clock.get self#child_clock)#end_tick;
      needs_tick <- false

    (* This always set [need_tick] to true. If the source is not
       [#is_ready], [#after_output] is called during a clock tick,
       which means that the children clock is _always_ animated by the
       main clock when the source becomes unavailable. Otherwise, we
       expect the source to make a decision about executing a child clock
       tick as part of its [#get_frame] implementation. See [cross.ml] or
       [soundtouch.ml] as examples. *)
    method virtual on_wake_up : (unit -> unit) -> unit

    method private child_before_output =
      needs_tick <- true;
      let clock = Source.Clock_variables.get self#clock in
      clock#on_after_output (fun () -> self#child_after_output)

    method private child_on_output fn =
      let clock = Source.Clock_variables.get self#child_clock in
      clock#on_output fn;
      self#child_tick

    method private child_after_output =
      if needs_tick then self#child_tick;
      let clock = Source.Clock_variables.get self#clock in
      clock#on_before_output (fun () -> self#child_before_output)

    initializer
      self#on_wake_up (fun () ->
          let clock = Source.Clock_variables.get self#clock in
          clock#on_before_output (fun () -> self#child_before_output))
  end
