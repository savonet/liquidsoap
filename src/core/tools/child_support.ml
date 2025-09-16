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

let conf_child_support =
  Dtools.Conf.void
    ~p:(Clock.conf_clock#plug "child")
    "Settings related to child clocks, i.e. clocks animating sources depending \
     on main source such as `crossfade`, `accelerate`, etc."

let conf_leak_warning =
  Dtools.Conf.float
    ~p:(conf_child_support#plug "leak_warning")
    ~d:60.
    "Amount of buffered time (in seconds) after which a warning is issued that \
     there might be a clock-induced data leak or accumulation."

class child_output src =
  object (self)
    inherit Source.operator ~name:(Printf.sprintf "%s.child" src#id) [src]
    method! source_type = `Output (self :> Source.active)
    method reset = Generator.clear self#buffer
    val mutable generator = None
    method generator = Option.get generator
    method set_generator gen = generator <- Some gen
    val mutable process_frame = fun gen frame -> Generator.append gen frame
    method set_process_frame fn = process_frame <- fn

    method output =
      if self#is_ready then (
        let gen = self#generator in
        let len = Generator.length gen in
        process_frame gen self#get_frame;
        let new_len = Generator.length gen in
        let leak_warning = Frame.main_of_seconds conf_leak_warning#get in
        if len / leak_warning < new_len / leak_warning then
          self#log#important
            "Generator for source %s has over %.02f of content. There might be \
             a data leak or accumulation due to imbalanced child clock usage."
            self#id
            (Frame.seconds_of_main new_len))

    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method seek_source = src#seek_source
    method private generate_frame = src#get_frame
  end

class virtual base ?child_frame_type ~check_self_sync child_val =
  let child = new child_output (Lang.to_source child_val) in
  object (self)
    method virtual clock : Clock.t
    method virtual pos : Pos.Option.t
    method virtual frame_type : Type.t
    method virtual on_wake_up : (unit -> unit) -> unit
    method child_clock_controller = None
    method virtual buffer : Generator.t
    method child = child
    val mutable child_clock = None

    initializer
      if check_self_sync then
        if child#self_sync <> (`Static, None) then
          raise
            (Error.Invalid_value
               ( child_val,
                 "This source may control its own latency and cannot be used \
                  with this operator." ));
      let c =
        Clock.create_sub_clock ?controller:self#child_clock_controller
          ~id:(Clock.id self#clock) self#clock
      in
      Clock.unify ~pos:self#pos c child#clock;
      let child_frame_type =
        Option.value ~default:self#frame_type child_frame_type
      in
      Typing.(child#frame_type <: child_frame_type);
      Typing.((Lang.to_source child_val)#frame_type <: child#frame_type);
      child_clock <- Some c;
      self#on_wake_up (fun () -> child#set_generator self#buffer)

    method child_clock =
      match child_clock with Some c -> c | None -> assert false

    method private child_tick = Clock.tick self#child_clock

    method child_get_frame ?(get_partial_frame = fun f -> f) () =
      let gen = self#buffer in
      let frame_size = Lazy.force Frame.size in
      while child#is_ready && Generator.length gen < frame_size do
        self#child_tick
      done;
      let frame =
        get_partial_frame (Generator.slice ~peek:true gen frame_size)
      in
      Generator.truncate gen (Frame.position frame);
      frame
  end

class producer ?stack ?child_frame_type ~check_self_sync ~name child_val =
  object (self)
    inherit Source.source ?stack ~name ()
    inherit base ?child_frame_type ~check_self_sync child_val
    method self_sync = self#child#self_sync
    method fallible = self#child#fallible
    method seek_source = (self :> Source.source)

    method! seek len =
      let gen_len = min (Generator.length self#buffer) len in
      Generator.truncate self#buffer gen_len;
      if gen_len < len then gen_len + self#child#seek (len - gen_len)
      else gen_len

    method remaining =
      match (Generator.remaining self#buffer, self#child#remaining) with
        | -1, -1 -> -1
        | -1, v -> v + Generator.length self#buffer
        | v, -1 -> v
        | v, v' -> v + v'

    method private can_generate_frame =
      0 < Generator.length self#buffer || self#child#is_ready

    method private generate_frame = self#child_get_frame ()

    method abort_track =
      Generator.clear self#buffer;
      Generator.add_track_mark self#buffer;
      self#child#abort_track
  end
