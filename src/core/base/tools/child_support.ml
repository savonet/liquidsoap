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

(** Utility for operators that animate a child source in a dedicated clock, e.g.
    to consume data at a different pace than they produce it ([crossfade],
    [stretch], inline encoders, etc.).

    The child source is wrapped in a [child_output], an output living in the
    child clock: each tick of the child clock makes it process the child's frame
    into a generator (by default, appending it). Operators read from the
    generator and only tick the child clock when they need more data. When
    several operators share a child clock, a tick issued by one of them fills
    the buffers of all of them: no data is lost and each reader consumes at its
    own pace. A warning is issued when a buffer accumulates, indicating
    imbalanced child clock usage. *)

type write_payload = [ `Frame of Frame.t | `Flush ]

let conf_child_support =
  Dtools.Conf.void
    ~p:(Clock.conf_clock#plug "child")
    "Settings related to child clocks, i.e. clocks animating sources depending \
     on a main source such as `crossfade`, `accelerate`, etc."

let conf_leak_warning =
  Dtools.Conf.float
    ~p:(conf_child_support#plug "leak_warning")
    ~d:60.
    "Amount of buffered time (in seconds) after which a warning is issued that \
     there might be a clock-induced data leak or accumulation."

(** Output wrapping a child source: on each tick of its (child) clock, it
    processes the child's frame into the generator installed by the controlling
    operator. [process_frame] can be overridden to transform the data instead of
    buffering it verbatim (inline encoders, resamplers). *)
class child_output src =
  object (self)
    inherit
      Source.active_operator ~name:(Printf.sprintf "%s.child" src#id) [src]

    method! source_type = `Output (self :> Source.active)
    val mutable generator = None
    method set_generator gen = generator <- Some gen

    val mutable process_frame : Generator.t -> write_payload -> unit =
      fun generator -> function
        | `Frame frame -> Generator.append generator frame | `Flush -> ()

    method set_process_frame fn = process_frame <- fn

    method flush =
      match generator with
        | Some generator -> process_frame generator `Flush
        | None -> ()

    method output =
      match generator with
        | Some generator when self#is_ready ->
            let frame = self#get_frame in
            if Frame.position frame > 0 then (
              let len = Generator.length generator in
              process_frame generator (`Frame frame);
              let new_len = Generator.length generator in
              let leak_warning =
                max 1 (Frame.main_of_seconds conf_leak_warning#get)
              in
              if len / leak_warning < new_len / leak_warning then
                self#log#important
                  "Generator for source %s has over %.02fs of content. There \
                   might be a data leak or accumulation due to imbalanced \
                   child clock usage."
                  self#id
                  (Frame.seconds_of_main new_len))
        | _ -> ()

    method reset =
      match generator with
        | Some generator -> Generator.clear generator
        | None -> ()

    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method private generate_frame = src#get_frame
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method effective_source = src#effective_source
  end

class virtual base ?child_frame_type ~check_self_sync child_val =
  let child = new child_output (Lang.to_source child_val) in
  object (self)
    initializer
      if check_self_sync then
        if (Lang.to_source child_val)#self_sync <> (`Static, None) then
          raise
            (Error.Invalid_value
               ( child_val,
                 "This source may control its own latency and cannot be used \
                  with this operator.",
                 [] ))

    method virtual id : string
    method virtual clock : Clock.t
    method virtual pos : Pos.Option.t
    method virtual stack : Pos.t list
    method virtual frame_type : Type.t
    method virtual content_type : Frame.content_type
    method virtual on_before_streaming_cycle : (unit -> unit) -> unit
    method virtual on_wake_up : (unit -> unit) -> unit
    method virtual on_sleep : (unit -> unit) -> unit
    method virtual self_sync : Clock.self_sync
    method virtual source_type : Clock.source_type
    method virtual activations : Clock.activation list
    method virtual wake_up : Clock.source -> Clock.activation
    method child_clock_controller = None
    method child = child
    val mutable child_clock = None

    (* Generator holding the data processed by [child] and not yet consumed
       by this operator. Created with this operator's content type: the
       default [process_frame] buffers child frames verbatim, transform-style
       users write data of this operator's type. *)
    val mutable child_buffer = Generator.create Frame.Fields.empty
    method child_buffer = child_buffer

    initializer
      let controller =
        Option.value self#child_clock_controller ~default:(`Clock self#clock)
      in
      child_clock <-
        Some
          (Clock.create ~controller ~sync:`Passive ~id:(Clock.id self#clock) ());
      Clock.unify ~pos:self#pos self#child_clock child#clock;
      let child_frame_type =
        Option.value ~default:self#frame_type child_frame_type
      in
      Typing.(child#frame_type <: child_frame_type);
      Typing.((Lang.to_source child_val)#frame_type <: child#frame_type);
      self#on_before_streaming_cycle (fun () ->
          if not (Clock.started self#child_clock) then
            Clock.start self#child_clock;
          Clock.activate_pending_sources self#child_clock)

    method child_clock =
      match child_clock with Some c -> c | None -> assert false

    val mutable child_activation = None

    initializer
      (* We need an early registration for sources such as source.dynamic. *)
      Clock.register_sub_clock self#clock self#child_clock;
      self#on_wake_up (fun () ->
          (* This is idempotent so it's okay to do it twice the first time. *)
          Clock.register_sub_clock self#clock self#child_clock;
          child_buffer <- Generator.create self#content_type;
          child#set_generator child_buffer;
          child#set_stack self#stack;
          assert (child_activation = None);
          child_activation <- Some (child#wake_up (self :> Clock.source)));
      self#on_sleep (fun () ->
          Clock.deregister_sub_clock self#clock self#child_clock;
          child#sleep (Option.get child_activation);
          child_activation <- None;
          child#flush)

    method child_tick = Clock.tick self#child_clock

    (* Return a frame of data from [child_buffer], ticking the child clock
       until enough data is available (or the child cannot produce more).
       [get_partial_frame] can decide to consume less than a full frame, e.g.
       to stop at a track boundary: only the returned frame's length is
       consumed from the buffer. *)
    method child_get_frame ?(get_partial_frame = fun frame -> frame) () =
      let size = Lazy.force Frame.size in
      while child#is_ready && Generator.length child_buffer < size do
        self#child_tick
      done;
      let frame =
        get_partial_frame (Generator.slice ~peek:true child_buffer size)
      in
      Generator.truncate child_buffer (Frame.position frame);
      frame
  end

(** Simple source reading verbatim (or transformed, see
    [child#set_process_frame]) data from a child source running in its own
    clock. *)
class producer ?stack ?child_frame_type ~check_self_sync ~name child_val =
  object (self)
    inherit Source.source ?stack ~name ()
    inherit base ?child_frame_type ~check_self_sync child_val
    method self_sync = self#child#self_sync
    method fallible = self#child#fallible
    method effective_source = (self :> Source.source)

    method! seek len =
      let gen_len = min (Generator.length self#child_buffer) len in
      Generator.truncate self#child_buffer gen_len;
      if gen_len < len then gen_len + self#child#seek (len - gen_len)
      else gen_len

    method remaining =
      match Generator.remaining self#child_buffer with
        | -1 -> (
            match self#child#remaining with
              | -1 -> -1
              | r -> r + Generator.length self#child_buffer)
        | r -> r

    method private can_generate_frame =
      0 < Generator.length self#child_buffer || self#child#is_ready

    method private generate_frame = self#child_get_frame ()

    method abort_track =
      Generator.clear self#child_buffer;
      Generator.add_track_mark self#child_buffer;
      self#child#abort_track
  end
