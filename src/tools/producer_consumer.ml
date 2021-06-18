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

module Generator = Generator.From_audio_video

let finalise_child_clock child_clock source =
  Clock.forget source#clock child_clock

(** The source which produces data by reading the buffer. *)
class producer ~consumers ~name ~kind g =
  let infallible =
    List.for_all (fun s -> s#stype = Source.Infallible) consumers
  in
  object (self)
    inherit Source.source kind ~name as super

    method self_sync =
      List.fold_left
        (fun (t, v) s ->
          let t', v' = s#self_sync in
          ((if t = `Static && t' = `Static then `Static else `Dynamic), v || v'))
        (`Static, false)
        consumers

    method stype = if infallible then Source.Infallible else Source.Fallible

    method remaining =
      match List.fold_left (fun r p -> min r p#remaining) (-1) consumers with
        | -1 -> -1
        | r -> Generator.remaining g + r

    method is_ready = List.for_all (fun c -> c#is_ready) consumers

    val mutable child_clock = None

    method private child_clock = Option.get child_clock

    method private set_clock =
      (* Same as [cross[] *)
      child_clock <- Some (Clock.create_known (new Clock.clock self#id));
      Clock.unify self#clock
        (Clock.create_unknown ~sources:[] ~sub_clocks:[self#child_clock]);

      List.iter (fun c -> Clock.unify self#child_clock c#clock) consumers;

      Gc.finalise (finalise_child_clock self#child_clock) self

    method wake_up a =
      super#wake_up a;
      List.iter
        (fun c -> c#get_ready ?dynamic:(Some false) [(self :> Source.source)])
        consumers

    method sleep =
      super#sleep;
      List.iter
        (fun c -> c#leave ?dynamic:(Some false) (self :> Source.source))
        consumers

    method private child_tick =
      List.iter (fun c -> c#output) consumers;
      (Clock.get self#child_clock)#end_tick;
      List.iter (fun c -> c#after_output) consumers

    method private get_frame frame =
      while Generator.length g < Lazy.force Frame.size && self#is_ready do
        self#child_tick
      done;
      Generator.fill g frame

    method abort_track =
      Generator.add_break g;
      List.iter (fun c -> c#abort_track) consumers
  end

type write_payload = [ `Frame of Frame.t | `Flush ]
type write_frame = write_payload -> unit

class consumer ?(write_frame : write_frame option) ~name ~kind ~content ~source
  g =
  let write_frame =
    match write_frame with
      | Some f -> f
      | None -> (
          function
          | `Frame frame -> Generator.feed_from_frame ~mode:content g frame
          | `Flush -> () )
  in
  object (self)
    inherit Source.operator ~name kind [source] as super

    method stype = source#stype

    method self_sync = source#self_sync

    method abort_track = source#abort_track

    method is_ready = source#is_ready

    method remaining = source#remaining

    val mutable frame_buffer = Frame.dummy

    method private wake_up x =
      super#wake_up x;
      frame_buffer <- Frame.create self#ctype

    method sleep =
      super#sleep;
      write_frame `Flush

    method output =
      let get_count = ref 0 in
      while Frame.is_partial frame_buffer && self#is_ready do
        incr get_count;
        if !get_count > Lazy.force Frame.size then
          self#log#severe
            "Warning: there may be an infinite sequence of empty tracks!";
        self#get frame_buffer
      done;
      if Frame.position frame_buffer > 0 then write_frame (`Frame frame_buffer)

    method get_frame frame = source#get frame

    method after_output =
      super#after_output;
      Frame.advance frame_buffer
  end
