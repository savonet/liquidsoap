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

(* The kind of value shared by a producer and a consumer. *)
type control = {
  lock : Mutex.t;
  generator : Generator.t;
  mutable buffering : bool;
  mutable abort : bool;
}

let proceed control f = Tutils.mutexify control.lock f ()

(** The source which produces data by reading the buffer. *)
class producer ~name ~kind c =
  object (self)
    inherit Source.source kind ~name

    method self_sync = false

    method stype = Source.Fallible

    method remaining = proceed c (fun () -> Generator.remaining c.generator)

    method is_ready = proceed c (fun () -> not c.buffering)

    method private get_frame frame =
      proceed c (fun () ->
          Generator.fill c.generator frame;
          if Frame.is_partial frame && Generator.length c.generator = 0 then (
            self#log#important "Buffer emptied, start buffering...";
            c.buffering <- true ))

    method abort_track = proceed c (fun () -> c.abort <- true)
  end

type write_payload = [ `Frame of Frame.t | `Flush ]
type write_frame = write_payload -> unit

class consumer ?(write_frame : write_frame option) ~output_kind ~producer ~kind
  ~content ~max_buffer ~pre_buffer ~source c =
  let prebuf = Frame.main_of_seconds pre_buffer in
  let max_buffer = Frame.main_of_seconds max_buffer in
  let autostart = true in
  let s = Lang.to_source source in
  let write_frame =
    match write_frame with
      | Some f -> f
      | None -> (
          function
          | `Frame frame ->
              Generator.feed_from_frame ~mode:content c.generator frame
          | `Flush -> () )
  in
  let infallible = s#stype = Source.Infallible in
  object (self)
    inherit
      Output.output
        ~output_kind ~content_kind:kind ~infallible
        ~on_start:(fun () -> ())
        ~on_stop:(fun () -> write_frame `Flush)
        source autostart

    method reset = ()

    method start = ()

    method stop = ()

    method private set_clock =
      Clock.unify self#clock producer#clock;
      Clock.unify self#clock s#clock

    method send_frame frame =
      proceed c (fun () ->
          if c.abort then (
            c.abort <- false;
            s#abort_track );

          write_frame (`Frame frame);
          if Generator.length c.generator > prebuf then (
            c.buffering <- false;
            if Generator.buffered_length c.generator > max_buffer then
              Generator.remove c.generator
                (Generator.length c.generator - max_buffer) ))
  end
