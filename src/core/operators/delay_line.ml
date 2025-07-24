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

open Source

class delay (source : source) duration =
  let duration () = Frame.main_of_seconds (duration ()) in
  object (self)
    inherit operator ~name:"delay_line" [source]
    val mutable override = None
    method fallible = source#fallible
    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek_source = source#seek_source
    method self_sync = source#self_sync
    val mutable should_queue = false
    val mutable deferred = true

    method private buffer_data =
      Generator.append self#buffer source#get_frame;
      if deferred && duration () <= Generator.length self#buffer then
        deferred <- false

    method private queue_output =
      Clock.on_tick self#clock (fun () ->
          if source#is_ready then self#buffer_data;
          if should_queue then self#queue_output)

    initializer
      self#on_wake_up (fun () ->
          should_queue <- true;
          self#queue_output);
      self#on_sleep (fun () -> should_queue <- false)

    method private can_generate_frame =
      (not deferred) && Generator.length self#buffer > 0

    method private generate_frame =
      Generator.slice self#buffer (Lazy.force Frame.size)
  end

let _ =
  let frame_t = Lang.univ_t () in
  Lang.add_operator "delay_line"
    [
      ( "",
        Lang.getter_t Lang.float_t,
        None,
        Some "Duration of the delay in seconds." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Track
    ~descr:"Delay the source by a given amount of time."
    (fun p ->
      let duration = Lang.assoc "" 1 p |> Lang.to_float_getter in
      let s = Lang.assoc "" 2 p |> Lang.to_source in
      new delay s duration)
