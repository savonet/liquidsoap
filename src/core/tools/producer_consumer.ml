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

type write_payload = [ `Frame of Frame.t | `Flush ]
type write_frame = write_payload -> unit

(* This here is tricky:
   - We want to use the output API to have a method for
     generating data when calling a clock tick.
   - We want to opt-out of the generic child_process
     clock animation framework.
   Thus, we manually mark this operator as ready only
   before we're about to pull from it. *)
class consumer ?(always_enabled = false) ~write_frame ~name ~source () =
  let s = Lang.to_source source in
  let infallible = not s#fallible in
  let noop () = () in
  object
    inherit
      Output.output
        ~output_kind:name ~register_telnet:false ~infallible ~on_start:noop
          ~on_stop:noop source true as super

    val mutable output_enabled = false
    val mutable producer_buffer = Generator.create Frame.Fields.empty
    method set_producer_buffer b = producer_buffer <- b
    method set_output_enabled v = output_enabled <- v
    method! reset = ()
    method start = ()
    method stop = write_frame producer_buffer `Flush
    method! output = if always_enabled || output_enabled then super#output
    method private send_frame frame = write_frame producer_buffer (`Frame frame)
  end

(** The source which produces data by reading the buffer. We do NOT want to use
    [operator] here b/c the [consumers] may have different content-kind when
    this is used in the muxers. *)
class producer ?stack ~check_self_sync ~consumers ~name () =
  let infallible = List.for_all (fun s -> not s#fallible) consumers in
  let self_sync = Clock_base.self_sync consumers in
  object (self)
    inherit Source.source ?stack ~name ()

    inherit
      Child_support.base
        ~check_self_sync
        (List.map (fun s -> Lang.source (s :> Source.source)) consumers)

    method self_sync = self_sync ~source:self ()
    method fallible = not infallible

    method! seek len =
      let len = min (Generator.length self#buffer) len in
      Generator.truncate self#buffer len;
      len

    method seek_source = (self :> Source.source)

    method remaining =
      match
        ( Generator.remaining self#buffer,
          List.fold_left
            (fun r s -> if r = -1 then s#remaining else min r s#remaining)
            (-1) consumers )
      with
        | -1, r -> r
        | r, _ -> r

    method private can_generate_frame =
      List.for_all (fun c -> c#is_ready) consumers

    initializer
      self#on_wake_up (fun () ->
          List.iter (fun c -> c#set_producer_buffer self#buffer) consumers)

    method private generate_frame =
      List.iter (fun c -> c#set_output_enabled true) consumers;
      while
        Generator.length self#buffer < Lazy.force Frame.size
        && self#can_generate_frame
      do
        self#child_tick
      done;
      List.iter (fun c -> c#set_output_enabled false) consumers;
      Generator.slice self#buffer (Lazy.force Frame.size)

    method abort_track =
      Generator.add_track_mark self#buffer;
      List.iter (fun c -> c#abort_track) consumers
  end
