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

(* Synchronous encoder with room for initial buffer. *)

(* Base class to inherit from. Should write to self#buffer. *)
class virtual base ?bufferize ~name source =
  let bufferize = Option.value ~default:0 bufferize in
  object (self)
    inherit Source.operator ~name [source]
    method fallible = true
    method virtual encode : [ `Frame of Frame.t | `Flush ] -> unit
    val mutable flushed = false
    val mutable started = false

    method flush =
      if not flushed then (
        flushed <- true;
        started <- false;
        self#encode `Flush)

    method private feed =
      flushed <- false;
      let frame = source#get_frame in
      self#encode (`Frame frame);
      if Frame.is_partial frame then self#flush

    method private can_generate_frame =
      if source#is_ready && Generator.length self#buffer <= bufferize then
        self#feed;
      if not source#is_ready then self#flush;
      if started then 0 < Generator.length self#buffer
      else bufferize < Generator.length self#buffer

    method remaining =
      match source#remaining with
        | -1 -> -1
        | n -> n + Generator.length self#buffer

    method seek_source = source#seek_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method generate_frame =
      started <- true;
      Generator.slice self#buffer (Lazy.force Frame.size)
  end

class encoder ~name ~encode source =
  object (self)
    inherit base ~name source
    method encode = encode self#buffer
  end
