(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Abstract classes for easy creation of output nodes. *)

class virtual output :
  kind:string ->
  ?name:string ->
  Lang.value ->
  bool ->
object
  inherit Source.active_source

  method remaining : int
  method output_get_ready : unit
  method output : unit
  method private get_frame : Frame.t -> unit
  method abort_track : unit

  val mutable start_output : bool
  val mutable stop_output : bool

  method add_metadata : Request.metadata -> unit
  method metadata_queue : Request.metadata Queue.t

  method virtual output_reset : unit
  method virtual output_send : Frame.t -> unit
  method virtual output_start : unit
  method virtual output_stop : unit
end

class virtual ['a] encoded :
  kind:string ->
  name:string ->
  autostart:bool ->
  Lang.value ->
object
  inherit output

  method output_send : Frame.t -> unit

  val mutable encoder : 'a option
  method virtual encode : 'a -> float array array -> int -> int -> string
  method virtual reset_encoder : 'a -> AFrame.metadata -> string
  method virtual send : string -> unit

  method virtual output_reset : unit
  method virtual output_start : unit
  method virtual output_stop : unit
end
