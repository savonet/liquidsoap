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

(** General classes for streaming files. *)

class virtual unqueued :
object
  (** [get_next_file] is the only thing you've got to define,
    * it's supposed to return "quickly" as it is run in the Root thread. *)
  method virtual get_next_file : Request.t option

  inherit Source.source
  method is_ready : bool
  method private get_frame : Frame.t -> unit
  method abort_track : unit
  method copy_queue : Request.t list
  method remaining : int
end

class virtual queued :
  ?length:float -> ?default_duration:float -> ?timeout:float -> unit ->
object
  method copy_queue : Request.t list

  (** You should only define this. *)
  method virtual get_next_request : Request.t option

  (** This method should be called whenever the feeding task gets
    * a new opportunity to add more data into the queue. *)
  method private notify_new_request : unit

  inherit unqueued

  (** Everything you need is defined. Dont touch. *)
  method private get_next_file : Request.t option
end

val queued_proto : Lang.operator_proto
val extract_queued_params : Lang.parameters -> float*float*float
