(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Parameters needed to instantiate an output. *)
val proto : (string * Lang.t * Lang.value option * string option) list

class virtual output :
  content_kind:Frame.content_kind
  -> output_kind:string
  -> ?name:string
  -> infallible:bool
  -> on_start:(unit -> unit)
  -> on_stop:(unit -> unit)
  -> Lang.value
  -> bool
  -> object
       inherit Source.active_source

       method stype : Source.source_t

       method self_sync : bool

       method remaining : int

       method output_get_ready : unit

       method output : unit

       method private get_frame : Frame.t -> unit

       method abort_track : unit

       val mutable request_start : bool

       val mutable request_stop : bool

       (** An infallible (normal) output can always stream.
    * Both fallible and infallible outputs may not always be outputting
    * (sending data to the world using [#output_send]).
    * Outputting can only be done when streaming.
    * The following two methods give those two aspects of the current status,
    * [#is_active] tells if the source is outputting and [#is_ready] tells
    * whether it is streaming or can start streaming. *)
       method is_active : bool

       method is_ready : bool

       method private add_metadata : Request.metadata -> unit

       method private metadata_queue : Request.metadata Queue.t

       (* TODO except for #output_reset those methods should be private
        *   while we're at it I'm tempted to remove #is_active and let each
        *   output deal with it *)
       method virtual private output_reset : unit

       method virtual private output_send : Frame.t -> unit

       method virtual private output_start : unit

       method virtual private output_stop : unit
     end

class virtual encoded :
  content_kind:Frame.content_kind
  -> output_kind:string
  -> name:string
  -> infallible:bool
  -> on_start:(unit -> unit)
  -> on_stop:(unit -> unit)
  -> autostart:bool
  -> Lang.value
  -> object
       inherit output

       method private output_send : Frame.t -> unit

       method virtual private encode : Frame.t -> int -> int -> 'a

       method virtual private insert_metadata :
         Meta_format.export_metadata -> unit

       method virtual private send : 'a -> unit

       method virtual private output_reset : unit

       method virtual private output_start : unit

       method virtual private output_stop : unit
     end
