(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Abstract classes for easy creation of output nodes. *)

val fallibility_check : bool ref

(** Parameters needed to instantiate an output. *)
val proto : (string * Lang.t * Lang.value option * string option) list

class virtual output :
  output_kind:string
  -> ?name:string
  -> infallible:bool
  -> register_telnet:bool
  -> on_start:(unit -> unit)
  -> on_stop:(unit -> unit)
  -> Lang.value
  -> bool
  -> object
       inherit Source.active_source
       method stype : [ `Fallible | `Infallible ]
       method self_sync : Source.self_sync
       method remaining : int
       method output : unit
       method abort_track : unit
       method private can_generate_frame : bool
       method private generate_frame : Frame.t
       method state : Start_stop.state
       method transition_to : Start_stop.state -> unit
       method seek_source : Source.source
       method private video_dimensions : int * int
       method private reset : unit
       method virtual private send_frame : Frame.t -> unit
       method virtual private start : unit
       method virtual private stop : unit
     end

(** Default methods on output values. *)
val meth : (string * Lang.scheme * string * (output -> Lang.value)) list

class virtual ['a] encoded :
  output_kind:string
  -> name:string
  -> infallible:bool
  -> on_start:(unit -> unit)
  -> on_stop:(unit -> unit)
  -> register_telnet:bool
  -> autostart:bool
  -> export_cover_metadata:bool
  -> Lang.value
  -> object
       inherit output
       method private send_frame : Frame.t -> unit
       method virtual private encode : Frame.t -> int -> int -> 'a
       method virtual private insert_metadata : Frame.Metadata.Export.t -> unit
       method virtual private send : 'a -> unit
       method private reset : unit
       method virtual private start : unit
       method virtual private stop : unit
     end

class dummy :
  infallible:bool
  -> on_start:(unit -> unit)
  -> on_stop:(unit -> unit)
  -> autostart:bool
  -> register_telnet:bool
  -> Lang.value
  -> object
       inherit output
       method private reset : unit
       method private start : unit
       method private stop : unit
       method private send_frame : Frame.t -> unit
     end
