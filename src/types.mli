(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Here is the definition of what's a [source], a streamer, a radio.
  * This definition is probably the most important in liquidsoap. *)

(** The liveness type of a source indicates whether or not it can
  * fail to broadcast.
  * A Infallible source never fails is always ready.
  *
  * In order to infer liveness information an operator, we must now
  * if it is a "And" operator, or an "Or" one.
  * A "And" operator behaves as a Infallible source if and only if
  * all of its sources are Infallible.
  * A "Or" operator is Infallible if at least one of its sources is Infallible.
  * Typically, an operator that switches between 2 sources is an "Or" one,
  * and an operator that mixes 2 sources together is an "And" one. *)
type source_t = Fallible | Infallible

(** The [source] use is to send music frames through the [get] method. *)
class virtual source :
object

  (** {1 Naming} *)

  (** Identifier of the source. *)
  method id : string
  method set_id : ?definitive:bool -> string -> unit

  (** {1 Liveness type}
    *
    * [stype] is the liveness type, telling whether a scheduler is
    * fallible or not, i.e. [get] will never fail.
    * It is defined in the derived classes. *)

  method stype : source_t

  (** {1 Init/shutdown} *)

  (** The operator says to the source that he will ask it frames. *)
  method get_ready : ?dynamic:bool -> source list -> unit

  (** Special init phase for outputs. This method is called by Root after the
    * standard get_ready propagation, after the Root clock is started.
    * It allows enhancements of the initial latency. *)
  method output_get_ready : unit

  (** Called when the source must be ready and had no active operator,
    * means that the source has to initialize. *)
  method private wake_up : source list -> unit

  (** Opposite of [get_ready] : the operator no longer needs the source. *)
  method leave : ?dynamic:bool -> source -> unit
  method private sleep : unit

  (** {1 Streaming} *)

  (** Number of frames left in the current track. Defaults to -1=infinity. *)
  method remaining : int

  (** [is_ready] tells you if [get] would succeed. *)
  method virtual is_ready : bool

  (** [get buf] asks the source to fill the buffer [buf] if possible. 
    * We say that [get] fails when nothing is added to the buffer.
    * The [get] call is partial when the buffer is not completely filled.
    * [get] should never be called with a full buffer. *)
  method get : Mixer.Buffer.t -> unit
  method private virtual get_frame : Mixer.Buffer.t -> unit

  (** Tells the source to finish the reading of current track. *)
  method virtual abort_track : unit

  method is_output : bool

  (** Start a new output round, triggers computation of a new frame
    * and output of it at the output nodes. *)
  method output : unit
  (** Wait for output round to finish.
    * Typically, output nodes compute an audio frame (a full buffer),
    * then launch a few output threads, which take care of encoding
    * and outputting (to a file, network, ...).
    * In that case, after_output allows the node to wait for its
    * output threads. *)
  method after_output : unit
  method clear_cache : unit

  (** {1 Utilities} *)

  (** Create a request with a "source" metadata. *)
  method private create_request :
    ?metadata:((string*string) list) -> 
    ?audio:bool -> ?persistent:bool ->
    ?indicators:(Request.indicator list) -> string ->
    Request.t option

  method private log : int -> string -> unit
  method private logl : int -> string Lazy.t -> unit

end

(* This is for defining a source which has children *)
class virtual operator : source list ->
object
  inherit source
end

(* Source with children, actively pulling the stream *)
class virtual active_operator : source ->
object
  inherit source
  val memo : Mixer.Buffer.t
  method is_ready : bool
end

(* Same without source input *)
class virtual active_source :
object
  inherit source
  val memo : Mixer.Buffer.t
  method is_ready : bool
end

val iter_outputs : (source -> unit) -> unit
