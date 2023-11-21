(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type handler = {
  req : Request.t;
  fill : Frame.t -> unit;
  seek : int -> int;
  close : unit -> unit;
}

class virtual unqueued :
  name:string
  -> object
       (** [get_next_file] is the only thing you've got to define,
    * it's supposed to return "quickly" as it is run in the Root thread. *)
       method virtual get_next_file :
         [ `Empty | `Request of Request.t | `Retry of unit -> float ]

       inherit Source.source
       method private _is_ready : ?frame:Frame.t -> unit -> bool
       method private get_frame : Frame.t -> unit
       method abort_track : unit
       method remaining : int
       method self_sync : Source.self_sync
       method current : handler option
       method seek : int -> int
       method seek_source : Source.source
     end

type queue_item = {
  request : Request.t;
  (* in seconds *)
  mutable expired : bool;
}

class virtual queued :
  name:string
  -> ?prefetch:int
  -> ?timeout:float
  -> unit
  -> object
       method stype : [ `Fallible | `Infallible ]

       (** You should only define this. *)
       method virtual get_next_request :
         [ `Empty | `Request of Request.t | `Retry of unit -> float ]

       (** This method should be called whenever the feeding task gets a new
           opportunity to add more data into the queue. *)
       method private notify_new_request : unit

       inherit unqueued

       (** Everything you need is defined. Dont touch. *)
       method private get_next_file :
         [ `Empty | `Request of Request.t | `Retry of unit -> float ]

       (** [#expire f] marks queued requests [r] such that [f r] as expired,
           which will trigger their removal from the queue as soon as
           possible. *)
       method private expire : (Request.t -> bool) -> unit

       (** Try to add a new request in the queue. This should be used in usual
           situations. *)
       method fetch : [ `Finished | `Retry of unit -> float | `Empty ]

       method queue : queue_item Queue.t
       method set_queue : queue_item Queue.t -> unit
       method add : queue_item -> bool
     end

val queued_proto : Lang.proto
val extract_queued_params : Lang.env -> int * float
