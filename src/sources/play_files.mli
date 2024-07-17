
(** General classes for streaming files. *)

open Types

class virtual unqueued : 
object
  (** [get_next_file] is the only thing you've got to define. *)
  method virtual get_next_file : Request.t option

  inherit source
  method wake_up : unit
  method sleep : unit
  method is_ready : bool
  method get : Mixer.Buffer.t -> unit
  method abort_track : unit
end

class virtual queued : 
object

  method copy_queue : Request.t list

  (** You should only define this. *)
  method virtual get_next_request : Request.t option

  (** You can fine tune this parameter. *)
  method next_resolve_time : int
  method next_timeout : float

  inherit unqueued

  (** Everything you need is defined. Dont touch. *)
  method private get_next_file : Request.t option
end
