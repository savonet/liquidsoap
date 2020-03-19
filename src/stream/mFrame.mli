(** MIDI frame manipulation *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number size of a MIDI frame. *)
val size : unit -> int

(** Position of the first break. *)
val position : t -> int

(** Add a break. *)
val add_break : t -> int -> unit

type metadata = (string, string) Hashtbl.t

val set_metadata : t -> int -> metadata -> unit
val get_metadata : t -> int -> metadata option
val get_all_metadata : t -> (int * metadata) list

(** Get the MIDI tracks at a given position, assuming that the frame
  * already contains only MIDI starting at this point. *)
val content : t -> Frame.midi_t array
