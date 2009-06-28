(** MIDI frame manipulation *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number of video frames. *)
val size : t -> int

(** Position of the first break. *)
val position : t -> int

(** Add a break. *)
val add_break : t -> int -> unit

type metadata = (string,string) Hashtbl.t

val set_metadata     : t -> int -> metadata -> unit
val get_metadata     : t -> int -> metadata option
