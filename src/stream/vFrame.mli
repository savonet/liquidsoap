type t = Frame.t

(** Number of video frames. *)
val size : t -> int

(** Position of the first break. *)
val position : t -> int

(** Add a break. *)
val add_break : t -> int -> unit

(** Get the contents of all video channels. *)
val get_rgb : t -> RGB.t array array
