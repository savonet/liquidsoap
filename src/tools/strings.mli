(** String buffers where the main operation is to add strings at the end. *)

(** A buffer of strings. *)
type t

(** The empty buffer. *)
val empty : t

val of_string : string -> t

(** Render a buffer into a string. This operation can be costly (in terms of memory copies), avoid it. *)
val to_string : t -> string

val substring : t -> int -> int -> string

(** Concatenation of strings. *)
val of_list : string list -> t

(** Add a string at the end of a buffer. *)
val add : t -> string -> t

(** Add subbytes at the end of a buffer. *)
val add_subbytes : t -> bytes -> int -> int -> t

(** Add a string at the beginning of a buffer. *)
val dda : string -> t -> t

(** Iterate a function on all the strings contained in the buffer. *)
val iter : (string -> unit) -> t -> unit

(** Drop the first given chars. *)
val drop : t -> int -> t

(** Sub-buffer of a buffer. *)
val sub : t -> int -> int -> t

(** Copy a substring. *)
val blit : t -> bytes -> int -> unit

(** Whether the buffer is the empty string. *)
val is_empty : t -> bool

(** Length of the buffer. *)
val length : t -> int

(** Append two buffers. *)
val append : t -> t -> t

(** Concatenate a list of buffers. *)
val concat : t list -> t
