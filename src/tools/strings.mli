(** A buffer of strings. *)
type t

val empty : t

val of_string : string -> t

(** Render a buffer into a string. This operation can be costly (in terms of memory copies), avoid it. *)
val to_string : t -> string

val of_list : string list -> t

val dda : string -> t -> t

(** Add a string at the end of a buffer. *)
val add : t -> string -> t

val add_subbytes : t -> Bytes.t -> int -> int -> t

val iter : (string -> unit) -> t -> unit

val length : t -> int

val append : t -> t -> t

val concat : t list -> t
