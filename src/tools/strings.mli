(** String buffers where the main operation is to add strings at the end. *)

(** A buffer of strings. *)
type t

(** The empty buffer. *)
val empty : t

val of_string : string -> t

(** Render a buffer into a string. This operation can be costly (in terms of memory copies), avoid it. *)
val to_string : t -> string

val to_string_list : t -> string list

val substring : t -> int -> int -> string

(** Concatenation of strings. *)
val of_list : string list -> t

(** Add a string at the end of a buffer. *)
val add : t -> string -> t

(** Add subbytes at the end of a buffer. *)
val add_subbytes : t -> bytes -> int -> int -> t

(** Add a string at the beginning of a buffer. *)
val dda : string -> t -> t

(** Add a substring of bytes. *)
val add_subbytes : t -> Bytes.t -> int -> int -> t

(** Iterate a function on all the strings contained in the buffer. *)
val iter : (string -> unit) -> t -> unit

(** Fold a function over all the strings in a buffer. *)
val fold : ('a -> string -> 'a) -> 'a -> t -> 'a

(** Drop the first given chars. *)
val drop : t -> int -> t

(** Keep a suffix of at least given length (the result might be longer). All
    data is kept in case the buffer was shorter. *)
val keep : t -> int -> t

(** Sub-buffer of a buffer. *)
val sub : t -> int -> int -> t

(** Copy a substring. *)
val blit : t -> int -> bytes -> int -> int -> unit

(** Whether the buffer is the empty string. *)
val is_empty : t -> bool

(** Length of the buffer. *)
val length : t -> int

(** Append two buffers. *)
val append : t -> t -> t

(** Concatenate a list of buffers. *)
val concat : t list -> t
