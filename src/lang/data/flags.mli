type flags
type flag

val empty : flags
val octal_int : flag
val hex_int : flag
val checked_value : flag

(** A term the reducer adds, such as the [()] ending a block whose last
    statement is a binding. It is positioned over the whole block. *)
val implicit : flag

val has : flags -> flag -> bool
val add : flags -> flag -> flags
val merge : flags -> flags -> flags
val remove : flags -> flag -> flags
