type flags
type flag

val empty : flags
val octal_int : flag
val hex_int : flag
val checked_value : flag
val itered_value : flag
val binary : flag
val has : flags -> flag -> bool
val add : flags -> flag -> flags
