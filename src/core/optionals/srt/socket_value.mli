(** The script socket value (spec/socket-value.md) and the [srt.socket] builtin.
    Operators hand sockets to scripts through [to_value], typed [t]. *)

(** A socket with its methods. *)
val t : Lang.t

val to_value : Srt.socket -> Lang.value
