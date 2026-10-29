(** The 16-byte digest the runtime gives the code fragment holding this
    program's OCaml code: the one [Marshal] checks when it reads closures. *)
val get : unit -> string
