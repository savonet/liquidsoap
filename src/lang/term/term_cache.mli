val cache_enabled : unit -> bool
val cache_dir : unit -> string option
val retrieve : ?name:string -> trim:bool -> Parsed_term.t -> Term.t option
val cache : trim:bool -> parsed_term:Parsed_term.t -> Term.t -> unit
