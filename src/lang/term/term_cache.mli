val retrieve :
  ?name:string -> ?dirtype:Cache.dirtype -> Parsed_term.t -> Term.t option

val cache :
  ?dirtype:Cache.dirtype -> parsed_term:Parsed_term.t -> Term.t -> unit
