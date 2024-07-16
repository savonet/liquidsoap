val retrieve :
  ?name:string ->
  ?dirtype:Cache.dirtype ->
  trim:bool ->
  Parsed_term.t ->
  Term.t option

val cache :
  ?dirtype:Cache.dirtype ->
  trim:bool ->
  parsed_term:Parsed_term.t ->
  Term.t ->
  unit
