type stdlib = {
  parsed_term : Parsed_term.t;
  term : Term.t;
  env : unit -> Typing.env;
}

val append :
  ?libs:string list ->
  config:Term_cache.eval_config ->
  error_on_no_stdlib:bool ->
  deprecated:bool ->
  parsed_term:Parsed_term.t ->
  Term.t ->
  stdlib
