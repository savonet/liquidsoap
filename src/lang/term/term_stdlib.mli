type stdlib = {
  parsed_term : Parsed_term.t;
  term : Term.t;
  env : unit -> Typing.env;
}

val append :
  ?libs:string list ->
  config:Runtime.eval_config ->
  error_on_no_stdlib:bool ->
  deprecated:bool ->
  parsed_term:Parsed_term.t ->
  Term.t ->
  stdlib
