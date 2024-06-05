type stdlib = {
  append_parsed_term : Parsed_term.t -> Parsed_term.t;
  append_term : Term.t -> Term.t;
  env : Typing.env;
  level : int;
}

val stdlib :
  config:Runtime.eval_config ->
  error_on_no_stdlib:bool ->
  deprecated:bool ->
  unit ->
  stdlib
