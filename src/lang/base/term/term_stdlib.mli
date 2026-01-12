val prepare :
  stdlib:string option ->
  cache:bool ->
  error_on_no_stdlib:bool ->
  deprecated:bool ->
  Parsed_term.t ->
  Parsed_term.t * Runtime.append_stdlib
