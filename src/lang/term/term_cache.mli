type typing_env = { term : Term.t; env : Typing.env }

type eval_config = {
  name : string;
  fetch_cache : bool;
  save_cache : bool;
  trim : bool;
  typing_env : (unit -> typing_env) option;
  eval : [ `True | `False | `Toplevel ];
}

type t = { config : eval_config; term : Parsed_term.t }

val cache_enabled : unit -> bool
val cache_dir : unit -> string option
val retrieve : t -> Term.t option
val cache : t -> Term.t -> unit
