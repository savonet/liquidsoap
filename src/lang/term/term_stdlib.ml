open Runtime_term

let rec append_ref = function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = append_ref body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, append_ref t2) }
  | { term = `Tuple [] } as tm -> { tm with term = `Cache_env (ref ([], 0)) }
  | _ -> assert false

let rec extract_ref = function
  | { term = `Let { body } } -> extract_ref body
  | { term = `Seq (_, tm) } -> extract_ref tm
  | { term = `Cache_env env } -> !env
  | _ -> assert false

let rec replace_ref ~term = function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = replace_ref ~term body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, replace_ref ~term t2) }
  | { term = `Cache_env _ } -> term
  | _ -> assert false

let rec concat_parsed_term :
    term:Parsed_term.t -> Parsed_term.t -> Parsed_term.t =
 fun ~term -> function
  | { term = `Let (def, body) } as tm ->
      { tm with term = `Let (def, concat_parsed_term ~term body) }
  | { term = `Def (def, body) } as tm ->
      { tm with term = `Def (def, concat_parsed_term ~term body) }
  | { term = `Binding (def, body) } as tm ->
      { tm with term = `Binding (def, concat_parsed_term ~term body) }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, concat_parsed_term ~term t2) }
  | { term = `Eof } | { term = `Tuple [] } -> term
  | _ -> assert false

type stdlib = {
  append_parsed_term : Parsed_term.t -> Parsed_term.t;
  append_term : Term.t -> Term.t;
  env : Typing.env;
  level : int;
}

let stdlib ~config ~error_on_no_stdlib ~deprecated () =
  let libs = Runtime.libs ~error_on_no_stdlib ~deprecated () in
  let script = List.fold_left (Printf.sprintf "%s\n%%include %S") "" libs in
  let lexbuf = Sedlexing.Utf8.from_string script in
  let parsed_term = Term_reducer.mk_expr Term_reducer.program lexbuf in
  let stdlib = Term_reducer.to_term parsed_term in
  let stdlib = append_ref stdlib in
  Runtime.report lexbuf (fun ~throw () ->
      Runtime.type_and_run ~config ~lib:true ~throw ~parsed_term stdlib);
  let env, level = extract_ref stdlib in
  {
    append_parsed_term = (fun term -> concat_parsed_term ~term parsed_term);
    append_term = (fun term -> replace_ref ~term stdlib);
    env;
    level;
  }
