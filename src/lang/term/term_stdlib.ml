open Runtime_term

let rec append_ref ~ref = function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = append_ref ~ref body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, append_ref ~ref t2) }
  | { term = `Tuple [] } as tm -> { tm with term = `Cache_env ref }
  | _ -> assert false

let rec extract_ref = function
  | { term = `Let { body } } -> extract_ref body
  | { term = `Seq (_, tm) } -> extract_ref tm
  | { term = `Cache_env env } -> !env
  | _ -> assert false

let rec prepend_stdlib ~term = function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = prepend_stdlib ~term body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, prepend_stdlib ~term t2) }
  | { term = `Tuple [] } -> term
  | _ -> assert false

let rec prepend_parsed_stdlib :
    parsed_term:Parsed_term.t -> Parsed_term.t -> Parsed_term.t =
 fun ~parsed_term -> function
  | { term = `Let (def, body) } as tm ->
      { tm with term = `Let (def, prepend_parsed_stdlib ~parsed_term body) }
  | { term = `Def (def, body) } as tm ->
      { tm with term = `Def (def, prepend_parsed_stdlib ~parsed_term body) }
  | { term = `Binding (def, body) } as tm ->
      { tm with term = `Binding (def, prepend_parsed_stdlib ~parsed_term body) }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, prepend_parsed_stdlib ~parsed_term t2) }
  | { term = `Eof } | { term = `Tuple [] } -> parsed_term
  | _ -> assert false

type stdlib = {
  parsed_term : Parsed_term.t;
  term : Term.t;
  env : unit -> Typing.env;
}

let append ~config ~error_on_no_stdlib ~deprecated ~parsed_term term =
  let libs = Runtime.libs ~error_on_no_stdlib ~deprecated () in
  let script = List.fold_left (Printf.sprintf "%s\n%%include %S") "" libs in
  let lexbuf = Sedlexing.Utf8.from_string script in
  let parsed_stdlib = Term_reducer.mk_expr Term_reducer.program lexbuf in
  let stdlib = Term_reducer.to_term parsed_stdlib in
  let env () =
    let ref = ref [] in
    let stdlib = append_ref ~ref stdlib in
    Runtime.(
      report lexbuf (fun ~throw () ->
          let stdlib =
            type_term ~name:"stdlib" ~config ~lib:true ~throw
              ~parsed_term:parsed_stdlib stdlib
          in
          ref := extract_ref stdlib));
    !ref
  in
  {
    parsed_term = prepend_parsed_stdlib ~parsed_term parsed_stdlib;
    term = prepend_stdlib ~term stdlib;
    env;
  }
