let rec append_ref =
  let open Runtime_term in
  function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = append_ref body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, append_ref t2) }
  | tm ->
      Term.make ?pos:tm.t.Type.pos
        (`Seq
           ( tm,
             Term.make ?pos:tm.t.Type.pos
               (`Cache_env (ref { var_name = 0; var_id = 0; env = [] })) ))

let rec extract_ref =
  let open Runtime_term in
  function
  | { term = `Let { body } } -> extract_ref body
  | { term = `Seq (_, tm) } -> extract_ref tm
  | { term = `Cache_env ref } -> !ref
  | _ -> assert false

let rec prepend_stdlib ~term =
  let open Runtime_term in
  function
  | { term = `Let ({ body } as _let) } as tm ->
      { tm with term = `Let { _let with body = prepend_stdlib ~term body } }
  | { term = `Seq (t1, t2) } as tm ->
      { tm with term = `Seq (t1, prepend_stdlib ~term t2) }
  | { term = `Cache_env _ } -> term
  | _ -> assert false

let rec prepend_parsed_stdlib =
  let open Parsed_term in
  fun ~parsed_term -> function
    | { term = `Let (def, body) } as tm ->
        { tm with term = `Let (def, prepend_parsed_stdlib ~parsed_term body) }
    | { term = `Def (def, body) } as tm ->
        { tm with term = `Def (def, prepend_parsed_stdlib ~parsed_term body) }
    | { term = `Binding (def, body) } as tm ->
        {
          tm with
          term = `Binding (def, prepend_parsed_stdlib ~parsed_term body);
        }
    | { term = `Seq (t1, t2) } as tm ->
        { tm with term = `Seq (t1, prepend_parsed_stdlib ~parsed_term t2) }
    | tm -> Parsed_term.make ~pos:tm.pos (`Seq (tm, parsed_term))

(** To be cacheable, the standard library is parsed and converted to a term. We
    add [`Cache_env] to it and type-check it. This results in having the full
    standard library env stored in it. We can then cache this term and retrieve
    the full env using it. Next, we append the user script to the resulting term
    and typecheck the user script only using the standard library environment.
*)
let prepare ?libs ~cache ~error_on_no_stdlib ~deprecated parsed_term =
  let libs =
    match libs with
      | Some libs -> libs
      | None -> Runtime.libs ~error_on_no_stdlib ~deprecated ()
  in
  let script = List.fold_left (Printf.sprintf "%s\n%%include %S") "" libs in
  let lexbuf = Sedlexing.Utf8.from_string script in
  let parsed_stdlib, stdlib =
    Runtime.report ~lexbuf:(Some lexbuf)
      ~default:(fun () -> raise Runtime.Error)
      (fun ~throw () ->
        let parsed_stdlib = Term_reducer.mk_expr Term_reducer.program lexbuf in
        (parsed_stdlib, Term_reducer.to_term ~throw parsed_stdlib))
  in
  let append () =
    let stdlib = append_ref stdlib in
    let stdlib =
      Runtime.type_term ~cache_dirtype:`System ~name:"stdlib" ~cache ~trim:false
        ~lib:true ~term:stdlib parsed_stdlib
    in
    let { Runtime_term.var_name; var_id; env } = extract_ref stdlib in
    Atomic.set Type_base.var_name_atom var_name;
    Atomic.set Type_base.var_id_atom var_id;
    let checked_term =
      Runtime.report ~lexbuf:None
        ~default:(fun () -> raise Runtime.Error)
        (fun ~throw () -> Term_reducer.to_term ~throw parsed_term)
    in
    let full_term = prepend_stdlib ~term:checked_term stdlib in
    { Runtime.checked_term; full_term; env }
  in
  (prepend_parsed_stdlib ~parsed_term parsed_stdlib, append)
