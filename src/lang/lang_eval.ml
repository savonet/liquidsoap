type check_stdlib = [ `If_present | `Force | `Override of string ]
type stdlib = [ `Disabled | check_stdlib ]

let type_term ?name ?(cache = true) ?(trim = true) ?(deprecated = false) ?ty
    ~stdlib ~parsed_term term =
  let parsed_term, stdlib =
    match stdlib with
      | `Disabled -> (parsed_term, None)
      | #check_stdlib as stdlib ->
          let parsed_term, env =
            let libs, error_on_no_stdlib =
              match stdlib with
                | `Override s -> (Some [s], true)
                | `If_present -> (None, false)
                | `Force -> (None, true)
            in
            Term_stdlib.prepare ?libs ~cache ~error_on_no_stdlib ~deprecated
              parsed_term
          in
          (parsed_term, Some env)
  in
  Runtime.type_term ?name ?stdlib ?ty ~term ~cache ~trim ~lib:false parsed_term

let effective_toplevel ~stdlib toplevel =
  (* Registering defined operators at top-level prevents reclaiming memory from unused operators
     so we try to avoid it by default. We need it for all documentation. Also, as soon as the user
     script contains [let eval ...], we have to retain values at top-level as the string being
     parsed will also expect the standard library to be available. *)
    match (stdlib, Term_reducer.needs_toplevel ()) with
    | `Disabled, _ -> toplevel
    | _, true -> true
    | _ -> toplevel

let eval ?(toplevel = false) ?(typecheck = true) ?cache ?deprecated ?ty ?name
    ~stdlib s =
  let parsed_term, term = Runtime.parse s in
  let toplevel = effective_toplevel ~stdlib toplevel in
  let term =
    if typecheck then
      type_term ?name ?cache ?deprecated ?ty ~trim:(not toplevel) ~stdlib
        ~parsed_term term
    else term
  in
  Runtime.eval_term ?name ~toplevel term
