type check_stdlib = [ `If_present | `Force | `Override of string ]
type stdlib = [ `Disabled | check_stdlib ]

let type_term ?name ?(cache = true) ?(trim = true) ?(deprecated = false) ~stdlib
    ~parsed_term term =
  let parsed_term, term, env =
    match stdlib with
      | `Disabled -> (parsed_term, term, None)
      | #check_stdlib as stdlib ->
          let { Term_stdlib.parsed_term; term = expanded_term; env } =
            let libs, error_on_no_stdlib =
              match stdlib with
                | `Override s -> (Some [s], true)
                | `If_present -> (None, false)
                | `Force -> (None, true)
            in
            Term_stdlib.append ?libs ~cache ~error_on_no_stdlib ~deprecated
              ~parsed_term term
          in
          ( parsed_term,
            expanded_term,
            Some (fun () -> { Runtime.term; env = env () }) )
  in
  Runtime.type_term ?name ~cache ~trim ?env ~lib:false ~parsed_term term

let eval ?(toplevel = false) ?(typecheck = true) ?cache ?deprecated ?ty ?name
    ~stdlib s =
  let parsed_term, term = Runtime.parse s in
  let term =
    match ty with
      | None -> term
      | Some typ ->
          Term.make ~pos:parsed_term.Parsed_term.pos
            (`Cast { cast = term; typ })
  in
  let toplevel, trim =
    match (stdlib, Term_reducer.needs_toplevel ()) with
      | `Disabled, _ -> (toplevel, not toplevel)
      | _, true -> (true, false)
      | _ -> (toplevel, not toplevel)
  in
  let term =
    if typecheck then
      type_term ?name ?cache ?deprecated ~trim ~stdlib ~parsed_term term
    else term
  in
  Runtime.eval_term ?name ~toplevel term
