open Term_hash

type t = {
  env : (string * Value.t) list;
  trim : bool;
  parsed_term : Parsed_term.t;
}
[@@deriving hash]

let cache_filename ?name ~trim parsed_term =
  match cache_dir () with
    | None -> None
    | Some dir ->
        recmkdir dir;
        let report fn =
          match name with
            | None -> fn ()
            | Some name ->
                Startup.time (Printf.sprintf "%s hash computation" name) fn
        in
        let hash =
          report (fun () ->
              hash
                { env = Environment.default_environment (); trim; parsed_term })
        in
        let fname = Printf.sprintf "%s.liq-cache" hash in
        Some (Filename.concat dir fname)

let retrieve ?name ~trim parsed_term : Term.t option =
  let report fn =
    match name with
      | None -> fn ()
      | Some name -> Startup.time (Printf.sprintf "%s cache retrieval" name) fn
  in
  report (fun () -> Cache.retrieve (cache_filename ?name ~trim parsed_term))

let cache ~trim ~parsed_term term =
  Cache.store (cache_filename ~trim parsed_term) term
