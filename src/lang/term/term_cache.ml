let cache_filename ~toplevel term =
  if Cache.dir () = None then "XXX"
  else (
    let hash = Parsed_term.hash term in
    let fname =
      Printf.sprintf "%s%s.liq-cache" hash
        (if toplevel then "-toplevel" else "")
    in
    fname)

let retrieve ~toplevel parsed_term : Term.t option =
  Startup.time "Cache retrieval" (fun () ->
      Cache.retrieve (cache_filename ~toplevel parsed_term))

let cache ~toplevel ~parsed_term term =
  Cache.store (cache_filename ~toplevel parsed_term) term
