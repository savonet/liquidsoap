(* Builtins' documentation is a large share of the heap once a script runs:
   clearing the top-level environments must drop it. *)
let () =
  assert (Doc.Value.count () > 0);
  Environment.clear_toplevel_environments ();
  assert (Doc.Value.count () = 0)
