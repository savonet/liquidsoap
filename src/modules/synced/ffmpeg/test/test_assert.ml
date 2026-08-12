(* Shared assertions for the test executables. [finish] fails on an empty
   count as well as on a recorded failure: a test that checked nothing must
   not be able to report success. *)

let checked = ref 0
let failed = ref 0

let check name ok =
  incr checked;
  if ok then Printf.printf "OK: %s\n%!" name
  else begin
    incr failed;
    Printf.eprintf "FAILED: %s\n%!" name
  end

let checkf ok fmt = Printf.ksprintf (fun name -> check name ok) fmt

let finish () =
  Printf.printf "%d checked, %d failed\n%!" !checked !failed;
  if !checked = 0 then begin
    prerr_endline "FAILED: test asserted nothing";
    exit 1
  end;
  if !failed > 0 then exit 1
