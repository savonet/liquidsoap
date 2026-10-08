(* One test program holds the requirements of one section of spec/tests.md.

   [main] runs the single requirement named on the command line, so that
   test/runner.ml gives each one its own process: [--list] prints the
   identifiers. The exit code is 0 for a pass, [skipped] for a skip, and
   anything else is a failure. A requirement that made no check fails. *)

exception Skip of string

let skipped = 77
let checks = ref 0

let check condition description =
  incr checks;
  if not condition then failwith ("check failed: " ^ description)

let equal expected actual description = check (expected = actual) description

(* [f] must raise an exception that [expected] accepts. *)
let raises expected description f =
  match f () with
    | _ -> check false (description ^ ": no exception")
    | exception exn ->
        check (expected exn)
          (Printf.sprintf "%s: unexpected %s" description
             (Printexc.to_string exn))

let skip reason = raise (Skip reason)

(* Collects everything that became unreachable, finalisers included. *)
let collect () =
  for _ = 1 to 3 do
    Gc.full_major ()
  done

let run requirement =
  match requirement () with
    | () when !checks = 0 ->
        print_endline "FAIL the requirement made no check";
        exit 1
    | () ->
        collect ();
        Printf.printf "PASS %d checks\n" !checks
    | exception Skip reason ->
        Printf.printf "SKIP %s\n" reason;
        exit skipped
    | exception exn ->
        Printf.printf "FAIL %s\n" (Printexc.to_string exn);
        exit 1

let main requirements =
  match Sys.argv with
    | [| _; "--list" |] ->
        List.iter (fun (id, _) -> print_endline id) requirements
    | [| _; id |] -> (
        match List.assoc_opt id requirements with
          | Some requirement -> run requirement
          | None ->
              Printf.printf "FAIL unknown requirement %s\n" id;
              exit 1)
    | _ ->
        prerr_endline "usage: <test> (--list | <requirement>)";
        exit 2
