(* Clock unification errors are raised by the streaming core but printed by the
   language runtime's error reporter, which the core plugs into via
   [Runtime.on_error_print]. Check that each of them reaches its printer. *)

let report exn =
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  (try
     Liquidsoap_lang.Runtime.throw ~formatter ~lexbuf:None
       ~bt:(Printexc.get_callstack 0) () exn
   with Liquidsoap_lang.Runtime.Error -> ());
  Format.pp_print_flush formatter ();
  Buffer.contents buf

let contains haystack needle =
  let n = String.length needle in
  let rec loop i =
    i + n <= String.length haystack
    && (String.sub haystack i n = needle || loop (i + 1))
  in
  loop 0

let check exn ~expect =
  let printed = report exn in
  if not (contains printed expect) then (
    Printf.eprintf "Expected %S in:\n%s\n" expect printed;
    assert false)

let () =
  (* Force [Clock]'s initializers, which register the printer. *)
  ignore (Clock.create ());

  check
    (Clock.Conflict (None, "a", "b"))
    ~expect:"A source cannot belong to two clocks";
  check (Clock.Loop (None, "a", "b")) ~expect:"Cannot unify two nested clocks";
  check
    (Clock.Main_conflict
       {
         pos = None;
         left_main = "main_a";
         left_child = "child_a";
         right_main = "main_b";
         right_child = "child_b";
       })
    ~expect:"is controlled by clock";

  (* An exception no printer claims still falls through to the generic
     message. *)
  check Not_found ~expect:"Exception raised";

  print_endline "clock_error_print_test passed!"
