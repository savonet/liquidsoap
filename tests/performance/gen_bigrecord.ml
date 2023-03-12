let parse_memory_consumption script =
  Gc.full_major ();
  Gc.full_major ();
  let mem_before = Mem_usage.info () in
  let tm = Liquidsoap_lang.Runtime.parse script in

  Gc.full_major ();
  Gc.full_major ();
  let mem_after = Mem_usage.info () in
  ignore tm;
  Printf.printf "Big record memory consumption: %s\n%!"
    Mem_usage.(
      prettify_bytes
        (mem_after.process_physical_memory - mem_before.process_physical_memory))

(* Remove type information. *)
let rec strip tm =
  let { Liquidsoap_lang.Term.term; methods; _ } = tm in
  let methods =
    Liquidsoap_lang.Term.Methods.fold (fun _ v ret -> strip v @ ret) methods []
  in
  term :: methods

let term_memory_consumption script =
  Gc.full_major ();
  Gc.full_major ();
  let mem_before = Mem_usage.info () in
  let terms = strip (Liquidsoap_lang.Runtime.parse script) in

  Gc.full_major ();
  Gc.full_major ();
  let mem_after = Mem_usage.info () in
  ignore terms;
  Printf.printf "Big record term-only memory consumption: %s\n%!"
    Mem_usage.(
      prettify_bytes
        (mem_after.process_physical_memory - mem_before.process_physical_memory))

let () =
  let indexes = List.init 5000 string_of_int in
  let methods =
    String.concat "\n"
      (List.map (fun idx -> [%string "let r.a%{idx} = %{idx}"]) indexes)
  in
  let sums =
    String.concat "\n"
      (List.map (fun idx -> [%string "n := n() + r.a%{idx}"]) indexes)
  in
  let script =
    Printf.sprintf "%s"
      [%string
        {|
def sum () =
  r = ()
  %{methods}
  n = ref(0)
  %{sums}
end
time("sum of fields (big record)", sum)
exit(0)
  |}]
  in

  parse_memory_consumption script;
  term_memory_consumption script;

  let fd = open_out Sys.argv.(1) in
  output_string fd script;
  close_out fd
