open Liquidsoap_lang

let () =
  (Hooks.liq_libs_dir := fun () -> "/static");
  Printf.printf "Test json parse: %s\n%!"
    (Json.to_string (Json.from_string "{\"foo\": \"bla\\\"blo\"}"));
  let s = Regexp.substitute ~pat:"foo" ~subst:(fun _ -> "gni") "fooblafoo" in
  Printf.printf "Test string subst: %s\n%!" s;
  Runtime.load_libs ()
