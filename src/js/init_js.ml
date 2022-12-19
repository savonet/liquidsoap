open Liquidsoap_lang

let () =
  Hooks.regexp := Regexp_js.make;
  (Hooks.liq_libs_dir := fun () -> "/static");
  Runtime.load_libs ~deprecated:false ~stdlib:"stdlib_js.liq" ()
