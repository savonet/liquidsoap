open Liquidsoap_lang

let () =
  (Hooks.liq_libs_dir := fun () -> "/static");
  Runtime.load_libs ~stdlib:"stdlib_js.liq" ()
