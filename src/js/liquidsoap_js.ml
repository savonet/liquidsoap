open Js_of_ocaml
open Liquidsoap_lang

let () =
  Hooks.regexp := (module Regexp_js : Hooks.Regexp_t);
  Hooks.liq_libs_dir := fun () -> "/static"

let execute expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let expr = Runtime.mk_expr ~pwd:"/static" Parser.program lexbuf in
  let throw exn = Printf.printf "Error: %s\n%!" (Printexc.to_string exn) in
  Typechecking.check ~throw ~ignored:false expr;
  Term.check_unused ~throw ~lib:true expr;
  let v = !Hooks.collect_after (fun () -> Evaluation.eval_toplevel expr) in
  Format.fprintf Format.str_formatter "- : %a = %s@." Repr.print_type expr.t
    (Value.to_string v);
  Format.flush_str_formatter ()

let on_execute =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      let text_area = Dom_html.getElementById_exn "input" in
      let expr = Js.to_string (Js.Unsafe.coerce text_area)##.value in
      let result = Printf.sprintf "%s\n%s\n" expr (execute expr) in
      let text_area = Dom_html.getElementById_exn "output" in
      (Js.Unsafe.coerce text_area)##.value := Js.string result;
      Js._true)

let on_load =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      let execute = Dom_html.getElementById_exn "execute" in
      ignore
        (Dom_html.addEventListener execute Dom_events.Typ.click on_execute
           Js._true);
      Js._true)

let () = Dom_html.window##.onload := on_load
