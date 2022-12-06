open Js_of_ocaml
open Liquidsoap_lang

let execute expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let throw = Runtime.throw ~formatter:Format.str_formatter lexbuf in
  (try
     try
       let expr = Runtime.mk_expr ~pwd:"/static" Parser.program lexbuf in
       Typechecking.check ~throw ~ignored:false expr;
       Term.check_unused ~throw ~lib:true expr;
       let v = !Hooks.collect_after (fun () -> Evaluation.eval_toplevel expr) in
       Format.fprintf Format.str_formatter "- : %a = %s@." Repr.print_type
         expr.t (Value.to_string v)
     with exn -> throw exn
   with
    | Runtime.Error -> ()
    | exn ->
        Format.fprintf Format.str_formatter "- : error %s@."
          (Printexc.to_string exn));
  Format.flush_str_formatter ()

let on_execute =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      let input = Js.Unsafe.coerce (Dom_html.getElementById_exn "input") in
      let expr = Js.to_string input##.value in
      let result = execute expr in
      let output = Js.Unsafe.coerce (Dom_html.getElementById_exn "output") in
      output##.value :=
        Printf.sprintf "%s\n%s\n%s" (Js.to_string output##.value) expr result;
      input##.value := "";
      Js._true)

let on_load =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      let output = Js.Unsafe.coerce (Dom_html.getElementById_exn "output") in
      output##.value :=
        [%string
          {|# Welcome to liquidsoap's online interpreter!
# Language version: %{Build_config.version}
|}];
      let execute = Dom_html.getElementById_exn "execute" in
      ignore
        (Dom_html.addEventListener execute Dom_events.Typ.click on_execute
           Js._true);
      Js._true)

let () = Dom_html.window##.onload := on_load
