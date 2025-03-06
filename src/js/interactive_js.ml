open Js_of_ocaml
open Liquidsoap_lang

let () =
  (Hooks.liq_libs_dir := fun () -> "/static");
  Runtime.load_libs ~stdlib:"stdlib_js.liq" ()

let execute ~throw expr =
  (try
     try
       Typechecking.check ~throw expr;
       Term.check_unused ~throw ~lib:true expr;
       let v = Evaluation.eval expr in
       Format.fprintf Format.str_formatter "- : %a = %s@." Repr.print_type
         expr.t (Value.to_string v)
     with exn -> throw exn
   with
    | Runtime.Error -> ()
    | exn ->
        Format.fprintf Format.str_formatter "- : error %s@."
          (Printexc.to_string exn));
  Format.flush_str_formatter ()

let setOutput s =
  let output = Js.Unsafe.coerce (Dom_html.getElementById_exn "output") in
  output##.value := s

let onLiqLoaded (version : Js.js_string Js.t) : unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "onLiqLoaded")
    [| Js.Unsafe.inject version |]

let getLiqCode () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "getLiqCode") [||]

let setLiqCode s =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "setLiqCode") [| Js.Unsafe.inject s |]

let formatLiqCode (s : Js.js_string Js.t) (cb : Js.js_string Js.t -> unit) :
    unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "formatLiqCode")
    [| Js.Unsafe.inject s; Js.Unsafe.inject cb |]

let on_format =
  Dom_html.handler (fun _ ->
      let expr = Js.to_string (getLiqCode ()) in
      (try
         let json =
           Liquidsoap_tooling.Parsed_json.parse_string
             ~formatter:Format.str_formatter expr
         in
         let json = Liquidsoap_lang.Json.to_string json in
         formatLiqCode (Js.string json) setLiqCode
       with _ -> setOutput (Format.flush_str_formatter ()));
      Js._true)

let on_execute =
  Dom_html.handler (fun _ ->
      let expr = Js.to_string (getLiqCode ()) in
      let lexbuf = Sedlexing.Utf8.from_string expr in
      let throw =
        Runtime.throw ~formatter:Format.str_formatter ~lexbuf:(Some lexbuf) ()
      in
      let tokenizer = Preprocessor.mk_tokenizer lexbuf in
      let parsed_term = Runtime.program tokenizer in
      let json = Liquidsoap_tooling.Parsed_json.to_json parsed_term in
      let json = Liquidsoap_lang.Json.to_string json in
      let term = Term_reducer.to_term parsed_term in
      let result = execute ~throw term in
      formatLiqCode (Js.string json) (fun formatted ->
          setOutput
            (Printf.sprintf "%s\n%s\n"
               (String.trim (Js.to_bytestring formatted))
               (String.trim result)));
      Js._true)

let on_clear =
  Dom_html.handler (fun _ ->
      let output = Js.Unsafe.coerce (Dom_html.getElementById_exn "output") in
      output##.value := "";
      onLiqLoaded (Js.string Build_config.version);
      Js._true)

let on_load =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      onLiqLoaded (Js.string Build_config.version);
      let execute = Dom_html.getElementById_exn "execute" in
      ignore
        (Dom_html.addEventListener execute Dom_events.Typ.click on_execute
           Js._true);
      let format = Dom_html.getElementById_exn "format" in
      ignore
        (Dom_html.addEventListener format Dom_events.Typ.click on_format
           Js._true);
      let clear = Dom_html.getElementById_exn "clear" in
      ignore
        (Dom_html.addEventListener clear Dom_events.Typ.click on_clear Js._true);
      Js._true)

let () = Dom_html.window##.onload := on_load
