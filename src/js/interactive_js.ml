open Js_of_ocaml
open Liquidsoap_lang

let unavailable name =
  Runtime_error.raise ~pos:[]
    ~message:(Printf.sprintf "%s is not available in the browser." name)
    "unavailable"

(* A plain value such as a list cannot be stubbed: code using it would get a
   function and fail in unrelated ways. *)
let has_unavailable_value t = Type.is_fun t || fst (Type.split_meths t) <> []

(* Arguments follow the type's labels since application looks each passed
   argument up by label before calling the function. *)
let rec unavailable_value name t =
  let meths, base = Type.split_meths t in
  let methods =
    List.fold_left
      (fun methods { Type.meth; scheme = _, t } ->
        if has_unavailable_value t then
          Methods.add meth (unavailable_value (name ^ "." ^ meth) t) methods
        else methods)
      Methods.empty meths
  in
  let ffi_args =
    match (Type.deref base).descr with
      | Type.Arrow (args, _) ->
          List.map
            (fun (optional, label, _) ->
              (label, label, if optional then Some (Value.make `Null) else None))
            args
      | _ -> []
  in
  Value.make ~methods
    (`FFI { Value.ffi_args; ffi_fn = (fun _ -> unavailable name) })

let load_full_stdlib_types () =
  let ic = open_in_bin "/static/stdlib.types" in
  let dump =
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let values = Environment.default_environment () in
  (* Names the browser implements keep their own types: stripped custom types
     do not unify with the ones its typechecker creates. *)
  List.iter
    (fun (name, ((_, t) as scheme)) ->
      if (not (List.mem_assoc name values)) && has_unavailable_value t then
        Environment.add_builtin ~register:false [name]
          (scheme, unavailable_value name t))
    Jsoo_safe_env.(
      restore
        (of_string ~version:Liquidsoap_lang_data.Build_config.version dump))

let () =
  (Hooks.liq_libs_dir := fun () -> "/static");
  Runtime.load_libs ~stdlib:"stdlib_js.liq" ();
  load_full_stdlib_types ()

let execute ~throw expr =
  (try
     try
       Typechecking.check ~throw ~check_top_level_override:false expr;
       Term.check_unused ~throw ~lib:true expr;
       let v = Evaluation.eval expr in
       Format.fprintf Format.str_formatter "- : %a = %s@." Repr.print_type
         expr.t (Value.to_string v)
     with exn ->
       let bt = Printexc.get_raw_backtrace () in
       throw ~bt exn
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

(* The playground's prettier parser takes the AST as JSON, with the comments
   inside it. *)
let prettier_json ~comments expr =
  match
    Liquidsoap_tooling.Parsed_json.parse_string ~formatter:Format.str_formatter
      expr
  with
    | `Assoc [("ast", `Assoc ast); ("comments", all_comments)] ->
        let comments = if comments then all_comments else `Tuple [] in
        Js.string
          (Liquidsoap_lang_data.Json.to_string
             (`Assoc (ast @ [("comments", comments)])))
    | _ -> assert false

let on_format =
  Dom_html.handler (fun _ ->
      let expr = Js.to_string (getLiqCode ()) in
      (try formatLiqCode (prettier_json ~comments:true expr) setLiqCode
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
      let json = prettier_json ~comments:false expr in
      let term = Term_reducer.to_term ~throw parsed_term in
      let result = execute ~throw term in
      formatLiqCode json (fun formatted ->
          setOutput
            (Printf.sprintf "%s\n%s\n"
               (String.trim (Js.to_bytestring formatted))
               (String.trim result)));
      Js._true)

let on_clear =
  Dom_html.handler (fun _ ->
      let output = Js.Unsafe.coerce (Dom_html.getElementById_exn "output") in
      output##.value := "";
      onLiqLoaded (Js.string Liquidsoap_lang_data.Build_config.version);
      Js._true)

let on_load =
  Dom_html.handler (fun e ->
      Dom.preventDefault e;
      onLiqLoaded (Js.string Liquidsoap_lang_data.Build_config.version);
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
