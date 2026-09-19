(* Dump each stage of the language pipeline for a set of scripts, so that
   changes to the parser, the reducer or the type checker show up as a diff.

   For every input file this prints:
     - [parsed]: the parsed term, as the JSON the formatter and LSP consume,
     - [hash]:   its hash, which is what the typechecking cache keys on,
     - [term]:   the runtime term, i.e. what [Term_reducer] desugared it to,
     - [type]:   the type inferred for the whole script,
     - [value]:  the result of evaluating it.

   A stage that raises prints its error and the following stages are skipped:
   error messages are part of the snapshot too.

   Everything runs against [liquidsoap_lang] alone, with no standard library
   and no streaming core, so the output only depends on the language.

   With [--canonical], it instead dumps [Parsed_json.parse_string]: the flat,
   keyword-anchored JSON that liquidsoap-prettier consumes, positions included.
   That is a separate contract from the [parsed] section above — it is spec'd in
   [parsed_json.mli] — and positions are exactly what it turns on.

   With [--analysis ENV], it runs [Liquidsoap_tooling.Analysis] against the full
   standard library's typing environment [ENV] and dumps the diagnostics, then
   answers the queries written in the script as
   [#? type|scope|locals|methods L:C]. *)

open Liquidsoap_lang

let section name = Printf.printf "--- %s ---\n" name

(* The reducer names the variables it introduces from a global counter
   ([Term_reducer], "_%d_pat"), so their numbers shift as soon as another case
   is added. Renumber them per script, in order of appearance. *)
let stable_generated_names =
  let re = Re.Pcre.regexp "_[0-9]+_pat" in
  fun s ->
    let seen = Hashtbl.create 8 in
    Re.replace re s ~f:(fun g ->
        let name = Re.Group.get g 0 in
        match Hashtbl.find_opt seen name with
          | Some n -> n
          | None ->
              let n = Printf.sprintf "_pat%d" (Hashtbl.length seen) in
              Hashtbl.add seen name n;
              n)

(* Positions dominate the parsed-term JSON and would bury the shape we actually
   want to review. *)
let rec without_positions : Json.t -> Json.t = function
  | `Assoc l ->
      `Assoc
        (List.filter_map
           (fun (k, v) ->
             if k = "position" then None else Some (k, without_positions v))
           l)
  | `Tuple l -> `Tuple (List.map without_positions l)
  | v -> v

(* Some error paths append an OCaml backtrace, whose file positions would churn
   on every unrelated edit. *)
let is_backtrace_frame line =
  List.exists
    (fun prefix -> String.starts_with ~prefix line)
    ["Raised at "; "Raised by "; "Called from "; "Re-raised at "]

(* Render an exception the way the user would see it: [Runtime.throw] formats
   language errors and then re-raises [Runtime.Error]. *)
let describe_error ~buffer ~throw exn =
  Buffer.clear buffer;
  (try throw ~bt:(Printexc.get_raw_backtrace ()) exn with _ -> ());
  match String.trim (Buffer.contents buffer) with
    | "" -> Printexc.to_string exn
    | s ->
        String.split_on_char '\n' s
        |> List.filter (fun l -> not (is_backtrace_frame l))
        |> String.concat "\n" |> String.trim

(* Doc comments attach to *statements*, and nothing else in this dump would
   show them, so a binding silently losing its documentation would not fail any
   other section. *)
let rec doc_bindings (tm : Term.t) =
  match tm.Term.term with
    | `Let { Term.doc; pat; def; body; _ } ->
        let name =
          match pat with
            | `PVar l -> String.concat "." l
            | `PTuple l -> "(" ^ String.concat ", " l ^ ")"
        in
        (match doc with
          | Some d -> [(name, d.Doc.Value.description)]
          | None -> [])
        @ doc_bindings def @ doc_bindings body
    | `Seq (a, b) -> doc_bindings a @ doc_bindings b
    | _ -> []

let read_source file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let echo_source file source =
  Printf.printf "=== %s ===\n" (Filename.basename file);
  print_string source;
  if not (String.ends_with ~suffix:"\n" source) then print_newline ()

(* The prettier ABI: block spans and comment offsets, which the pipeline
   snapshots above deliberately strip. Nothing else covers this. *)
let run_canonical_file file =
  let source = read_source file in
  echo_source file source;
  section "canonical";
  match Liquidsoap_tooling.Parsed_json.parse_string source with
    | json -> print_endline (Json.to_string ~compact:false json)
    | exception exn -> print_endline (Printexc.to_string exn)

let run_file file =
  let source = read_source file in
  echo_source file source;
  let buffer = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buffer in
  let lexbuf = Sedlexing.Utf8.from_string source in
  let throw = Runtime.throw ~formatter ~lexbuf:(Some lexbuf) () in
  let stage name fn =
    Buffer.clear buffer;
    section name;
    match fn () with
      | v ->
          Format.pp_print_flush formatter ();
          Some v
      | exception exn ->
          Format.pp_print_flush formatter ();
          let message = describe_error ~buffer ~throw exn in
          Format.pp_print_flush formatter ();
          Printf.printf "%s\n" message;
          None
  in
  let parsed =
    stage "parsed" (fun () ->
        (* [expand_term] is what splices `%include`d files in, and the filename is
           what relative include paths resolve against. Running it here also makes
           the `hash` section below the actual cache key: [Term_cache] hashes the
           expanded parsed term. *)
        (* Comments are collected by the lexer into a global and attached
           afterwards, exactly as Term_preprocessor.mk_expr does; without this
           no binding would carry its doc comment. *)
        Parser_helper.clear_comments ();
        let parsed =
          Runtime.program (Preprocessor.mk_tokenizer ~fname:file lexbuf)
        in
        Parser_helper.attach_comments parsed;
        let parsed = Term_preprocessor.expand_term parsed in
        print_string
          (Json.to_string ~compact:false
             (without_positions (Liquidsoap_tooling.Parsed_json.to_json parsed)));
        print_newline ();
        parsed)
  in
  (* The parsed term's hash is what the typechecking cache keys on, so a change
     here invalidates every cached script on every user's disk. *)
  ignore
    (Option.map
       (fun parsed ->
         stage "hash" (fun () -> print_endline (Parsed_term.hash parsed)))
       parsed);
  let term =
    Option.bind parsed (fun parsed ->
        stage "term" (fun () ->
            let term = Term_reducer.to_term ~throw parsed in
            print_endline (stable_generated_names (Term.to_string term));
            term))
  in
  ignore
    (Option.map
       (fun term ->
         match doc_bindings term with
           | [] -> ()
           | docs ->
               section "doc";
               List.iter
                 (fun (name, description) ->
                   Printf.printf "%s: %s\n" name description)
                 docs)
       term);
  let typed =
    Option.bind term (fun term ->
        stage "type" (fun () ->
            Typechecking.check ~throw ~check_top_level_override:false term;
            print_endline (Type.to_string term.Term.t);
            term))
  in
  ignore
    (Option.bind typed (fun term ->
         stage "value" (fun () ->
             print_endline (Value.to_string (Evaluation.eval term)))))

module Analysis = Liquidsoap_tooling.Analysis

let queries source =
  List.filter_map
    (fun line ->
      match
        Scanf.sscanf_opt line "#? %s %d:%d" (fun query line column ->
            (query, Some (line, column)))
      with
        | Some query -> Some query
        | None -> Scanf.sscanf_opt line "#? %s%!" (fun query -> (query, None)))
    (String.split_on_char '\n' source)

(* The type printer leaves a space before its line breaks, which the repository's
   whitespace checks would strip from the expected files. *)
let print_trimmed text =
  String.split_on_char '\n' text
  |> List.map (fun line ->
      let n = ref (String.length line) in
      while !n > 0 && line.[!n - 1] = ' ' do
        decr n
      done;
      String.sub line 0 !n)
  |> String.concat "\n" |> print_endline

(* Scopes are shown without the standard library's names that a script can
   write, which every script has in scope. *)
let print_query ~env result = function
  | "null_methods", None ->
      section "null_methods";
      Analysis.null_methods ~env |> List.map fst |> String.concat ", "
      |> print_endline
  | query, None -> failwith ("Query needs a position: " ^ query)
  | query, Some (line, column) -> (
      section (Printf.sprintf "%s %d:%d" query line column);
      match query with
        | "type" ->
            print_trimmed
              (Option.value ~default:"(none)"
                 (Analysis.type_at result ~line ~column))
        | "scope" ->
            Analysis.scope_at ~env result ~line ~column
            |> List.filter (fun name ->
                not (List.mem_assoc name env && Lexer.is_var name))
            |> String.concat ", " |> print_endline
        | "locals" ->
            Analysis.locals_at result ~line ~column
            |> String.concat ", " |> print_endline
        | "methods" ->
            Analysis.methods_at result ~line ~column
            |> List.map fst |> String.concat ", " |> print_endline
        | query -> failwith ("Unknown query: " ^ query))

let run_analysis_file ~env file =
  let source = read_source file in
  echo_source file source;
  let result = Analysis.check ~env source in
  section "diagnostics";
  List.iter
    (fun { Analysis.severity; code; pos; message } ->
      Printf.printf "%s %d %s:\n"
        (match severity with `Error -> "Error" | `Warning -> "Warning")
        code (Pos.Option.to_string pos);
      print_trimmed message)
    result.diagnostics;
  List.iter (print_query ~env result) (queries source)

let () =
  match List.tl (Array.to_list Sys.argv) with
    | "--analysis" :: env :: files ->
        let env =
          Analysis.load_env ~version:Build_config.version (read_source env)
        in
        List.iter (run_analysis_file ~env) (List.sort compare files)
    | "--canonical" :: files ->
        List.iter run_canonical_file (List.sort compare files)
    | files -> List.iter run_file (List.sort compare files)
