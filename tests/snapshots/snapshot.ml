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
   and no streaming core, so the output only depends on the language. *)

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

let run_file file =
  Printf.printf "=== %s ===\n" (Filename.basename file);
  let source =
    let ic = open_in_bin file in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  print_string source;
  if not (String.ends_with ~suffix:"\n" source) then print_newline ();
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
        let parsed = Runtime.program (Preprocessor.mk_tokenizer lexbuf) in
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

let () =
  let files = List.tl (Array.to_list Sys.argv) in
  List.iter run_file (List.sort compare files)
