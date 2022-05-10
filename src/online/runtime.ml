let print = ref (fun s : unit -> assert false)
let printf k = Format.ksprintf (fun s -> !print s) k
let error = "Error"
let warning = "Warning"
let position pos = Console.colorize [`bold] (String.capitalize_ascii pos)

let error_header idx pos =
  let e = Option.value (Repr.excerpt_opt pos) ~default:"" in
  let pos = Pos.Option.to_string pos in
  Format.printf "@[%s:\n%s\n%s %i: " (position pos) e error idx

exception Error

let throw print_error = function
  | e ->
      let bt = Printexc.get_backtrace () in
      error_header (-1) None;
      Format.printf "Exception raised: %s@.%s@]@." (Printexc.to_string e) bt;
      raise Error

let report lexbuf f =
  let print_error idx error =
    flush_all ();
    let pos = Sedlexing.lexing_positions lexbuf in
    error_header idx (Some pos);
    Format.printf "%s\n@]@." error
  in
  let throw = throw print_error in
  if Term.conf_debug_errors#get then f ~throw ()
  else (try f ~throw () with exn -> throw exn)

let mk_expr ?fname ~pwd processor lexbuf =
  let processor = MenhirLib.Convert.Simplified.traditional2revised processor in
  let tokenizer = Preprocessor.mk_tokenizer ?fname ~pwd lexbuf in
  processor tokenizer

let interactive () =
  printf
    "\n\
     Welcome to the liquidsoap interactive loop.\n\n\
     You may enter any sequence of expressions, terminated by \";;\".\n\
     Each input will be fully processed: parsing, type-checking,\n\
     evaluation (forces default types), output startup (forces default clock).\n\
     @.";
  let lexbuf =
    (* See ocaml-community/sedlex#45 *)
    let chunk_size = 512 in
    let buf = Bytes.create chunk_size in
    let cached = ref (-1) in
    let position = ref (-1) in
    let rec gen () =
      match (!position, !cached) with
        | _, 0 -> None
        | -1, _ ->
            position := 0;
            cached := input stdin buf 0 chunk_size;
            gen ()
        | len, c when len = c ->
            position := -1;

            (* This means that the last read was a full chunk. Safe to try a new
               one right away. *)
            if len = chunk_size then gen () else None
        | len, _ ->
            position := len + 1;
            Some (Bytes.get buf len)
    in
    Sedlexing.Utf8.from_gen gen
  in
  let rec loop () =
    printf "# %!";
    if
      try
        report lexbuf (fun ~throw () ->
            let expr =
              mk_expr ~pwd:(Unix.getcwd ()) Parser.interactive lexbuf
            in
            Typechecking.check ~throw ~ignored:false expr;
            Term.check_unused ~throw ~lib:true expr;
            ignore (Evaluation.eval_toplevel ~interactive:true expr));
        true
      with
        | End_of_file ->
            printf "Bye bye!@.";
            false
        | Error -> true
        | e ->
            let e = Console.colorize [`white; `bold] (Printexc.to_string e) in
            printf "Exception: %s!@." e;
            true
    then loop ()
  in
  loop ()
