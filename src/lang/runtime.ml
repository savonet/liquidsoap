(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** {1 Running} *)

let () = Printexc.record_backtrace true
let () = Lang_core.apply_fun := Evaluation.apply

type eval_config = {
  fetch_cache : bool;
  save_cache : bool;
  eval : [ `True | `False | `Toplevel ];
}

type eval_mode = [ `Parse_only | `Eval of eval_config ]

let type_and_run ~throw ~config ~lib ~parsed_term term =
  let toplevel = config.eval = `Toplevel in
  let ast =
    let cached_term =
      if config.fetch_cache then Term_cache.retrieve ~toplevel parsed_term
      else None
    in
    match cached_term with
      | Some term -> term
      | None ->
          if Lazy.force Term.debug then Printf.eprintf "Type checking...\n%!";
          (* Type checking *)
          Startup.time "Typechecking" (fun () ->
              Typechecking.check ~throw ~ignored:true term);

          if Lazy.force Term.debug then
            Printf.eprintf "Checking for unused variables...\n%!";
          (* Check for unused variables, relies on types *)
          Term.check_unused ~throw ~lib term;
          if not toplevel then Term_trim.trim_term term;
          if config.save_cache then Term_cache.cache ~toplevel ~parsed_term term;
          term
  in
  match config.eval with
    | `False -> ()
    | (`True as v) | (`Toplevel as v) ->
        let eval =
          match v with
            | `True -> fun () -> Evaluation.eval ast
            | `Toplevel -> fun () -> Evaluation.eval_toplevel ast
        in
        if Lazy.force Term.debug then Printf.eprintf "Evaluating...\n%!";
        ignore (Startup.time "Evaluation" eval)

(** {1 Error reporting} *)

let error = Console.colorize [`red; `bold] "Error"
let warning = Console.colorize [`magenta; `bold] "Warning"
let position pos = Console.colorize [`bold] (String.capitalize_ascii pos)

let error_header ~formatter idx pos =
  let e = Option.value (Repr.excerpt_opt pos) ~default:"" in
  let pos = Pos.Option.to_string pos in
  Format.fprintf formatter "@[%s:\n%s\n%s %i: " (position pos) e error idx

let warning_header ~formatter idx pos =
  let e = Option.value (Repr.excerpt_opt pos) ~default:"" in
  let pos = Pos.Option.to_string pos in
  Format.fprintf formatter "@[%s:\n%s\n%s %i: " (position pos) e warning idx

(** Exception raised by report_error after an error has been displayed.
  * Unknown errors are re-raised, so that their content is not totally lost. *)
exception Error

let strict = ref false

let throw ?(formatter = Format.std_formatter) lexbuf =
  let print_error ~formatter idx error =
    flush_all ();
    let pos = Sedlexing.lexing_bytes_positions lexbuf in
    error_header ~formatter idx (Some pos);
    Format.fprintf formatter "%s\n@]@." error
  in
  function
  (* Warnings *)
  | Term.Ignored tm when Type.is_fun tm.Term.t ->
      flush_all ();
      warning_header ~formatter 1 tm.Term.t.Type.pos;
      Format.fprintf formatter
        "Trying to ignore a function,@ which is of type %s.@ Did you forget to \
         apply it to arguments?@]@."
        (Type.to_string tm.Term.t);
      if !strict then raise Error
  | Term.Ignored tm when Type.is_source tm.Term.t ->
      flush_all ();
      warning_header ~formatter 2 tm.Term.t.Type.pos;
      Format.fprintf formatter
        "This source is unused, maybe it needs to@ be connected to an \
         output.@]@.";
      if !strict then raise Error
  | Term.Ignored tm ->
      flush_all ();
      warning_header ~formatter 3 tm.Term.t.Type.pos;
      Format.fprintf formatter "This expression should have type unit.@]@.";
      if !strict then raise Error
  | Term.Unused_variable (s, pos) ->
      flush_all ();
      warning_header ~formatter 4 (Some pos);
      Format.fprintf formatter "Unused variable %s@]@." s;
      if !strict then raise Error
  (* Errors *)
  | Failure s when s = "lexing: empty token" ->
      print_error ~formatter 1 "Empty token";
      raise Error
  | Parser.Error | Parsing.Parse_error ->
      print_error ~formatter 2 "Parse error";
      raise Error
  | Term_base.Parse_error (pos, s) ->
      error_header ~formatter 3 (Some pos);
      Format.fprintf formatter "%s@]@." s;
      raise Error
  | Term.Unbound (pos, s) ->
      error_header ~formatter 4 pos;
      Format.fprintf formatter "Undefined variable %s@]@." s;
      raise Error
  | Repr.Type_error explain ->
      flush_all ();
      Repr.print_type_error ~formatter (error_header ~formatter 5) explain;
      raise Error
  | Term.No_label (f, lbl, first, x) ->
      let pos_f = Pos.Option.to_string f.Term.t.Type.pos in
      flush_all ();
      error_header ~formatter 6 x.Term.t.Type.pos;
      Format.fprintf formatter
        "Cannot apply that parameter because the function %s@ has %s@ %s!@]@."
        pos_f
        (if first then "no" else "no more")
        (if lbl = "" then "unlabeled argument"
         else Format.sprintf "argument labeled %S" lbl);
      raise Error
  | Term.Duplicate_label (pos, lbl) ->
      error_header ~formatter 6 pos;
      Format.fprintf formatter
        "Function has multiple arguments with the same label: %s@]@." lbl;
      raise Error
  | Error.Invalid_value (v, msg) ->
      error_header ~formatter 7 v.Value.pos;
      Format.fprintf formatter "Invalid value:@ %s@]@." msg;
      raise Error
  | Lang_error.Encoder_error (pos, s) ->
      error_header ~formatter 8 pos;
      Format.fprintf formatter "%s@]@." (String.capitalize_ascii s);
      raise Error
  | Failure s ->
      let bt = Printexc.get_backtrace () in
      print_error ~formatter 9 (Printf.sprintf "Failure: %s\n%s" s bt);
      raise Error
  | Error.Clock_conflict (pos, a, b) ->
      (* TODO better printing of clock errors: we don't have position
       *   information, use the source's ID *)
      error_header ~formatter 10 pos;
      Format.fprintf formatter
        "A source cannot belong to two clocks (%s,@ %s).@]@." a b;
      raise Error
  (* Error 11 used to be Clock_loop. *)
  | Term.Unsupported_encoder (pos, fmt) ->
      error_header ~formatter 12 pos;
      (if Sys.unix then
         Format.fprintf formatter
           "Unsupported encoder: %s.@ You must be missing an optional \
            dependency.@]@."
       else
         Format.fprintf formatter
           "Unsupported encoder: %s.@ Please note that, on windows, %%mp3, \
            %%vorbis and many other encoders are not available. Instead, you \
            should use the %%ffmpeg encoder.@]@.")
        fmt;
      raise Error
  | Term.Internal_error (pos, e) ->
      (* Bad luck, error 13 should never have happened. *)
      error_header ~formatter 13
        (try Some (Pos.List.to_pos pos) with _ -> None);
      let pos = Pos.List.to_string ~newlines:true pos in
      Format.fprintf formatter "Internal error: %s,@ stack:\n%s\n@]@." e pos;
      raise Error
  | Runtime_error.(Runtime_error { kind; msg; pos }) ->
      error_header ~formatter 14
        (try Some (Pos.List.to_pos pos) with _ -> None);
      let pos = Pos.List.to_string ~newlines:true pos in
      Format.fprintf formatter
        "Uncaught runtime error:@ type: %s,@ message: %s,@\nstack: %s\n@]@."
        kind
        (Lang_string.quote_string msg)
        pos;
      raise Error
  | Sedlexing.MalFormed -> print_error ~formatter 15 "Malformed UTF8 content."
  | Term.Missing_arguments (pos, args) ->
      let args =
        List.map
          (fun (l, t) -> (if l = "" then "" else l ^ " : ") ^ Type.to_string t)
          args
        |> String.concat ", "
      in
      error_header ~formatter 15 pos;
      Format.fprintf formatter
        "Missing arguments in function application: %s.@]@." args;
      raise Error
  | Type.Exists (pos, typ) ->
      error_header ~formatter 16 pos;
      Format.fprintf formatter "Type %s already exists.@]@." typ;
      raise Error
  | End_of_file -> raise End_of_file
  | e ->
      let bt = Printexc.get_backtrace () in
      error_header ~formatter (-1) None;
      Format.fprintf formatter "Exception raised: %s@.%s@]@."
        (Printexc.to_string e) bt;
      raise Error

let report lexbuf f =
  let throw = throw lexbuf in
  if !Term.conf_debug_errors then f ~throw ()
  else (try f ~throw () with exn -> throw exn)

(** {1 Parsing} *)

let program = Term_reducer.program

let interactive =
  MenhirLib.Convert.Simplified.traditional2revised Parser.interactive

let mk_expr ?fname processor lexbuf =
  let parsed_term = Term_reducer.mk_expr ?fname processor lexbuf in
  Term_reducer.to_term parsed_term

let from_string ~eval_mode expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  match eval_mode with
    | `Parse_only -> ()
    | `Eval config ->
        report lexbuf (fun ~throw () ->
            let parsed_term = Term_reducer.mk_expr program lexbuf in
            let expr = Term_reducer.to_term parsed_term in
            type_and_run ~config ~throw ~lib:false ~parsed_term expr)

let parse_with_lexbuf s =
  let gen =
    let pos = ref (-1) in
    let len = String.length s in
    fun () ->
      incr pos;
      if !pos < len then Some s.[!pos] else None
  in
  let lexbuf = Sedlexing.Utf8.from_gen gen in
  (mk_expr program lexbuf, lexbuf)

let parse s = fst (parse_with_lexbuf s)

let eval ~ignored ~ty s =
  let expr, lexbuf = parse_with_lexbuf s in
  let expr = Term.(make (`Cast { cast = expr; typ = ty })) in
  report lexbuf (fun ~throw () -> Typechecking.check ~throw ~ignored expr);
  Evaluation.eval expr

let interactive () =
  Format.printf
    "\n\
     Welcome to the liquidsoap interactive loop.\n\n\
     You may enter any sequence of expressions, terminated by \";;\".\n\
     Each input will be fully processed: parsing, type-checking,\n\
     evaluation (forces default types), output startup (forces default clock).\n\
     @.";
  (match !Hooks.log_path with
    | None -> ()
    | Some path ->
        Format.printf "Logs can be found in %s.\n@."
          (Lang_string.quote_string path));
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
    Format.printf "# %!";
    if
      try
        report lexbuf (fun ~throw () ->
            let expr = mk_expr interactive lexbuf in
            Typechecking.check ~throw ~ignored:false expr;
            Term.check_unused ~throw ~lib:true expr;
            ignore (Evaluation.eval_toplevel ~interactive:true expr));
        true
      with
        | End_of_file ->
            Format.printf "Bye bye!@.";
            false
        | Error -> true
        | e ->
            let e = Console.colorize [`bold] (Printexc.to_string e) in
            Format.printf "Exception: %s!@." e;
            true
    then loop ()
  in
  loop ()

let libs ?(error_on_no_stdlib = true) ?(deprecated = true) () =
  let stdlib = "stdlib.liq" in
  let dir = !Hooks.liq_libs_dir () in
  let file = Filename.concat dir stdlib in
  let libs =
    if not (Sys.file_exists file) then
      if error_on_no_stdlib then
        failwith "Could not find default stdlib.liq library!"
      else []
    else [file]
  in
  let file = Filename.concat (Filename.concat dir "extra") "deprecations.liq" in
  if deprecated && Sys.file_exists file then libs @ [file] else libs

let load_libs () =
  List.iter
    (fun fname ->
      let filename = Lang_string.home_unrelate fname in
      let ic = open_in filename in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let lexbuf = Sedlexing.Utf8.from_channel ic in
          Sedlexing.set_filename lexbuf fname;
          report lexbuf (fun ~throw () ->
              let parsed_term = Term_reducer.mk_expr ~fname program lexbuf in
              let expr = Term_reducer.to_term parsed_term in
              type_and_run ~throw
                ~config:
                  { fetch_cache = true; save_cache = true; eval = `Toplevel }
                ~lib:true ~parsed_term expr)))
    (libs ())
