(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

type stdlib = { full_term : Term.t; checked_term : Term.t; env : Typing.env }
type append_stdlib = unit -> stdlib

(** {1 Error reporting} *)

let error = Console.colorize [`red; `bold] "Error"
let warning = Console.colorize [`magenta; `bold] "Warning"
let position pos = Console.colorize [`bold] (String.capitalize_ascii pos)
let strict = ref false
let deprecated = ref true

let () =
  Array.iter
    (function
      | "--disable-deprecated" -> deprecated := false
      | "--enable-deprecated" -> deprecated := true
      | _ -> ())
    Sys.argv

let raw_errors = ref false

let error_header ~formatter idx pos =
  let e = Option.value (Repr.excerpt_opt pos) ~default:"" in
  let pos = Pos.Option.to_string pos in
  Format.fprintf formatter "@[%s:\n%s\n%s %i: " (position pos) e error idx

let warning_header ~formatter idx pos =
  let e = Option.value (Repr.excerpt_opt pos) ~default:"" in
  let pos = Pos.Option.to_string pos in
  Format.fprintf formatter "@[%s:\n%s\n%s %i: " (position pos) e warning idx

(* Error printers contributed by layers above the language, e.g. clock errors
   from the streaming core. Consulted before the generic fallback; a printer
   returns [true] when it has handled the exception. *)
let error_printers = Atomic.make []

let on_error_print fn =
  Atomic.set error_printers (fn :: Atomic.get error_printers)

(** Exception raised by report_error after an error has been displayed. Unknown
    errors are re-raised, so that their content is not totally lost. *)
exception Error

exception Warning of string

let () =
  Printexc.register_printer (function
    | Error -> Some "Liquidsoap Error"
    | Warning s -> Some (Printf.sprintf "Warning: %s" s)
    | _ -> None)

type diagnostic = {
  severity : [ `Warning of string | `Error ];
  code : int;
  pos : Pos.t option;
  message : Format.formatter -> unit;
}

let warning_diagnostic ~strict_message code pos message =
  Some { severity = `Warning strict_message; code; pos; message }

let error_diagnostic code pos message =
  Some { severity = `Error; code; pos; message }

let rec describe ~lexbuf ~bt = function
  | Term_preprocessor.Includer_error (exn, lexbuf, bt) ->
      describe ~lexbuf:(Some lexbuf) ~bt exn
  | exn -> (
      let lexbuf_pos () =
        match lexbuf with
          | Some lexbuf ->
              Some (Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf))
          | None -> None
      in
      let lexbuf_error code text =
        error_diagnostic code (lexbuf_pos ()) (fun formatter ->
            Format.fprintf formatter "%s\n@]@." text)
      in
      match exn with
        (* Warnings *)
        | Term.Ignored tm when Type.is_fun tm.Term.t ->
            let typ = Type.to_string tm.Term.t in
            warning_diagnostic
              ~strict_message:
                (Printf.sprintf
                   "Trying to ignore a function, which is of type %s. Did you \
                    forget to apply it to arguments?"
                   typ) 1 tm.Term.t.Type.pos (fun formatter ->
                Format.fprintf formatter
                  "Trying to ignore a function,@ which is of type %s.@ Did you \
                   forget to apply it to arguments?@]@."
                  typ)
        | Term.Ignored tm when Type.is_source tm.Term.t ->
            warning_diagnostic
              ~strict_message:
                "This source is unused, maybe it needs to be connected to an \
                 output."
              2 tm.Term.t.Type.pos (fun formatter ->
                Format.fprintf formatter
                  "This source is unused, maybe it needs to@ be connected to \
                   an output.@]@.")
        | Term.Ignored tm ->
            warning_diagnostic
              ~strict_message:"This expression should have type unit." 3
              tm.Term.t.Type.pos (fun formatter ->
                Format.fprintf formatter
                  "This expression is returning a value that is ignored. Do \
                   you need to use its return value? If not, you can use the \
                   `ignore()` operator.@]@.")
        | Term.Unused_variable (s, pos) ->
            warning_diagnostic
              ~strict_message:(Printf.sprintf "Unused variable %s" s)
              4 (Some pos) (fun formatter ->
                Format.fprintf formatter "Unused variable %s@]@." s)
        | Term.Deprecated (s, pos) ->
            warning_diagnostic
              ~strict_message:(Printf.sprintf "Deprecated: %s" s) 5 (Some pos)
              (fun formatter -> Format.fprintf formatter "Deprecated: %s@]@." s)
        | Typechecking.Top_level_override (s, pos) ->
            warning_diagnostic
              ~strict_message:
                (Printf.sprintf "Top-level variable %s is overridden!" s) 6 pos
              (fun formatter ->
                Format.fprintf formatter
                  "Top-level variable %s is overridden!@]@." s)
        (* Errors *)
        | Failure s when s = "lexing: empty token" ->
            lexbuf_error 1 "Empty token"
        | Parser.Error | Parsing.Parse_error -> lexbuf_error 2 "Parse error"
        | Term.Parse_error (pos, s) ->
            error_diagnostic 3
              (Some (Pos.of_lexing_pos pos))
              (fun formatter -> Format.fprintf formatter "%s@]@." s)
        | Term.Unbound (pos, s) ->
            error_diagnostic 4 pos (fun formatter ->
                Format.fprintf formatter "Undefined variable %s@]@." s)
        | Repr.Type_error ((_, ta, _, _, _) as explain) ->
            (* [print_type_error] prints the header itself, at [ta]'s position. *)
            error_diagnostic 5 ta.Type.pos (fun formatter ->
                Repr.print_type_error ~formatter (fun _ -> ()) explain)
        | Typechecking.No_method (name, typ) ->
            error_diagnostic 5 typ.Type.pos (fun formatter ->
                Format.fprintf formatter
                  "This value has type %s, it cannot have method %s.@]@."
                  (Repr.string_of_type typ) name)
        | Term.No_label (f, lbl, first, x) ->
            let pos_f = Pos.Option.to_string f.Term.t.Type.pos in
            error_diagnostic 6 x.Term.t.Type.pos (fun formatter ->
                Format.fprintf formatter
                  "Cannot apply that parameter because the function %s@ has \
                   %s@ %s!@]@."
                  pos_f
                  (if first then "no" else "no more")
                  (if lbl = "" then "unlabeled argument"
                   else Format.sprintf "argument labeled %S" lbl))
        | Term.Duplicate_label (pos, lbl) ->
            error_diagnostic 6 pos (fun formatter ->
                Format.fprintf formatter
                  "Function has multiple arguments with the same label: %s@]@."
                  lbl)
        | Error.Invalid_value (v, msg, stack) ->
            error_diagnostic 7 (Value.pos v) (fun formatter ->
                Format.fprintf formatter "Invalid value:@ %s" msg;
                if stack <> [] then
                  Format.fprintf formatter
                    "@\n\
                     This value was passed through the following call stack:@\n\
                     %s"
                    (Pos.List.to_string ~newlines:true stack);
                Format.fprintf formatter "@]@.")
        | Lang_error.Encoder_error (pos, s) ->
            error_diagnostic 8 pos (fun formatter ->
                Format.fprintf formatter "%s@]@." (String.capitalize_ascii s))
        | Failure s ->
            lexbuf_error 9
              (Printf.sprintf "Failure: %s\n%s" s
                 (Printexc.raw_backtrace_to_string bt))
        | Term.Unsupported_encoder (pos, fmt) ->
            error_diagnostic 12 pos (fun formatter ->
                if Sys.unix then
                  Format.fprintf formatter
                    "Unsupported encoder: %s.@ You must be missing an optional \
                     dependency.@]@."
                    fmt
                else
                  Format.fprintf formatter
                    "Unsupported encoder: %s.@ Please note that, on windows, \
                     %%mp3, %%vorbis and many other encoders are not \
                     available. Instead, you should use the %%ffmpeg \
                     encoder.@]@."
                    fmt)
        | Term.Internal_error (pos, e) ->
            (* Bad luck, error 13 should never have happened. *)
            error_diagnostic 13
              (try Some (Pos.List.to_pos pos) with _ -> None)
              (fun formatter ->
                Format.fprintf formatter "Internal error: %s,@ stack:\n%s\n@]@."
                  e
                  (Pos.List.to_string ~newlines:true pos))
        | Runtime_error.(Runtime_error { kind; msg; pos }) ->
            error_diagnostic 14
              (try Some (Pos.List.to_pos pos) with _ -> None)
              (fun formatter ->
                Format.fprintf formatter
                  "Uncaught runtime error:@ type: %s,@ message: %s,@\n\
                   stack: %s\n\
                   @]@."
                  kind
                  (Lang_string.quote_string msg)
                  (Pos.List.to_string ~newlines:true pos))
        | Sedlexing.MalFormed -> lexbuf_error 15 "Malformed UTF8 content."
        | Term.Missing_arguments (pos, args) ->
            let args =
              List.map
                (fun (l, t) ->
                  (if l = "" then "" else l ^ " : ") ^ Type.to_string t)
                args
              |> String.concat ", "
            in
            error_diagnostic 15 pos (fun formatter ->
                Format.fprintf formatter
                  "Missing arguments in function application: %s.@]@." args)
        | _ -> None)

let rec throw ?(formatter = Format.err_formatter) ~lexbuf ~bt () = function
  | exn when !raw_errors -> Printexc.raise_with_backtrace exn bt
  | Term_preprocessor.Includer_error (exn, lexbuf, bt) ->
      throw ~formatter ~lexbuf:(Some lexbuf) ~bt () exn
  | End_of_file -> Printexc.raise_with_backtrace End_of_file bt
  | exn -> (
      flush_all ();
      match describe ~lexbuf ~bt exn with
        | Some { severity; code; pos; message } -> (
            (match severity with
              | `Warning _ -> warning_header ~formatter code pos
              | `Error -> error_header ~formatter code pos);
            message formatter;
            match (severity, exn) with
              | `Warning strict_message, _ ->
                  if !strict then
                    Printexc.raise_with_backtrace (Warning strict_message) bt
              (* A malformed UTF-8 error does not stop processing. *)
              | `Error, Sedlexing.MalFormed -> ()
              | `Error, _ -> Printexc.raise_with_backtrace Error bt)
        | None ->
            if
              not
                (List.exists
                   (fun print -> print ~formatter exn)
                   (Atomic.get error_printers))
            then (
              error_header ~formatter (-1) None;
              Format.fprintf formatter "Exception raised: %s@.%s@]@."
                (Printexc.to_string exn)
                (Printexc.raw_backtrace_to_string bt));
            Printexc.raise_with_backtrace Error bt)

(* This is not great but it works for now. The problem being that we are relying on exception
   raising and catching to transmit language error, translate them into human readable errors and
   optionally ignore them with a warning. But, in some cases, we still want to return afterward
   so the return value has to be something else than [unit] in those cases. Essentially, this means
   that [default] becomes [fun () -> raise Error] to keep typechecking consistent.. *)
let report :
    'a.
    ?default:(unit -> 'a) ->
    lexbuf:Sedlexing.lexbuf option ->
    (throw:(bt:Printexc.raw_backtrace -> exn -> unit) -> unit -> 'a) ->
    'a =
 fun ?(default = fun () -> raise Error) ~lexbuf f ->
  let throw = throw ~lexbuf () in
  if !Term.conf_debug_errors then f ~throw ()
  else (
    try f ~throw ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      throw ~bt exn;
      default ())

let type_term ?name ?stdlib ?term ?ty ?cache_dirtype ~cache ~trim ~lib
    parsed_term =
  let cached_term =
    if cache then
      Term_cache.retrieve ?name ?dirtype:cache_dirtype ~trim parsed_term
    else None
  in
  match cached_term with
    | Some term -> term
    | None ->
        if Lazy.Mutexed.force Term.debug then
          Printf.eprintf "Type checking...\n%!";
        (* Type checking *)
        let time fn =
          match name with
            | None -> fn ()
            | Some name ->
                Startup.time (Printf.sprintf "Typechecking %s" name) fn
        in
        let full_term, checked_term, env =
          match stdlib with
            | Some fn ->
                let { full_term; checked_term; env } = fn () in
                (full_term, checked_term, Some env)
            | None ->
                let term =
                  match term with
                    | None ->
                        report ~lexbuf:None
                          ~default:(fun () -> raise Error)
                          (fun ~throw () ->
                            Term_reducer.to_term ~throw parsed_term)
                    | Some tm -> tm
                in
                (term, term, None)
        in
        let checked_term =
          match ty with
            | None -> checked_term
            | Some typ ->
                Term.make
                  ~pos:(Pos.of_lexing_pos parsed_term.Parsed_term.pos)
                  (`Cast { cast = checked_term; typ })
        in
        time (fun () ->
            report ~lexbuf:None
              ~default:(fun () -> ())
              (fun ~throw () ->
                Typechecking.check ?env
                  ~check_top_level_override:(stdlib <> None) ~throw checked_term));

        if Lazy.Mutexed.force Term.debug then
          Printf.eprintf "Checking for unused variables...\n%!";
        (* Check for unused variables, relies on types *)
        report ~lexbuf:None
          ~default:(fun () -> ())
          (fun ~throw () -> Term.check_unused ~throw ~lib full_term);
        let full_term =
          if trim then Term_trim.trim_term full_term else full_term
        in
        if cache then
          Term_cache.cache ?dirtype:cache_dirtype ~trim ~parsed_term full_term;
        full_term

let eval_term ?name ~toplevel ast =
  let eval () =
    report ~lexbuf:None
      ~default:(fun () -> assert false)
      (fun ~throw:_ () ->
        if toplevel then Evaluation.eval_toplevel ast else Evaluation.eval ast)
  in
  if Lazy.Mutexed.force Term.debug then Printf.eprintf "Evaluating...\n%!";
  match name with
    | None -> eval ()
    | Some name ->
        Startup.time
          (Printf.sprintf "Evaluating %s%s" name
             (if toplevel then " at toplevel" else ""))
          eval

(** {1 Parsing} *)

let program = Term_reducer.program

let interactive =
  MenhirLib.Convert.Simplified.traditional2revised Parser.interactive

let mk_expr ?fname processor lexbuf =
  report ~lexbuf:(Some lexbuf)
    ~default:(fun () -> raise Error)
    (fun ~throw () ->
      let parsed_term = Term_reducer.mk_expr ?fname processor lexbuf in
      (parsed_term, Term_reducer.to_term ~throw parsed_term))

let parse s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  mk_expr program lexbuf

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
        report ~lexbuf:(Some lexbuf)
          ~default:(fun () -> ())
          (fun ~throw () ->
            let _, expr = mk_expr interactive lexbuf in
            Typechecking.check ~throw ~check_top_level_override:true expr;
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

let libs ?(error_on_no_stdlib = true) ?(deprecated = true) ~stdlib () =
  let libs =
    if not (Sys.file_exists stdlib) then
      if error_on_no_stdlib then
        failwith (Printf.sprintf "Could not find default %s library!" stdlib)
      else []
    else [stdlib]
  in
  let dir = Filename.dirname stdlib in
  let file = Filename.concat (Filename.concat dir "extra") "deprecations.liq" in
  if deprecated && Sys.file_exists file then libs @ [file] else libs

let load_libs ~stdlib () =
  List.iter
    (fun fname ->
      let filename = Lang_string.home_unrelate fname in
      let ic = open_in filename in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let lexbuf = Sedlexing.Utf8.from_channel ic in
          Sedlexing.set_filename lexbuf fname;
          let parsed_term =
            report ~lexbuf:(Some lexbuf)
              ~default:(fun () -> raise Error)
              (fun ~throw:_ () -> Term_reducer.mk_expr ~fname program lexbuf)
          in
          let term =
            type_term ~name:"stdlib" ~trim:true ~cache:true ~lib:true
              parsed_term
          in
          ignore (eval_term ~name:"stdlib" ~toplevel:true term)))
    (libs ~stdlib ())
