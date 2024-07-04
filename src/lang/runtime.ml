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

type stdlib = { full_term : Term.t; checked_term : Term.t; env : Typing.env }
type append_stdlib = unit -> stdlib

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

let throw ?(formatter = Format.std_formatter) ?lexbuf () =
  let print_error ~formatter idx error =
    flush_all ();
    let pos =
      match lexbuf with
        | Some lexbuf ->
            Some (Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf))
        | None -> None
    in
    error_header ~formatter idx pos;
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
      error_header ~formatter 3 (Some (Pos.of_lexing_pos pos));
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
  | Typechecking.No_method (name, typ) ->
      error_header ~formatter 5 typ.Type.pos;
      Format.fprintf formatter
        "This value has type %s, it cannot have method %s.@]@."
        (Repr.string_of_type typ) name;
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
      error_header ~formatter 7 (Value.pos v);
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

(* This is not great but it works for now. The problem being that we are relying on exception
   raising and catching to transmit language error, translate them into human readable errors and
   optionally ignore them with a warning. But, in some cases, we still want to return afterward
   so the return value has to be something else than [unit] in those cases. Essentially, this means
   that [default] becomes [fun () -> raise Error] to keep typechecking consistent.. *)
let report :
      'a.
      ?lexbuf:Sedlexing.lexbuf ->
      ?default:(unit -> 'a) ->
      (throw:(exn -> unit) -> unit -> 'a) ->
      'a =
 fun ?lexbuf ?(default = fun () -> raise Error) f ->
  let throw = throw ?lexbuf () in
  if !Term.conf_debug_errors then f ~throw ()
  else (
    try f ~throw ()
    with exn ->
      throw exn;
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
        if Lazy.force Term.debug then Printf.eprintf "Type checking...\n%!";
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
                        report
                          ~default:(fun () -> raise Error)
                          (fun ~throw:_ () -> Term_reducer.to_term parsed_term)
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
            report
              ~default:(fun () -> ())
              (fun ~throw () -> Typechecking.check ?env ~throw checked_term));

        if Lazy.force Term.debug then
          Printf.eprintf "Checking for unused variables...\n%!";
        (* Check for unused variables, relies on types *)
        report
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
    report
      ~default:(fun () -> assert false)
      (fun ~throw:_ () ->
        if toplevel then Evaluation.eval_toplevel ast else Evaluation.eval ast)
  in
  if Lazy.force Term.debug then Printf.eprintf "Evaluating...\n%!";
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
  report
    ~default:(fun () -> raise Error)
    (fun ~throw:_ () ->
      let parsed_term = Term_reducer.mk_expr ?fname processor lexbuf in
      (parsed_term, Term_reducer.to_term parsed_term))

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
        report ~lexbuf
          ~default:(fun () -> ())
          (fun ~throw () ->
            let _, expr = mk_expr interactive lexbuf in
            Typechecking.check ~throw expr;
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

let libs ?(stdlib = "stdlib.liq") ?(error_on_no_stdlib = true)
    ?(deprecated = true) () =
  let dir = !Hooks.liq_libs_dir () in
  let file = Filename.concat dir stdlib in
  let libs =
    if not (Sys.file_exists file) then
      if error_on_no_stdlib then
        failwith (Printf.sprintf "Could not find default %s library!" stdlib)
      else []
    else [file]
  in
  let file = Filename.concat (Filename.concat dir "extra") "deprecations.liq" in
  if deprecated && Sys.file_exists file then libs @ [file] else libs

let load_libs ?stdlib () =
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
            report
              ~default:(fun () -> raise Error)
              (fun ~throw:_ () -> Term_reducer.mk_expr ~fname program lexbuf)
          in
          let term =
            type_term ~name:"stdlib" ~trim:true ~cache:true ~lib:true
              parsed_term
          in
          ignore (eval_term ~name:"stdlib" ~toplevel:true term)))
    (libs ?stdlib ())
