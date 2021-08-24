(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let () = Lang.apply_fun := Evaluation.apply

let type_and_run ~throw ~lib ast =
  Clock.collect_after (fun () ->
      if Lazy.force Term.debug then Printf.eprintf "Type checking...\n%!";
      (* Type checking *)
      Typechecking.check ~throw ~ignored:true ast;

      if Lazy.force Term.debug then
        Printf.eprintf "Checking for unused variables...\n%!";
      (* Check for unused variables, relies on types *)
      Term.check_unused ~throw ~lib ast;
      if Lazy.force Term.debug then Printf.eprintf "Evaluating...\n%!";
      ignore (Evaluation.eval_toplevel ast))

(** {1 Parsing} *)

let mk_expr ?fname ~pwd processor lexbuf =
  let processor = MenhirLib.Convert.Simplified.traditional2revised processor in
  let tokenizer = Preprocessor.mk_tokenizer ?fname ~pwd lexbuf in
  let tokenizer () =
    let token, (startp, endp) = tokenizer () in
    (token, startp, endp)
  in
  processor tokenizer

let from_in_channel ?fname ?(dir = Unix.getcwd ()) ?(parse_only = false) ~ns
    ~lib in_chan =
  let lexbuf = Sedlexing.Utf8.from_channel in_chan in
  begin
    match ns with
    | Some ns -> Sedlexing.set_filename lexbuf ns
    | None -> ()
  end;
  try
    Error.report lexbuf (fun ~throw () ->
        let expr = mk_expr ?fname ~pwd:dir Parser.program lexbuf in
        if not parse_only then type_and_run ~throw ~lib expr)
  with Error.Error -> exit 1

let from_file ?parse_only ~ns ~lib filename =
  let ic = open_in filename in
  let fname = Utils.home_unrelate filename in
  from_in_channel ~fname
    ~dir:(Filename.dirname filename)
    ?parse_only ~ns ~lib ic;
  close_in ic

let load_libs ?(error_on_no_stdlib = true) ?parse_only ?(deprecated = true) () =
  let dir = Configure.liq_libs_dir in
  let file = Filename.concat dir "stdlib.liq" in
  if not (Sys.file_exists file) then (
    if error_on_no_stdlib then
      failwith "Could not find default stdlib.liq library!")
  else from_file ?parse_only ~ns:(Some file) ~lib:true file;
  let file = Filename.concat dir "deprecations.liq" in
  if deprecated && Sys.file_exists file then
    from_file ?parse_only ~ns:(Some file) ~lib:true file

let from_file = from_file ~ns:None

let from_string ?parse_only ~lib expr =
  let i, o = Unix.pipe ~cloexec:true () in
  let i = Unix.in_channel_of_descr i in
  let o = Unix.out_channel_of_descr o in
  output_string o expr;
  close_out o;
  from_in_channel ?parse_only ~ns:None ~lib i;
  close_in i

let eval s =
  try
    let lexbuf = Sedlexing.Utf8.from_string s in
    let expr = mk_expr ~pwd:"/nonexistent" Parser.program lexbuf in
    Clock.collect_after (fun () ->
        Error.report lexbuf (fun ~throw () ->
            Typechecking.check ~throw ~ignored:false expr);
        Some (Evaluation.eval expr))
  with e ->
    Printf.eprintf "Evaluating %S failed: %s!" s (Printexc.to_string e);
    None

let from_in_channel ?parse_only ~lib x =
  from_in_channel ?parse_only ~ns:None ~lib x

let interactive () =
  Format.printf
    "\n\
     Welcome to the liquidsoap interactive loop.\n\n\
     You may enter any sequence of expressions, terminated by \";;\".\n\
     Each input will be fully processed: parsing, type-checking,\n\
     evaluation (forces default types), output startup (forces default clock).\n\
     @.";
  if Dtools.Log.conf_file#get then
    Format.printf "Logs can be found in %s.\n@."
      (Utils.escape_utf8 Dtools.Log.conf_file_path#get);
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
        Error.report lexbuf (fun ~throw () ->
            let expr =
              mk_expr ~pwd:(Unix.getcwd ()) Parser.interactive lexbuf
            in
            Typechecking.check ~throw ~ignored:false expr;
            Term.check_unused ~throw ~lib:true expr;
            Clock.collect_after (fun () ->
                ignore (Evaluation.eval_toplevel ~interactive:true expr)));
        true
      with
        | End_of_file ->
            Format.printf "Bye bye!@.";
            false
        | Error.Error -> true
        | e ->
            let e = Console.colorize [`white; `bold] (Printexc.to_string e) in
            Format.printf "Exception: %s!@." e;
            true
    then loop ()
  in
  loop ();
  Tutils.shutdown 0
