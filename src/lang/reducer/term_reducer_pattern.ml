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

(** Reduce destructuring patterns ([let [a, b, ...c] = ...], records, tuples) to
    plain [let] bindings and list/record accesses. *)

open Parsed_term
include Runtime_term
open Term_reducer_helpers

(* Names the desugaring binds are spelled with a digit right after the leading
   underscore. `Lexer.var_lit` requires an alphabetic character there, so no
   script can write one, and user code cannot shadow what the sugar expands to. *)
let pat_var_name =
  let idx = ref 1 in
  fun () ->
    incr idx;
    Printf.sprintf "_%d_pat" !idx

let rec pattern_reducer (pat : Parsed_term.pattern) =
  let mk = mk ~pos:pat.pat_pos in
  match pat.pat_entry with
    | `PVar _ as pat ->
        fun ?doc ?(replace = false) ~body def ->
          `Let { doc; replace; pat; gen = []; def; body }
    | `PTuple l ->
        fun ?doc ?(replace = false) ~body def ->
          let vars, body =
            List.fold_left
              (fun (vars, body) pat ->
                match pat.pat_entry with
                  | `PVar [var] -> (var :: vars, body)
                  (* let (<var>, pattern, ...) = .. in .. becomes:
                     let (<var>, _1_pat, ...) = .. in
                     let pattern = _1_pat in
                     .. *)
                  | _ ->
                      let var = pat_var_name () in
                      let mk_term = pattern_reducer pat in
                      let body = mk (mk_term ~body (mk (`Var var))) in
                      (var :: vars, body))
              ([], body) l
          in
          `Let
            { doc; replace; pat = `PTuple (List.rev vars); gen = []; def; body }
    (* let [x, y, ...spread, z, t] = ... in ... becomes:
       let _1_pat = ... in
       let _2_pat = list.length(l) in
       if _2_pat < 4 then error.raise(error.register("not_found", ...)) end
       let x = list.nth(0, _1_pat) in
       let y = list.nth(1, _1_pat) in
       let spread = list.slice(offset=2, length=_2_pat-4) in
       let z = list.nth(_2_pat-1, l) in
       let t = list.nth(_2_pat-2, l) in
       ... *)
    | `PList (prefix, spread, suffix) ->
        fun ?doc ?replace ~body def ->
          let list_var_name = pat_var_name () in
          let list_var = mk (`Var list_var_name) in
          let list = mk (`Var "list") in
          let nth_op =
            mk (`Invoke { invoked = list; meth = "nth"; invoke_default = None })
          in
          let len_op =
            mk
              (`Invoke
                 { invoked = list; meth = "length"; invoke_default = None })
          in
          let len_app = mk (`App (len_op, [("", list_var)])) in
          let len_var_name = pat_var_name () in
          let len_var = mk (`Var len_var_name) in
          let minus = mk (`Var "-") in
          let len_minus pos =
            mk (`App (minus, [("", len_var); ("", mk (`Int pos))]))
          in
          let body, suffix_len =
            match suffix with
              | [] -> (body, 0)
              | suffix ->
                  let _, body =
                    List.fold_left
                      (fun (idx, body) pat ->
                        let mk_term = pattern_reducer pat in
                        let body =
                          mk
                            (mk_term ~body
                               (mk
                                  (`App
                                     ( nth_op,
                                       [("", list_var); ("", len_minus idx)] ))))
                        in
                        (idx + 1, body))
                      (1, body) (List.rev suffix)
                  in
                  (body, List.length suffix)
          in
          let body, prefix_len =
            match prefix with
              | [] -> (body, 0)
              | prefix ->
                  let _, body =
                    List.fold_left
                      (fun (idx, body) pat ->
                        let mk_term = pattern_reducer pat in
                        let body =
                          mk
                            (mk_term ~body
                               (Term.make
                                  (`App
                                     ( nth_op,
                                       [("", list_var); ("", mk (`Int idx))] ))))
                        in
                        (idx + 1, body))
                      (0, body) prefix
                  in
                  (body, List.length prefix)
          in
          let body =
            match spread with
              | None -> body
              | Some (pos, var) ->
                  let op =
                    mk
                      (`Invoke
                         {
                           invoked = list;
                           meth = "slice";
                           invoke_default = None;
                         })
                  in
                  let def =
                    mk
                      (`App
                         ( op,
                           [
                             ("offset", mk (`Int prefix_len));
                             ("length", len_minus (prefix_len + suffix_len));
                             ("", list_var);
                           ] ))
                  in
                  let mk_term =
                    pattern_reducer { pat_pos = pos; pat_entry = `PVar [var] }
                  in
                  mk (mk_term ~body def)
          in
          let body =
            let if_op = mk (`Var "if") in
            let lt_op = mk (`Var "<") in
            let condition =
              mk
                (`App
                   ( lt_op,
                     [("", len_var); ("", mk (`Int (prefix_len + suffix_len)))]
                   ))
            in
            let error = mk (`Var "error") in
            let raise =
              mk
                (`Invoke
                   { invoked = error; meth = "raise"; invoke_default = None })
            in
            let register =
              mk
                (`Invoke
                   { invoked = error; meth = "register"; invoke_default = None })
            in
            let not_found =
              mk (`App (register, [("", mk (`String "not_found"))]))
            in
            let _then =
              mk_fun ~pos:pat.pat_pos []
                (mk
                   (`App
                      ( raise,
                        [
                          ("", not_found);
                          ( "",
                            mk
                              (`String
                                 "List value does not have enough elements to \
                                  fit the extraction pattern!") );
                        ] )))
            in
            let check_len =
              mk
                (`App
                   ( if_op,
                     [
                       ("", condition);
                       ("then", _then);
                       ("else", mk_fun ~pos:pat.pat_pos [] (mk (`Tuple [])));
                     ] ))
            in
            mk (`Seq (check_len, body))
          in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [len_var_name] }
          in
          let body = mk (mk_term ~body len_app) in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [list_var_name] }
          in
          mk_term ?doc ?replace ~body def
    (* let <pat>.{m = <pat'>; p?; q } = .. in .. becomes:
       let _1_pat = .. in
       let _2_pat = _.m in
       let <pat'> = _2_pat in
       let p = _1_pat?.p in
       let p = _1_pat.q in
       let <pat> = _1_pat in
       ... *)
    | `PMeth (base, meths) ->
        fun ?doc ?replace ~body def ->
          let base_var_name = pat_var_name () in
          let base_var = mk (`Var base_var_name) in
          let body =
            List.fold_left
              (fun body (name, default) ->
                let invoke invoke_default =
                  mk
                    (`Invoke { invoked = base_var; meth = name; invoke_default })
                in
                match default with
                  | `None ->
                      let mk_term =
                        pattern_reducer { pat with pat_entry = `PVar [name] }
                      in
                      mk (mk_term ~body (invoke None))
                  | `Nullable ->
                      let mk_term =
                        pattern_reducer { pat with pat_entry = `PVar [name] }
                      in
                      mk (mk_term ~body (invoke (Some (mk `Null))))
                  | `Pattern pat ->
                      let mk_term = pattern_reducer pat in
                      mk (mk_term ~body (invoke None)))
              body (List.rev meths)
          in
          let body =
            match base with
              | None -> body
              | Some pat ->
                  let mk_term = pattern_reducer pat in
                  let body_var = mk (`Var base_var_name) in
                  mk
                    (mk_term ~body
                       (mk
                          (`Hide
                             (body_var, List.map (fun (name, _) -> name) meths))))
          in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [base_var_name] }
          in
          mk_term ?doc ?replace ~body def

let pattern_reducer ?doc ?replace ~body ~pat def =
  pattern_reducer ~body pat ?doc ?replace def

(** Time intervals *)
