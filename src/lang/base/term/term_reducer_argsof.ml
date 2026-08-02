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

(** Expand [%argsof(f)] in argument lists and [f(...@x)] in applications, by
    looking up the referenced function's arguments in the environment. *)

open Parsed_term
include Runtime_term
open Term_reducer_helpers
open Term_reducer_ty
open Term_reducer_pattern

let rec get_env_args ~pos t args =
  let get_arg_type t name =
    match (Type.deref t).Type.descr with
      | Type.Arrow (l, _) ->
          let _, _, t = List.find (fun (_, n, _) -> n = name) l in
          t
      | _ ->
          parse_error ~pos
            (Printf.sprintf
               "Cannot get argument type of %s, this is not a function, it has \
                type: %s."
               name (Type.to_string t))
  in
  List.map
    (fun (n, n', v) ->
      let t = mk_ty ~pos (get_arg_type t n).Type.descr in
      let as_variable = if n = n' then None else Some n' in
      {
        label = n;
        as_variable;
        typ = t;
        default = Option.map (term_of_value ~pos ~name:n t) v;
        pos = t.pos;
      })
    args

and term_of_value_base ~pos t v =
  let get_list_type () =
    match (Type.deref t).Type.descr with
      | Type.(List { t }) -> t
      | _ -> assert false
  in
  let get_tuple_type pos =
    match (Type.deref t).Type.descr with
      | Type.Tuple t -> List.nth t pos
      | _ -> assert false
  in
  let process_value ~t v =
    let mk_tm ?(flags = Flags.empty) term =
      mk ~flags ~t:(mk_ty ~pos t.Type.descr) term
    in
    match v with
      | Value.Int { value = i; flags } -> mk_tm ~flags (`Int i)
      | Value.Float { value = f } -> mk_tm (`Float f)
      | Value.Bool { value = b } -> mk_tm (`Bool b)
      | Value.String { value = s } -> mk_tm (`String s)
      | Value.Custom { value = g } -> mk_tm (`Custom g)
      | Value.List { value = l } ->
          mk_tm
            (`List (List.map (term_of_value_base ~pos (get_list_type ())) l))
      | Value.Tuple { value = l } ->
          mk_tm
            (`Tuple
               (List.mapi
                  (fun idx v -> term_of_value_base ~pos (get_tuple_type idx) v)
                  l))
      | Value.Null _ -> mk_tm `Null
      (* Ignoring env is not correct here but this is an internal operator
         so we have to trust that devs using it via %argsof now that they are doing. *)
      | Value.Fun { fun_args = args; fun_body = body } ->
          let body =
            mk
              ~t:(mk_ty ~pos body.t.Type.descr)
              ~methods:body.Term.methods body.Term.term
          in
          mk_tm
            (`Fun
               {
                 Term.name = None;
                 arguments = get_env_args ~pos t args;
                 body;
                 free_vars = None;
               })
      | _ -> assert false
  in
  let meths, _ = Type.split_meths t in
  let tm = process_value ~t v in
  mk ~t:tm.Term.t
    ~methods:
      (Methods.mapi
         (fun key meth ->
           let { Type.scheme = _, t } =
             List.find (fun { Type.meth } -> meth = key) meths
           in
           process_value ~t meth)
         Value.(methods v))
    tm.Term.term

and term_of_value ~pos ~name t v =
  try term_of_value_base ~pos t v
  with _ ->
    parse_error ~pos
      (Printf.sprintf "Argument %s: value %s cannot be represented as a term"
         name (Value.to_string v))

let builtin_args_of ~only ~except ~pos name =
  match Environment.get_builtin name with
    | Some ((_, t), Value.Fun { fun_args = args })
    | Some ((_, t), Value.FFI { ffi_args = args; _ }) ->
        let filtered_args = List.filter (fun (n, _, _) -> n <> "") args in
        let filtered_args =
          if only <> [] then
            List.map
              (fun n ->
                try List.find (fun (n', _, _) -> n = n') filtered_args
                with Not_found ->
                  parse_error ~pos
                    (Printf.sprintf
                       "Builtin %s does not have an argument named %s" name n))
              only
          else filtered_args
        in
        List.iter
          (fun n ->
            match List.find_opt (fun (n', _, _) -> n = n') args with
              | Some _ -> ()
              | None ->
                  parse_error ~pos
                    (Printf.sprintf
                       "Builtin %s does not have an argument named %s" name n))
          except;
        let filtered_args =
          List.filter (fun (n, _, _) -> not (List.mem n except)) filtered_args
        in
        get_env_args ~pos t filtered_args
    | Some _ ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not a function!" name)
    | None ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not registered!" name)

let args_of ~only ~except ~pos ~env name =
  match List.assoc_opt name env with
    | Some { term = `Fun { arguments } } ->
        let filtered_args =
          List.filter (fun { label } -> label <> "") arguments
        in
        let filtered_args =
          if only <> [] then
            List.map
              (fun n ->
                try List.find (fun { label } -> label = n) filtered_args
                with Not_found ->
                  parse_error ~pos
                    (Printf.sprintf "%s does not have an argument named %s" name
                       n))
              only
          else filtered_args
        in
        List.iter
          (fun n ->
            match List.find_opt (fun { label } -> label = n) filtered_args with
              | Some _ -> ()
              | None ->
                  parse_error ~pos
                    (Printf.sprintf "%s does not have an argument named %s" name
                       n))
          except;
        List.filter (fun { label } -> not (List.mem label except)) filtered_args
    | Some _ -> parse_error ~pos (Printf.sprintf "%s is not a function!" name)
    | None -> builtin_args_of ~only ~except ~pos name

let expand_argsof ~pos ~env ~to_term ~throw args =
  let anonymous_var_id = ref 0 in
  let mk_def, args =
    List.fold_left
      (fun (mk_def, args) -> function
        | `Argsof { only; except; source } ->
            (mk_def, List.rev (args_of ~pos ~env ~only ~except source) @ args)
        | `Term { label; as_variable; default; typ; annotations; pos } ->
            report_annotations ~throw ~pos annotations;
            let mk_def, as_variable =
              match as_variable with
                | None -> (mk_def, None)
                | Some { pat_entry = `PVar [v] } -> (mk_def, Some v)
                | Some pat ->
                    incr anonymous_var_id;
                    let v = Printf.sprintf "_ann_%d" !anonymous_var_id in
                    let mk_def def =
                      mk_def
                        (mk ~pos (pattern_reducer ~body:def ~pat (mk (`Var v))))
                    in
                    (mk_def, Some v)
            in
            ( mk_def,
              {
                label;
                as_variable;
                typ =
                  (match typ with
                    | None -> mk_var ()
                    | Some typ -> mk_parsed_ty ~env ~to_term typ);
                default = Option.map (to_term ~env) default;
                pos = Some (Pos.of_lexing_pos pos);
              }
              :: args ))
      ((fun b -> b), [])
      args
  in
  (mk_def, List.rev args)

let app_of ~pos ~only ~except ~env source =
  let args = args_of ~pos ~only ~except ~env source in
  List.map (fun { label } -> (label, mk ~t:(mk_var ~pos ()) (`Var label))) args

let expand_appof ~pos ~env ~to_term args =
  List.rev
    (List.fold_left
       (fun args -> function
         | `Argsof { only; except; source } ->
             List.rev (app_of ~pos ~only ~except ~env source) @ args
         | `Term (l, v) -> (l, to_term ~env v) :: args)
       [] args)
