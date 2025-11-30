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

(** {1 Evaluation environment} *)

module Env = Value.Methods

let type_environment : Type.scheme Env.t ref = ref Env.empty
let value_environment : Value.t Env.t ref = ref Env.empty
let default_environment () = Env.bindings !value_environment
let default_typing_environment () = Env.bindings !type_environment

(* Just like builtins but we register a.b under the name "a.b" (instead of
   adding a field b to a). It is used only for [has_builtins] and
   [get_builtins]. *)
let flat_enviroment : (string * (Type.scheme * Value.t)) list ref = ref []

let clear_toplevel_environments () =
  type_environment := Env.empty;
  flat_enviroment := []

let has_builtin name = List.mem_assoc name !flat_enviroment
let get_builtin name = List.assoc_opt name !flat_enviroment

let add_builtin ?(override = false) ?(register = true) ?doc name ((g, t), v) =
  if register && doc <> None then
    Doc.Value.add (String.concat "." name) (Option.get doc);
  flat_enviroment := (String.concat "." name, ((g, t), v)) :: !flat_enviroment;
  match name with
    | [name] ->
        (* Don't allow overriding builtins. *)
        if (not override) && Env.mem name !type_environment then
          failwith ("Trying to override builtin " ^ name);
        type_environment := Env.add name (g, t) !type_environment;
        value_environment := Env.add name v !value_environment
    | x :: ll ->
        let (g0, t0), v0 =
          try (Env.find x !type_environment, Env.find x !value_environment)
          with Not_found -> failwith ("Could not find builtin variable " ^ x)
        in
        (* x.l1.l2.l3 = v means
           x = (x where l1 = (x.l1 where l2 = (x.l1.l2 where l3 = v)))
        *)
        (* Inductive step: we compute the new type scheme and value of
           x.l1...li. The variable prefix contains [li; ...; l1] and the second
           argument is [li+1; ...; ln]. *)
        let rec aux (g0, t0) v0 = function
          | l :: [] ->
              let t =
                Type.make ?pos:t.Type.pos
                  Type.(
                    Meth
                      {
                        meth =
                          {
                            meth = l;
                            optional = false;
                            scheme = (g, t);
                            doc = { meth_descr = ""; category = `Method };
                            json_name = None;
                          };
                        t = t0;
                      })
              in
              ((g0, t), Value.map_methods v0 (Methods.add l v))
          | l :: ll ->
              let (vg, vt), v = aux (Type.invoke t0 l) (Value.invoke v0 l) ll in
              let t =
                Type.make ?pos:t.Type.pos
                  Type.(
                    Meth
                      {
                        meth =
                          {
                            meth = l;
                            optional = false;
                            scheme = (vg, vt);
                            doc = { meth_descr = ""; category = `Method };
                            json_name = None;
                          };
                        t = t0;
                      })
              in
              ((g0, t), Value.map_methods v0 (Methods.add l v))
          | [] -> ((g, t), v)
        in
        let (g, t), v = aux (g0, t0) v0 ll in
        assert (g == g0);
        type_environment := Env.add x (g0, t) !type_environment;
        value_environment := Env.add x v !value_environment
    | [] -> assert false

(** Declare a module. *)
let add_module name =
  (* Ensure that it does not already exist. *)
  (match name with
    | [] -> assert false
    | [x] ->
        if Env.mem x !type_environment then
          failwith ("Module " ^ String.concat "." name ^ " already declared")
    | x :: mm -> (
        let mm = List.rev mm in
        let l = List.hd mm in
        let mm = List.rev (List.tl mm) in
        let e =
          try Value.invokes (Env.find x !value_environment) mm
          with _ ->
            failwith
              ("Could not find the parent module of " ^ String.concat "." name)
        in
        try
          ignore (Value.invoke e l);
          failwith ("Module " ^ String.concat "." name ^ " already exists")
        with _ -> ()));
  add_builtin ~register:false name (([], Type.make Type.unit), Value.(make unit))
