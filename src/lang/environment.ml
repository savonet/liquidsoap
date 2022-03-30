(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let builtins : (Type.scheme * Term.Value.t) Plug.plug =
  Plug.create ~duplicates:false ~doc:"scripting values" "scripting values"

(* Environment for builtins. *)
let builtins_env : (string * (Type.scheme * Term.Value.t)) list ref = ref []
let default_environment () = !builtins_env

let default_typing_environment () =
  List.map (fun (x, (t, _)) -> (x, t)) !builtins_env

let add_builtin ?(override = false) ?(register = true) ?doc name ((g, t), v) =
  if register then builtins#register ?doc (String.concat "." name) ((g, t), v);
  match name with
    | [name] ->
        (* Don't allow overriding builtins. *)
        if (not override) && List.mem_assoc name !builtins_env then
          failwith ("Trying to override builtin " ^ name);
        builtins_env := (name, ((g, t), v)) :: !builtins_env
    | x :: ll ->
        let (g0, t0), xv =
          try List.assoc x !builtins_env
          with Not_found -> failwith ("Could not find builtin variable " ^ x)
        in
        (* x.l1.l2.l3 = v means
           x = (x where l1 = (x.l1 where l2 = (x.l1.l2 where l3 = v)))
        *)
        (* Inductive step: we compute the new type scheme and value of
           x.l1...li. The variable prefix contains [li; ...; l1] and the second
           argument is [li+1; ...; ln]. *)
        let rec aux prefix = function
          | l :: ll ->
              (* Previous type scheme for x.l1...li. *)
              let vg, vt = Type.invokes t0 (List.rev prefix) in
              (* Previous value of x.l1...li.  *)
              let v = Term.Value.invokes xv (List.rev prefix) in
              (* Updated value of x.l1...li+1. *)
              let (lvg, lvt), lv = aux (l :: prefix) ll in
              (* Updated type for x.l1...li, obtained by changing the type of
                 the field li+1. *)
              let t =
                Type.make ~pos:t.Type.pos (Type.Meth (l, (lvg, lvt), "", vt))
              in
              (* Update value for x.l1...li. *)
              ( (vg, t),
                Term.Value.{ v with methods = Methods.add l lv v.methods } )
          | [] -> ((g, t), v)
        in
        let (g, t), v = aux [] ll in
        assert (g = []);
        builtins_env := (x, ((g0, t), v)) :: !builtins_env
    | [] -> assert false

let has_builtin name = builtins#is_registered name
let get_builtin name = builtins#get name

(** Declare a module. *)
let add_module name =
  (* Ensure that it does not already exist. *)
  (match name with
    | [] -> assert false
    | [x] ->
        if List.mem_assoc x !builtins_env then
          failwith ("Module " ^ String.concat "." name ^ " already declared")
    | x :: mm -> (
        let mm = List.rev mm in
        let l = List.hd mm in
        let mm = List.rev (List.tl mm) in
        let e =
          try Term.Value.invokes (snd (List.assoc x !builtins_env)) mm
          with _ ->
            failwith
              ("Could not find the parent module of " ^ String.concat "." name)
        in
        try
          ignore (Term.Value.invoke e l);
          failwith ("Module " ^ String.concat "." name ^ " already exists")
        with _ -> ()));
  add_builtin ~register:false name
    ( ([], Type.make Type.unit),
      {
        Term.Value.pos = None;
        value = Term.Value.unit;
        methods = Term.Value.Methods.empty;
      } )

(* Builtins are only used for documentation now. *)
let builtins = (builtins :> Doc.item)
