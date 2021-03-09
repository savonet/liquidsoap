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

open Lang_builtins

let () =
  let a = Lang.univ_t () in
  add_builtin ~cat:Liq "getter" ~descr:"Create a getter."
    [("", a, None, Some "Value from which the getter should be created.")]
    (Lang.getter_t a) (fun p -> List.assoc "" p)

let () =
  let a = Lang.univ_t () in
  add_builtin ~cat:Liq "getter.get" ~descr:"Get the value of a getter."
    [("", Lang.getter_t a, None, None)]
    a
    (fun p ->
      let x = List.assoc "" p |> Lang.to_getter in
      x ())

let () =
  let a = Lang.univ_t () in
  add_builtin ~cat:Liq "getter.is_constant"
    ~descr:"If true then the getter in argument is constant."
    [("", Lang.getter_t a, None, Some "Getter to inspect.")]
    Lang.bool_t
    (fun p ->
      let g = List.assoc "" p in
      match (Lang.demeth g).Lang.value with
        | Lang.Ground _ -> Lang.bool true
        | _ -> Lang.bool false)

let () = Lang.add_module "getter.int"
let () = Lang.add_module "getter.bool"
let () = Lang.add_module "getter.float"
let () = Lang.add_module "getter.string"

let () =
  let add_getters name get_t type_t to_get to_val =
    add_builtin ~cat:Liq
      ("to_" ^ name ^ "_getter")
      ~descr:("Return a function from a " ^ name ^ " getter")
      [("", get_t (), None, None)]
      (Lang.fun_t [] type_t)
      (fun p ->
        let getter = to_get (Lang.assoc "" 1 p) in
        Lang.val_fun [] (fun _ -> to_val (getter ())));
    add_builtin ~cat:Liq (name ^ "_getter")
      ~descr:
        ( "Identity function over " ^ name ^ " getters. "
        ^ "This is useful to make types explicit." )
      [("", get_t (), None, None)]
      (get_t ())
      (fun p -> List.assoc "" p)
  in
  add_getters "string" Lang.string_getter_t Lang.string_t Lang.to_string_getter
    Lang.string;
  add_getters "float" Lang.float_getter_t Lang.float_t Lang.to_float_getter
    Lang.float;
  add_getters "int" Lang.int_getter_t Lang.int_t Lang.to_int_getter Lang.int;
  add_getters "bool" Lang.bool_getter_t Lang.bool_t Lang.to_bool_getter
    Lang.bool
