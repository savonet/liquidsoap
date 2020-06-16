(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module Error = Lang.MkAbstract (struct
  type content = string

  let name = "error"
  let descr = Printf.sprintf "error(type=%s)"
  let compare = Stdlib.compare
end)

let () = Lang.add_module "error"

let () =
  add_builtin "error.register" ~cat:Liq
    ~descr:"Return an error of the given type"
    [("", Lang.string_t, None, Some "Type of the error")] Error.t (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      Error.to_value s)

let () =
  add_builtin "error.type" ~cat:Liq ~descr:"Get the type of an error"
    [("", Error.t, None, None)] Lang.string_t (fun p ->
      Lang.string (Error.of_value (List.assoc "" p)))

let () =
  add_builtin "error.raise" ~cat:Liq ~descr:"Raise an error."
    [("", Lang.string_t, None, Some "Description of the error.")]
    (Lang.univ_t ()) (fun p ->
      let s = Lang.to_string (Lang.assoc "" 1 p) in
      raise (Lang_values.Runtime_error ([], s)))

let () =
  let a = Lang.univ_t () in
  add_builtin "error.catch" ~cat:Liq
    ~descr:"Execute a function, catching eventual exceptions."
    [
      ("", Lang.fun_t [] a, None, Some "Function to execute.");
      ( "",
        Lang.fun_t [(false, "", Lang.string_t)] a,
        None,
        Some "Error handler." );
    ]
    a
    (fun p ->
      let f = Lang.to_fun (Lang.assoc "" 1 p) in
      let h = Lang.to_fun (Lang.assoc "" 2 p) in
      try f []
      with Lang_values.Runtime_error (_, e) -> h [("", Lang.string e)])
