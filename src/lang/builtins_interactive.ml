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

module Var = struct
  exception Invalid_value of string

  type variable = {
    name : string;
    t : Lang.t;
    get : unit -> string;
    set : string -> unit;
    validate : string -> unit;
  }

  let variables = ref []
  let ns = Server.register ["var"] "interactive variables"

  let () =
    let usage = "list" in
    Server.add ~ns ~usage "list" ~descr:"List available interactive variables."
      (fun _ ->
        String.concat "\n"
          (List.map
             (fun (_, v) ->
               Printf.sprintf "%s : %s" v.name (Lang_types.print v.t))
             (List.sort (fun (m, _) (n, _) -> compare m n) !variables)))

  let () =
    let usage = "set <variable> = <value>" in
    Server.add ~ns ~usage "set" ~descr:"Set a variable's value." (fun s ->
        let pat = "^([a-zA-Z_][a-zA-Z0-9_.]*) *= *(\"[^\"]*\"|[^ ]+)" in
        try
          let sub = Pcre.exec ~pat s in
          let name = Pcre.get_substring sub 1 in
          let v = Pcre.get_substring sub 2 in
          try
            let var = List.assoc name !variables in
            let oldval = var.get () in
            var.validate v;
            var.set v;
            Printf.sprintf "Variable %s set (was %s)." name oldval
          with
            | Not_found -> Printf.sprintf "Variable %s is not defined." name
            | Invalid_value s -> Printf.sprintf "Invalid value: %s." s
        with Not_found -> "Usage: var." ^ usage)

  let () =
    let usage = "get <variable>" in
    Server.add ~ns ~usage "get" ~descr:"Get a variable's value." (fun name ->
        try
          let var = List.assoc name !variables in
          Printf.sprintf "%s" (var.get ())
        with Not_found -> Printf.sprintf "Variable %s is not defined." name)

  let add name t ~get ~set ~validate =
    let var = { name; t; get; set; validate } in
    variables := (name, var) :: !variables
end

let () =
  add_builtin "interactive.string" ~cat:Interaction
    ~descr:"Read a string from an interactive input."
    [
      ("", Lang.string_t, None, Some "Name of the variable.");
      ("", Lang.string_t, None, Some "Initial value.");
    ] (Lang.fun_t [] Lang.string_t) (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_string (Lang.assoc "" 2 p) in
      let v = ref v in
      Var.add name Lang.string_t
        ~get:(fun () -> Printf.sprintf "%S" !v)
        ~set:(fun s -> v := Scanf.sscanf s "%S" (fun s -> s))
        ~validate:(fun s ->
          try ignore (Scanf.sscanf s "%S" (fun s -> s))
          with _ -> raise (Var.Invalid_value (s ^ " is not a string")));
      Lang.val_fun [] ~ret_t:Lang.string_t (fun _ _ -> Lang.string !v))

let () =
  add_builtin "interactive.float" ~cat:Interaction
    ~descr:"Read a float from an interactive input."
    [
      ("", Lang.string_t, None, Some "Name of the variable.");
      ("", Lang.float_t, None, Some "Initial value.");
    ] (Lang.fun_t [] Lang.float_t) (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_float (Lang.assoc "" 2 p) in
      let v = ref v in
      Var.add name Lang.float_t
        ~get:(fun () -> Printf.sprintf "%.04f" !v)
        ~set:(fun s -> v := float_of_string s)
        ~validate:(fun s ->
          try ignore (float_of_string s)
          with _ -> raise (Var.Invalid_value (s ^ " is not a float")));
      Lang.val_fun [] ~ret_t:Lang.float_t (fun _ _ -> Lang.float !v))

let () =
  add_builtin "interactive.bool" ~cat:Interaction
    ~descr:"Read a boolean from an interactive input."
    [
      ("", Lang.string_t, None, Some "Name of the variable.");
      ("", Lang.bool_t, None, Some "Initial value.");
    ] (Lang.fun_t [] Lang.bool_t) (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_bool (Lang.assoc "" 2 p) in
      let v = ref v in
      Var.add name Lang.bool_t
        ~get:(fun () -> Printf.sprintf "%B" !v)
        ~set:(fun s -> v := s = "true")
        ~validate:(fun s ->
          if s <> "true" && s <> "false" then
            raise (Var.Invalid_value (s ^ " is not a boolean")));
      Lang.val_fun [] ~ret_t:Lang.bool_t (fun _ _ -> Lang.bool !v))
