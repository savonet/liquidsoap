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

open Builtin

type error = { kind : string; msg : string option }

module ErrorDef = struct
  type content = error

  let name = "error"

  let descr { kind; msg } =
    Printf.sprintf "error(kind=%S,message=%s)" kind
      (match msg with Some msg -> Printf.sprintf "%S" msg | None -> "none")

  let to_json ~compact:_ ~json5:_ v = Printf.sprintf "%S" (descr v)
  let compare = Stdlib.compare
end

module Error = Lang.MkAbstract ((ErrorDef : Lang.AbstractDef))

let () = Lang.add_module "error"

let () =
  add_builtin "error.register" ~cat:Liq
    ~descr:"Register an error of the given kind"
    [("", Lang.string_t, None, Some "Kind of the error")] Error.t (fun p ->
      let kind = Lang.to_string (List.assoc "" p) in
      Error.to_value { kind; msg = None })

let () =
  add_builtin "error.kind" ~cat:Liq ~descr:"Get the kind of an error"
    [("", Error.t, None, None)] Lang.string_t (fun p ->
      Lang.string (Error.of_value (List.assoc "" p)).kind)

let () =
  add_builtin "error.message" ~cat:Liq ~descr:"Get the message of an error"
    [("", Error.t, None, None)] (Lang.nullable_t Lang.string_t) (fun p ->
      match Error.of_value (List.assoc "" p) with
        | { msg = Some msg } -> Lang.string msg
        | { msg = None } -> Lang.null)

let () =
  add_builtin "error.raise" ~cat:Liq ~descr:"Raise an error."
    [
      ("", Error.t, None, Some "Error kind.");
      ( "",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Description of the error." );
    ]
    (Lang.univ_t ())
    (fun p ->
      let { kind } = Error.of_value (Lang.assoc "" 1 p) in
      let msg =
        Option.map Lang.to_string (Lang.to_option (Lang.assoc "" 2 p))
      in
      raise (Term.Runtime_error { Term.kind; msg; pos = [] }))

let () =
  let a = Lang.univ_t () in
  add_builtin "error.catch" ~cat:Liq ~flags:[Lang.Hidden]
    ~descr:"Execute a function, catching eventual exceptions."
    [
      ( "errors",
        Lang.nullable_t (Lang.list_t Error.t),
        None,
        Some "Kinds of errors to catch. Catches all errors if not set." );
      ("", Lang.fun_t [] a, None, Some "Function to execute.");
      ("", Lang.fun_t [(false, "", Error.t)] a, None, Some "Error handler.");
    ]
    a
    (fun p ->
      let errors =
        Option.map
          (fun v -> List.map Error.of_value (Lang.to_list v))
          (Lang.to_option (Lang.assoc "errors" 1 p))
      in
      let f = Lang.to_fun (Lang.assoc "" 1 p) in
      let h = Lang.to_fun (Lang.assoc "" 2 p) in
      try f []
      with
      | Term.Runtime_error { Term.kind; msg }
      when errors = None
           || List.exists (fun err -> err.kind = kind) (Option.get errors)
      ->
        h [("", Error.to_value { kind; msg })])
