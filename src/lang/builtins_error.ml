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

include Runtime_error

type pos = Runtime_error.pos

type error = Runtime_error.runtime_error = {
  kind : string;
  msg : string option;
  pos : pos list;
}

module ErrorDef = struct
  type content = error

  let name = "error"

  let descr { kind; msg; pos } =
    let pos =
      String.concat ", " (List.map (fun pos -> Runtime_error.print_pos pos) pos)
    in
    Printf.sprintf "error(kind=%S,message=%s,positions=%s)" kind
      (match msg with Some msg -> Printf.sprintf "%S" msg | None -> "none")
      (Utils.escape_utf8_string pos)

  let to_json ~compact:_ ~json5:_ v = Printf.sprintf "%S" (descr v)
  let compare = Stdlib.compare
end

module Error = struct
  include Lang.MkAbstract ((ErrorDef : Lang.AbstractDef))

  let meths =
    [
      ( "kind",
        ([], Lang.string_t),
        "Error kind.",
        fun { kind } -> Lang.string kind );
      ( "message",
        ([], Lang.(nullable_t string_t)),
        "Error message.",
        fun { msg } ->
          Option.value ~default:Lang.null
            (Option.map (fun v -> Lang.string v) msg) );
      ( "positions",
        ([], Lang.(list_t string_t)),
        "Error positions.",
        fun { pos } ->
          Lang.list
            (List.map
               (fun pos -> Lang.string (Runtime_error.print_pos pos))
               pos) );
    ]

  let t =
    Lang.method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value err =
    Lang.meth (to_value err)
      (List.map (fun (lbl, _, _, m) -> (lbl, m err)) meths)
end

let () = Lang.add_module "error"

let () =
  Lang.add_builtin "error.register" ~category:`Liquidsoap
    ~descr:"Register an error of the given kind"
    [("", Lang.string_t, None, Some "Kind of the error")] Error.t (fun p ->
      let kind = Lang.to_string (List.assoc "" p) in
      Error.to_value { kind; msg = None; pos = [] })

let () =
  Lang.add_builtin "error.raise" ~category:`Liquidsoap ~descr:"Raise an error."
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
  Lang.add_builtin "error.catch" ~category:`Liquidsoap ~flags:[`Hidden]
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
        h [("", Error.to_value { kind; msg; pos = [] })])
