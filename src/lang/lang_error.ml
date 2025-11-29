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

include Runtime_error

exception Encoder_error of (Pos.Option.t * string)

let error_module = Lang_core.add_module "error"

type error = Runtime_error.runtime_error = private {
  kind : string;
  msg : string;
  pos : Pos.List.t;
}

module ErrorDef = struct
  type content = error

  let name = "error"

  let to_string { kind; msg; pos } =
    let pos =
      if pos <> [] then
        Printf.sprintf ",positions=%s"
          (Lang_string.quote_string (Pos.List.to_string ~newlines:false pos))
      else ""
    in
    Printf.sprintf "error(kind=%s,message=%s%s)"
      (Lang_string.quote_string kind)
      (Lang_string.quote_string msg)
      pos

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Error cannot be represented as json"
      "json"

  let compare = Stdlib.compare
end

module Error = struct
  include Value.MkCustom (ErrorDef : Value.CustomDef)

  let meths =
    [
      ( "kind",
        ([], Lang_core.string_t),
        "Error kind.",
        fun { kind } -> Lang_core.string kind );
      ( "message",
        ([], Lang_core.string_t),
        "Error message.",
        fun { msg } -> Lang_core.string msg );
      ( "trace",
        ([], Lang_core.Stacktrace.t),
        "Error stacktrace.",
        fun { pos } -> Lang_core.Stacktrace.to_value pos );
    ]

  let base_t = t

  let t =
    Lang_core.method_t t
      (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value err =
    Lang_core.meth (to_value err)
      (List.map (fun (lbl, _, _, m) -> (lbl, m err)) meths)
end

let _ =
  Lang_core.add_builtin ~base:error_module "register" ~category:`Programming
    ~descr:"Register an error of the given kind"
    [("", Lang_core.string_t, None, Some "Kind of the error")]
    Error.t
    (fun p ->
      let kind = Lang_core.to_string (List.assoc "" p) in
      Error.to_value (Runtime_error.make ~pos:[] kind))

let _ =
  Lang_core.add_builtin ~base:error_module "raise" ~category:`Programming
    ~descr:"Raise an error."
    [
      ("", Error.base_t, None, Some "Error kind.");
      ( "",
        Lang_core.string_t,
        Some (Lang_core.string ""),
        Some "Description of the error." );
    ]
    (Lang_core.univ_t ())
    (fun p ->
      let { kind; pos } = Error.of_value (Lang_core.assoc "" 1 p) in
      let message = Lang_core.to_string (Lang_core.assoc "" 2 p) in
      Runtime_error.raise ~pos:(Lang_core.pos p @ pos) ~message kind)

let _ =
  Lang_core.add_builtin ~base:error_module "on_error" ~category:`Programming
    ~descr:
      "Register a callback to monitor errors raised during the execution of \
       the program. The callback is allow to re-raise a different error if \
       needed. The callback passed to this function is called on every errors, \
       not just uncaught errors."
    [("", Lang_core.fun_t [(false, "", Error.t)] Lang_core.unit_t, None, None)]
    Lang_core.unit_t
    (fun p ->
      let fn = List.assoc "" p in
      let fn err = ignore (Lang_core.apply fn [("", Error.to_value err)]) in
      Runtime_error.on_error fn;
      Lang_core.unit)

let error_t = Error.base_t
let error_meths_t = Error.t
let error = Error.to_value
let to_error = Error.of_value

let _ =
  let a = Lang_core.univ_t () in
  Lang_core.add_builtin ~base:error_module "catch" ~category:`Programming
    ~flags:[`Hidden] ~descr:"Execute a function, catching eventual exceptions."
    [
      ( "errors",
        Lang_core.nullable_t (Lang_core.list_t Error.base_t),
        None,
        Some "Kinds of errors to catch. Catches all errors if not set." );
      ("body", Lang_core.fun_t [] a, None, Some "Function to execute.");
      ( "catch",
        Lang_core.fun_t [(false, "", Error.t)] a,
        None,
        Some "Error handler." );
      ( "finally",
        Lang_core.fun_t [] Lang_core.unit_t,
        None,
        Some "Invariant handler." );
    ]
    a
    (fun p ->
      let errors =
        Option.map
          (fun v -> List.map Error.of_value (Lang_core.to_list v))
          (Lang_core.to_option (List.assoc "errors" p))
      in
      let body = Lang_core.to_fun (List.assoc "body" p) in
      let catch = Lang_core.to_fun (List.assoc "catch" p) in
      let finally = Lang_core.to_fun (List.assoc "finally" p) in
      let finally_called = Atomic.make false in
      try
        let v = body [] in
        Atomic.set finally_called true;
        ignore (finally []);
        v
      with
        | Runtime_error.(Runtime_error { kind; msg })
          when errors = None
               || List.exists (fun err -> err.kind = kind) (Option.get errors)
          ->
            let v =
              try
                catch
                  [
                    ( "",
                      Error.to_value
                        (Runtime_error.make ~pos:(Lang_core.pos p) ~message:msg
                           kind) );
                  ]
              with exn when not (Atomic.get finally_called) ->
                let bt = Printexc.get_raw_backtrace () in
                ignore (finally []);
                Printexc.raise_with_backtrace exn bt
            in
            if not (Atomic.get finally_called) then ignore (finally []);
            v
        | exn when Atomic.get finally_called ->
            let bt = Printexc.get_raw_backtrace () in
            ignore (finally []);
            Printexc.raise_with_backtrace exn bt)
