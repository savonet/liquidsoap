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

(** An error at runtime. *)

type runtime_error = { kind : string; msg : string; pos : Pos.t list }

exception Runtime_error of runtime_error

let () =
  Printexc.register_printer (function
    | Runtime_error { kind; msg; pos } ->
        Some
          (Printf.sprintf "Lang.Runtime_error { kind: %s, msg: %s, pos: [%s] }"
             (Utils.quote_string kind) (Utils.quote_string msg)
             (String.concat ", " (List.map (fun pos -> Pos.to_string pos) pos)))
    | _ -> None)

let error ?bt ?(pos = []) ?(message = "") kind =
  let e = Runtime_error { kind; msg = message; pos } in
  match bt with
    | None -> raise e
    | Some bt -> Printexc.raise_with_backtrace e bt
