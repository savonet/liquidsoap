(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let _ =
  let meth =
    let check ans =
      if ans <> Sqlite3.Rc.OK then (
        let message =
          Printf.sprintf "Command failed: %s." (Sqlite3.Rc.to_string ans)
        in
        Runtime_error.raise ~pos:[] ~message "sqlite")
    in
    [
      ( "exec",
        ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
        "Execute an SQL operation.",
        fun db ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let sql = List.assoc "" p |> Lang.to_string in
              Sqlite3.exec db sql |> check;
              Lang.unit) );
      ( "close",
        ([], Lang.fun_t [] Lang.unit_t),
        "Close the database. It should not be accessed afterward.",
        fun db ->
          Lang.val_fun [] (fun _p ->
              ignore (Sqlite3.db_close db);
              Lang.unit) );
    ]
  in
  let t =
    List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth
    |> Lang.method_t Lang.unit_t
  in
  Lang.add_builtin "sqlite" ~category:`Programming
    ~descr:"Manipulate an SQLITE database."
    [("", Lang.string_t, None, Some "File where the data base is stored")]
    t
    (fun p ->
      let fname = List.assoc "" p |> Lang.to_string in
      let db = Sqlite3.db_open fname in
      let meth = List.map (fun (name, _, _, f) -> (name, f db)) meth in
      Lang.meth Lang.unit meth)
