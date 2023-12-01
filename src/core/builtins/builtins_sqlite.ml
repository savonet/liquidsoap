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

let error fmt =
  Printf.ksprintf
    (fun message -> Runtime_error.raise ~pos:[] ~message "sqlite")
    fmt

let _ =
  let meth =
    let check db ans =
      if not (Sqlite3.Rc.is_success ans) then
        error "Command failed (%s): %s." (Sqlite3.Rc.to_string ans)
          (Sqlite3.errmsg db)
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
              Sqlite3.exec db sql |> check db;
              Lang.unit) );
      ( "query",
        ( [],
          Lang.fun_t
            [(false, "", Lang.string_t)]
            (Lang.list_t
               (Lang.list_t
                  (Lang.product_t Lang.string_t (Lang.nullable_t Lang.string_t))))
        ),
        "Execute an SQL operation returning the result.",
        fun db ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let sql = List.assoc "" p |> Lang.to_string in
              let ans = ref [] in
              let cb row headers =
                let l =
                  Array.map2 (fun h r -> (h, r)) headers row
                  |> Array.to_list
                  |> List.map (fun (h, r) ->
                         Lang.product (Lang.string h)
                           (Option.fold ~none:Lang.null ~some:Lang.string r))
                  |> Lang.list
                in
                ans := l :: !ans
              in
              Sqlite3.exec ~cb db sql |> check db;
              let ans = List.rev !ans in
              Lang.list ans) );
      ( "insert",
        ( [],
          Lang.fun_t
            [(false, "table", Lang.string_t); (false, "", Lang.unit_t)]
            Lang.unit_t ),
        "Insert a value represented as a record into a table.",
        fun db ->
          Lang.val_fun
            [("table", "table", None); ("", "", None)]
            (fun p ->
              let table = List.assoc "table" p |> Lang.to_string in
              let v =
                List.assoc "" p |> Liquidsoap_lang.Builtins_json.json_of_value
              in
              match v with
                | `Assoc l ->
                    let l =
                      List.map
                        (fun (k, v) ->
                          ( k,
                            match v with
                              | `String s -> Sqlite3.Data.opt_text (Some s)
                              | `Int n -> Sqlite3.Data.opt_int (Some n)
                              | `Float x -> Sqlite3.Data.opt_float (Some x)
                              | _ -> error "Unexpected content for field %s." k
                          ))
                        l
                    in
                    let fields = l |> List.map fst |> String.concat ", " in
                    let values =
                      l |> List.map (fun _ -> "?") |> String.concat ", "
                    in
                    let insert =
                      Printf.sprintf "INSERT INTO %s (%s) VALUES (%s)" table
                        fields values
                    in
                    let insert = Sqlite3.prepare db insert in
                    l |> List.map snd
                    |> List.iteri (fun i v ->
                           Sqlite3.bind insert (i + 1) v |> check db);
                    Sqlite3.step insert |> check db;
                    Sqlite3.finalize insert |> check db;
                    Lang.unit
                | _ -> error "A record was expected.") );
      ( "close",
        ([], Lang.fun_t [] Lang.unit_t),
        "Close the database. It should not be accessed afterward.",
        fun db ->
          Lang.val_fun [] (fun _p ->
              Sqlite3.db_close db |> ignore;
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
