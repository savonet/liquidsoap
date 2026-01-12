(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let escape s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"

let insert_value_constr =
  let open Type in
  {
    constr_descr = "int, float, string or null.";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let rec check typ =
          match (deref typ).descr with
            | Var _ -> satisfies typ
            | Nullable typ -> check typ
            | Float | Int | String -> ()
            | _ -> raise Unsatisfied_constraint
        in
        check b);
  }

let insert_record_constr =
  let open Type in
  {
    constr_descr = "a record with int, float, string or null methods.";
    univ_descr = None;
    satisfied =
      (fun ~subtype ~satisfies b ->
        let m, b = split_meths b in
        match b.descr with
          | Var _ -> satisfies b
          | Tuple [] when m = [] -> raise Unsatisfied_constraint
          | Tuple [] ->
              List.iter
                (fun { scheme = _, typ } ->
                  subtype typ (var ~constraints:[insert_value_constr] ()))
                m
          | _ -> raise Unsatisfied_constraint);
  }

type row = { row : Sqlite3.row; headers : Sqlite3.headers }

module SqliteRow = struct
  include Value.MkCustom (struct
    type content = row

    let name = "sqlite.row"

    let to_json ~pos _ =
      Runtime_error.raise ~pos
        ~message:"Sqlite rows cannot be represented as json" "json"

    let to_string _ = "sqlite.row"
    let compare = Stdlib.compare
  end)

  let t =
    Lang.method_t t
      [
        ( "to_list",
          ( [],
            Lang.fun_t []
              (Lang.list_t
                 (Lang.product_t Lang.string_t (Lang.nullable_t Lang.string_t)))
          ),
          "Return the row as an associative `[(label, value)]` list." );
      ]

  let to_value v =
    let to_list { headers; row } =
      List.fold_left2
        (fun l header row ->
          Lang.product (Lang.string header)
            (match row with None -> Lang.null | Some r -> Lang.string r)
          :: l)
        [] (Array.to_list headers) (Array.to_list row)
    in
    Lang.meth (to_value v)
      [("to_list", Lang.val_fun [] (fun _ -> Lang.list (to_list v)))]

  let of_value v = of_value (Lang.demeth v)
end

let header_types ty =
  let open Type in
  let headers, _ = split_meths ty in
  let rec to_type ~nullable ty =
    match (deref ty).descr with
      | Nullable typ ->
          let typ = to_type ~nullable:false typ in
          if nullable then `Nullable typ else typ
      | Float -> `Float
      | Int -> `Int
      | String -> `String
      | Var _ -> raise Not_found
      | _ -> assert false
  in
  let headers =
    List.fold_left
      (fun headers { Type.meth = lbl; scheme = _, ty } ->
        try (lbl, to_type ~nullable:true ty) :: headers
        with Not_found -> headers)
      [] headers
  in
  headers

let rec string_of_typ = function
  | `Nullable typ -> Printf.sprintf "%s?" (string_of_typ typ)
  | `Int -> "int"
  | `Float -> "float"
  | `String -> "string"

let check db ?sql ans =
  let sql =
    match sql with
      | Some sql -> Printf.sprintf " Statement: %s." sql
      | None -> ""
  in
  if not (Sqlite3.Rc.is_success ans) then
    error "Command failed (%s): %s.%s" (Sqlite3.Rc.to_string ans)
      (Sqlite3.errmsg db) sql

let exec db ?cb sql = Sqlite3.exec db ?cb sql |> check db ~sql

let rec parse_value ~pos ~header ~typ v =
  let open Sqlite3.Data in
  match (typ, v) with
    | `Nullable _, NULL -> Lang.null
    | `Nullable typ, v -> parse_value ~pos ~header ~typ v
    | `Int, INT i -> Lang.int (Int64.to_int i)
    | `Int, TEXT s -> Lang.int (int_of_string s)
    | `Int, BLOB s -> Lang.int (int_of_string s)
    | `Float, FLOAT f -> Lang.float f
    | `Float, TEXT s -> Lang.float (float_of_string s)
    | `Float, BLOB s -> Lang.float (float_of_string s)
    | `String, TEXT s -> Lang.string s
    | `String, BLOB s -> Lang.string s
    | _ ->
        Runtime_error.raise ~pos
          ~message:
            (Printf.sprintf
               "Parse error: sqlite query response for column %s with value %s \
                cannot be parsed as type %s"
               header
               (match to_string v with None -> "NULL" | Some s -> s)
               (string_of_typ typ))
          "sqlite"

let parse_row ~pos ~header_types { row; headers } =
  let row = Array.to_list row in
  let headers = Array.to_list headers in
  List.fold_left2
    (fun l header row ->
      match List.assoc_opt header header_types with
        | Some typ ->
            (header, parse_value ~pos ~header ~typ (Sqlite3.Data.opt_text row))
            :: l
        | None -> l)
    [] headers row
  |> Lang.record

let query_parser ~db query =
  let ans = ref [] in
  let cb row headers = ans := SqliteRow.to_value { row; headers } :: !ans in
  exec db ~cb query;
  let ans = List.rev !ans in
  Lang.list ans

let _ =
  let return_t = Type.var ~constraints:[insert_record_constr] () in
  Lang.add_builtin "_sqlite_row_parser_" ~category:`String ~flags:[`Hidden]
    ~descr:"Internal sql row parser"
    [
      ("type", Value.RuntimeType.t, None, Some "Runtime type");
      ("", SqliteRow.t, None, None);
    ]
    return_t
    (fun p ->
      let row = SqliteRow.of_value (List.assoc "" p) in
      let ty = Value.RuntimeType.of_value (List.assoc "type" p) in
      let header_types = header_types ty in
      let pos = Lang.pos p in
      try parse_row ~pos ~header_types row
      with exn -> (
        let bt = Printexc.get_raw_backtrace () in
        match exn with
          | Runtime_error.Runtime_error _ as exn ->
              Printexc.raise_with_backtrace exn bt
          | _ ->
              Runtime_error.raise ~bt ~pos
                ~message:
                  (Printf.sprintf
                     "Parse error: sqlite query response value cannot be \
                      parsed as type: %s"
                     (Type.to_string ty))
                "sqlite"))

let sqlite =
  let meth =
    [
      ( "exec",
        ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
        "Execute an SQL operation.",
        fun db ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let sql = List.assoc "" p |> Lang.to_string in
              exec db sql;
              Lang.unit) );
      ( "query",
        ([], Lang.fun_t [(false, "", Lang.string_t)] (Lang.list_t SqliteRow.t)),
        "Execute an SQL operation returning the result. Result can be parsed \
         using `let sqlite.query = ...`.",
        fun db ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let query = List.assoc "" p |> Lang.to_string in
              query_parser ~db query) );
      ( "iter",
        ( [],
          Lang.fun_t
            [
              (false, "", Lang.fun_t [(false, "", SqliteRow.t)] Lang.unit_t);
              (false, "", Lang.string_t);
            ]
            Lang.unit_t ),
        "Iterate a function over all the results of a query. Result can be \
         parsed using `let sqlite.row = ...`.",
        fun db ->
          Lang.val_fun
            [("", "", None); ("", "", None)]
            (fun p ->
              let f = Lang.assoc "" 1 p in
              let sql = Lang.assoc "" 2 p |> Lang.to_string in
              let cb row headers =
                let row = SqliteRow.to_value { row; headers } in
                ignore (Lang.apply f [("", row)])
              in
              exec db ~cb sql;
              Lang.unit) );
      ( "insert",
        ( [],
          Lang.fun_t
            [
              (false, "table", Lang.string_t);
              (true, "replace", Lang.bool_t);
              (false, "", Type.var ~constraints:[insert_record_constr] ());
            ]
            Lang.unit_t ),
        "Insert a value represented as a record into a table.",
        fun db ->
          Lang.val_fun
            [
              ("table", "table", None);
              ("replace", "replace", Some (Lang.bool false));
              ("", "", None);
            ]
            (fun p ->
              let table = List.assoc "table" p |> Lang.to_string in
              let replace = List.assoc "replace" p |> Lang.to_bool in
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
                              | `Null -> Sqlite3.Data.NULL
                              | _ -> error "Unexpected content for field %s." k
                          ))
                        l
                    in
                    let sql =
                      let replace = if replace then " OR REPLACE" else "" in
                      let fields = l |> List.map fst |> String.concat ", " in
                      let values =
                        l |> List.map (fun _ -> "?") |> String.concat ", "
                      in
                      Printf.sprintf "INSERT%s INTO %s (%s) VALUES (%s)" replace
                        table fields values
                    in
                    let insert = Sqlite3.prepare db sql in
                    l |> List.map snd
                    |> List.iteri (fun i v ->
                        Sqlite3.bind insert (i + 1) v |> check db);
                    Sqlite3.step insert |> check db ~sql;
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

let _ =
  Lang.add_builtin "escape" ~base:sqlite ~category:`Programming
    ~descr:"Escape a string for use in a query."
    [("", Lang.string_t, None, Some "String to escape.")]
    Lang.string_t
    (fun p -> List.assoc "" p |> Lang.to_string |> escape |> Lang.string)
