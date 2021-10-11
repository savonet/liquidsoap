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

open Lang

let log = Log.make ["lang"; "json"]

let rec json_of_value v : Json.t =
  match v.Value.value with
    | Value.Null -> `Null
    | Value.Ground g -> Term.Ground.to_json g
    | Value.List l -> `Tuple (List.map json_of_value l)
    | Value.Tuple l -> `Tuple (List.map json_of_value l)
    | Value.Meth _ -> (
        let m, v = Value.split_meths v in
        match v.Value.value with
          | Value.Tuple [] ->
              `Assoc (List.map (fun (l, v) -> (l, json_of_value v)) m)
          | _ -> json_of_value v)
    | _ ->
        raise
          Runtime_error.(
            Runtime_error
              {
                kind = "json";
                msg =
                  Printf.sprintf "Value %s cannot be represented as json"
                    (Value.print_value v);
                pos = (match v.Value.pos with Some p -> [p] | None -> []);
              })

module JsonValue = Value.MkAbstract (struct
  type content = (string, Lang.value) Hashtbl.t

  let name = "json"
  let descr _ = "json"

  let to_json v =
    `Assoc (Hashtbl.fold (fun k v l -> (k, json_of_value v) :: l) v [])

  let compare = Stdlib.compare
end)

let () =
  let val_t = univ_t () in
  let var =
    match val_t.Type.descr with
      | Type.Var { contents = Type.Free v } -> v
      | _ -> assert false
  in
  let meth =
    [
      ( "add",
        ([var], fun_t [(false, "", string_t); (false, "", val_t)] unit_t),
        "Add or replace a new `key`/`value` pair to the object.",
        fun v ->
          val_fun [("", "", None); ("", "", None)] (fun p ->
              let key = to_string (assoc "" 1 p) in
              let value = assoc "" 2 p in
              Hashtbl.replace v key value;
              unit) );
      ( "remove",
        ([], fun_t [(false, "", string_t)] unit_t),
        "Remove a key from the object. Does not nothing if the key does not \
         exist.",
        fun v ->
          val_fun [("", "", None)] (fun p ->
              let key = to_string (List.assoc "" p) in
              Hashtbl.remove v key;
              unit) );
    ]
  in
  let t =
    method_t JsonValue.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  add_builtin "json" ~category:`String ~descr:"Create a generic json object" []
    t (fun _ ->
      let v = Hashtbl.create 10 in
      let meth = List.map (fun (name, _, _, fn) -> (name, fn v)) meth in
      Lang.meth (JsonValue.to_value v) meth)

let () =
  add_builtin "json.stringify" ~category:`String
    ~descr:"Convert a value to a json string."
    [
      ("compact", bool_t, Some (bool false), Some "Output compact text.");
      ("json5", bool_t, Some (bool false), Some "Use json5 extended spec.");
      ("", univ_t (), None, None);
    ]
    string_t
    (fun p ->
      let compact = to_bool (List.assoc "compact" p) in
      let json5 = to_bool (List.assoc "json5" p) in
      let v = List.assoc "" p in
      let v = Json.to_string ~compact ~json5 (json_of_value v) in
      string v)

exception Failed

let () =
  Printexc.register_printer (function
    | Failed -> Some "Liquidsoap count not parse JSON string"
    | _ -> None)

(* We compare the default's type with the parsed json value and return if they
   match. This comes with json.stringify in Builtin. *)
let rec of_json d j =
  match (d.value, j) with
    | Tuple [], `Null -> unit
    | Ground (Ground.Bool _), `Bool b -> bool b
    (* JSON specs do not differentiate between ints and floats. Therefore, we
       should parse int as floats when required. *)
    | Ground (Ground.Int _), `Int i -> int i
    | Ground (Ground.Float _), `Int i -> float (float_of_int i)
    | Ground (Ground.Float _), `Float x -> float x
    | Ground (Ground.String _), `String s -> string s
    (* Be liberal and allow casting basic types to string. *)
    | Ground (Ground.String _), `Int i -> string (string_of_int i)
    | Ground (Ground.String _), `Float x -> string (string_of_float x)
    | Ground (Ground.String _), `Bool b -> string (string_of_bool b)
    | List [], `Tuple [] -> list []
    | List (d :: _), `Tuple l ->
        (* TODO: we could also try with other elements of the default list... *)
        let l = List.map (of_json d) l in
        list l
    | Tuple dd, `Tuple jj when List.length dd = List.length jj ->
        tuple (List.map2 of_json dd jj)
    | ( List ({ value = Tuple [{ value = Ground (Ground.String _) }; d] } :: _),
        `Assoc l ) ->
        (* Try to convert the object to a list of pairs, dropping fields that
           cannot be parsed.  This requires the target type to be [(string*'a)],
           currently it won't work if it is [?T] which would be obtained with
           of_json(default=[],...). *)
        let l =
          List.fold_left
            (fun cur (x, y) ->
              try product (string x) (of_json d y) :: cur with _ -> cur)
            [] l
        in
        list l
    (* Parse records. *)
    | Meth (l, x, d), `Assoc a -> (
        try
          let y = List.assoc l a in
          let v = of_json x y in
          let a' = List.remove_assoc l a in
          meth (of_json d (`Assoc a')) [(l, v)]
        with Not_found -> raise Failed)
    | Tuple [], `Assoc _ -> unit
    | _ -> raise Failed

let () =
  let t = univ_t () in
  add_builtin ~category:`String
    ~descr:
      "Parse a json string into a liquidsoap value. The value provided in the \
       `default` parameter is quite important: only the part of the JSON data \
       which has the same type as the `default` parameter will be kept \
       (heterogeneous data cannot be represented in Liquidsoap because of \
       typing). For instance, if the JSON `j` is\n\
       ```\n\
       {\n\
      \ \"a\": \"test\",\n\
      \ \"b\": 5\n\
       }\n\
       ```\n\
       the value returned by `json.parse(default=[(\"\",0)], j)` will be \
       `[(\"b\",5)]`: the pair `(\"a\",\"test\")` is not kept because it is \
       not of type `string * int`." "json.parse"
    [
      ("default", t, None, Some "Default value if string cannot be parsed.");
      ("", string_t, None, None);
    ] t (fun p ->
      let default = List.assoc "default" p in
      let s = to_string (List.assoc "" p) in
      try
        let json = Json.from_string s in
        of_json default json
      with e ->
        log#info "JSON parsing failed: %s" (Printexc.to_string e);
        default)
