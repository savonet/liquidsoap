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

let log = Hooks.log ["json"; "parse"]

let rec json_of_value ?pos v : Json.t =
  let pos =
    match (pos, Value.pos v) with
      | Some p, _ -> p
      | None, Some p -> [p]
      | None, None -> []
  in
  let m, v = Value.split_meths v in
  match v with
    | Null _ -> `Null
    | Int { value = i } -> `Int i
    | Float { value = f } -> `Float f
    | Bool { value = b } -> `Bool b
    | String { value = s } -> `String s
    | Custom { value = g } -> Term.Custom.to_json ~pos g
    | List { value = l } -> `Tuple (List.map (json_of_value ~pos) l)
    | Tuple { value = [] } when m <> [] ->
        `Assoc (List.map (fun (l, v) -> (l, json_of_value ~pos v)) m)
    | Tuple { value = l } -> `Tuple (List.map (json_of_value ~pos) l)
    | _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf "Value %s cannot be represented as json"
               (Value.to_string v))
          ~pos "json"

let rec type_of_json = function
  | `Assoc l ->
      Lang.record_t (List.map (fun (lbl, j) -> (lbl, type_of_json j)) l)
  | `Tuple l -> Lang.tuple_t (List.map type_of_json l)
  | `String _ -> Lang.string_t
  | `Bool _ -> Lang.bool_t
  | `Float _ -> Lang.float_t
  | `Int _ -> Lang.int_t
  | `Null -> Lang.(nullable_t (univ_t ()))

let nullable_deref ty =
  let ty = Type.deref ty in
  match ty.Type.descr with Type.Nullable ty -> (true, ty) | _ -> (false, ty)

type json_ellipsis_base =
  [ `Assoc of (string * string option * json_ellipsis) list
  | `List of json_ellipsis
  | `Tuple of json_ellipsis list
  | `String
  | `Bool
  | `Float
  | `Int
  | `Null
  | `Ignore ]

and json_ellipsis = bool * json_ellipsis_base

let rec string_of_json_ellipsis (nullable, ty) =
  let f ty = if nullable then ty ^ "?" else ty in
  match ty with
    | `Ignore -> "_"
    | `Null -> f "null"
    | `Int -> f "int"
    | `Float -> f "float"
    | `Bool -> f "bool"
    | `String -> f "string"
    | `List ty -> f ("[" ^ string_of_json_ellipsis ty ^ "]")
    | `Tuple [] -> f "()"
    | `Tuple l ->
        f ("(" ^ String.concat "," (List.map string_of_json_ellipsis l) ^ ")")
    | `Assoc [] -> f "{}"
    (* We should only report the one attribute that has failed to parse. *)
    | `Assoc ((lbl, lbl', ty) :: _) ->
        let lbl =
          match lbl' with
            | Some v ->
                Printf.sprintf "%s as %s" (Lang_string.quote_utf8_string v) lbl
            | None -> lbl
        in
        f
          ("{"
          ^ Printf.sprintf "%s: %s" lbl (string_of_json_ellipsis ty)
          ^ ", _}")

exception Failed of json_ellipsis

let rec value_of_typed_json ~ty json =
  let nullable, _ty = nullable_deref ty in
  try
    let tm, ty = Type.split_meths _ty in
    let () =
      match ty.Type.descr with
        | Type.Var _ ->
            log#important
              "You are parsing a JSON value without type annotation. This has \
               confusing side-effects. Consider adding a type annotation: `let \
               json.parse (x : ...) = ...`"
        | _ -> ()
    in
    match (json, ty.Type.descr) with
      | `Assoc l, Type.Var _ | `Assoc l, Type.Tuple [] ->
          Typing.(ty <: Lang.unit_t);
          let meth =
            List.map
              (fun Type.{ name = meth; optional; json_name; scheme = _, ty } ->
                let ty = Type.deref ty in
                let lbl = Option.value ~default:meth json_name in
                let nullable_meth = optional || fst (nullable_deref ty) in
                let v =
                  match List.assoc_opt lbl l with
                    | Some v -> (
                        try value_of_typed_json ~ty v with
                          | _ when nullable_meth -> Lang.null
                          | Failed v ->
                              raise
                                (Failed (nullable, `Assoc [(meth, json_name, v)]))
                        )
                    | None when nullable_meth -> Lang.null
                    | _ ->
                        raise
                          (Failed
                             ( nullable,
                               `Assoc
                                 [(meth, json_name, (nullable_meth, `Ignore))]
                             ))
                in
                (meth, v))
              tm
          in
          Lang.record meth
      | ( `Assoc l,
          Type.(
            List
              {
                t = { descr = Tuple [{ descr = String }; ty] };
                json_repr = `Object;
              }) ) ->
          Lang.list
            (List.map
               (fun (lbl, v) ->
                 try Lang.product (Lang.string lbl) (value_of_typed_json ~ty v)
                 with Failed v ->
                   raise (Failed (nullable, `Assoc [(lbl, None, v)])))
               l)
      | `Tuple l, Type.(List { t = ty; json_repr = `Tuple }) ->
          Lang.list
            (List.map
               (fun v ->
                 try value_of_typed_json ~ty v
                 with Failed v -> raise (Failed (nullable, `List v)))
               l)
      | `Tuple l, Type.Tuple t when tm = [] ->
          Lang.tuple
            (List.mapi
               (fun idx ty ->
                 try value_of_typed_json ~ty (List.nth l idx)
                 with Failed v ->
                   let l =
                     List.init (List.length t) (fun _ -> (false, `Ignore))
                   in
                   let l =
                     List.mapi (fun idx' el -> if idx = idx' then v else el) l
                   in
                   raise (Failed (nullable, `Tuple l)))
               t)
      | `String s, Type.String -> Lang.string s
      | `Bool b, Type.Bool -> Lang.bool b
      | `Float f, Type.Float -> Lang.float f
      | `Int i, Type.Float -> Lang.float (float i)
      | `Int i, Type.Int -> Lang.int i
      | _, Type.Var _ ->
          Typing.(ty <: type_of_json json);
          Lang.null
      | _, Type.String -> raise (Failed (nullable, `String))
      | _, Type.Bool -> raise (Failed (nullable, `Bool))
      | _, Type.Float -> raise (Failed (nullable, `Float))
      | _, Type.Int -> raise (Failed (nullable, `Int))
      | _ -> assert false
  with _ when nullable -> Lang.null

let value_of_typed_json ~ty json =
  try value_of_typed_json ~ty json with
    | Failed v ->
        let bt = Printexc.get_raw_backtrace () in
        Runtime_error.raise ~bt
          ~message:
            (Printf.sprintf
               "Parsing error: json value cannot be parsed as type %s"
               (string_of_json_ellipsis v))
          ~pos:(match ty.Type.pos with Some p -> [p] | None -> [])
          "json"
    | _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf
               "Parsing error: json value cannot be parsed as type %s"
               (Type.to_string ty))
          ~pos:(match ty.Type.pos with Some p -> [p] | None -> [])
          "json"

module JsonSpecs = struct
  type content =
    [ `Object of (string, Lang.value) Hashtbl.t | `Value of Lang.value ]

  let name = "json"
  let to_string _ = "json"

  let to_json ~pos = function
    | `Object v ->
        `Assoc (Hashtbl.fold (fun k v l -> (k, json_of_value ~pos v) :: l) v [])
    | `Value v -> json_of_value v

  let compare = Stdlib.compare
end

module JsonValue = Value.MkCustom (JsonSpecs)

let json = Lang.add_module "json"

let _ =
  Lang.add_builtin ~base:json "value" ~category:`String
    ~descr:"Create a generic json value"
    [("", Lang.univ_t (), None, None)]
    JsonValue.t
    (fun p -> JsonValue.to_value (`Value (List.assoc "" p)))

let _ =
  let val_t = Lang.univ_t () in
  let var =
    match val_t.Type.descr with
      | Type.Var { contents = Type.Free v } -> v
      | _ -> assert false
  in
  let meth =
    [
      ( "add",
        ( [var],
          Lang.fun_t
            [(false, "", Lang.string_t); (false, "", val_t)]
            Lang.unit_t ),
        "Add or replace a new `key`/`value` pair to the object.",
        fun v ->
          Lang.val_fun
            [("", "", None); ("", "", None)]
            (fun p ->
              let key = Lang.to_string (Lang.assoc "" 1 p) in
              let value = Lang.assoc "" 2 p in
              Hashtbl.replace v key value;
              Lang.unit) );
      ( "remove",
        ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
        "Remove a key from the object. Does not nothing if the key does not \
         exist.",
        fun v ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let key = Lang.to_string (List.assoc "" p) in
              Hashtbl.remove v key;
              Lang.unit) );
      ( "stringify",
        ( [],
          Lang.fun_t
            [(true, "compact", Lang.bool_t); (true, "json5", Lang.bool_t)]
            Lang.string_t ),
        "Render object as json string.",
        fun v ->
          Lang.val_fun
            [
              ("compact", "compact", Some (Lang.bool false));
              ("json5", "json5", Some (Lang.bool false));
            ]
            (fun p ->
              let compact = Lang.to_bool (List.assoc "compact" p) in
              let json5 = Lang.to_bool (List.assoc "json5" p) in
              Lang.string
                (Json.to_string ~compact ~json5
                   (JsonSpecs.to_json ~pos:(Lang.pos p) (`Object v)))) );
    ]
  in
  let t =
    Lang.method_t JsonValue.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  Lang.add_builtin ~base:json "object" ~category:`String
    ~descr:"Create a generic json object" [] t (fun _ ->
      let v = Hashtbl.create 10 in
      let meth = List.map (fun (name, _, _, fn) -> (name, fn v)) meth in
      Lang.meth (JsonValue.to_value (`Object v)) meth)

let _ =
  Lang.add_builtin ~base:json "stringify" ~category:`String
    ~descr:
      "Convert a value to JSON. If the value cannot be represented as JSON \
       (for instance a function), a `error.json` exception is raised."
    [
      ( "compact",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Output compact text." );
      ( "json5",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Use json5 extended spec." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.string_t
    (fun p ->
      let v = List.assoc "" p in
      let compact = Lang.to_bool (List.assoc "compact" p) in
      let json5 = Lang.to_bool (List.assoc "json5" p) in
      let v = Json.to_string ~compact ~json5 (json_of_value v) in
      Lang.string v)

let _ =
  Lang.add_builtin "_internal_json_parser_" ~category:`String ~flags:[`Hidden]
    ~descr:"Internal json parser"
    [
      ("type", Value.RuntimeType.t, None, Some "Runtime type");
      ( "json5",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Use json5 extended spec." );
      ("", Lang.string_t, None, None);
    ]
    (Lang.univ_t ())
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let ty = Value.RuntimeType.of_value (List.assoc "type" p) in
      let ty = Type.fresh ty in
      let json5 = Lang.to_bool (List.assoc "json5" p) in
      try
        let json = Json.from_string ~json5 s in
        value_of_typed_json ~ty json
      with exn -> (
        let bt = Printexc.get_raw_backtrace () in
        match exn with
          | Runtime_error.Runtime_error _ ->
              Printexc.raise_with_backtrace exn bt
          | _ ->
              Runtime_error.raise ~bt ~pos:(Lang.pos p)
                ~message:
                  (Printf.sprintf "Parse error: %s" (Printexc.to_string exn))
                "json"))

exception DeprecatedFailed

let () =
  Printexc.register_printer (function
    | DeprecatedFailed -> Some "Liquidsoap count not parse JSON string"
    | _ -> None)

(* We compare the default's type with the parsed json value and return if they
   match. This comes with json.stringify in Builtin. *)
let rec deprecated_of_json d j =
  let m, d = Value.split_meths d in
  Lang.(
    match (d, j) with
      | Tuple { value = [] }, `Null -> unit
      | Bool _, `Bool b -> bool b
      (* JSON specs do not differentiate between ints and floats. Therefore, we
         should parse int as floats when required. *)
      | Int _, `Int i -> int i
      | Float _, `Int i -> float (float_of_int i)
      | Float _, `Float x -> float x
      | String _, `String s -> string s
      (* Be liberal and allow casting basic types to string. *)
      | String _, `Int i -> string (string_of_int i)
      | String _, `Float x -> string (Utils.string_of_float x)
      | String _, `Bool b -> string (string_of_bool b)
      | List { value = [] }, `Tuple [] -> list []
      | List { value = d :: _ }, `Tuple l ->
          (* TODO: we could also try with other elements of the default list... *)
          let l = List.map (deprecated_of_json d) l in
          list l
      | Tuple { value = dd }, `Tuple jj when List.length dd = List.length jj ->
          tuple (List.map2 deprecated_of_json dd jj)
      | ( List { value = Tuple { value = [String { value = _ }; d] } :: _ },
          `Assoc l ) ->
          (* Try to convert the object to a list of pairs, dropping fields that
             cannot be parsed.  This requires the target type to be [(string*'a)],
             currently it won't work if it is [?T] which would be obtained with
             of_json(default=[],...). *)
          let l =
            List.fold_left
              (fun cur (x, y) ->
                try product (string x) (deprecated_of_json d y) :: cur
                with _ -> cur)
              [] l
          in
          list l
      (* Parse records. *)
      | Tuple { value = [] }, `Assoc a when m <> [] -> (
          try
            List.fold_left
              (fun parsed (key, meth) ->
                let json_meth = List.assoc key a in
                let parsed_meth = deprecated_of_json meth json_meth in
                Value.map_methods parsed (Methods.add key parsed_meth))
              unit m
          with Not_found -> raise DeprecatedFailed)
      | Tuple { value = [] }, `Assoc _ -> unit
      | _ -> raise DeprecatedFailed)

let _ =
  let t = Lang.univ_t () in
  Lang.add_builtin ~category:`String ~flags:[`Deprecated; `Hidden]
    ~descr:"Deprecated: use `let json.parse ..`" ~base:json "parse"
    [
      ("default", t, None, Some "Default value if string cannot be parsed.");
      ("", Lang.string_t, None, None);
    ]
    t
    (fun p ->
      log#important
        "WARNING: \"json.parse\" is deprecated and will be removed in future \
         version. Please use \"let json.parse ...\" instead";
      let default = List.assoc "default" p in
      let s = Lang.to_string (List.assoc "" p) in
      try
        let json = Json.from_string s in
        deprecated_of_json default json
      with e ->
        log#important "JSON parsing failed: %s" (Printexc.to_string e);
        default)
