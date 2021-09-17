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
let to_json_ref = ref (fun ~compact:_ ~json5:_ _ -> assert false)

module JSON = Value.MkAbstract (struct
  type content = (string, value) Hashtbl.t

  let name = "json"
  let descr _ = "json"

  let to_json ~compact ~json5 v =
    if Hashtbl.length v > 0 then
      !to_json_ref ~compact ~json5
        (record (Hashtbl.fold (fun k v l -> (k, v) :: l) v []))
    else "{}"

  let compare = Stdlib.compare
end)

let rec to_json_compact ~json5 v =
  match v.value with
    | Ground g -> Term.Ground.to_json ~compact:true ~json5 g
    | List l ->
        Printf.sprintf "[%s]"
          (String.concat "," (List.map (to_json_compact ~json5) l))
    | Null -> "null"
    | Tuple l ->
        "[" ^ String.concat "," (List.map (to_json_compact ~json5) l) ^ "]"
    | Meth _ -> (
        let m, v = Value.split_meths v in
        match v.value with
          | Tuple [] ->
              let l =
                List.map
                  (fun (l, v) ->
                    Printf.sprintf "\"%s\":%s" l (to_json_compact ~json5 v))
                  m
              in
              Printf.sprintf "{%s}" (String.concat "," l)
          | _ -> to_json_compact ~json5 v)
    | Source _ -> "\"<source>\""
    | Ref v -> Printf.sprintf "{\"reference\": %s}" (to_json_compact ~json5 !v)
    | Encoder e -> Utils.quote_utf8_string (Encoder.string_of_format e)
    | FFI _ | Fun _ -> "\"<fun>\""

let rec to_json_pp ~json5 f v =
  match v.value with
    | Ground g ->
        Format.fprintf f "%s" (Term.Ground.to_json ~compact:false ~json5 g)
    | List l ->
        let print f l =
          let len = List.length l in
          let f pos x =
            if pos < len - 1 then
              Format.fprintf f "%a,@;<1 0>" (to_json_pp ~json5) x
            else Format.fprintf f "%a" (to_json_pp ~json5) x;
            pos + 1
          in
          ignore (List.fold_left f 0 l)
        in
        Format.fprintf f "@[[@;<1 1>@[%a@]@;<1 0>]@]" print l
    | Tuple l ->
        Format.fprintf f "@[[@;<1 1>@[";
        let rec aux = function
          | [] -> ()
          | [p] -> Format.fprintf f "%a" (to_json_pp ~json5) p
          | p :: l ->
              Format.fprintf f "%a,@;<1 0>" (to_json_pp ~json5) p;
              aux l
        in
        aux l;
        Format.fprintf f "@]@;<1 0>]@]"
    | Meth _ -> (
        let l, v = Value.split_meths v in
        match v.value with
          | Tuple [] ->
              Format.fprintf f "@{{@;<1 1>@[";
              let rec aux = function
                | [] -> ()
                | [(k, v)] ->
                    Format.fprintf f "%s: %a"
                      (Utils.quote_utf8_string k)
                      (to_json_pp ~json5) v
                | (k, v) :: l ->
                    Format.fprintf f "%s: %a,@;<1 0>"
                      (Utils.quote_utf8_string k)
                      (to_json_pp ~json5) v;
                    aux l
              in
              aux l;
              Format.fprintf f "@]@;<1 0>}@]"
          | _ -> Format.fprintf f "%a" (to_json_pp ~json5) v)
    | Ref v ->
        Format.fprintf f "@[{@;<1 1>@[\"reference\":@;<0 1>%a@]@;<1 0>}@]"
          (to_json_pp ~json5) !v
    | _ -> Format.fprintf f "%s" (to_json_compact ~json5 v)

let to_json_pp ~json5 v =
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  ignore (to_json_pp ~json5 f v);
  Format.pp_print_flush f ();
  Buffer.contents b

let to_json ~compact ~json5 v =
  if compact then to_json_compact ~json5 v else to_json_pp ~json5 v

let () = to_json_ref := to_json

let () =
  let val_t = univ_t () in
  let var =
    match val_t.Type.descr with Type.EVar v -> v | _ -> assert false
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
    method_t JSON.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  add_builtin "json" ~category:`String ~descr:"Create a generic json object" []
    t (fun _ ->
      let v = Hashtbl.create 10 in
      let meth = List.map (fun (name, _, _, fn) -> (name, fn v)) meth in
      Lang.meth (JSON.to_value v) meth)

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
      let v = to_json ~compact ~json5 (List.assoc "" p) in
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
    | List [], `List [] -> list []
    | List (d :: _), `List l ->
        (* TODO: we could also try with other elements of the default list... *)
        let l = List.map (of_json d) l in
        list l
    | Tuple dd, `List jj when List.length dd = List.length jj ->
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
        let json = Configure.JSON.from_string s in
        of_json default json
      with e ->
        log#info "JSON parsing failed: %s" (Printexc.to_string e);
        default)
