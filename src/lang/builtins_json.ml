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

let log = Log.make ["lang"; "json"]
let to_json_ref = ref (fun ~compact:_ ~json5:_ _ -> assert false)

module JSON = Lang.MkAbstract (struct
  type content = (string, Lang.value) Hashtbl.t

  let name = "json"
  let descr _ = "json"

  let to_json ~compact ~json5 v =
    if Hashtbl.length v > 0 then
      !to_json_ref ~compact ~json5
        (Lang.record (Hashtbl.fold (fun k v l -> (k, v) :: l) v []))
    else "{}"

  let compare = Stdlib.compare
end)

let rec to_json_compact ~json5 v =
  match v.Lang.value with
    | Lang.Ground g -> Term.Ground.to_json ~compact:true ~json5 g
    | Lang.List l ->
        Printf.sprintf "[%s]"
          (String.concat "," (List.map (to_json_compact ~json5) l))
    | Lang.Null -> "null"
    | Lang.Tuple l ->
        "[" ^ String.concat "," (List.map (to_json_compact ~json5) l) ^ "]"
    | Lang.Meth _ -> (
        let m, v = Term.Value.split_meths v in
        match v.Lang.value with
          | Lang.Tuple [] ->
              let l =
                List.map
                  (fun (l, v) ->
                    Printf.sprintf "\"%s\":%s" l (to_json_compact ~json5 v))
                  m
              in
              Printf.sprintf "{%s}" (String.concat "," l)
          | _ -> to_json_compact ~json5 v)
    | Lang.Source _ -> "\"<source>\""
    | Lang.Ref v ->
        Printf.sprintf "{\"reference\": %s}" (to_json_compact ~json5 !v)
    | Lang.Encoder e -> Utils.quote_string (Encoder.string_of_format e)
    | Lang.FFI _ | Lang.Fun _ -> "\"<fun>\""

let pp_list sep ppx f l =
  let pp_sep f () = Format.fprintf f "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx f l

let rec to_json_pp ~json5 f v =
  match v.Lang.value with
    | Lang.Ground g ->
        Format.fprintf f "%s" (Term.Ground.to_json ~compact:false ~json5 g)
    | Lang.Tuple [] | Lang.List [] -> Format.fprintf f "[]"
    | Lang.Tuple l | Lang.List l ->
        Format.fprintf f "[@;<1 0>%a@;<1 -2>]"
          (pp_list "," (to_json_pp ~json5))
          l
    | Lang.Meth _ -> (
        let l, v = Term.Value.split_meths v in
        match v.Lang.value with
          | Lang.Tuple [] ->
              let format_field f (k, v) =
                Format.fprintf f "@[<hv2>%s: %a@]" (Utils.quote_string k)
                  (to_json_pp ~json5) v
              in
              Format.fprintf f "{@;<1 0>%a@;<1 -2>}" (pp_list "," format_field)
                l
          | _ -> Format.fprintf f "%a" (to_json_pp ~json5) v)
    | Lang.Ref v ->
        Format.fprintf f "@[{@;<1 1>@[\"reference\":@;<0 1>%a@]@;<1 0>}@]"
          (to_json_pp ~json5) !v
    | _ -> Format.fprintf f "%s" (to_json_compact ~json5 v)

let to_json_pp ~json5 v = Format.asprintf "@[<hv2>%a@]" (to_json_pp ~json5) v

let to_json ~compact ~json5 v =
  if compact then to_json_compact ~json5 v else to_json_pp ~json5 v

let () = to_json_ref := to_json

let () =
  let val_t = Lang.univ_t () in
  let var =
    match val_t.Type.descr with Type.EVar v -> v | _ -> assert false
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
          Lang.val_fun [("", "", None); ("", "", None)] (fun p ->
              let key = Lang.to_string (Lang.assoc "" 1 p) in
              let value = Lang.assoc "" 2 p in
              Hashtbl.replace v key value;
              Lang.unit) );
      ( "remove",
        ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
        "Remove a key from the object. Does not nothing if the key does not \
         exist.",
        fun v ->
          Lang.val_fun [("", "", None)] (fun p ->
              let key = Lang.to_string (List.assoc "" p) in
              Hashtbl.remove v key;
              Lang.unit) );
    ]
  in
  let t =
    Lang.method_t JSON.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  Lang.add_builtin "json" ~category:`String
    ~descr:"Create a generic json object" [] t (fun _ ->
      let v = Hashtbl.create 10 in
      let meth = List.map (fun (name, _, _, fn) -> (name, fn v)) meth in
      Lang.meth (JSON.to_value v) meth)

let () =
  Lang.add_builtin "json.stringify" ~category:`String
    ~descr:"Convert a value to a json string."
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
      let compact = Lang.to_bool (List.assoc "compact" p) in
      let json5 = Lang.to_bool (List.assoc "json5" p) in
      let v = to_json ~compact ~json5 (List.assoc "" p) in
      Lang.string v)

exception Failed

let () =
  Printexc.register_printer (function
    | Failed -> Some "Liquidsoap count not parse JSON string"
    | _ -> None)

(* We compare the default's type with the parsed json value and return if they
   match. This comes with json.stringify in Builtin. *)
let rec of_json d j =
  match (d.Lang.value, j) with
    | Lang.Tuple [], `Null -> Lang.unit
    | Lang.Ground (Lang.Ground.Bool _), `Bool b -> Lang.bool b
    (* JSON specs do not differentiate between ints and floats. Therefore, we
       should parse int as floats when required. *)
    | Lang.Ground (Lang.Ground.Int _), `Int i -> Lang.int i
    | Lang.Ground (Lang.Ground.Float _), `Int i -> Lang.float (float_of_int i)
    | Lang.Ground (Lang.Ground.Float _), `Float x -> Lang.float x
    | Lang.Ground (Lang.Ground.String _), `String s -> Lang.string s
    (* Be liberal and allow casting basic types to string. *)
    | Lang.Ground (Lang.Ground.String _), `Int i ->
        Lang.string (string_of_int i)
    | Lang.Ground (Lang.Ground.String _), `Float x ->
        Lang.string (string_of_float x)
    | Lang.Ground (Lang.Ground.String _), `Bool b ->
        Lang.string (string_of_bool b)
    | Lang.List [], `List [] -> Lang.list []
    | Lang.List (d :: _), `List l ->
        (* TODO: we could also try with other elements of the default list... *)
        let l = List.map (of_json d) l in
        Lang.list l
    | Lang.Tuple dd, `List jj when List.length dd = List.length jj ->
        Lang.tuple (List.map2 of_json dd jj)
    | ( Lang.List
          ({
             Lang.value =
               Lang.Tuple
                 [{ Lang.value = Lang.Ground (Lang.Ground.String _) }; d];
           }
          :: _),
        `Assoc l ) ->
        (* Try to convert the object to a list of pairs, dropping fields that
           cannot be parsed.  This requires the target type to be [(string*'a)],
           currently it won't work if it is [?T] which would be obtained with
           of_json(default=[],...). *)
        let l =
          List.fold_left
            (fun cur (x, y) ->
              try Lang.product (Lang.string x) (of_json d y) :: cur
              with _ -> cur)
            [] l
        in
        Lang.list l
    (* Parse records. *)
    | Lang.Meth (l, x, d), `Assoc a -> (
        try
          let y = List.assoc l a in
          let v = of_json x y in
          let a' = List.remove_assoc l a in
          Lang.meth (of_json d (`Assoc a')) [(l, v)]
        with Not_found -> raise Failed)
    | Lang.Tuple [], `Assoc _ -> Lang.unit
    | _ -> raise Failed

let () =
  let t = Lang.univ_t () in
  Lang.add_builtin ~category:`String
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
      ("", Lang.string_t, None, None);
    ] t (fun p ->
      let default = List.assoc "default" p in
      let s = Lang.to_string (List.assoc "" p) in
      try
        let json = Configure.JSON.from_string s in
        of_json default json
      with e ->
        log#info "JSON parsing failed: %s" (Printexc.to_string e);
        default)
