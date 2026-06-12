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

let () =
  Atomic.set Liquidsoap_lang.Builtins_yaml.yaml_parser (fun s ->
      let rec yaml_of_yamlx = function
        | YAMLx.Null _ -> `Null
        | YAMLx.Bool (_, b) -> `Bool b
        | YAMLx.Int (_, i) -> `Float (Int64.to_float i)
        | YAMLx.Float (_, f) -> `Float f
        | YAMLx.String (_, s) -> `String s
        | YAMLx.Seq (_, l) -> `A (List.map yaml_of_yamlx l)
        | YAMLx.Map (_, l) ->
            `O
              (List.filter_map
                 (fun (_, k, v) ->
                   let key_str =
                     match k with
                       | YAMLx.String (_, s) -> Some s
                       | YAMLx.Int (_, i) -> Some (Int64.to_string i)
                       | YAMLx.Float (_, f) -> Some (string_of_float f)
                       | YAMLx.Bool (_, b) -> Some (string_of_bool b)
                       | YAMLx.Null _ -> Some "null"
                       | _ -> None
                   in
                   Option.map (fun s -> (s, yaml_of_yamlx v)) key_str)
                 l)
      in
      match YAMLx.Values.of_yaml_exn s with
        | [] -> `Null
        | [v] -> yaml_of_yamlx v
        | _ -> failwith "Multiple YAML documents not supported")

let yaml = Lang.add_module "yaml"

let rec yamlx_value_of_json = function
  | `Assoc l ->
      YAMLx.Map
        ( YAMLx.zero_loc,
          List.map
            (fun (k, v) ->
              ( YAMLx.zero_loc,
                YAMLx.String (YAMLx.zero_loc, k),
                yamlx_value_of_json v ))
            l )
  | `Tuple l -> YAMLx.Seq (YAMLx.zero_loc, List.map yamlx_value_of_json l)
  | `String s -> YAMLx.String (YAMLx.zero_loc, s)
  | `Bool b -> YAMLx.Bool (YAMLx.zero_loc, b)
  | `Float f -> YAMLx.Float (YAMLx.zero_loc, f)
  | `Int i -> YAMLx.Int (YAMLx.zero_loc, Int64.of_int i)
  | `Null -> YAMLx.Null YAMLx.zero_loc

let rec apply_styles ~scalar_style ~flow = function
  | YAMLx.Scalar_node r ->
      let style = Option.value ~default:r.style scalar_style in
      YAMLx.Scalar_node { r with style }
  | YAMLx.Sequence_node r ->
      let items = List.map (apply_styles ~scalar_style ~flow) r.items in
      YAMLx.Sequence_node { r with items; flow }
  | YAMLx.Mapping_node r ->
      let pairs =
        List.map
          (fun (k, v) ->
            ( apply_styles ~scalar_style ~flow k,
              apply_styles ~scalar_style ~flow v ))
          r.pairs
      in
      YAMLx.Mapping_node { r with pairs; flow }
  | node -> node

let scalar_style pos = function
  | "any" -> None
  | "plain" -> Some YAMLx.Plain
  | "single_quoted" -> Some YAMLx.Single_quoted
  | "double_quoted" -> Some YAMLx.Double_quoted
  | "literal" -> Some YAMLx.Literal
  | "folded" -> Some YAMLx.Folded
  | v ->
      Runtime_error.raise
        ~message:(Printf.sprintf "Invalid scalar style: %s" v)
        ~pos "yaml"

let layout_style pos = function
  | "any" | "block" -> false
  | "flow" -> true
  | v ->
      Runtime_error.raise
        ~message:(Printf.sprintf "Invalid layout style: %s" v)
        ~pos "yaml"

let _ =
  Lang.add_builtin ~base:yaml "stringify" ~category:`String
    ~descr:
      "Convert a value to YAML. If the value cannot be represented as YAML \
       (for instance a function), a `error.yaml` exception is raised."
    [
      ( "scalar_style",
        Lang.string_t,
        Some (Lang.string "any"),
        Some
          "Scalar style. One of: \"any\", \"plain\", \"single_quoted\", \
           \"double_quoted\", \"literal\" or \"folded\"." );
      ( "layout_style",
        Lang.string_t,
        Some (Lang.string "any"),
        Some "Layout style. One of: \"any\", \"block\" or \"flow\"." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.string_t
    (fun p ->
      let pos = Lang.pos p in
      let v = List.assoc "" p in
      let scalar_style =
        scalar_style pos (Lang.to_string (List.assoc "scalar_style" p))
      in
      let flow =
        layout_style pos (Lang.to_string (List.assoc "layout_style" p))
      in
      try
        let json = Liquidsoap_lang.Builtins_json.json_of_value v in
        let nodes = YAMLx.Values.to_nodes [yamlx_value_of_json json] in
        let nodes = List.map (apply_styles ~scalar_style ~flow) nodes in
        Lang.string (YAMLx.Nodes.to_yaml nodes)
      with _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf "Value %s cannot be represented as YAML"
               (Value.to_string v))
          ~pos "yaml")
