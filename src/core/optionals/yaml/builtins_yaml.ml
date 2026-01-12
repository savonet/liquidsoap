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

let () = Atomic.set Liquidsoap_lang.Builtins_yaml.yaml_parser Yaml.of_string_exn
let yaml = Lang.add_module "yaml"

let rec yaml_of_json = function
  | `Assoc l -> `O (List.map (fun (lbl, v) -> (lbl, yaml_of_json v)) l)
  | `Tuple l -> `A (List.map yaml_of_json l)
  | `String s -> `String s
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int i -> `Float (float i)
  | `Null -> `Null

let scalar_style pos = function
  | "any" -> `Any
  | "plain" -> `Plain
  | "single_quoted" -> `Single_quoted
  | "double_quoted" -> `Double_quoted
  | "literal" -> `Literal
  | "folded" -> `Folded
  | v ->
      Runtime_error.raise
        ~message:(Printf.sprintf "Invalid scalar style: %s" v)
        ~pos "yaml"

let layout_style pos = function
  | "any" -> `Any
  | "block" -> `Block
  | "flow" -> `Flow
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
      let layout_style =
        layout_style pos (Lang.to_string (List.assoc "layout_style" p))
      in
      try
        let json = Liquidsoap_lang.Builtins_json.json_of_value v in
        Lang.string
          (Yaml.to_string_exn ~encoding:`Utf8 ~scalar_style ~layout_style
             (yaml_of_json json))
      with _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf "Value %s cannot be represented as YAML"
               (Value.to_string v))
          ~pos "yaml")
