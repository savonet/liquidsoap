(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let log = Dtools.Log.make ["lang";"json"]

(* We compare the default's type with
 * the parsed json value and return if they match..
 * This comes with json_of in Lang_builtins.. *)
let rec of_json t j =
  let (<:) = Lang_types.(<:) in
  let f x =
    try
      ignore (x <: t) ;
      true
     with _ -> false
  in
  match j with
    | `Null when f Lang.unit_t -> Lang.unit
    | `Bool b when f Lang.bool_t -> Lang.bool b
    (* JSON specs do not differenciate between ints
     * and floats. Therefore, we should parse int as
     * floats when required.. *)
    | `Int i when f Lang.int_t -> Lang.int i
    | `Int i when f Lang.float_t -> Lang.float (float_of_int i)
    | `String s when f Lang.string_t -> Lang.string s
    | `Float x when f Lang.float_t -> Lang.float x
    | `List l ->
       (* First, try to parse as a list. *)
       begin
        try
         let t = Lang.of_list_t t in
         let l = List.map (of_json t) l in
         Lang.list ~t l
        with _  ->
         (* Otherwise try to parse as product. *)
         begin
          match l with
            | [j;j'] ->
                let (t,t') = Lang.of_product_t t in
                Lang.product (of_json t j)
                             (of_json t' j')
            | _ -> failwith "could not parse JSON string."
         end
       end
    | `Assoc l ->
        (* Try to convert the object to a list of pairs of strings
         * This requires the target type to be [(string*string)],
         * currently it won't work if it is [?T] which would be
         * obtained with of_json(default=[],...). *)
        let lt = Lang.of_list_t t in
        let (t,t') = Lang.of_product_t lt in
        ignore (Lang.string_t <: t) ;
        let l =
          List.map
            (fun (x,y) -> Lang.product (Lang.string x)
                                       (of_json t' y))
            l
        in
        Lang.list ~t:lt l
    | _ -> failwith "could not parse JSON string."

let () =
  let t = Lang.univ_t 1 in
  Lang_builtins.add_builtin
   ~cat:Lang_builtins.String
   ~descr:"Parse a json string into a liquidsoap value."
   "of_json"
   ["default", t, None, Some "Default value if string cannot \
                              be parsed.";
    "", Lang.string_t, None, None ] t
   (fun p ->
     let default = List.assoc "default" p in
     let s = Lang.to_string (List.assoc "" p) in
     try
       let json =
         Yojson.Basic.from_string s
       in
       of_json default.Lang.t json
     with
       | e ->
          log#f 4 "JSON parsing failed: %s" (Utils.error_message e);
          default)
