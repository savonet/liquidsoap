(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

let () =
  let add op name descr =
    let t = Lang.float_t in
    ignore
      (Lang.add_builtin name ~category:`Math ~descr
         [("", t, None, None)]
         t
         (fun p ->
           let a = Lang.to_float (List.assoc "" p) in
           Lang.float (op a)))
  in
  add sqrt "sqrt" "Square root.";
  add exp "exp" "Exponential.";
  add Stdlib.log "ln" "Natural logarithm.";
  add log10 "log10" "Base 10 logarithm.";
  add sin "sin" "Sine. Argument is in radians.";
  add cos "cos" "Cosine. Argument is in radians.";
  add tan "tan" "Tangent. Argument is in radians.";
  add acos "acos"
    "Arc cosine. The argument must fall within the range [-1.0, 1.0]. Result \
     is in radians and is between 0.0 and pi.";
  add asin "asin"
    "Arc sine. The argument must fall within the range [-1.0, 1.0]. Result is \
     in radians and is between -pi/2 and pi/2.";
  add atan "atan"
    "Arc tangent. Result is in radians and is between -pi/2 and pi/2.";
  add cosh "cosh" "Hyperbolic cosine. Argument is in radians.";
  add sinh "sinh" "Hyperbolic sine. Argument is in radians.";
  add tanh "tanh" "Hyperbolic tangent. Argument is in radians."

let _ =
  let t = Lang.univ_t ~constraints:[Type.num_constr] () in
  Lang.add_builtin "~-" ~category:`Math
    ~descr:"Returns the opposite of its argument."
    [("", t, None, None)]
    t
    (fun p ->
      match Lang.to_num (List.assoc "" p) with
        | `Int i -> Lang.int ~-i
        | `Float i -> Lang.float ~-.i)

let _ =
  let t = Lang.univ_t ~constraints:[Type.num_constr] () in
  Lang.add_builtin "abs" ~category:`Math ~descr:"Absolute value."
    [("", t, None, None)]
    t
    (fun p ->
      match Lang.to_num (List.assoc "" p) with
        | `Int i -> Lang.int (abs i)
        | `Float i -> Lang.float (abs_float i))

let () =
  let register_op doc name op_int op_float =
    let t = Lang.univ_t ~constraints:[Type.num_constr] () in
    ignore
      (Lang.add_builtin name ~category:`Math
         ~descr:(Printf.sprintf "%s of numbers." doc)
         [("", t, None, None); ("", t, None, None)]
         t
         (fun p ->
           let a = Lang.to_num (Lang.assoc "" 1 p) in
           let b = Lang.to_num (Lang.assoc "" 2 p) in
           match (a, b) with
             | `Int a, `Int b -> Lang.int (op_int a b)
             | `Float a, `Float b -> Lang.float (op_float a b)
             | _ -> assert false))
  in
  register_op "Multiplication" "*" ( * ) ( *. );
  register_op "Division" "/" ( / ) ( /. );
  register_op "Addition" "+" ( + ) ( +. );
  register_op "Subtraction " "-" ( - ) ( -. );
  register_op "Exponentiation" "pow"
    (fun a b -> int_of_float (float_of_int a ** float_of_int b))
    ( ** );
  register_op "Remainder of division" "mod" ( mod ) mod_float

let _ =
  let t = Lang.univ_t ~constraints:[Type.num_constr] () in
  Lang.add_builtin "float" ~category:`Math ~descr:"Convert a number to a float."
    [("", t, None, None)]
    Lang.float_t
    (fun p ->
      let x = List.assoc "" p |> Lang.to_num in
      let x = match x with `Int x -> float x | `Float x -> x in
      Lang.float x)

let _ =
  let t = Lang.univ_t ~constraints:[Type.num_constr] () in
  Lang.add_builtin "int" ~category:`Math
    ~descr:"Convert a number to an integer."
    [("", t, None, None)]
    Lang.int_t
    (fun p ->
      let x = List.assoc "" p |> Lang.to_num in
      let x = match x with `Int x -> x | `Float x -> int_of_float x in
      Lang.int x)

let _ =
  Lang.add_builtin ~base:Modules.random "float" ~category:`Math
    ~descr:
      "Generate a random value between `min` (included) and `max` (excluded)."
    [
      ("min", Lang.float_t, Some (Lang.float 0.), None);
      ("max", Lang.float_t, Some (Lang.float 1.), None);
    ]
    Lang.float_t
    (fun p ->
      let min = Lang.to_float (List.assoc "min" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      Lang.float (Random.float (max -. min) +. min))

let _ =
  Lang.add_builtin ~base:Modules.random "int" ~category:`Math
    ~descr:
      "Generate a random value between `min` (included) and `max` (excluded)."
    [
      ("min", Lang.int_t, Some (Lang.int (1 - (1 lsl 29))), None);
      ("max", Lang.int_t, Some (Lang.int (1 lsl 29)), None);
    ]
    Lang.int_t
    (fun p ->
      let min = Lang.to_int (List.assoc "min" p) in
      let max = Lang.to_int (List.assoc "max" p) in
      Lang.int (Random.int (max - min) + min))

let _ =
  Lang.add_builtin ~base:Modules.random "bool" ~category:`Bool
    ~descr:"Generate a random boolean." [] Lang.bool_t (fun _ ->
      Lang.bool (Random.bool ()))

let _ =
  Lang.add_builtin_base ~category:`Math ~descr:"Maximal representable integer."
    "max_int"
    Lang.(Ground (Ground.Int max_int))
    Lang.int_t

let _ =
  Lang.add_builtin_base ~category:`Math ~descr:"Minimal representable integer."
    "min_int"
    Lang.(Ground (Ground.Int min_int))
    Lang.int_t

let _ =
  Lang.add_builtin_base ~category:`Math
    ~descr:"Float representation of infinity." "infinity"
    Lang.(Ground (Ground.Float infinity))
    Lang.float_t

let _ =
  Lang.add_builtin_base ~category:`Math
    ~descr:
      "A special floating-point value denoting the result of an undefined \
       operation such as 0.0 /. 0.0. Stands for 'not a number'. Any \
       floating-point operation with nan as argument returns nan as result. As \
       for floating-point comparisons, `==`, `<`, `<=`, `>` and `>=` return \
       `false` and `!=` returns `true` if one or both of their arguments is \
       `nan`."
    "nan"
    Lang.(Ground (Ground.Float nan))
    Lang.float_t

let _ =
  Lang.add_builtin "lsl" ~category:`Math ~descr:"Logical shift left."
    [
      ("", Lang.int_t, None, Some "Number to shift.");
      ("", Lang.int_t, None, Some "Number of bits to shift.");
    ]
    Lang.int_t
    (fun p ->
      let n = Lang.to_int (Lang.assoc "" 1 p) in
      let b = Lang.to_int (Lang.assoc "" 2 p) in
      Lang.int (n lsl b))

let _ =
  Lang.add_builtin "lsr" ~category:`Math ~descr:"Logical shift right."
    [
      ("", Lang.int_t, None, Some "Number to shift.");
      ("", Lang.int_t, None, Some "Number of bits to shift.");
    ]
    Lang.int_t
    (fun p ->
      let n = Lang.to_int (Lang.assoc "" 1 p) in
      let b = Lang.to_int (Lang.assoc "" 2 p) in
      Lang.int (n lsr b))
