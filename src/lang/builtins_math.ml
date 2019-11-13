(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_builtins

let () =
  let add op name descr =
    let t = Lang.float_t in
    add_builtin name ~cat:Math ~descr [("", t, None, None)] t (fun p ->
        match p with
          | [("", {Lang.value= Lang.Float a; _})] ->
              Lang.float (op a)
          | _ ->
              assert false)
  in
  add sqrt "sqrt" "Square root." ;
  add exp "exp" "Exponential." ;
  add Stdlib.log "log_e" "Natural logarithm." ;
  add log10 "log_10" "Base 10 logarithm." ;
  add sin "sin" "Sine. Argument is in radians." ;
  add cos "cos" "Cosine. Argument is in radians." ;
  add tan "tan" "Tangent. Argument is in radians." ;
  add acos "acos"
    "Arc cosine. The argument must fall within the range [-1.0, 1.0]. Result \
     is in radians and is between 0.0 and pi." ;
  add asin "asin"
    "Arc sine. The argument must fall within the range [-1.0, 1.0]. Result is \
     in radians and is between -pi/2 and pi/2." ;
  add atan "atan"
    "Arc tangent. Result is in radians and is between -pi/2 and pi/2." ;
  add cosh "cosh" "Hyperbolic cosine. Argument is in radians." ;
  add sinh "sinh" "Hyperbolic sine. Argument is in radians." ;
  add tanh "tanh" "Hyperbolic tangent. Argument is in radians."

let () =
  let t = Lang.int_t in
  add_builtin "mod" ~cat:Math
    ~descr:
      "Integer remainder. If y is not zero, x == (x / y) * y + x mod y, and \
       abs(x mod y) <= abs(y)-1." [("", t, None, None); ("", t, None, None)] t
    (fun p ->
      match p with
        | [("", {Lang.value= Lang.Int a; _}); ("", {Lang.value= Lang.Int b; _})]
          ->
            Lang.int (a mod b)
        | _ ->
            assert false)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Num] () in
  add_builtin "~-" ~cat:Math ~descr:"Returns the opposite of its argument."
    [("", t, None, None)] t (function
    | [("", {Lang.value= Lang.Int i; _})] ->
        Lang.int ~-i
    | [("", {Lang.value= Lang.Float i; _})] ->
        Lang.float ~-.i
    | _ ->
        assert false)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Num] () in
  add_builtin "abs" ~cat:Math ~descr:"Absolute value." [("", t, None, None)] t
    (fun p ->
      match (snd (List.hd p)).Lang.value with
        | Lang.Int i ->
            Lang.int (abs i)
        | Lang.Float i ->
            Lang.float (abs_float i)
        | _ ->
            assert false)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Num] () in
  let register_op doc name op_int op_float =
    add_builtin name ~cat:Math ~descr:(Printf.sprintf "%s of numbers." doc)
      [("", t, None, None); ("", t, None, None)] t (fun p ->
        match p with
          | [ ("", {Lang.value= Lang.Int a; _});
              ("", {Lang.value= Lang.Int b; _}) ] ->
              Lang.int (op_int a b)
          | [ ("", {Lang.value= Lang.Float a; _});
              ("", {Lang.value= Lang.Float b; _}) ] ->
              Lang.float (op_float a b)
          | _ ->
              assert false)
  in
  register_op "Multiplication" "*" ( * ) ( *. ) ;
  register_op "Division" "/" ( / ) ( /. ) ;
  register_op "Addition" "+" ( + ) ( +. ) ;
  register_op "Substraction " "-" ( - ) ( -. ) ;
  register_op "Exponentiation" "pow"
    (fun a b -> int_of_float (float_of_int a ** float_of_int b))
    ( ** )

let () =
  add_builtin "random.float" ~cat:Math
    ~descr:
      "Generate a random value between `min` (included) and `max` (excluded)."
    (* TODO better default values *)
    [ ("min", Lang.float_t, Some (Lang.float (-1000000.)), None);
      ("max", Lang.float_t, Some (Lang.float 1000000.), None) ]
    Lang.float_t
    (fun p ->
      let min = Lang.to_float (List.assoc "min" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      Lang.float (Random.float (max -. min) +. min))

let () =
  add_builtin "random.int" ~cat:Math
    ~descr:
      "Generate a random value between `min` (included) and `max` (excluded)."
    [ ("min", Lang.int_t, Some (Lang.int (1 - (1 lsl 29))), None);
      ("max", Lang.int_t, Some (Lang.int (1 lsl 29)), None) ]
    Lang.int_t
    (fun p ->
      let min = Lang.to_int (List.assoc "min" p) in
      let max = Lang.to_int (List.assoc "max" p) in
      Lang.int (Random.int (max - min) + min))

let () =
  add_builtin "random.bool" ~cat:Bool ~descr:"Generate a random boolean." []
    Lang.bool_t (fun _ -> Lang.bool (Random.bool ()))

let () =
  add_builtin "max_int" ~cat:Math ~descr:"Maximal representable integer."
    ~flags:[Lang.Hidden] [] Lang.int_t (fun _ -> Lang.int max_int)

let () =
  add_builtin "min_int" ~cat:Math ~descr:"Minimal representable integer."
    ~flags:[Lang.Hidden] [] Lang.int_t (fun _ -> Lang.int min_int)

let () =
  add_builtin "lsl" ~cat:Math ~descr:"Logical shift left."
    [ ("", Lang.int_t, None, Some "Number to shift.");
      ("", Lang.int_t, None, Some "Number of bits to shift.") ] Lang.int_t
    (fun p ->
      let n = Lang.to_int (Lang.assoc "" 1 p) in
      let b = Lang.to_int (Lang.assoc "" 2 p) in
      Lang.int (n lsl b))

let () =
  add_builtin "lsr" ~cat:Math ~descr:"Logical shift right."
    [ ("", Lang.int_t, None, Some "Number to shift.");
      ("", Lang.int_t, None, Some "Number of bits to shift.") ] Lang.int_t
    (fun p ->
      let n = Lang.to_int (Lang.assoc "" 1 p) in
      let b = Lang.to_int (Lang.assoc "" 2 p) in
      Lang.int (n lsr b))
