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

let list = Modules.list

let _ =
  let a = Lang.univ_t ~constraints:[Type.ord_constr] () in
  Lang.add_builtin "_[_]" ~category:`List
    ~descr:
      "l[k] returns the first v such that (k,v) is in the list l (or \"\" if \
       no such v exists)."
    [
      ("", Lang.list_t (Lang.product_t a Lang.string_t), None, None);
      ("", a, None, None);
    ]
    Lang.string_t
    (fun p ->
      let l = List.map Lang.to_product (Lang.to_list (Lang.assoc "" 1 p)) in
      let k = Lang.assoc "" 2 p in
      let ans =
        try
          Lang.to_string
            (snd (List.find (fun (k', _) -> Value.compare k k' = 0) l))
        with _ -> ""
      in
      Lang.string ans)

let _ =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~base:list "case" ~category:`List
    ~descr:
      "Define a function by case analysis, depending on whether a list is \
       empty or not."
    [
      ("", Lang.list_t a, None, Some "List to perform case analysis on.");
      ("", b, None, Some "Result when the list is empty.");
      ( "",
        Lang.fun_t [(false, "", a); (false, "", Lang.list_t a)] b,
        None,
        Some "Result when the list is non-empty." );
    ]
    b
    (fun p ->
      let l, e, f = (Lang.assoc "" 1 p, Lang.assoc "" 2 p, Lang.assoc "" 3 p) in
      match Lang.to_list l with
        | [] -> e
        | x :: l -> Lang.apply ~pos:(Lang.pos p) f [("", x); ("", Lang.list l)])

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "length" ~category:`List
    ~descr:"Compute the length of a list, i.e., the number of its elements."
    [("", Lang.list_t a, None, None)]
    Lang.int_t
    (fun p -> Lang.int (List.length (Lang.to_list (List.assoc "" p))))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "nth" ~category:`List
    ~descr:
      "Get the n-th element of a list (the first element is at position 0), or \
       `default` if element does not exist."
    [
      ( "default",
        Lang.nullable_t a,
        Some Lang.null,
        Some
          "Default element. Raises `error.not_found` if `null` and no element \
           can be found in the list." );
      ("", Lang.list_t a, None, None);
      ("", Lang.int_t, None, None);
    ]
    a
    (fun p ->
      let default = Lang.to_option (List.assoc "default" p) in
      let l = Lang.to_list (Lang.assoc "" 1 p) in
      let n = Lang.to_int (Lang.assoc "" 2 p) in
      try List.nth l n
      with _ -> (
        match default with
          | Some v -> v
          | None ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:"no default value for list.nth" "not_found"))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "slice" ~category:`List
    ~descr:
      "Return the sublist of length `length` starting with the element at \
       index `offset`."
    [
      ( "offset",
        Lang.int_t,
        Some (Lang.int 0),
        Some "Index of the first element." );
      ( "length",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some
          "Length of the returned list. Include all elements from `offset` if \
           `null`." );
      ("", Lang.list_t a, None, None);
    ]
    (Lang.list_t a)
    (fun p ->
      let start = Lang.to_int (List.assoc "offset" p) in
      let length = Lang.to_valued_option Lang.to_int (List.assoc "length" p) in
      let l = Lang.to_list (List.assoc "" p) in
      let stop =
        match length with Some l -> start + l | None -> List.length l
      in
      Lang.list (List.filteri (fun pos _ -> start <= pos && pos < stop) l))

let _ =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~base:list "ind" ~category:`List
    ~descr:
      "Define a function by induction on a list. This is slightly more \
       efficient than defining a recursive function. The list is scanned from \
       the right."
    [
      ("", Lang.list_t a, None, Some "List to perform induction on.");
      ("", b, None, Some "Result when the list is empty.");
      ( "",
        Lang.fun_t
          [(false, "", a); (false, "", Lang.list_t a); (false, "", b)]
          b,
        None,
        Some
          "Result when the list is non-empty, given the current element, the \
           tail and the result of the recursive call on the tail." );
    ]
    b
    (fun p ->
      let l, e, f = (Lang.assoc "" 1 p, Lang.assoc "" 2 p, Lang.assoc "" 3 p) in
      let rec aux k = function
        | [] -> k e
        | x :: l ->
            aux
              (fun r ->
                k
                  (Lang.apply ~pos:(Lang.pos p) f
                     [("", x); ("", Lang.list l); ("", r)]))
              l
      in
      aux (fun r -> r) (Lang.to_list l))

let _ =
  let a = Lang.univ_t () in
  List.iter
    (fun (base, name) ->
      ignore
        (Lang.add_builtin ?base name ~category:`List
           ~descr:"Add an element at the top of a list."
           [("", a, None, None); ("", Lang.list_t a, None, None)]
           (Lang.list_t a)
           (fun p ->
             let x, l = (Lang.assoc "" 1 p, Lang.assoc "" 2 p) in
             let l = Lang.to_list l in
             Lang.list (x :: l))))
    [(Some list, "add"); (None, "_::_")]

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "sort" ~category:`List
    ~descr:"Sort a list according to a comparison function."
    [
      ( "",
        Lang.fun_t [(false, "", a); (false, "", a)] Lang.int_t,
        None,
        Some
          "Comparison function f such that f(x,y)<0 when x<y, f(x,y)=0 when \
           x=y, and f(x,y)>0 when x>y." );
      ("", Lang.list_t a, None, Some "List to sort.");
    ]
    (Lang.list_t a)
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let sort x y =
        Lang.to_int (Lang.apply ~pos:(Lang.pos p) f [("", x); ("", y)])
      in
      let l = Lang.assoc "" 2 p in
      Lang.list (List.sort sort (Lang.to_list l)))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "init" ~category:`List ~descr:"Initialize a list."
    [
      ("", Lang.int_t, None, Some "Number of elements in the list.");
      ( "",
        Lang.fun_t [(false, "", Lang.int_t)] a,
        None,
        Some "Function such that `f i` is the `i`th element." );
    ]
    (Lang.list_t a)
    (fun p ->
      let n = Lang.to_int (Lang.assoc "" 1 p) in
      let fn = Lang.assoc "" 2 p in
      let apply n = Lang.apply ~pos:(Lang.pos p) fn [("", Lang.int n)] in
      Lang.list (List.init n apply))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "iteri" ~category:`List
    ~descr:"Call a function on every element of a list, along with its index."
    [
      ( "",
        Lang.fun_t [(false, "", Lang.int_t); (false, "", a)] Lang.unit_t,
        None,
        None );
      ("", Lang.list_t a, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let fn = Lang.assoc "" 1 p in
      let l = Lang.to_list (Lang.assoc "" 2 p) in
      List.iteri
        (fun pos v ->
          ignore (Lang.apply ~pos:(Lang.pos p) fn [("", Lang.int pos); ("", v)]))
        l;
      Lang.unit)

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "append" ~category:`List
    ~descr:"Concatenate two lists."
    [("", Lang.list_t a, None, None); ("", Lang.list_t a, None, None)]
    (Lang.list_t a)
    (fun p ->
      Lang.list
        (List.append
           (Lang.to_list (Lang.assoc "" 1 p))
           (Lang.to_list (Lang.assoc "" 2 p))))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "rev" ~category:`List ~descr:"Revert list order."
    [("", Lang.list_t a, None, None)]
    (Lang.list_t a)
    (fun p -> Lang.list (List.rev (Lang.to_list (List.assoc "" p))))

let _ =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~base:list "map" ~category:`List
    ~descr:"Map a function on every element of a list."
    [
      ("", Lang.fun_t [(false, "", a)] b, None, None);
      ("", Lang.list_t a, None, None);
    ]
    (Lang.list_t b)
    (fun p ->
      let fn = Lang.assoc "" 1 p in
      let l = Lang.to_list (Lang.assoc "" 2 p) in
      Lang.list
        (List.map (fun v -> Lang.apply ~pos:(Lang.pos p) fn [("", v)]) l))

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:list "remove" ~category:`List
    ~descr:"Remove the first occurrence of a value from a list."
    [("", a, None, None); ("", Lang.list_t a, None, None)]
    (Lang.list_t a)
    (fun p ->
      let v = Lang.assoc "" 1 p in
      let lv = Lang.assoc "" 2 p in
      let l = Lang.to_list lv in
      try
        let v = List.find (fun v' -> Value.compare v v' == 0) l in
        Lang.list (List.filter (fun v' -> v' != v) l)
      with Not_found -> lv)

let _ =
  let t = Lang.list_t (Lang.univ_t ()) in
  Lang.add_builtin ~base:list "shuffle" ~category:`List
    ~descr:
      "Shuffle the content of a list. The function returns a list with the \
       same elements but in different, random, order."
    [("", t, None, None)]
    t
    (fun p ->
      List.assoc "" p |> Lang.to_list |> Extralib.List.shuffle |> Lang.list)
