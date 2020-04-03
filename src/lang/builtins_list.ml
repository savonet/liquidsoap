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
  (* TODO It would be good to generalize this one but we'd need a way to handle
     errors. *)
  add_builtin "_[_]" ~cat:List
    ~descr:
      "l[k] returns the first v such that (k,v) is in the list l (or \"\" if \
       no such v exists)."
    [("", Lang.string_t, None, None); ("", Lang.metadata_t, None, None)]
    Lang.string_t (fun p ->
      let k = Lang.to_string (Lang.assoc "" 1 p) in
      let l =
        List.map
          (fun p ->
            let a, b = Lang.to_product p in
            (Lang.to_string a, Lang.to_string b))
          (Lang.to_list (Lang.assoc "" 2 p))
      in
      Lang.string (try List.assoc k l with _ -> ""))

let () =
  let a = Lang.univ_t 1 in
  let b = Lang.univ_t 2 in
  Lang.add_builtin "list.case" ~category:(string_of_category List)
    ~descr:
      "Define a function by case analysis, depending on whether a list is \
       empty or not."
    [
      ("", b, None, Some "Result when the list is empty.");
      ( "",
        Lang.fun_t [(false, "", a); (false, "", Lang.list_t a)] b,
        None,
        Some "Result when the list it non-empty." );
      ("", Lang.list_t a, None, Some "List to perform case analysis on.");
    ]
    b
    (fun p b ->
      let e, f, l =
        match p with
          | [("", e); ("", f); ("", l)] -> (e, f, l)
          | _ -> assert false
      in
      let a = Lang.of_list_t l.Lang.t in
      match Lang.to_list l with
        | [] -> e
        | x :: l -> Lang.apply ~t:b f [("", x); ("", Lang.list ~t:a l)])

exception Found_assoc of (Lang.value * Lang.value)

let assoc_value k l =
  try
    List.iter
      (fun ((k', _) as el) ->
        if compare_value k k' == 0 then raise (Found_assoc el))
      l;
    raise Not_found
  with Found_assoc v -> v

let () =
  let t1 = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
  let t2 = Lang.univ_t 2 in
  let lt = Lang.list_t (Lang.product_t t1 t2) in
  add_builtin "list.assoc" ~cat:List
    ~descr:"Generalized l[k] with default value."
    [
      ("default", t2, None, Some "Default value if key does not exist");
      ("", t1, None, None);
      ("", lt, None, None);
    ] t2 (fun p ->
      let default = List.assoc "default" p in
      let k = Lang.assoc "" 1 p in
      let l = List.map Lang.to_product (Lang.to_list (Lang.assoc "" 2 p)) in
      try snd (assoc_value k l) with Not_found -> default)

let () =
  let t1 = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
  let t2 = Lang.univ_t 2 in
  let lt = Lang.list_t (Lang.product_t t1 t2) in
  add_builtin "list.remove_assoc" ~cat:List
    ~descr:"Remove the first pair from an associative list."
    [
      ("", t1, None, Some "Key of pair to be removed");
      ("", lt, None, Some "List of pairs (key,value)");
    ] lt (fun p ->
      let k = Lang.assoc "" 1 p in
      let l = Lang.assoc "" 2 p in
      let t = Lang.of_list_t l.Lang.t in
      let l = List.map Lang.to_product (Lang.to_list (Lang.assoc "" 2 p)) in
      let l =
        try
          let k = fst (assoc_value k l) in
          List.remove_assoc k l
        with Not_found -> l
      in
      Lang.list ~t (List.map (fun (x, y) -> Lang.product x y) l))

let () =
  Lang.add_builtin "list.add" ~category:(string_of_category List)
    ~descr:"Add an element at the top of a list."
    [
      ("", Lang.univ_t 1, None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 1))
    (fun p t ->
      let t = Lang.of_list_t t in
      let x, l =
        match p with [("", x); ("", l)] -> (x, l) | _ -> assert false
      in
      let l = Lang.to_list l in
      Lang.list ~t (x :: l))

let () =
  add_builtin "list.iter" ~cat:List
    ~descr:"Call a function on every element of a list."
    [
      ("", Lang.fun_t [(false, "", Lang.univ_t 1)] Lang.unit_t, None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    Lang.unit_t
    (fun p ->
      let f, l =
        match p with (_, f) :: (_, l) :: _ -> (f, l) | _ -> assert false
      in
      let l = Lang.to_list l in
      List.iter (fun c -> ignore (Lang.apply ~t:Lang.unit_t f [("", c)])) l;
      Lang.unit)

let () =
  Lang.add_builtin "list.map" ~category:(string_of_category List)
    ~descr:"Map a function on every element of a list."
    [
      ("", Lang.fun_t [(false, "", Lang.univ_t 1)] (Lang.univ_t 2), None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 2))
    (fun p t ->
      let f, l =
        match p with [("", f); ("", l)] -> (f, l) | _ -> assert false
      in
      let t = Lang.of_list_t t in
      let l = Lang.to_list l in
      let l = List.map (fun c -> Lang.apply ~t f [("", c)]) l in
      Lang.list ~t l)

let () =
  Lang.add_builtin "list.mapi" ~category:(string_of_category List)
    ~descr:"Map a function on every element of a list, along with its index."
    [
      ( "",
        Lang.fun_t
          [(false, "", Lang.int_t); (false, "", Lang.univ_t 1)]
          (Lang.univ_t 2),
        None,
        None );
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 2))
    (fun p t ->
      let f, l =
        match p with [("", f); ("", l)] -> (f, l) | _ -> assert false
      in
      let t = Lang.of_list_t t in
      let l = Lang.to_list l in
      let l =
        List.mapi (fun i c -> Lang.apply ~t f [("", Lang.int i); ("", c)]) l
      in
      Lang.list ~t l)

let () =
  let t = Lang.list_t (Lang.univ_t 1) in
  Lang.add_builtin "list.randomize" ~category:(string_of_category List)
    ~descr:"Shuffle the content of a list." [("", t, None, None)] t (fun p t ->
      let t = Lang.of_list_t t in
      let l = Array.of_list (Lang.to_list (List.assoc "" p)) in
      Utils.randomize l;
      Lang.list ~t (Array.to_list l))

let () =
  add_builtin "list.fold" ~cat:List
    ~descr:
      "Fold a function on every element of a list: list.fold(f,x1,[e1,..,en]) \
       is f(...f(f(x1,e1),e2)...,en)."
    [
      ( "",
        Lang.fun_t
          [(false, "", Lang.univ_t 1); (false, "", Lang.univ_t 2)]
          (Lang.univ_t 1),
        None,
        Some
          "Function f for which f(x,e) which will be called on every element e \
           with the current value of x, returning the new value of x." );
      ( "",
        Lang.univ_t 1,
        None,
        Some "Initial value x1, to be updated by successive calls of f(x,e)." );
      ("", Lang.list_t (Lang.univ_t 2), None, None);
    ]
    (Lang.univ_t 1)
    (fun p ->
      let f, x, l =
        match p with
          | (_, f) :: (_, x) :: (_, l) :: _ -> (f, x, l)
          | _ -> assert false
      in
      let l = Lang.to_list l in
      List.fold_left
        (fun x y -> Lang.apply ~t:x.Lang.t f [("", x); ("", y)])
        x l)

let () =
  let t = Lang.univ_t 1 in
  add_builtin "list.nth" ~cat:List
    ~descr:
      "Get the n-th element of a list (the first element is at position 0), \
       or'default' if element does not exist."
    [
      ("default", t, None, Some "Default value if key does not exist");
      ("", Lang.list_t t, None, None);
      ("", Lang.int_t, None, None);
    ]
    t
    (fun p ->
      let default = List.assoc "default" p in
      try
        List.nth
          (Lang.to_list (Lang.assoc "" 1 p))
          (Lang.to_int (Lang.assoc "" 2 p))
      with _ -> default)

let () =
  let t = Lang.univ_t 1 in
  add_builtin "list.hd" ~cat:List
    ~descr:
      "Return the head (first element) of a list, or 'default' if the list is \
       empty."
    [
      ("default", t, None, Some "Default value if key does not exist");
      ("", Lang.list_t t, None, None);
    ]
    t
    (fun p ->
      let default = List.assoc "default" p in
      try List.hd (Lang.to_list (Lang.assoc "" 1 p)) with _ -> default)

let () =
  add_builtin "list.sort" ~cat:List
    ~descr:"Sort a list according to a comparison function."
    [
      ( "",
        Lang.fun_t
          [(false, "", Lang.univ_t 1); (false, "", Lang.univ_t 1)]
          Lang.int_t,
        None,
        None );
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let sort x y =
        Lang.to_int (Lang.apply ~t:Lang.int_t f [("", x); ("", y)])
      in
      let l = Lang.assoc "" 2 p in
      Lang.list ~t:(Lang.of_list_t l.Lang.t) (List.sort sort (Lang.to_list l)))

let () =
  add_builtin "list.filter" ~cat:List
    ~descr:"Filter a list according to a filtering function."
    [
      ("", Lang.fun_t [(false, "", Lang.univ_t 1)] Lang.bool_t, None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let filter x = Lang.to_bool (Lang.apply ~t:Lang.bool_t f [("", x)]) in
      let l = Lang.assoc "" 2 p in
      Lang.list ~t:(Lang.of_list_t l.Lang.t)
        (List.filter filter (Lang.to_list l)))

let () =
  add_builtin "list.tl" ~cat:List
    ~descr:"Return the list without its first element."
    [("", Lang.list_t (Lang.univ_t 1), None, None)]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let l = Lang.assoc "" 1 p in
      let t = Lang.of_list_t l.Lang.t in
      let l = Lang.to_list l in
      match l with [] -> Lang.list ~t [] | _ :: tl -> Lang.list ~t tl)

let () =
  add_builtin "list.append" ~cat:List ~descr:"Catenate two lists."
    [
      ("", Lang.list_t (Lang.univ_t 1), None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let l = Lang.assoc "" 1 p in
      let t = Lang.of_list_t l.Lang.t in
      let l = Lang.to_list l in
      let l' = Lang.to_list (Lang.assoc "" 2 p) in
      Lang.list ~t (l @ l'))

let () =
  add_builtin "list.remove" ~cat:List
    ~descr:"Remove a the first occurence of a value from a list."
    [
      ("", Lang.univ_t 1, None, None);
      ("", Lang.list_t (Lang.univ_t 1), None, None);
    ]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let a = Lang.assoc "" 1 p in
      let l = Lang.assoc "" 2 p in
      let t = Lang.of_list_t l.Lang.t in
      let l = Lang.to_list l in
      let rec aux k = function
        | [] -> k []
        | x :: l ->
            if compare_value x a = 0 then k l else aux (fun l -> k (x :: l)) l
      in
      let l = aux (fun l -> l) l in
      Lang.list ~t l)

let () =
  add_builtin "list.rev" ~cat:List ~descr:"Revert list order."
    [("", Lang.list_t (Lang.univ_t 1), None, None)]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
      let l = Lang.assoc "" 1 p in
      let t = Lang.of_list_t l.Lang.t in
      let l = Lang.to_list l in
      Lang.list ~t (List.rev l))

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
  add_builtin "list.mem" ~cat:List
    ~descr:"Check if an element belongs to a list."
    [("", t, None, None); ("", Lang.list_t t, None, None)]
    Lang.bool_t
    (fun p ->
      let e = Lang.assoc "" 1 p in
      let l = Lang.to_list (Lang.assoc "" 2 p) in
      Lang.bool (List.exists (fun e' -> compare_value e e' = 0) l))

let () =
  add_builtin "list.length" ~cat:List
    ~descr:"Get the length of a list, i.e. its number of elements."
    [("", Lang.list_t (Lang.univ_t 1), None, None)]
    Lang.int_t
    (fun p ->
      let l = Lang.to_list (Lang.assoc "" 1 p) in
      Lang.int (List.length l))
