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
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin "list.case" ~category:(string_of_category List)
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
    (fun p b ->
      let l, e, f =
        match p with
          | [("", l); ("", e); ("", f)] -> (l, e, f)
          | _ -> assert false
      in
      let a = Lang.of_list_t l.Lang.t in
      match Lang.to_list l with
        | [] -> e
        | x :: l -> Lang.apply ~t:b f [("", x); ("", Lang.list ~t:a l)])

let () =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin "list.ind" ~category:(string_of_category List)
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
    (fun p b ->
      let l, e, f =
        match p with
          | [("", l); ("", e); ("", f)] -> (l, e, f)
          | _ -> assert false
      in
      let a = Lang.of_list_t l.Lang.t in
      let rec aux k = function
        | [] -> k e
        | x :: l ->
            aux
              (fun r ->
                k (Lang.apply ~t:b f [("", x); ("", Lang.list ~t:a l); ("", r)]))
              l
      in
      aux (fun r -> r) (Lang.to_list l))

let () =
  let a = Lang.univ_t () in
  Lang.add_builtin "list.add" ~category:(string_of_category List)
    ~descr:"Add an element at the top of a list."
    [("", a, None, None); ("", Lang.list_t a, None, None)]
    (Lang.list_t a)
    (fun p t ->
      let t = Lang.of_list_t t in
      let x, l =
        match p with [("", x); ("", l)] -> (x, l) | _ -> assert false
      in
      let l = Lang.to_list l in
      Lang.list ~t (x :: l))

let () =
  let t = Lang.list_t (Lang.univ_t ()) in
  Lang.add_builtin "list.randomize" ~category:(string_of_category List)
    ~descr:"Shuffle the content of a list." [("", t, None, None)] t (fun p t ->
      let t = Lang.of_list_t t in
      let l = Array.of_list (Lang.to_list (List.assoc "" p)) in
      Utils.randomize l;
      Lang.list ~t (Array.to_list l))

let () =
  let a = Lang.univ_t () in
  add_builtin "list.sort" ~cat:List
    ~descr:"Sort a list according to a comparison function."
    [
      ("", Lang.fun_t [(false, "", a); (false, "", a)] Lang.int_t, None, None);
      ("", Lang.list_t a, None, None);
    ]
    (Lang.list_t a)
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let sort x y =
        Lang.to_int (Lang.apply ~t:Lang.int_t f [("", x); ("", y)])
      in
      let l = Lang.assoc "" 2 p in
      Lang.list ~t:(Lang.of_list_t l.Lang.t) (List.sort sort (Lang.to_list l)))
