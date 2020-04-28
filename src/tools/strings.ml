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

(** Operations on lists of strings. This is module is used in order to avoid
    concatenating (large) strings. Iterators are FIFO. *)

module S = StringView

(* List of "concatenated" strings, stored backwards. *)
type t = S.t list
type buffer = t

let empty = []
let of_string s : t = [S.of_string s]
let of_bytes b = of_string (Bytes.to_string b)
let unsafe_of_bytes b = of_string (Bytes.unsafe_to_string b)

let of_list l =
  let rec aux acc = function
    | [] -> acc
    | x :: l -> aux (S.of_string x :: acc) l
  in
  aux [] l

let dda x l = l @ [S.of_string x]
let add_view l x = x :: l
let add (l : t) x : t = add_view l (S.of_string x)
let add_substring b x o l = add_view b (S.of_substring x o l)
let add_subbytes l s o len = add l (Bytes.sub_string s o len)
let unsafe_add_subbytes l b = add_substring l (Bytes.unsafe_to_string b)
let add_bytes t b = add_subbytes t b 0 (Bytes.length b)
let unsafe_add_bytes t b = unsafe_add_subbytes t b 0 (Bytes.length b)
let is_empty l = List.for_all S.is_empty l

let rec iter_view f = function
  | [] -> ()
  | x :: l ->
      iter_view f l;
      f x

let iter f b =
  iter_view
    (fun s ->
      let s, o, l = S.to_substring s in
      f s o l)
    b

let fold_view f x0 l =
  let rec aux = function [] -> x0 | x :: l -> f (aux l) x in
  aux l

let fold f x0 l =
  fold_view
    (fun cur view ->
      let s, o, l = S.to_substring view in
      f cur s o l)
    x0 l

let map_view = List.map

let map f l =
  map_view
    (fun view ->
      let s, o, l = S.to_substring view in
      let s, o, l = f s o l in
      S.of_substring s o l)
    l

let length l = fold_view (fun n s -> n + S.length s) 0 l
let append l1 l2 = l2 @ l1
let concat ll = List.concat (List.rev ll)

let drop l len =
  let rec aux len = function
    | [] -> (len, [])
    | x :: l ->
        let len, l = aux len l in
        if len = 0 then (0, x :: l)
        else (
          let lx = S.length x in
          if len >= lx then (len - lx, l) else (0, S.sub x len (lx - len) :: l)
          )
  in
  let r, l = aux len l in
  assert (r = 0);
  l

let keep l len =
  let cur_len = length l in
  if cur_len <= len then [] else drop l (cur_len - len)

let sub l o len =
  assert (o + len <= length l);
  let o = ref o in
  let len = ref len in
  let ans = ref empty in
  iter_view
    (fun s ->
      if !len = 0 then ()
      else (
        let ls = S.length s in
        if !o >= ls then o := !o - ls
        else (
          let r = min (ls - !o) !len in
          let s = S.sub s !o r in
          ans := add_view !ans s;
          o := 0;
          len := !len - r ) ))
    l;
  assert (!len = 0);
  !ans

let blit l b o =
  let len = length l in
  assert (o + len <= Bytes.length b);
  let o = ref o in
  iter_view
    (fun s ->
      S.blit s b !o;
      o := !o + S.length s)
    l

let to_bytes l =
  let ans = Bytes.create (length l) in
  blit l ans 0;
  ans

let to_string l = Bytes.unsafe_to_string (to_bytes l)
let substring l o len = to_string (sub l o len)

module Mutable = struct
  type nonrec t = { mutable strings : t; mutex : Mutex.t }

  let of_strings strings = { strings; mutex = Mutex.create () }
  let of_list l = of_strings (of_list l)
  let of_string s = of_list [s]
  let of_bytes b = of_strings (of_bytes b)
  let unsafe_of_bytes b = of_strings (unsafe_of_bytes b)
  let empty () = of_strings []
  let to_strings { strings } = strings

  (* Copied from tutils.ml to avoid circular references. *)
  let mutexify lock f x =
    Mutex.lock lock;
    try
      let ans = f x in
      Mutex.unlock lock;
      ans
    with e ->
      Mutex.unlock lock;
      raise e

  let add m s = mutexify m.mutex (fun () -> m.strings <- add m.strings s) ()

  let add_substring m s ofs len =
    mutexify m.mutex
      (fun () -> m.strings <- add_substring m.strings s ofs len)
      ()

  let add_subbytes m b ofs len =
    mutexify m.mutex
      (fun () -> m.strings <- add_subbytes m.strings b ofs len)
      ()

  let unsafe_add_subbytes m b ofs len =
    mutexify m.mutex
      (fun () -> m.strings <- unsafe_add_subbytes m.strings b ofs len)
      ()

  let add_bytes t b = add_subbytes t b 0 (Bytes.length b)
  let unsafe_add_bytes t b = unsafe_add_subbytes t b 0 (Bytes.length b)
  let dda s m = mutexify m.mutex (fun () -> m.strings <- dda s m.strings) ()

  let append_strings m t =
    mutexify m.mutex (fun () -> m.strings <- append m.strings t) ()

  let drop m len =
    mutexify m.mutex (fun () -> m.strings <- drop m.strings len) ()

  let keep m len =
    mutexify m.mutex (fun () -> m.strings <- keep m.strings len) ()

  let append m m' =
    mutexify m.mutex
      (fun () ->
        mutexify m'.mutex
          (fun () -> m.strings <- append m.strings m'.strings)
          ())
      ()

  let iter_view fn m = mutexify m.mutex (fun () -> iter_view fn m.strings) ()
  let iter fn m = mutexify m.mutex (fun () -> iter fn m.strings) ()

  let map_view fn m =
    mutexify m.mutex (fun () -> of_strings (map_view fn m.strings)) ()

  let map fn m = mutexify m.mutex (fun () -> of_strings (map fn m.strings)) ()

  let fold_view fn x0 m =
    mutexify m.mutex (fun () -> fold_view fn x0 m.strings) ()

  let fold fn x0 m = mutexify m.mutex (fun () -> fold fn x0 m.strings) ()

  let flush m =
    mutexify m.mutex
      (fun () ->
        let content = m.strings in
        m.strings <- [];
        content)
      ()

  let is_empty m = mutexify m.mutex (fun () -> is_empty m.strings) ()
  let length m = mutexify m.mutex (fun () -> length m.strings) ()
  let to_bytes m = mutexify m.mutex (fun () -> to_bytes m.strings) ()
  let to_string m = mutexify m.mutex (fun () -> to_string m.strings) ()

  let blit m mo b bo len =
    mutexify m.mutex (fun () -> blit (sub m.strings mo len) b bo) ()

  let sub m ofs len =
    mutexify m.mutex (fun () -> of_strings (sub m.strings ofs len)) ()

  let substring m ofs len =
    mutexify m.mutex (fun () -> substring m.strings ofs len) ()
end
