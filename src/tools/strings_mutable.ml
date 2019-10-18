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

open Strings

type t = {
  mutable strings: Strings.t;
  mutex: Mutex.t
}

let of_strings strings = {
  strings;mutex=Mutex.create()
}

let of_list l = of_strings (of_list l)

let of_string s = of_list [s]

let of_bytes b =
  of_strings (of_bytes b)

let unsafe_of_bytes b =
  of_strings (unsafe_of_bytes b)  

let empty () = of_strings Strings.empty

let to_strings {strings} = strings

(* Copied from tutils.ml to avoid circular references. *)
let mutexify lock f =
  fun x ->
    Mutex.lock lock ;
    try
      let ans = f x in Mutex.unlock lock ; ans
    with
      | e -> Mutex.unlock lock ; raise e

let  add m s = mutexify m.mutex (fun () ->
  m.strings <- add m.strings s) ()

let add_substring m s ofs len = mutexify m.mutex (fun () ->
  m.strings <- add_substring m.strings s ofs len) ()

let add_subbytes m b ofs len = mutexify m.mutex (fun () ->
  m.strings <- add_subbytes m.strings b ofs len) ()

let unsafe_add_subbytes m b ofs len = mutexify m.mutex (fun () ->
  m.strings <- unsafe_add_subbytes m.strings b ofs len) ()

let add_bytes t b = add_subbytes t b 0 (Bytes.length b)

let unsafe_add_bytes t b = unsafe_add_subbytes t b 0 (Bytes.length b)

let dda s m = mutexify m.mutex (fun () ->
  m.strings <- dda s m.strings) () 

let append_strings m t = mutexify m.mutex (fun () ->
  m.strings <- append m.strings t) ()

let drop m len = mutexify m.mutex (fun () ->
  m.strings <- drop m.strings len) ()

let keep m len = mutexify m.mutex (fun () ->
  m.strings <- keep m.strings len) ()

let append m m' =
  mutexify m.mutex (fun () ->
    mutexify m'.mutex (fun () ->
      m.strings <- append m.strings m'.strings) ()) ()

let iter_view fn m = mutexify m.mutex (fun () ->
  iter_view fn m.strings) ()

let iter fn m = mutexify m.mutex (fun () ->
  iter fn m.strings) ()

let map_view fn m = mutexify m.mutex (fun () ->
  of_strings (map_view fn m.strings)) ()

let map fn m = mutexify m.mutex (fun () ->
  of_strings (map fn m.strings)) ()

let fold_view fn x0 m = mutexify m.mutex (fun () ->
  fold_view fn x0 m.strings) ()

let fold fn x0 m = mutexify m.mutex (fun () ->
  fold fn x0 m.strings) ()

let flush m = mutexify m.mutex (fun () ->
  let content = m.strings in
  m.strings <- Strings.empty;
  content) ()

let is_empty m = mutexify m.mutex (fun () ->
  is_empty m.strings) ()

let length m = mutexify m.mutex (fun () ->
  length m.strings) () 

let to_string m = mutexify m.mutex (fun () ->
  to_string m.strings) ()

let blit m b n = mutexify m.mutex (fun () ->
  blit m.strings b n) ()

let sub m ofs len = mutexify m.mutex (fun () ->
  of_strings (sub m.strings ofs len)) () 

let substring m ofs len = mutexify m.mutex (fun () ->
  substring m.strings ofs len) ()
