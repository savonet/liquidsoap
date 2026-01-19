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
          if len >= lx then (len - lx, l) else (0, S.sub x len (lx - len) :: l))
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
          len := !len - r)))
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
  type content = { buffer : bytes; ofs : int; pos : int; size : int }
  type nonrec t = { content : content Atomic.t; mutex : Mutex.t }

  let initial_size = 1024
  let mutate m fn = Mutex_utils.mutexify m.mutex fn m
  let get m fn = fn (Atomic.get m.content)

  let create ?(size = initial_size) () =
    {
      content =
        Atomic.make { buffer = Bytes.create size; ofs = 0; pos = 0; size = 0 };
      mutex = Mutex.create ();
    }

  let empty = create

  (* Ensure buffer has enough capacity for writing n bytes at current pos.
     Compacts the buffer if needed. Returns new content. *)
  let ensure_capacity c n =
    let required = c.ofs + c.pos + n in
    if required > Bytes.length c.buffer then begin
      let new_capacity =
        let cap = ref (max (Bytes.length c.buffer) 1) in
        while !cap < required do
          cap := !cap + (!cap / 2) + 1
        done;
        !cap
      in
      let new_buffer = Bytes.create new_capacity in
      Bytes.blit c.buffer c.ofs new_buffer 0 c.size;
      { c with buffer = new_buffer; ofs = 0 }
    end
    else if c.ofs > 0 && c.ofs + c.pos + n > Bytes.length c.buffer then begin
      Bytes.blit c.buffer c.ofs c.buffer 0 c.size;
      { c with ofs = 0 }
    end
    else c

  let strings_of_bytes = of_bytes

  let of_bytes b =
    let len = Bytes.length b in
    let buffer = Bytes.create (max len initial_size) in
    Bytes.blit b 0 buffer 0 len;
    {
      content = Atomic.make { buffer; ofs = 0; pos = len; size = len };
      mutex = Mutex.create ();
    }

  let unsafe_of_bytes b =
    let len = Bytes.length b in
    {
      content = Atomic.make { buffer = b; ofs = 0; pos = len; size = len };
      mutex = Mutex.create ();
    }

  let of_string s = of_bytes (Bytes.of_string s)

  let of_strings strings =
    let len = length strings in
    let buffer = Bytes.create (max len initial_size) in
    blit strings buffer 0;
    {
      content = Atomic.make { buffer; ofs = 0; pos = len; size = len };
      mutex = Mutex.create ();
    }

  let of_list l = of_strings (of_list l)
  let to_bytes m = get m (fun c -> Bytes.sub c.buffer c.ofs c.size)
  let to_string m = Bytes.unsafe_to_string (to_bytes m)

  let to_strings m =
    get m (fun c -> strings_of_bytes (Bytes.sub c.buffer c.ofs c.size))

  let seek m offset whence =
    mutate m (fun m ->
        let c = Atomic.get m.content in
        let new_pos =
          match whence with
            | Unix.SEEK_SET -> offset
            | Unix.SEEK_CUR -> c.pos + offset
            | Unix.SEEK_END -> c.size + offset
        in
        if new_pos < 0 then
          raise (Invalid_argument "Strings.Mutable.seek: negative position");
        if new_pos > c.size then
          raise (Invalid_argument "Strings.Mutable.seek: position past end");
        Atomic.set m.content { c with pos = new_pos };
        new_pos)

  let pos m = get m (fun c -> c.pos)

  let add_subbytes m b src_ofs len =
    mutate m (fun m ->
        let c = ensure_capacity (Atomic.get m.content) len in
        Bytes.blit b src_ofs c.buffer (c.ofs + c.pos) len;
        let new_pos = c.pos + len in
        Atomic.set m.content { c with pos = new_pos; size = max c.size new_pos })

  let unsafe_add_subbytes = add_subbytes
  let add_bytes m b = add_subbytes m b 0 (Bytes.length b)
  let unsafe_add_bytes = add_bytes

  let add_substring m s ofs len =
    add_subbytes m (Bytes.unsafe_of_string s) ofs len

  let add m s = add_substring m s 0 (String.length s)

  let dda s m =
    mutate m (fun m ->
        let c = Atomic.get m.content in
        let slen = String.length s in
        if slen <= c.ofs then begin
          let new_ofs = c.ofs - slen in
          Bytes.blit_string s 0 c.buffer new_ofs slen;
          Atomic.set m.content
            { c with ofs = new_ofs; size = c.size + slen; pos = c.pos + slen }
        end
        else begin
          let c = ensure_capacity { c with pos = c.size } slen in
          Bytes.blit c.buffer c.ofs c.buffer (c.ofs + slen) c.size;
          Bytes.blit_string s 0 c.buffer c.ofs slen;
          Atomic.set m.content
            { c with size = c.size + slen; pos = c.pos + slen }
        end)

  let append_strings m strings =
    mutate m (fun m ->
        let len = length strings in
        let c = Atomic.get m.content in
        let c = ensure_capacity { c with pos = c.size } len in
        blit strings c.buffer (c.ofs + c.size);
        let new_size = c.size + len in
        Atomic.set m.content { c with pos = new_size; size = new_size })

  let drop m len =
    mutate m (fun m ->
        let c = Atomic.get m.content in
        if len >= c.size then
          Atomic.set m.content { c with ofs = 0; size = 0; pos = 0 }
        else
          Atomic.set m.content
            {
              c with
              ofs = c.ofs + len;
              size = c.size - len;
              pos = max 0 (c.pos - len);
            })

  let keep m len =
    mutate m (fun m ->
        let c = Atomic.get m.content in
        if len >= c.size then ()
        else (
          let drop_len = c.size - len in
          Atomic.set m.content
            { c with ofs = c.ofs + drop_len; size = len; pos = min c.pos len }))

  let append m m' =
    mutate m (fun m ->
        let c' = Atomic.get m'.content in
        let c = Atomic.get m.content in
        let c = ensure_capacity { c with pos = c.size } c'.size in
        Bytes.blit c'.buffer c'.ofs c.buffer (c.ofs + c.size) c'.size;
        let new_size = c.size + c'.size in
        Atomic.set m.content { c with pos = new_size; size = new_size })

  let iter_view fn m =
    get m (fun c ->
        if c.size > 0 then
          fn (S.of_substring (Bytes.unsafe_to_string c.buffer) c.ofs c.size))

  let iter fn m =
    get m (fun c ->
        if c.size > 0 then fn (Bytes.unsafe_to_string c.buffer) c.ofs c.size)

  let map_view fn m =
    get m (fun c ->
        if c.size = 0 then create ()
        else begin
          let view =
            S.of_substring (Bytes.unsafe_to_string c.buffer) c.ofs c.size
          in
          let new_view = fn view in
          let s, o, l = S.to_substring new_view in
          of_string (String.sub s o l)
        end)

  let map fn m =
    get m (fun c ->
        if c.size = 0 then create ()
        else begin
          let s, o, l = fn (Bytes.unsafe_to_string c.buffer) c.ofs c.size in
          of_string (String.sub s o l)
        end)

  let fold_view fn x0 m =
    get m (fun c ->
        if c.size = 0 then x0
        else
          fn x0 (S.of_substring (Bytes.unsafe_to_string c.buffer) c.ofs c.size))

  let fold fn x0 m =
    get m (fun c ->
        if c.size = 0 then x0
        else fn x0 (Bytes.unsafe_to_string c.buffer) c.ofs c.size)

  let flush m =
    mutate m (fun m ->
        let c = Atomic.get m.content in
        let result = strings_of_bytes (Bytes.sub c.buffer c.ofs c.size) in
        Atomic.set m.content { c with ofs = 0; size = 0; pos = 0 };
        result)

  let is_empty m = get m (fun c -> c.size = 0)
  let length m = get m (fun c -> c.size)

  let blit m mo b bo len =
    get m (fun c ->
        if mo + len > c.size then
          raise (Invalid_argument "Strings.Mutable.blit: out of bounds");
        Bytes.blit c.buffer (c.ofs + mo) b bo len)

  let sub m src_ofs len =
    get m (fun c ->
        if src_ofs + len > c.size then
          raise (Invalid_argument "Strings.Mutable.sub: out of bounds");
        let buffer = Bytes.create (max len initial_size) in
        Bytes.blit c.buffer (c.ofs + src_ofs) buffer 0 len;
        {
          content = Atomic.make { buffer; ofs = 0; pos = len; size = len };
          mutex = Mutex.create ();
        })

  let substring m src_ofs len =
    get m (fun c ->
        if src_ofs + len > c.size then
          raise (Invalid_argument "Strings.Mutable.substring: out of bounds");
        Bytes.sub_string c.buffer (c.ofs + src_ofs) len)
end
