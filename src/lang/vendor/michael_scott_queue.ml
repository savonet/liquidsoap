(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Michael-Scott queue *)

type 'a node = Nil | Next of 'a * 'a node Atomic.t

type 'a t = {
  head : 'a node Atomic.t Atomic.t;
  tail : 'a node Atomic.t Atomic.t;
}

let create () =
  let next = Atomic.make Nil in
  { head = Atomic.make next; tail = Atomic.make next }

let is_empty { head; _ } = Atomic.get (Atomic.get head) == Nil

exception Empty

let pop_opt { head; _ } =
  let b = Backoff.default in
  let rec loop b =
    let old_head = Atomic.get head in
    match Atomic.get old_head with
      | Nil -> None
      | Next (value, next) when Atomic.compare_and_set head old_head next ->
          Some value
      | _ ->
          let b = Backoff.once b in
          loop b
  in
  loop b

let pop { head; _ } =
  let b = Backoff.default in
  let rec loop b =
    let old_head = Atomic.get head in
    match Atomic.get old_head with
      | Nil -> raise Empty
      | Next (value, next) when Atomic.compare_and_set head old_head next ->
          value
      | _ ->
          let b = Backoff.once b in
          loop b
  in
  loop b

let peek_opt { head; _ } =
  let old_head = Atomic.get head in
  match Atomic.get old_head with Nil -> None | Next (value, _) -> Some value

let peek { head; _ } =
  let old_head = Atomic.get head in
  match Atomic.get old_head with Nil -> raise Empty | Next (value, _) -> value

let rec fix_tail tail new_tail =
  let old_tail = Atomic.get tail in
  if
    Atomic.get new_tail == Nil
    && not (Atomic.compare_and_set tail old_tail new_tail)
  then fix_tail tail new_tail

let push { tail; _ } value =
  let rec find_tail_and_enq curr_end node =
    if not (Atomic.compare_and_set curr_end Nil node) then (
      match Atomic.get curr_end with
        | Nil -> find_tail_and_enq curr_end node
        | Next (_, n) -> find_tail_and_enq n node)
  in
  let new_tail = Atomic.make Nil in
  let newnode = Next (value, new_tail) in
  let old_tail = Atomic.get tail in
  find_tail_and_enq old_tail newnode;
  if not (Atomic.compare_and_set tail old_tail new_tail) then
    fix_tail tail new_tail

type 'a cursor = 'a node

let snapshot { head; _ } = Atomic.get (Atomic.get head)
let next = function Nil -> None | Next (a, n) -> Some (a, Atomic.get n)
