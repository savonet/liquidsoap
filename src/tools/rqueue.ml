(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*        François Pottier, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(*         Modifications by David Baelde, ENS Lyon, May 2003           *)

exception Not_found

type 'a cell = { content : 'a; mutable next : 'a cell; mutable prev : 'a cell }

(* 
 *
 *  Realistic queues FIFO & LIFO, thread-safe :
 *  
 *       TOP [ rqueue ] BOTTOM
 *  unshift> [ rqueue ] >pop
 *    shift< [ rqueue ] <push
 *
 *  [ ... cell -(next)-> cell ... ]
 *
 *)

type 'a t = { lock : Mutex.t; mutable length : int; mutable top : 'a cell }

let create () = { lock = Mutex.create (); length = 0; top = Obj.magic None }

let top q =
  Mutex.lock q.lock;
  match q.length with
    | 0 ->
        Mutex.unlock q.lock;
        raise Not_found
    | _ ->
        let ans = q.top.content in
        Mutex.unlock q.lock;
        ans

(* No need to lock here, length is always valid and we don't need coherence
 * for reading anything -- unlike q.top.content in [top]. *)
let length q = q.length
let is_empty q = 0 = q.length

let to_list q =
  Mutex.lock q.lock;
  if q.length = 0 then (
    Mutex.unlock q.lock;
    [] )
  else (
    let rec aux ans c =
      if c.next == q.top then List.rev (c.content :: ans)
      else aux (c.content :: ans) c.next
    in
    let ans = aux [] q.top in
    Mutex.unlock q.lock;
    ans )

let insert_pred ?top q f x =
  Mutex.lock q.lock;
  q.length <- q.length + 1;
  match q.length with
    | 1 ->
        let rec cell = { content = x; next = cell; prev = cell } in
        q.top <- cell;
        Mutex.unlock q.lock
    | _ -> (
        let i = ref (-1) in
        let rec aux c =
          incr i;
          if f !i c.content then Some c
          else if c.next == q.top then None
          else aux c.next
        in
        match aux q.top with
          | None ->
              Mutex.unlock q.lock;
              raise Not_found
          | Some c ->
              let cell = { content = x; next = c; prev = c.prev } in
              if Some true = top then q.top <- cell;
              c.prev.next <- cell;
              c.prev <- cell;
              Mutex.unlock q.lock )

let remove_pred_index q f =
  Mutex.lock q.lock;
  match q.length with
    | 0 ->
        Mutex.unlock q.lock;
        raise Not_found
    | _ -> (
        let i = ref (-1) in
        let rec aux c =
          incr i;
          if f !i c.content then Some c
          else if c.next == q.top then None
          else aux c.next
        in
        let a = aux q.top in
        match a with
          | Some c ->
              c.prev.next <- c.next;
              c.next.prev <- c.prev;
              q.length <- q.length - 1;
              if q.top == c then q.top <- c.next;
              Mutex.unlock q.lock;
              (c.content, !i)
          | None ->
              Mutex.unlock q.lock;
              raise Not_found )

let remove_pred q f = fst (remove_pred_index q f)

let remove q pos =
  (* This isn't very efficient, but q.length needs to be evaluated
   * withing the q.lock protection. *)
  remove_pred q (fun i _ -> (i - pos) mod q.length = 0)

let insert q pos x =
  let p = if pos < 0 then -pos - 1 else pos in
  insert_pred q ~top:(pos = 0) (fun i _ -> (i - p) mod q.length = 0) x

let unshift q x = insert q 0 x
let push q x = insert q (-1) x
let shift q = remove q 0
let pop q = remove q (-1)

let fold f x q =
  Mutex.lock q.lock;
  if q.length = 0 then (
    Mutex.unlock q.lock;
    x )
  else (
    let rec fold c x =
      if c.next == q.top then f x c.content else fold c.next (f x c.content)
    in
    try
      let a = fold q.top x in
      Mutex.unlock q.lock;
      a
    with e ->
      Mutex.unlock q.lock;
      raise e )
