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

(* Atomic queues with slow lock for mutable operations. Queues are order-preserving. *)

module Queue = struct
  type 'a t = { m : Mutex.t; l : 'a List.t Atomic.t }

  let create () = { m = Mutex.create (); l = Atomic.make [] }
  let mutate q fn = Mutex_utils.mutexify q.m fn q
  let get q fn = fn (Atomic.get q.l)
  let is_empty q = get q (function [] -> true | _ -> false)
  let push q v = mutate q (fun q -> Atomic.set q.l (Atomic.get q.l @ [v]))
  let append q v = mutate q (fun q -> Atomic.set q.l (v :: Atomic.get q.l))

  let pop_opt q =
    mutate q (fun q ->
        match Atomic.get q.l with
          | el :: rest ->
              Atomic.set q.l rest;
              Some el
          | _ -> None)

  let peek_opt q = get q (function el :: _ -> Some el | _ -> None)
  let flush_elements q = mutate q (fun q -> Atomic.exchange q.l [])

  let pop q =
    mutate q (fun q ->
        match Atomic.get q.l with
          | el :: rest ->
              Atomic.set q.l rest;
              el
          | _ -> raise Not_found)

  let peek q = get q (function el :: _ -> el | _ -> raise Not_found)
  let flush_iter q fn = List.iter fn (flush_elements q)

  let flush_fold q fn ret =
    let flush_fold_f ret el = fn el ret in
    List.fold_left flush_fold_f ret (flush_elements q)

  let elements q = Atomic.get q.l
  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    mutate q (fun q -> Atomic.set q.l (List.filter fn (Atomic.get q.l)))

  let filter_out q fn = filter q (fun el -> not (fn el))
end

module WeakQueue = struct
  type 'a t = { m : Mutex.t; a : 'a Weak.t Atomic.t }

  let create () = { m = Mutex.create (); a = Atomic.make (Weak.create 0) }
  let mutate q fn = Mutex_utils.mutexify q.m fn q
  let get q fn = fn (Atomic.get q.a)

  let rec count_live arr len i acc =
    if i >= len then acc
    else count_live arr len (i + 1) (if Weak.check arr i then acc + 1 else acc)

  let rec copy_live src dst len i j =
    if i >= len then ()
    else (
      match Weak.get src i with
        | Some el ->
            Weak.set dst j (Some el);
            copy_live src dst len (i + 1) (j + 1)
        | None -> copy_live src dst len (i + 1) j)

  let compact_and_push arr v =
    let len = Weak.length arr in
    let live_count = count_live arr len 0 0 in
    let new_arr = Weak.create (live_count + 1) in
    if live_count = len then Weak.blit arr 0 new_arr 0 len
    else copy_live arr new_arr len 0 0;
    Weak.set new_arr live_count (Some v);
    new_arr

  let push q v =
    mutate q (fun q ->
        let arr = Atomic.get q.a in
        let len = Weak.length arr in
        let new_arr =
          if (len + 1) mod 100 = 0 then compact_and_push arr v
          else begin
            let new_arr = Weak.create (len + 1) in
            Weak.blit arr 0 new_arr 0 len;
            Weak.set new_arr len (Some v);
            new_arr
          end
        in
        Atomic.set q.a new_arr)

  let rec find_fn fn arr len i =
    if i >= len then false
    else (
      match Weak.get arr i with
        | Some el when fn el -> true
        | _ -> find_fn fn arr len (i + 1))

  let exists q fn = get q (fun arr -> find_fn fn arr (Weak.length arr) 0)
  let length q = get q (fun arr -> count_live arr (Weak.length arr) 0 0)

  let rec iter_fn fn arr len i =
    if i >= len then ()
    else (
      (match Weak.get arr i with Some el -> fn el | None -> ());
      iter_fn fn arr len (i + 1))

  let iter q fn = get q (fun arr -> iter_fn fn arr (Weak.length arr) 0)

  let rec fold_fn fn arr len i acc =
    if i >= len then acc
    else
      fold_fn fn arr len (i + 1)
        (match Weak.get arr i with Some el -> fn el acc | None -> acc)

  let fold q fn v = get q (fun arr -> fold_fn fn arr (Weak.length arr) 0 v)

  let rec get_elements arr len i acc =
    if i >= len then List.rev acc
    else
      get_elements arr len (i + 1)
        (match Weak.get arr i with Some el -> el :: acc | None -> acc)

  let elements q = get q (fun arr -> get_elements arr (Weak.length arr) 0 [])

  let rec flush_elements_fn arr len i acc =
    if i >= len then List.rev acc
    else
      flush_elements_fn arr len (i + 1)
        (match Weak.get arr i with Some el -> el :: acc | None -> acc)

  let flush_elements q =
    let arr = mutate q (fun q -> Atomic.exchange q.a (Weak.create 0)) in
    flush_elements_fn arr (Weak.length arr) 0 []

  let rec flush_iter_fn fn arr len i =
    if i >= len then ()
    else (
      (match Weak.get arr i with Some el -> fn el | None -> ());
      flush_iter_fn fn arr len (i + 1))

  let flush_iter q fn =
    let arr = mutate q (fun q -> Atomic.exchange q.a (Weak.create 0)) in
    flush_iter_fn fn arr (Weak.length arr) 0

  let rec filter_fn fn arr len i =
    if i >= len then ()
    else (
      (match Weak.get arr i with
        | Some el when not (fn el) -> Weak.set arr i None
        | _ -> ());
      filter_fn fn arr len (i + 1))

  let filter q fn =
    mutate q (fun q ->
        let arr = Atomic.get q.a in
        filter_fn fn arr (Weak.length arr) 0)

  let filter_out q fn = filter q (fun el -> not (fn el))
end
