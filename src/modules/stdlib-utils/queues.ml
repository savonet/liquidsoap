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
  type 'a t = { state : Mutex_utils.state; l : 'a List.t Atomic.t }

  let create () = { state = Mutex_utils.mk_state (); l = Atomic.make [] }
  let mutate q fn = Mutex_utils.mutable_lock ~state:q.state fn q
  let get q fn = Mutex_utils.atomic_lock ~state:q.state fn (Atomic.get q.l)
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
  (* Backing store: a weak array with a fill-mark [size].
     Slots 0..size-1 may be Some or None (dead weak refs); slots size.. are
     unused.  This lets push be O(1) in the fast path — we just write into the
     next free slot without touching the rest of the array.  When the array is
     full (size = capacity) we compact live entries and grow geometrically only
     when every slot is truly occupied. *)
  type 'a store = { arr : 'a Weak.t; size : int }
  type 'a t = { state : Mutex_utils.state; s : 'a store Atomic.t }

  let create () =
    {
      state = Mutex_utils.mk_state ();
      s = Atomic.make { arr = Weak.create 0; size = 0 };
    }

  let mutate q fn = Mutex_utils.mutable_lock ~state:q.state fn q
  let get q fn = Mutex_utils.atomic_lock ~state:q.state fn (Atomic.get q.s)

  let rec count_live arr size i acc =
    if i >= size then acc
    else count_live arr size (i + 1) (if Weak.check arr i then acc + 1 else acc)

  (* Copy live entries from src[0..src_size-1] into dst starting at dst_i. *)
  let rec copy_live src src_size dst src_i dst_i =
    if src_i >= src_size then ()
    else (
      match Weak.get src src_i with
        | Some _ as v ->
            Weak.set dst dst_i v;
            copy_live src src_size dst (src_i + 1) (dst_i + 1)
        | None -> copy_live src src_size dst (src_i + 1) dst_i)

  let push q v =
    mutate q (fun q ->
        let { arr; size } = Atomic.get q.s in
        let capacity = Weak.length arr in
        if size < capacity then begin
          (* Fast path: next slot is free, write directly without any copy. *)
          Weak.set arr size (Some v);
          Atomic.set q.s { arr; size = size + 1 }
        end
        else begin
          (* Slow path: array is full — compact live entries into a new array,
             growing geometrically only when all slots are occupied. *)
          let live_count = count_live arr size 0 0 in
          let new_capacity =
            if live_count + 1 <= capacity then capacity else max 1 (capacity * 2)
          in
          let new_arr = Weak.create new_capacity in
          copy_live arr size new_arr 0 0;
          Weak.set new_arr live_count (Some v);
          Atomic.set q.s { arr = new_arr; size = live_count + 1 }
        end)

  let rec find_fn fn arr size i =
    if i >= size then false
    else (
      match Weak.get arr i with
        | Some el when fn el -> true
        | _ -> find_fn fn arr size (i + 1))

  let exists q fn = get q (fun { arr; size } -> find_fn fn arr size 0)
  let length q = get q (fun { arr; size } -> count_live arr size 0 0)

  let rec iter_fn fn arr size i =
    if i >= size then ()
    else (
      (match Weak.get arr i with Some el -> fn el | None -> ());
      iter_fn fn arr size (i + 1))

  let iter q fn = get q (fun { arr; size } -> iter_fn fn arr size 0)

  let rec fold_fn fn arr size i acc =
    if i >= size then acc
    else
      fold_fn fn arr size (i + 1)
        (match Weak.get arr i with Some el -> fn el acc | None -> acc)

  let fold q fn v = get q (fun { arr; size } -> fold_fn fn arr size 0 v)

  let rec get_elements arr size i acc =
    if i >= size then List.rev acc
    else
      get_elements arr size (i + 1)
        (match Weak.get arr i with Some el -> el :: acc | None -> acc)

  let elements q = get q (fun { arr; size } -> get_elements arr size 0 [])

  let rec flush_elements_fn arr size i acc =
    if i >= size then List.rev acc
    else
      flush_elements_fn arr size (i + 1)
        (match Weak.get arr i with Some el -> el :: acc | None -> acc)

  let flush_elements q =
    let { arr; size } =
      mutate q (fun q -> Atomic.exchange q.s { arr = Weak.create 0; size = 0 })
    in
    flush_elements_fn arr size 0 []

  let rec flush_iter_fn fn arr size i =
    if i >= size then ()
    else (
      (match Weak.get arr i with Some el -> fn el | None -> ());
      flush_iter_fn fn arr size (i + 1))

  let flush_iter q fn =
    let { arr; size } =
      mutate q (fun q -> Atomic.exchange q.s { arr = Weak.create 0; size = 0 })
    in
    flush_iter_fn fn arr size 0

  let rec filter_fn fn arr size i =
    if i >= size then ()
    else (
      (match Weak.get arr i with
        | Some el when not (fn el) -> Weak.set arr i None
        | _ -> ());
      filter_fn fn arr size (i + 1))

  let filter q fn =
    mutate q (fun q ->
        let { arr; size } = Atomic.get q.s in
        filter_fn fn arr size 0)

  let filter_out q fn = filter q (fun el -> not (fn el))
end
