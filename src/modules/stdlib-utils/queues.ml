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

  let count_live { arr; size } =
    let n = ref 0 in
    for i = 0 to size - 1 do
      if Weak.check arr i then incr n
    done;
    !n

  (* Copy live entries from src into dst starting at slot 0; return count copied. *)
  let copy_live src dst =
    let dst_i = ref 0 in
    Weak_utils.iter src (fun el ->
        Weak.set dst !dst_i (Some el);
        incr dst_i);
    !dst_i

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
          let live_count = count_live { arr; size } in
          let new_capacity =
            if live_count + 1 <= capacity then capacity else max 1 (capacity * 2)
          in
          let new_arr = Weak.create new_capacity in
          let copied = copy_live arr new_arr in
          Weak.set new_arr copied (Some v);
          Atomic.set q.s { arr = new_arr; size = copied + 1 }
        end)

  let exists q fn =
    get q (fun { arr; _ } ->
        Weak_utils.fold_left (fun found el -> found || fn el) false arr)

  let length q = get q count_live
  let iter q fn = get q (fun { arr; _ } -> Weak_utils.iter arr fn)

  let fold q fn v =
    get q (fun { arr; _ } ->
        Weak_utils.fold_left (fun acc el -> fn el acc) v arr)

  let elements q = List.rev (fold q (fun v elements -> v :: elements) [])

  let flush_elements q =
    let { arr; _ } =
      mutate q (fun q -> Atomic.exchange q.s { arr = Weak.create 0; size = 0 })
    in
    List.rev (Weak_utils.fold_left (fun acc el -> el :: acc) [] arr)

  let flush_iter q fn =
    let { arr; _ } =
      mutate q (fun q -> Atomic.exchange q.s { arr = Weak.create 0; size = 0 })
    in
    Weak_utils.iter arr fn

  let filter q fn =
    mutate q (fun q ->
        let { arr; _ } = Atomic.get q.s in
        let live =
          List.rev
            (Weak_utils.fold_left
               (fun acc el -> if fn el then el :: acc else acc)
               [] arr)
        in
        let new_size = List.length live in
        let new_arr = Weak.create new_size in
        List.iteri (fun i el -> Weak.set new_arr i (Some el)) live;
        Atomic.set q.s { arr = new_arr; size = new_size })

  let filter_out q fn = filter q (fun el -> not (fn el))
end
