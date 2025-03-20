(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

module OrigQueue = Queue

module Queue = struct
  type 'a t = { m : Mutex.t; q : 'a Queue.t }

  exception Empty = Queue.Empty

  let create () = { m = Mutex.create (); q = Queue.create () }
  let apply { m; q } fn = Mutex_utils.mutexify m fn q [@@inline always]
  let is_empty q = apply q Queue.is_empty
  let push q v = apply q (fun q -> Queue.push v q)
  let pop_opt q = apply q (fun q -> Queue.take_opt q)
  let peek_opt q = apply q Queue.peek_opt

  let flush_elements q =
    let q' = Queue.create () in
    apply q (fun q -> Queue.transfer q q');
    List.rev (Queue.fold (fun l el -> el :: l) [] q')

  let pop q = apply q Queue.pop
  let peek q = apply q Queue.peek
  let flush_iter q fn = List.iter fn (flush_elements q)

  let flush_fold q fn ret =
    let flush_fold_f ret el = fn el ret in
    List.fold_left flush_fold_f ret (flush_elements q)

  let elements q =
    let q = apply q (fun q -> Queue.copy q) in
    List.rev (Queue.fold (fun l el -> el :: l) [] q)

  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let elements = flush_elements q in
    apply q (fun q ->
        List.iter (fun el -> if fn el then Queue.push el q) elements)

  let filter_out q fn = filter q (fun el -> not (fn el))
end

module WeakQueue = struct
  include Queue

  type nonrec 'a t = 'a Weak.t t

  let push q v =
    let w = Weak.create 1 in
    Weak.set w 0 (Some v);
    push q w

  let flush_iter q fn =
    flush_iter q (fun x ->
        for i = 0 to Weak.length x - 1 do
          match Weak.get x i with Some v -> fn v | None -> ()
        done)

  let flush_elements q =
    let elements = ref [] in
    flush_iter q (fun el -> elements := el :: !elements);
    List.rev !elements

  let elements q =
    let rec elements_f rem =
      match Queue.pop_opt q with
        | Some entry ->
            let len = Weak.length entry in
            let rec get_weak_entries pos ret =
              if len <= pos then ret
              else (
                let ret =
                  match Weak.get entry pos with
                    | Some v -> v :: ret
                    | None -> ret
                in
                get_weak_entries (pos + 1) ret)
            in
            elements_f (get_weak_entries 0 rem)
        | None -> rem
    in
    let rem = elements_f [] in
    let len = List.length rem in
    if len > 0 then (
      let entry = Weak.create len in
      for i = 0 to len - 1 do
        Weak.set entry i (Some (List.nth rem i))
      done;
      Queue.push q entry);
    rem

  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec filter_f q =
      match OrigQueue.pop q with
        | el ->
            for i = 0 to Weak.length el - 1 do
              match Weak.get el i with
                | Some p when fn p -> ()
                | _ -> Weak.set el i None
            done;
            filter_f q
        | exception Queue.Empty -> ()
    in
    apply q filter_f

  let filter_out q fn = filter q (fun el -> not (fn el))
end
