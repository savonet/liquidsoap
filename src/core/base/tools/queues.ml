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

  let push q v =
    mutate q (fun q ->
        let arr = Atomic.get q.a in
        let len = Weak.length arr in
        let rec count_live i acc =
          if i >= len then acc
          else count_live (i + 1) (if Weak.check arr i then acc + 1 else acc)
        in
        let live_count = count_live 0 0 in
        let new_arr = Weak.create (live_count + 1) in
        if live_count = len then Weak.blit arr 0 new_arr 0 len
        else begin
          let j = ref 0 in
          for i = 0 to len - 1 do
            match Weak.get arr i with
              | Some el ->
                  Weak.set new_arr !j (Some el);
                  incr j
              | None -> ()
          done
        end;
        Weak.set new_arr live_count (Some v);
        Atomic.set q.a new_arr)

  let exists q fn =
    get q (fun arr ->
        let len = Weak.length arr in
        let rec loop i =
          if i >= len then false
          else (
            match Weak.get arr i with
              | Some el when fn el -> true
              | _ -> loop (i + 1))
        in
        loop 0)

  let length q =
    get q (fun arr ->
        let len = Weak.length arr in
        let rec loop i acc =
          if i >= len then acc
          else loop (i + 1) (if Weak.check arr i then acc + 1 else acc)
        in
        loop 0 0)

  let iter q fn =
    get q (fun arr ->
        let len = Weak.length arr in
        let rec loop i =
          if i >= len then ()
          else (
            (match Weak.get arr i with Some el -> fn el | None -> ());
            loop (i + 1))
        in
        loop 0)

  let fold q fn v =
    get q (fun arr ->
        let len = Weak.length arr in
        let rec loop i acc =
          if i >= len then acc
          else
            loop (i + 1)
              (match Weak.get arr i with Some el -> fn el acc | None -> acc)
        in
        loop 0 v)

  let elements q =
    get q (fun arr ->
        let len = Weak.length arr in
        let rec loop i acc =
          if i >= len then List.rev acc
          else
            loop (i + 1)
              (match Weak.get arr i with Some el -> el :: acc | None -> acc)
        in
        loop 0 [])

  let flush_elements q =
    let arr = mutate q (fun q -> Atomic.exchange q.a (Weak.create 0)) in
    let len = Weak.length arr in
    let rec loop i acc =
      if i >= len then List.rev acc
      else
        loop (i + 1)
          (match Weak.get arr i with Some el -> el :: acc | None -> acc)
    in
    loop 0 []

  let flush_iter q fn =
    let arr = mutate q (fun q -> Atomic.exchange q.a (Weak.create 0)) in
    let len = Weak.length arr in
    let rec loop i =
      if i >= len then ()
      else (
        (match Weak.get arr i with Some el -> fn el | None -> ());
        loop (i + 1))
    in
    loop 0

  let filter q fn =
    mutate q (fun q ->
        let arr = Atomic.get q.a in
        let len = Weak.length arr in
        let rec loop i =
          if i >= len then ()
          else (
            (match Weak.get arr i with
              | Some el when not (fn el) -> Weak.set arr i None
              | _ -> ());
            loop (i + 1))
        in
        loop 0)

  let filter_out q fn = filter q (fun el -> not (fn el))
end
