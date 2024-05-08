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

module Queue = struct
  include Saturn_lockfree.Queue

  let flush q fn =
    let rec f () =
      match pop_opt q with
        | Some el ->
            fn el;
            f ()
        | None -> ()
    in
    f ()

  let fold_flush q fn ret =
    let rec f ret =
      match pop_opt q with Some el -> f (fn el ret) | None -> ret
    in
    f ret

  let elements q =
    let rec f l cursor =
      match next cursor with
        | Some (el, cursor) -> f (el :: l) cursor
        | None -> List.rev l
    in
    f [] (snapshot q)

  let exists q fn =
    let rec f l cursor =
      match next cursor with
        | Some (el, _) when fn el -> true
        | Some (el, cursor) -> f (el :: l) cursor
        | None -> false
    in
    f [] (snapshot q)

  let length q =
    let rec f pos cursor =
      match next cursor with
        | Some (_, cursor) -> f (pos + 1) cursor
        | None -> pos
    in
    f 0 (snapshot q)

  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec f elements =
      match pop_opt q with Some el -> f (el :: elements) | None -> elements
    in
    List.iter (fun el -> if fn el then push q el) (f [])
end

module WeakQueue = struct
  include Queue

  type 'a q = 'a t
  type 'a t = 'a Weak.t q

  let push q v =
    let w = Weak.create 1 in
    Weak.set w 0 (Some v);
    push q w

  let flush q fn =
    flush q (fun x ->
        for i = 0 to Weak.length x - 1 do
          match Weak.get x i with Some v -> fn v | None -> ()
        done)

  let batch_size = 1024

  let push_batches q elements =
    let len = List.length elements in
    let n_batches = int_of_float (ceil (float len /. float batch_size)) in
    for i = 0 to n_batches - 1 do
      let entry = Weak.create batch_size in
      let offset = i * batch_size in
      let max_pos = min (len - offset) batch_size in
      for pos = 0 to max_pos - 1 do
        Weak.set entry pos (Some (List.nth elements (pos + offset)))
      done;
      Queue.push q entry
    done

  let elements q =
    let rec f rem =
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
            f (get_weak_entries 0 rem)
        | None -> rem
    in
    let rem = f [] in
    push_batches q rem;
    rem

  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec f cursor =
      match next cursor with
        | Some (el, cursor) ->
            for i = 0 to Weak.length el - 1 do
              match Weak.get el i with
                | Some p when fn p -> ()
                | _ -> Weak.set el i None
            done;
            f cursor
        | None -> ()
    in
    f (snapshot q)
end
