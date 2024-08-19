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

  let pop q = try pop q with Empty -> raise Not_found

  let flush_iter q fn =
    let rec flush_iter_f () =
      match pop_opt q with
        | Some el ->
            fn el;
            flush_iter_f ()
        | None -> ()
    in
    flush_iter_f ()

  let flush_fold q fn ret =
    let rec flush_fold_f ret =
      match pop_opt q with Some el -> flush_fold_f (fn el ret) | None -> ret
    in
    flush_fold_f ret

  let flush_elements q =
    let flush_elements_f el elements = el :: elements in
    List.rev (flush_fold q flush_elements_f [])

  let elements q =
    let rec elements_f l cursor =
      match next cursor with
        | Some (el, cursor) -> elements_f (el :: l) cursor
        | None -> List.rev l
    in
    elements_f [] (snapshot q)

  let exists q fn =
    let rec exists_f l cursor =
      match next cursor with
        | Some (el, _) when fn el -> true
        | Some (el, cursor) -> exists_f (el :: l) cursor
        | None -> false
    in
    exists_f [] (snapshot q)

  let length q =
    let rec length_f pos cursor =
      match next cursor with
        | Some (_, cursor) -> length_f (pos + 1) cursor
        | None -> pos
    in
    length_f 0 (snapshot q)

  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec filter_f elements =
      match pop_opt q with
        | Some el -> filter_f (el :: elements)
        | None -> elements
    in
    List.iter (fun el -> if fn el then push q el) (filter_f [])
end

module WeakQueue = struct
  include Queue

  type 'a q = 'a t
  type 'a t = 'a Weak.t q

  let push q v =
    let w = Weak.create 1 in
    Weak.set w 0 (Some v);
    push q w

  let flush_iter q fn =
    flush_iter q (fun x ->
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
    push_batches q rem;
    rem

  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec filter_f cursor =
      match next cursor with
        | Some (el, cursor) ->
            for i = 0 to Weak.length el - 1 do
              match Weak.get el i with
                | Some p when fn p -> ()
                | _ -> Weak.set el i None
            done;
            filter_f cursor
        | None -> ()
    in
    filter_f (snapshot q)
end
