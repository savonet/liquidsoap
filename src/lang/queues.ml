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

  let flush_elements q =
    let rec flush_elements_f elements =
      match pop_exn q with
        | el -> flush_elements_f (el :: elements)
        | exception Empty -> List.rev elements
    in
    flush_elements_f []

  let pop q = try pop_exn q with Empty -> raise Not_found
  let peek q = try peek_exn q with Empty -> raise Not_found
  let flush_iter q fn = List.iter fn (flush_elements q)

  let flush_fold q fn ret =
    let flush_fold_f ret el = fn el ret in
    List.fold_left flush_fold_f ret (flush_elements q)

  let elements q =
    let rec elements_f l =
      match pop_exn q with
        | el -> elements_f (el :: l)
        | exception Empty -> List.rev l
    in
    let elements = elements_f [] in
    List.iter (push q) elements;
    elements

  let exists q fn = List.exists fn (elements q)
  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let filter_f el = if fn el then push q el in
    List.iter filter_f (flush_elements q)
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
    let rec filter_f () =
      match pop_exn q with
        | el ->
            for i = 0 to Weak.length el - 1 do
              match Weak.get el i with
                | Some p when fn p -> ()
                | _ -> Weak.set el i None
            done;
            filter_f ()
        | exception Empty -> ()
    in
    filter_f ()
end
