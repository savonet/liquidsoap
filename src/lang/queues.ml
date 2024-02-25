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
  include Michael_scott_queue

  let flush q fn =
    let rec f () =
      match pop_opt q with
        | Some el ->
            fn el;
            f ()
        | None -> ()
    in
    f ()

  let elements q =
    let rec f l cursor =
      match next cursor with
        | Some (el, cursor) -> f (el :: l) cursor
        | None -> List.rev l
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
  let filter q fn = flush q (fun entry -> if fn entry then push q entry)
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
    let len = List.length rem in
    if len > 0 then (
      let entry = Weak.create len in
      for i = 0 to len - 1 do
        Weak.set entry i (Some (List.nth rem i))
      done;
      Queue.push q entry);
    rem

  let length q = List.length (elements q)
  let iter q fn = List.iter fn (elements q)
  let fold q fn v = List.fold_left (fun v e -> fn e v) v (elements q)

  let filter q fn =
    let rec f cursor =
      match next cursor with
        | Some (el, cursor) ->
            for i = 0 to Weak.length el do
              match Weak.get el i with
                | Some p when fn p -> ()
                | _ -> Weak.set el i None
            done;
            f cursor
        | None -> ()
    in
    f (snapshot q)
end
