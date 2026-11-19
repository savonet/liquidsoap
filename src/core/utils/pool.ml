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

(** Manage an unbounded pool of objects identified by integers. This is used for
    Requests. *)

module type T = sig
  (** Type of objects stored in the pool. *)
  type t

  val id : t -> int
  val destroyed : int -> t
  val is_destroyed : t -> bool
end

module type S = sig
  type t

  val add : (int -> t) -> t
  val find : int -> t option
  val fold : (int -> t -> 'a -> 'a) -> 'a -> 'a
  val iter : (int -> t -> unit) -> unit
end

module Make (P : T) : S with type t = P.t = struct
  type t = P.t

  module WeakHash = Weak.Make (struct
    type t = P.t

    let equal t t' = P.id t = P.id t'
    let hash = P.id
  end)

  let h = WeakHash.create 10

  (* Weak hash sets are not thread-safe: requests are created and looked up
     from every clock's thread, and a resize racing another access corrupts
     the table. The lock is held across allocations, where GC finalisers can
     run script callbacks on the same thread: those re-enter without locking
     again instead of deadlocking. *)
  let lock = Mutex.create ()
  let owner = Atomic.make None

  let locked fn =
    let self = Thread.id (Thread.self ()) in
    if Atomic.get owner = Some self then fn ()
    else
      Mutex.protect lock (fun () ->
          Atomic.set owner (Some self);
          Fun.protect ~finally:(fun () -> Atomic.set owner None) fn)

  let find id =
    match locked (fun () -> WeakHash.find_opt h (P.destroyed id)) with
      | Some v when not (P.is_destroyed v) -> Some v
      | _ -> None

  (* Callbacks run outside the lock, on a snapshot of the live entries. *)
  let live () =
    locked (fun () ->
        WeakHash.fold
          (fun v cur -> if P.is_destroyed v then cur else v :: cur)
          h [])

  let fold f init =
    List.fold_left (fun cur v -> f (P.id v) v cur) init (live ())

  let iter f = List.iter (fun v -> f (P.id v) v) (live ())
  let current_id = Atomic.make 0

  let add fn =
    let rec f () =
      let id = Atomic.fetch_and_add current_id 1 in
      let v = fn id in
      match locked (fun () -> WeakHash.merge h v) with
        | v' when v == v' -> v
        | _ -> f ()
    in
    f ()
end
