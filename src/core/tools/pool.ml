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
  val remove : int -> unit
  val size : unit -> int
  val clear : unit -> unit
end

module Make (P : T) : S with type t = P.t = struct
  type t = P.t

  module WeakHash = Weak.Make (struct
    type t = P.t

    let equal t t' = P.id t = P.id t'
    let hash = P.id
  end)

  let h = WeakHash.create 10

  let find id =
    match WeakHash.find_opt h (P.destroyed id) with
      | Some v when not (P.is_destroyed v) -> Some v
      | _ -> None

  let fold f =
    WeakHash.fold
      (fun v cur -> if P.is_destroyed v then cur else f (P.id v) v cur)
      h

  let iter f =
    WeakHash.iter
      (fun entry -> if not (P.is_destroyed entry) then f (P.id entry) entry)
      h

  let remove id = WeakHash.remove h (P.destroyed id)
  let current_id = Atomic.make 0

  let add fn =
    let rec f () =
      let id = Atomic.fetch_and_add current_id 1 in
      let v = fn id in
      match WeakHash.merge h v with v' when v == v' -> v | _ -> f ()
    in
    f ()

  let size () = WeakHash.count h
  let clear () = WeakHash.clear h
end
