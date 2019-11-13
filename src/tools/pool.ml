(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Manage an unbounded pool of objects identified by integers.
  * This is used for Requests. *)

module type T = sig
  (** Type of objects stored in the pool. *)
  type t
end

module type S = sig
  type t

  val add : unit -> int * (t -> unit)

  val kill : int -> float -> unit

  val find : int -> t option

  val fold : (int -> t -> 'a -> 'a) -> 'a -> 'a

  val iter : (int -> t -> unit) -> unit

  val remove : int -> unit

  val size : unit -> int
end

module Make (P : T) : S with type t = P.t = struct
  type t = P.t

  type entry = {death_time: float option; value: t option}

  let m = Mutex.create ()

  let h : (int, entry) Hashtbl.t = Hashtbl.create 100

  let size = Tutils.mutexify m (fun () -> Hashtbl.length h)

  let find =
    Tutils.mutexify m (fun i ->
        try (Hashtbl.find h i).value with Not_found -> None)

  let fold f x =
    Tutils.mutexify m
      (fun f x ->
        Hashtbl.fold
          (fun i entry x ->
            match entry.value with Some t -> f i t x | None -> x)
          h x)
      f x

  let iter f =
    Tutils.mutexify m
      (fun f ->
        Hashtbl.iter
          (fun i entry ->
            match entry.value with
              | Some t ->
                  Mutex.unlock m ; f i t ; Mutex.lock m
              | None ->
                  ())
          h)
      f

  let remove i = Hashtbl.remove h i

  let kill i grace =
    Tutils.mutexify m
      (fun () ->
        Hashtbl.replace h i
          {(Hashtbl.find h i) with death_time= Some (Unix.time () +. grace)})
      ()

  let next =
    let rec find i =
      try
        match (Hashtbl.find h i).death_time with
          | Some t when Unix.time () > t ->
              i
          | _ ->
              find (i + 1)
      with Not_found -> i
    in
    Tutils.mutexify m (fun () ->
        let i = find 0 in
        Hashtbl.replace h i {death_time= None; value= None} ;
        i)

  let add () =
    let i = next () in
    ( i,
      Tutils.mutexify m (fun t ->
          Hashtbl.replace h i {death_time= None; value= Some t}) )
end
