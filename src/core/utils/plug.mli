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

(** Registries that plug-ins register into: protocols, decoders, encoders,
    metadata resolvers.

    Entries come from two places: OCaml modules register theirs as they
    initialise, and scripts register more through [add_protocol] and friends
    while they run. A plug is therefore never closed -- but it is {e incomplete}
    until [Lifecycle.load] has run, because before that point which modules have
    initialised is decided by link order.

    So {!entries} and everything built on it raise until then. Asking a plug
    what it contains during module initialisation is not a question about the
    program, it is a question about the linker, and the answer changes when an
    unrelated dependency is added. That is not a mistake worth debugging twice.
*)

type 'a t

(** Raised by {!entries} and everything built on it before [Lifecycle.load].
    Carries the plug's name. *)
exception Incomplete of string

(** [ordered_by] is a settings list that records the entries as they register
    and then decides the order {!ordered_entries} returns them in, so that users
    can change which resolver or decoder is tried first. *)
val create :
  ?ordered_by:string list Dtools.Conf.t -> doc:string -> string -> 'a t

(** Add an entry. Legal at any time: scripts add protocols and playlist parsers
    long after the program has started. *)
val register : 'a t -> string -> doc:string -> 'a -> unit

(** Look one entry up by name. Legal at any time: the answer does not depend on
    who else has registered. *)
val get : 'a t -> string -> 'a option

(** Every entry, in registration order. Raises {!Incomplete} if the program is
    still loading. *)
val entries : 'a t -> (string * 'a) list

(** Every entry, in the order configured through the plug's [ordered_by]
    setting. Same as {!entries} for plugs that do not have one. Raises
    {!Incomplete} if the program is still loading. *)
val ordered_entries : 'a t -> (string * 'a) list

val iter : 'a t -> (string -> 'a -> unit) -> unit
val find : 'a t -> (string -> 'a -> bool) -> (string * 'a) option

(** Do something with everything the OCaml modules registered, once, as soon as
    they all have. This is how to derive anything from a whole plug during
    initialisation -- registering one builtin per entry, say. Runs immediately
    if loading is already done. *)
val on_complete : 'a t -> ((string * 'a) list -> unit) -> unit

(** Marks every plug complete and runs the {!on_complete} callbacks. Called from
    [Lifecycle.load]; there should be no reason to call it anywhere else. *)
val mark_complete : unit -> unit
