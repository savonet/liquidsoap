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

(* Performed for every callback a script registers, carrying what releases it.
   Registering is the only way a script can add a callback to a source, so an
   operator handing sources to a script function can take back everything that
   function registered on them, and nothing else: the engine's own wiring does
   not go through here. *)
type _ Effect.t +=
  | Registered : { owner : int; release : unit -> unit } -> unit Effect.t

type 'a collected = { release : unit -> unit; result : 'a }

let notify ~owner release = Effect.perform (Registered { owner; release })

let collect owners fn =
  let releases = ref [] in
  let result =
    Effect.Deep.try_with fn ()
      {
        Effect.Deep.effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
              | Registered { owner; release } when List.mem owner owners ->
                  Some
                    (fun (k : (a, _) Effect.Deep.continuation) ->
                      releases := release :: !releases;
                      Effect.Deep.continue k ())
              | _ -> None);
      }
  in
  let release () =
    List.iter (fun release -> release ()) !releases;
    releases := []
  in
  { release; result }

(* Effects do not cross thread boundaries, so every thread that can run script
   code installs this outermost handler: a registration no operator asked to
   collect is simply not collected. A context that forgot to install it raises
   [Effect.Unhandled] at the registration site rather than losing the release
   silently, which would keep what it counts alive for good. *)
let uncollected fn =
  Effect.Deep.try_with fn ()
    {
      Effect.Deep.effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
            | Registered _ ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    Effect.Deep.continue k ())
            | _ -> None);
    }
