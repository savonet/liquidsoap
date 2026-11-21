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

let _ =
  Lang.add_builtin ~category:`Liquidsoap Reserved.eval
    ~descr:"Parse and evaluate a string." ~flags:[`Hidden]
    [("type", Value.RuntimeType.t, None, None); ("", Lang.string_t, None, None)]
    (Lang.univ_t ())
    (fun p ->
      let ty = Value.RuntimeType.of_value (List.assoc "type" p) in
      let ty = Type.fresh ty in
      let s = Lang.to_string (List.assoc "" p) in
      Lang.protect ~kind:"eval" (fun () ->
          Lang.eval ~toplevel:false ~stdlib:`Disabled ~ty s))

type lock = { id : int; lock : Atomic_section.lock }

module Lock = Value.MkCustom (struct
  type content = lock

  let name = "atomic_lock"
  let to_string { id } = Printf.sprintf "<atomic_lock %d>" id

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Lock cannot be represented as json"
      "json"

  let compare { id } { id = id' } = Int.compare id id'
end)

let global_lock = Lock.to_value { id = 0; lock = Atomic_section.global }

let atomic =
  let a = Lang.univ_t () in
  Lang.add_builtin "atomic" ~category:`Programming
    ~descr:
      "Run a function without letting any other atomic section on the same \
       lock run at the same time, and return its value. Use it to group \
       changes that must not be observed half-done, such as writing several \
       references together. Without `lock`, sections use one lock for the \
       whole process. Only other atomic sections on that lock are held back: \
       code that does not take it is free to run alongside. Sections are \
       indivisible, not transactional: if the function raises, the changes it \
       already made stand. Never wait on another thread inside a section, and \
       do not call source methods there."
    [
      ( "fast",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Whether the function is supposed to return quickly or not. When \
           `true`, threads waiting for the section spin instead of going to \
           sleep, which is cheaper for a short section and wasteful for a long \
           one." );
      ( "lock",
        Lock.t,
        Some global_lock,
        Some
          "Lock to take, from `atomic.lock()`. Defaults to the process-wide \
           lock." );
      ("", Lang.fun_t [] a, None, Some "Function to run.");
    ]
    a
    (fun p ->
      let fast = Lang.to_bool (List.assoc "fast" p) in
      let lock = (Lock.of_value (List.assoc "lock" p)).lock in
      let f = List.assoc "" p in
      Atomic_section.run ~fast ~lock (fun () ->
          Lang.apply ~pos:(Lang.pos p) f []))

let _ =
  let next_id = Atomic.make 1 in
  Lang.add_builtin ~base:atomic "lock" ~category:`Programming
    ~descr:
      "A new lock for `atomic`. Sections on different locks run alongside each \
       other." [] Lock.t (fun _ ->
      Lock.to_value
        { id = Atomic.fetch_and_add next_id 1; lock = Atomic_section.create () })
