(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Lang_core

(* Augment source_t and source with default methods. *)

let source_methods =
  [
    ( "id",
      ([], fun_t [] string_t),
      "Identifier of the source.",
      fun s -> val_fun [] (fun _ -> string s#id) );
    ( "is_ready",
      ([], fun_t [] bool_t),
      "Indicate if a source is ready to stream. This does not mean that the \
       source is currently streaming, just that its resources are all properly \
       initialized.",
      fun s -> val_fun [] (fun _ -> bool s#is_ready) );
    ( "last_metadata",
      ([], fun_t [] (nullable_t metadata_t)),
      "Return the last metadata from the source.",
      fun s ->
        val_fun [] (fun _ ->
            match s#last_metadata with None -> null | Some m -> metadata m) );
    ( "on_metadata",
      ([], fun_t [(false, "", fun_t [(false, "", metadata_t)] unit_t)] unit_t),
      "Call a given handler on metadata packets.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_metadata (fun m -> ignore (apply f [("", metadata m)]));
            unit) );
    ( "on_get_ready",
      ([], fun_t [(false, "", fun_t [] unit_t)] unit_t),
      "Register a function to be called after the source is asked to get \
       ready. This is when, for instance, the source's final ID is set.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_get_ready (fun () -> ignore (apply f []));
            unit) );
    ( "on_shutdown",
      ([], fun_t [(false, "", fun_t [] unit_t)] unit_t),
      "Register a function to be called when source shuts down.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_shutdown (fun () -> ignore (apply f []));
            unit) );
    ( "on_leave",
      ([], fun_t [(false, "", fun_t [] unit_t)] unit_t),
      "Register a function to be called when source is not used anymore by \
       another source.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_leave (fun () -> ignore (apply f []));
            unit) );
    ( "on_track",
      ([], fun_t [(false, "", fun_t [(false, "", metadata_t)] unit_t)] unit_t),
      "Call a given handler on new tracks.",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            let f = assoc "" 1 p in
            s#on_track (fun m -> ignore (apply f [("", metadata m)]));
            unit) );
    ( "remaining",
      ([], fun_t [] float_t),
      "Estimation of remaining time in the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let r = s#remaining in
               if r < 0 then infinity else Frame.seconds_of_main r)) );
    ( "elapsed",
      ([], fun_t [] float_t),
      "Elapsed time in the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let e = s#elapsed in
               if e < 0 then infinity else Frame.seconds_of_main e)) );
    ( "duration",
      ([], fun_t [] float_t),
      "Estimation of the duration of the current track.",
      fun s ->
        val_fun [] (fun _ ->
            float
              (let d = s#duration in
               if d < 0 then infinity else Frame.seconds_of_main d)) );
    ( "self_sync",
      ([], fun_t [] bool_t),
      "Is the source currently controling its own real-time loop.",
      fun s -> val_fun [] (fun _ -> bool (snd s#self_sync)) );
    ( "log",
      ( [],
        record_t
          [
            ( "level",
              method_t
                (fun_t [] (nullable_t int_t))
                [
                  ( "set",
                    ([], fun_t [(false, "", int_t)] unit_t),
                    "Set the source's log level" );
                ] );
          ] ),
      "Get or set the source's log level, from `1` to `5`.",
      fun s ->
        record
          [
            ( "level",
              meth
                (val_fun [] (fun _ ->
                     match s#log#level with Some lvl -> int lvl | None -> null))
                [
                  ( "set",
                    val_fun
                      [("", "", None)]
                      (fun p ->
                        let lvl = min 5 (max 1 (to_int (List.assoc "" p))) in
                        s#log#set_level lvl;
                        unit) );
                ] );
          ] );
    ( "is_up",
      ([], fun_t [] bool_t),
      "Indicate that the source can be asked to produce some data at any time. \
       This is `true` when the source is currently being used or if it could \
       be used at any time, typically inside a `switch` or `fallback`.",
      fun s -> val_fun [] (fun _ -> bool s#is_up) );
    ( "is_active",
      ([], fun_t [] bool_t),
      "`true` if the source is active, i.e. it is continuously animated by its \
       own clock whenever it is ready. Typically, `true` for outputs and \
       sources such as `input.http`.",
      fun s -> val_fun [] (fun _ -> bool s#is_active) );
    ( "seek",
      ([], fun_t [(false, "", float_t)] float_t),
      "Seek forward, in seconds (returns the amount of time effectively \
       seeked).",
      fun s ->
        val_fun
          [("", "", None)]
          (fun p ->
            float
              (Frame.seconds_of_main
                 (s#seek (Frame.main_of_seconds (to_float (List.assoc "" p))))))
    );
    ( "skip",
      ([], fun_t [] unit_t),
      "Skip to the next track.",
      fun s ->
        val_fun [] (fun _ ->
            s#abort_track;
            unit) );
    ( "fallible",
      ([], bool_t),
      "Indicate if a source may fail, i.e. may not be ready to stream.",
      fun s -> bool (s#stype = `Fallible) );
    ( "time",
      ([], fun_t [] float_t),
      "Get a source's time, based on its assigned clock.",
      fun s ->
        val_fun [] (fun _ ->
            let ticks =
              if Source.Clock_variables.is_known s#clock then
                (Source.Clock_variables.get s#clock)#get_tick
              else 0
            in
            let frame_position =
              Lazy.force Frame.duration *. float_of_int ticks
            in
            let in_frame_position =
              Frame.seconds_of_main (Frame.position s#memo)
            in
            float (frame_position +. in_frame_position)) );
  ]

let source_t ?(methods = false) t =
  let t = source_t t in
  if methods then
    method_t t
      (List.map (fun (name, t, doc, _) -> (name, t, doc)) source_methods)
  else t

let () =
  Term.source_methods_t :=
    fun () -> source_t ~methods:true (kind_type_of_kind_format any)

let source v =
  meth (source v)
    (List.map (fun (name, _, _, fn) -> (name, fn v)) source_methods)

(** A method: name, type scheme, documentation and implementation (which takes
    the currently defined source as argument). *)
type 'a operator_method = string * scheme * string * ('a -> value)

(** An operator is a builtin function that builds a source.
  * It is registered using the wrapper [add_operator].
  * Creating the associated function type (and function) requires some work:
  *  - Specify which content_kind the source will carry:
  *    a given fixed number of channels, any fixed, a variable number?
  *  - The content_kind can also be linked to a type variable,
  *    e.g. the parameter of a format type.
  * From this high-level description a type is created. Often it will
  * carry a type constraint.
  * Once the type has been inferred, the function might be executed,
  * and at this point the type might still not be known completely
  * so we have to force its value within the acceptable range. *)
let add_operator =
  let _meth = meth in
  fun ~(category : Documentation.source) ~descr ?(flags = [])
      ?(meth = ([] : 'a operator_method list)) name proto ~return_t f ->
    let compare (x, _, _, _) (y, _, _, _) =
      match (x, y) with
        | "", "" -> 0
        | _, "" -> -1
        | "", _ -> 1
        | x, y -> Stdlib.compare x y
    in
    let proto =
      ( "id",
        nullable_t string_t,
        Some null,
        Some "Force the value of the source ID." )
      :: List.stable_sort compare proto
    in
    let f env =
      let src : < Source.source ; .. > = f env in
      ignore
        (Option.map
           (fun id -> src#set_id id)
           (to_valued_option to_string (List.assoc "id" env)));
      let v = source (src :> Source.source) in
      _meth v (List.map (fun (name, _, _, fn) -> (name, fn src)) meth)
    in
    let f env =
      let pos = None in
      try
        let ret = f env in
        if category = `Output then (
          let m, _ = Value.split_meths ret in
          _meth unit m)
        else ret
      with
        | Source.Clock_conflict (a, b) ->
            raise (Error.Clock_conflict (pos, a, b))
        | Source.Clock_loop (a, b) -> raise (Error.Clock_loop (pos, a, b))
        | Kind.Conflict (a, b) -> raise (Error.Kind_conflict (pos, a, b))
    in
    let return_t = source_t ~methods:true return_t in
    let return_t =
      method_t return_t
        (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
    in
    let return_t =
      if category = `Output then (
        let m, _ = Type.split_meths return_t in
        let m =
          List.map (fun Type.{ meth = x; scheme = y; doc = z } -> (x, y, z)) m
        in
        method_t unit_t m)
      else return_t
    in
    let category = `Source category in
    add_builtin ~category ~descr ~flags name proto return_t f
