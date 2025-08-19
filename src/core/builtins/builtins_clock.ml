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

let clock =
  Lang.add_builtin "clock" ~category:`Liquidsoap
    ~descr:"Decorate a clock with all its methods."
    [("", Lang_source.ClockValue.base_t, None, None)]
    Lang_source.ClockValue.t
    (fun p -> Lang_source.ClockValue.(to_value (of_value (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:clock "active" ~category:`Liquidsoap
    ~descr:"Return the list of clocks currently in use." []
    (Lang.list_t Lang_source.ClockValue.t) (fun _ ->
      Lang.list (List.map Lang_source.ClockValue.to_value (Clock.clocks ())))

let _ =
  Lang.add_builtin ~base:clock "create" ~category:`Liquidsoap
    ~descr:"Create a new clock"
    [
      ( "id",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Identifier for the new clock." );
      ( "on_error",
        Lang.nullable_t (Lang.fun_t [(false, "", Lang.error_t)] Lang.unit_t),
        Some Lang.null,
        Some
          "Error callback executed when a streaming error occurs. When passed, \
           all streaming errors are silenced. Intended mostly for debugging \
           purposes." );
      ( "sync",
        Lang.string_t,
        Some (Lang.string "auto"),
        Some
          "Clock sync mode. Should be one of: `\"auto\"`, `\"CPU\"`, \
           `\"unsynced\"` or `\"passive\"`. Defaults to `\"auto\"`. Defaults \
           to: \"auto\"" );
    ]
    Lang_source.ClockValue.t
    (fun p ->
      let id = Lang.to_valued_option Lang.to_string (List.assoc "id" p) in
      let id = Option.value ~default:"scripted_clock" id in
      let id = Lang_string.generate_id ~category:"clock" id in
      let on_error = Lang.to_option (List.assoc "on_error" p) in
      let on_error =
        Option.map
          (fun on_error exn bt ->
            let error =
              Lang.runtime_error_of_exception ~bt ~kind:"output" exn
            in
            ignore (Lang.apply on_error [("", Lang.error error)]))
          on_error
      in
      let sync = List.assoc "sync" p in
      let sync =
        match Lang.to_string sync with
          | "auto" -> `Automatic
          | "cpu" -> `CPU
          | "none" -> `Unsynced
          | "passive" ->
              `Passive
                (object
                   method id = id
                end)
          | _ ->
              raise
                (Error.Invalid_value
                   ( sync,
                     "Invalid sync mode! Should be one of: `\"auto\"`, \
                      `\"CPU\"`, `\"unsynced\"` or `\"passive\"`" ))
      in
      Lang_source.ClockValue.to_value
        (Clock.create ~stack:(Lang.pos p) ?on_error ~id ~sync ()))
