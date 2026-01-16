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

module Lang = Liquidsoap_lang.Lang
open Lang

module ClockSource = struct
  include Value.MkCustom (struct
    type content = Clock.source

    let name = "clock_source"
    let to_string s = Printf.sprintf "clock_source<id=%s>" s#id

    let to_json ~pos _ =
      Lang.raise_error ~message:"Clock sources cannot be represented as json"
        ~pos "json"

    let compare = Stdlib.compare
  end)

  let base_t = t
  let to_base_value = to_value

  let methods =
    [
      ( "id",
        Lang.fun_t [] Lang.string_t,
        "The sources's id",
        fun s -> Lang.val_fun [] (fun _ -> Lang.string s#id) );
    ]

  let t =
    method_t base_t
      (List.map (fun (lbl, typ, descr, _) -> (lbl, ([], typ), descr)) methods)

  let to_value c =
    Lang.meth (to_base_value c)
      (List.map (fun (lbl, _, _, v) -> (lbl, v c)) methods)
end

module ClockValue = struct
  include Value.MkCustom (struct
    type content = Clock.t

    let name = "clock"
    let to_string = Clock.descr

    let to_json ~pos _ =
      Lang.raise_error ~message:"Clocks cannot be represented as json" ~pos
        "json"

    let compare = Stdlib.compare
  end)

  let base_t = t
  let to_base_value = to_value

  let methods =
    [
      ( "id",
        Lang.ref_t Lang.string_t,
        "The clock's id",
        fun c ->
          let get () = Lang.string (Clock.id c) in
          let set v = Clock.set_id c (Lang.to_string v) in
          Lang.reference get set );
      ( "sub_clocks",
        Lang.fun_t [] (Lang.list_t base_t),
        "The list of sub-clocks for this clock.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.list (List.map to_base_value (Clock.sub_clocks c))) );
      ( "sync",
        Lang.fun_t [] Lang.string_t,
        "The clock's current sync mode. One of: `\"stopped\"`, `\"stopping\"`, \
         `\"auto\"`, `\"CPU\"`, `\"unsynced\"` or `\"passive\"`.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.string Clock.(string_of_sync_mode (sync c))) );
      ( "start",
        Lang.fun_t [(true, "force", Lang.bool_t)] Lang.unit_t,
        "Start the clock.",
        fun c ->
          Lang.val_fun
            [("force", "force", Some (Lang.bool true))]
            (fun p ->
              let pos = Lang.pos p in
              let force = Lang.to_bool (List.assoc "force" p) in
              try
                Clock.start ~force c;
                Lang.unit
              with Clock.Invalid_state ->
                Runtime_error.raise
                  ~message:
                    (Printf.sprintf "Invalid clock state: %s"
                       Clock.(string_of_sync_mode (sync c)))
                  ~pos "clock") );
      ( "stop",
        Lang.fun_t [] Lang.unit_t,
        "Stop the clock. Does nothing if the clock is stopping or stopped.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Clock.stop c;
              Lang.unit) );
      ( "self_sync",
        Lang.fun_t [] Lang.bool_t,
        "`true` if the clock is in control of its latency.",
        fun c -> Lang.val_fun [] (fun _ -> Lang.bool (Clock.self_sync c)) );
      ( "unify",
        Lang.fun_t [(false, "", base_t)] Lang.unit_t,
        "Unify the clock with another one. One of the two clocks should be in \
         `\"stopped\"` sync mode.",
        fun c ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let pos = match Lang.pos p with p :: _ -> Some p | [] -> None in
              let c' = of_value (List.assoc "" p) in
              Clock.unify ~pos c c';
              Lang.unit) );
      ( "sources",
        Lang.fun_t [] (Lang.list_t ClockSource.t),
        "List of sources connected to the clock. This returns abstract sources \
         for logging and etc. These sources cannot be used in operators.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.list (List.map ClockSource.to_value (Clock.sources c))) );
      ( "active_sources",
        Lang.fun_t [] (Lang.list_t ClockSource.t),
        "List of active sources connected to the clock. This returns abstract \
         sources for logging and etc. These sources cannot be used in \
         operators.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.list (List.map ClockSource.to_value (Clock.active_sources c)))
      );
      ( "passive_sources",
        Lang.fun_t [] (Lang.list_t ClockSource.t),
        "List of passive sources connected to the clock. This returns abstract \
         sources for logging and etc. These sources cannot be used in \
         operators.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.list
                (List.map ClockSource.to_value (Clock.passive_sources c))) );
      ( "outputs",
        Lang.fun_t [] (Lang.list_t ClockSource.t),
        "List of outputs connected to the clock. This returns abstract sources \
         for logging and etc. These sources cannot be used in operators.",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Lang.list (List.map ClockSource.to_value (Clock.outputs c))) );
      ( "tick",
        Lang.fun_t [] Lang.unit_t,
        "Animate the clock and run one tick",
        fun c ->
          Lang.val_fun [] (fun _ ->
              Clock.tick c;
              Lang.unit) );
      ( "ticks",
        Lang.fun_t [] Lang.int_t,
        "The total number of times the clock has ticked.",
        fun c -> Lang.val_fun [] (fun _ -> Lang.int (Clock.ticks c)) );
      ( "dump",
        Lang.fun_t [] Lang.string_t,
        "Dump source graph for the clock.",
        fun c -> Lang.val_fun [] (fun _ -> Lang.string (Clock.dump_sources c))
      );
    ]

  let t =
    method_t base_t
      (List.map (fun (lbl, typ, descr, _) -> (lbl, ([], typ), descr)) methods)

  let to_value c =
    Lang.meth (to_base_value c)
      (List.map (fun (lbl, _, _, v) -> (lbl, v c)) methods)
end
