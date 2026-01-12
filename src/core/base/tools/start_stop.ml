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

(* [`Start] and [`Stop] are explicit states resulting of a
   user command. [`Idle] is a default state at init or after a source
   failure. [`Idle] is used to captures situations where the output is
   stopped by can be restarted immediately when its underlying source
   becomes available again. In contrast, [`Stopped] indicates an
   explicit user-requested stop and the output will not automatically
   restart. *)
type state = [ `Started | `Stopped | `Idle ]

(** Base class for sources with start/stop methods. Class ineheriting it should
    declare their own [start]/[stop] method and users should call [#set_start]
*)
class virtual base =
  object (self)
    val mutable state : state = `Idle
    method state = state
    method virtual private start : unit
    method virtual private stop : unit
    val mutable on_start = []
    val mutable on_stop = []
    method on_start fn = on_start <- on_start @ [fn]
    method on_stop fn = on_stop <- on_stop @ [fn]

    (* Default [reset] method. Can be overridden if necessary. *)
    method reset =
      match self#state with
        | `Started ->
            self#stop;
            self#start
        | _ -> ()

    method transition_to (s : state) =
      match (s, self#state) with
        | `Started, `Stopped | `Started, `Idle ->
            self#start;
            List.iter (fun fn -> fn ()) on_start;
            state <- `Started
        | `Started, `Started -> ()
        | `Stopped, `Started ->
            self#stop;
            List.iter (fun fn -> fn ()) on_stop;
            state <- `Stopped
        | `Stopped, `Idle -> state <- `Stopped
        | `Stopped, `Stopped -> ()
        | `Idle, `Started ->
            self#stop;
            List.iter (fun fn -> fn ()) on_stop;
            state <- `Idle
        | `Idle, `Stopped | `Idle, `Idle -> ()
  end

class virtual active_source ~name ~fallible ~autostart () =
  object (self)
    inherit Source.active_source ~name ()
    inherit base as base

    initializer
      self#on_wake_up (fun () -> if autostart then base#transition_to `Started);
      self#on_sleep (fun () -> base#transition_to `Stopped)

    method fallible = fallible
    method private started = state = `Started
    method private output = if self#is_ready then ignore self#get_frame
  end

let base_proto ~label =
  [
    ( "start",
      Lang.bool_t,
      Some (Lang.bool true),
      Some ("Start " ^ label ^ " as soon as it is available.") );
  ]

let output_proto = base_proto ~label:"output"

let active_source_proto ~fallible_opt =
  base_proto ~label:"input"
  @
    match fallible_opt with
    | `Nope -> []
    | `Yep v ->
        [
          ( "fallible",
            Lang.bool_t,
            Some (Lang.bool v),
            Some
              "Allow the source to fail. If set to `false`, `start` must be \
               `true` and `stop` method raises an error." );
        ]

let callbacks ~label =
  Lang_source.
    [
      {
        name = "on_start";
        params = [];
        descr = "when " ^ label ^ " starts";
        register_deprecated_argument = true;
        arg_t = [];
        register = (fun ~params:_ s f -> s#on_start (fun () -> f []));
      };
      {
        name = "on_stop";
        params = [];
        descr = "when " ^ label ^ " stops";
        register_deprecated_argument = true;
        arg_t = [];
        register = (fun ~params:_ s f -> s#on_stop (fun () -> f []));
      };
    ]

let meth :
    unit ->
    (< state : state
     ; transition_to : state -> unit
     ; source_type : Source.source_type
     ; .. > ->
    Lang.value)
    Lang.meth
    list =
 fun () ->
  Lang.
    [
      {
        name = "is_started";
        scheme = ([], fun_t [] bool_t);
        descr = "`true` if the output or source is started.";
        value = (fun s -> val_fun [] (fun _ -> bool (s#state = `Started)));
      };
      {
        name = "start";
        scheme = ([], fun_t [] unit_t);
        descr = "Ask the source or output to start.";
        value =
          (fun s ->
            val_fun [] (fun _ ->
                s#transition_to `Started;
                unit));
      };
      {
        name = "stop";
        scheme = ([], fun_t [] unit_t);
        descr = "Ask the source or output to stop.";
        value =
          (fun s ->
            val_fun [] (fun p ->
                (match (s#source_type, s#fallible) with
                  | `Output _, _ -> ()
                  | _, false ->
                      Lang.raise_error ~pos:(Lang.pos p)
                        ~message:"Source is infallible and cannot be stopped"
                        "input"
                  | _ -> ());
                s#transition_to `Stopped;
                unit));
      };
    ]
