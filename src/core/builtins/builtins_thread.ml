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

let thread = Modules.thread
let thread_run = Lang.add_module ~base:thread "run"
let should_stop = Atomic.make false

let () =
  Lifecycle.before_scheduler_shutdown ~name:"thread shutdown" (fun () ->
      Atomic.set should_stop true)

let _ =
  Lang.add_builtin ~base:thread "delay" ~category:`Programming
    ~descr:"Delay the current thread by the given duration in seconds."
    [("", Lang.float_t, None, None)]
    Lang.unit_t
    (fun p ->
      Unix.sleepf (Lang.to_float (List.assoc "" p));
      Lang.unit)

let _ =
  Lang.add_builtin ~base:thread_run "recurrent" ~category:`Programming
    [
      ( "fast",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Whether the thread is supposed to return quickly or not. Typically, \
           blocking tasks (e.g. fetching data over the internet) should not be \
           considered to be fast. When set to `false` its priority will be \
           lowered below that of request resolutions and fast timeouts. This \
           is only effective if you set a dedicated queue for fast tasks, see \
           the \"scheduler\" settings for more details." );
      ( "delay",
        Lang.float_t,
        Some (Lang.float 0.),
        Some "Delay (in sec.) after which the thread should be launched." );
      ( "on_error",
        Lang.nullable_t (Lang.fun_t [(false, "", Lang.error_t)] Lang.float_t),
        Some Lang.null,
        Some
          "Error callback executed when an error occurred while running the \
           given function. When passed, all raised errors are silenced unless \
           re-raised by the callback." );
      ( "",
        Lang.fun_t [] Lang.float_t,
        None,
        Some
          "Function to execute recurrently. The returned value is the delay \
           (in sec.) in which the function should be run again (it won't be \
           run if the value is strictly negative)." );
    ]
    Lang.unit_t ~descr:"Run a recurrent function in a separate thread."
    (fun p ->
      let delay = Lang.to_float (List.assoc "delay" p) in
      let f = List.assoc "" p in
      let priority =
        if Lang.to_bool (List.assoc "fast" p) then `Maybe_blocking
        else `Blocking
      in
      let on_error = Lang.to_option (List.assoc "on_error" p) in
      let on_error =
        Option.map
          (fun on_error exn bt ->
            let error =
              Lang.runtime_error_of_exception ~bt ~kind:"output" exn
            in
            Lang.apply on_error [("", Lang.error error)])
          on_error
      in
      let f () =
        try Lang.to_float (Lang.apply f [])
        with exn -> (
          let bt = Printexc.get_raw_backtrace () in
          match on_error with
            | Some fn -> Lang.to_float (fn exn bt)
            | None -> Lang.raise_as_runtime ~bt ~kind:"eval" exn)
      in
      let rec task delay =
        {
          Duppy.Task.priority;
          events = [`Delay delay];
          handler =
            (fun _ ->
              let delay = f () in
              if Atomic.get should_stop then []
              else (
                Clock.after_eval ();
                if delay >= 0. then [task delay] else []));
        }
      in
      Lifecycle.after_start ~name:"thread start" (fun () ->
          Duppy.Task.add Tutils.scheduler (task delay));
      Lang.unit)

let _ =
  let fun_t =
    Lang.fun_t
      [(false, "backtrace", Lang.string_t); (false, "", Lang.error_t)]
      Lang.unit_t
  in
  Lang.add_builtin ~base:thread "on_error" ~category:`Programming
    ~descr:
      "Register the function to be called when an error of the given kind is \
       raised in a thread. Catches all errors if first argument is `null`."
    [("", Lang.nullable_t Lang.error_t, None, None); ("", fun_t, None, None)]
    Lang.unit_t
    (fun p ->
      let on_err = Lang.to_valued_option Lang.to_error (Lang.assoc "" 1 p) in
      let fn = Lang.assoc "" 2 p in
      let handler ~bt err =
        match (err, on_err) with
          | Runtime_error.(Runtime_error error), None ->
              let error = Lang.error error in
              let bt = Lang.string bt in
              ignore (Lang.apply fn [("backtrace", bt); ("", error)]);
              true
          | Runtime_error.(Runtime_error error), Some err
            when error.Runtime_error.kind = err.Runtime_error.kind ->
              let error = Lang.error error in
              let bt = Lang.string bt in
              ignore (Lang.apply fn [("backtrace", bt); ("", error)]);
              true
          | _ -> false
      in
      Stack.push handler Tutils.error_handlers;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:thread "pause" ~category:`Programming
    ~descr:
      "Pause execution for a given amount of seconds. This puts the calling \
       thread to sleep and should not be used in the main streaming loop."
    [("", Lang.float_t, None, Some "Number of seconds of pause.")]
    Lang.unit_t
    (fun p ->
      let t = Lang.to_float (List.assoc "" p) in
      Thread.delay t;
      Lang.unit)
