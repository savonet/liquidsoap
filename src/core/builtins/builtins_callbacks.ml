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
  Lang.add_builtin "on_shutdown" ~category:`System
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap shuts down."
    (fun p ->
      let f = List.assoc "" p in
      Lifecycle.before_core_shutdown ~name:"on shutdown execution" (fun () ->
          ignore (Lang.apply f []));
      Lang.unit)

let _ =
  Lang.add_builtin "on_cleanup" ~category:`System
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t ~descr:"Register a function to be called for the final cleanup."
    (fun p ->
      let f = List.assoc "" p in
      Lifecycle.on_final_cleanup ~name:"on cleanup execution" (fun () ->
          ignore (Lang.apply f []));
      Lang.unit)

let _ =
  Lang.add_builtin "on_start" ~category:`System
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap starts."
    (fun p ->
      let f = List.assoc "" p in
      let wrap_f () = ignore (Lang.apply f []) in
      Lifecycle.after_start ~name:"on start execution" wrap_f;
      Lang.unit)

let catchable_signals =
  [
    ("hup", Sys.sighup);
    ("quit", Sys.sigquit);
    ("usr1", Sys.sigusr1);
    ("usr2", Sys.sigusr2);
    ("alrm", Sys.sigalrm);
    ("cont", Sys.sigcont);
    ("tstp", Sys.sigtstp);
    ("ttin", Sys.sigttin);
    ("ttou", Sys.sigttou);
    ("urg", Sys.sigurg);
    ("vtalrm", Sys.sigvtalrm);
    ("prof", Sys.sigprof);
    ("xcpu", Sys.sigxcpu);
    ("xfsz", Sys.sigxfsz);
    ("poll", Sys.sigpoll);
  ]

let signal_log = Log.make ["signal"]

let run_signal_callback f =
  try ignore (Lang.apply f [])
  with exn ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log:signal_log ~bt
      (Printf.sprintf "Error in signal callback: %s" (Printexc.to_string exn))

(* Callbacks queued by signal handlers, newest first. A callback released after
   its signal was received does not run. *)
let pending_signal_callbacks = Atomic.make []

let rec queue_signal_callback f =
  let pending = Atomic.get pending_signal_callbacks in
  if
    not (Atomic.compare_and_set pending_signal_callbacks pending (f :: pending))
  then queue_signal_callback f

(* Handlers only queue their callback and write to a never-closed pipe:
   running liq code, adding a task or waking a [Duppy.Async] takes a lock the
   interrupted code may hold. *)
let wake_signal_task =
  Lazy.Mutexed.from_fun (fun () ->
      let read_fd, write_fd = Unix.pipe ~cloexec:true () in
      Unix.set_nonblock read_fd;
      Unix.set_nonblock write_fd;
      let buffer = Bytes.create 256 in
      let rec task () =
        {
          Duppy.Task.priority = `Threaded;
          events = [`Read read_fd];
          handler =
            (fun _ ->
              (try ignore (Unix.read read_fd buffer 0 (Bytes.length buffer))
               with
               | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                 ());
              List.iter
                (fun run -> run ())
                (List.rev (Atomic.exchange pending_signal_callbacks []));
              [task ()]);
        }
      in
      Duppy.Task.add Tutils.scheduler (task ());
      let byte = Bytes.make 1 '\000' in
      fun () ->
        try ignore (Unix.single_write write_fd byte 0 1)
        with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ())

let _ =
  Lang.add_builtin "on_signal" ~category:`System
    [
      ( "",
        Lang.string_t,
        None,
        Some
          "Signal name, lowercase and without the `SIG` prefix, e.g. `\"hup\"`."
      );
      ("", Lang.fun_t [] Lang.unit_t, None, None);
    ]
    (Lang.method_t Lang.unit_t
       [
         ( "release",
           ([], Lang.fun_t [] Lang.unit_t),
           "Stop calling the function. Calling it more than once is a no-op." );
       ])
    ~descr:
      (Printf.sprintf
         "Register a function to be called when the process receives the given \
          signal. A signal keeps its default action until a function is \
          registered for it, and does not get it back once released: `SIGHUP` \
          terminates the process by default. Functions run in registration \
          order, and their errors are logged. Returns a `release` method that \
          unregisters the function. Supported signals: %s. `int` and `term` \
          shut liquidsoap down, so they cannot be registered. `usr1` also \
          keeps reopening the log file. Does nothing on Windows."
         (String.concat ", "
            (List.map (fun (name, _) -> "`" ^ name ^ "`") catchable_signals)))
    (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let released remove =
        Lang.meth Lang.unit
          [
            ( "release",
              Lang.val_fun [] (fun _ ->
                  remove ();
                  Lang.unit) );
          ]
      in
      match List.assoc_opt name catchable_signals with
        | None ->
            Runtime_error.raise ~pos:(Lang.pos p)
              ~message:(Printf.sprintf "Unsupported signal: %s" name)
              "invalid"
        | Some _ when Sys.win32 ->
            signal_log#important "Signals are not supported on Windows.";
            released (fun () -> ())
        | Some signal ->
            let wake = Lazy.Mutexed.force wake_signal_task in
            let active = Atomic.make true in
            let run () = if Atomic.get active then run_signal_callback f in
            let remove =
              Signal_handlers.add signal (fun _ ->
                  queue_signal_callback run;
                  wake ())
            in
            released (fun () ->
                Atomic.set active false;
                remove ()))
