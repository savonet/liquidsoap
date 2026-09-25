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
            released
              (Signal_callbacks.add signal (fun () -> ignore (Lang.apply f []))))
