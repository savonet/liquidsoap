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

let () =
  Lang.add_module "thread";
  Lang.add_module "thread.run"

let error_handlers = Stack.create ()

exception Error_processed

let rec error_handler ~bt exn =
  try
    Stack.iter
      (fun handler -> if handler ~bt exn then raise Error_processed)
      error_handlers;
    false
  with
    | Error_processed -> true
    | exn ->
        let bt = Printexc.get_backtrace () in
        error_handler ~bt exn

let () =
  Lang.add_builtin "thread.run.recurrent" ~category:`Programming
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
      let f () =
        try Lang.to_float (Lang.apply f [])
        with e ->
          let raw_bt = Printexc.get_raw_backtrace () in
          let bt = Printexc.get_backtrace () in
          if error_handler ~bt e then -1.
          else Printexc.raise_with_backtrace e raw_bt
      in
      let rec task delay =
        {
          Duppy.Task.priority;
          events = [`Delay delay];
          handler =
            (fun _ ->
              let delay = f () in
              if delay >= 0. then [task delay] else []);
        }
      in
      Duppy.Task.add Tutils.scheduler (task delay);
      Lang.unit)

let () =
  let fun_t =
    Lang.fun_t
      [(false, "backtrace", Lang.string_t); (false, "", Lang.error_t)]
      Lang.unit_t
  in
  Lang.add_builtin "thread.on_error" ~category:`Programming
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
          | Runtime_error.(Runtime_error { kind; msg; _ }), None ->
              let error = Lang.error { Runtime_error.kind; msg; pos = [] } in
              let bt = Lang.string bt in
              ignore (Lang.apply fn [("backtrace", bt); ("", error)]);
              true
          | Runtime_error.(Runtime_error { kind; msg; _ }), Some err
            when kind = err.Runtime_error.kind ->
              let error = Lang.error { Runtime_error.kind; msg; pos = [] } in
              let bt = Lang.string bt in
              ignore (Lang.apply fn [("backtrace", bt); ("", error)]);
              true
          | _ -> false
      in
      Stack.push handler error_handlers;
      Lang.unit)
