(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_builtins

let () =
  add_builtin "thread.run.recurrent" ~cat:Control
    [ ( "fast",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Whether the thread is supposed to return quickly or not. \
           Typically, blocking tasks (e.g. fetching data over the internet) \
           should not be considered to be fast. When set to `false` its \
           priority will be lowered below that of request resolutions and \
           fast timeouts. This is only effective if you set a dedicated queue \
           for fast tasks, see the \"scheduler\" settings for more details." );
      ( "delay",
        Lang.float_t,
        Some (Lang.float 0.),
        Some "Delay (in sec.) after which the thread should be lauched." );
      ( "",
        Lang.fun_t [] Lang.float_t,
        None,
        Some
          "Function to execute recurrently. The returned value is the delay \
           (in sec.) in which the function should be run again (it won't be \
           run if the value is strictly negative)." ) ]
    Lang.unit_t ~descr:"Run a recurrent function in a separate thread."
    (fun p ->
      let delay = Lang.to_float (List.assoc "delay" p) in
      let f = List.assoc "" p in
      let priority =
        if Lang.to_bool (List.assoc "fast" p) then Tutils.Maybe_blocking
        else Tutils.Blocking
      in
      let rec task delay =
        {
          Duppy.Task.priority;
          events= [`Delay delay];
          handler=
            (fun _ ->
              let delay = Lang.to_float (Lang.apply ~t:Lang.float_t f []) in
              if delay >= 0. then [task delay] else []);
        }
      in
      Duppy.Task.add Tutils.scheduler (task delay) ;
      Lang.unit)

let () =
  let t = Lang.univ_t () in
  add_builtin "thread.mutexify" ~cat:Liq
    ~descr:
      "Protect functions with a mutex in order to avoid concurrent calls. It \
       returns the original value when the argument is not a function."
    [("", t, None, None)] t (fun p ->
      let m = Mutex.create () in
      let v = List.assoc "" p in
      match v.Lang.value with
        | Lang.Fun (p, args, env, body) ->
            let fn (args : Lang.full_env) t =
              Tutils.mutexify m
                (fun () ->
                  let args =
                    List.map (fun (x, gv) -> (x, Lazy.from_val gv)) args
                  in
                  let env = List.rev_append args env in
                  let v = {v with Lang.value= Lang.Fun ([], [], env, body)} in
                  Lang.apply ~t v [])
                ()
            in
            {v with Lang.value= Lang.FFI (p, args, fn)}
        | Lang.FFI (p, args, fn) ->
            let fn args t = Tutils.mutexify m (fun () -> fn args t) () in
            {v with Lang.value= Lang.FFI (p, args, fn)}
        | _ ->
            v)
