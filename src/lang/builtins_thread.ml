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
  add_builtin "thread.run" ~cat:Control
    [
      "fast", Lang.bool_t, Some (Lang.bool true),
      Some "Whether the thread is supposed to quickly or not. Typically, \
            blocking tasks (e.g. fetching data over the internet) should not be \
            considered to be fast. When set to `false` its priority will be \
            lowered below that of request resolutions and fast timeouts. This is \
            only effective if you set a dedicated queue for fast tasks, see the \
            \"scheduler\" settings for more details." ;
      "delay", Lang.float_t, Some (Lang.float 0.),
      Some "Delay (in sec.) after which the thread should be lauched.";
      "", Lang.fun_t [] Lang.unit_t, None, Some "Function to execute."
    ]
    Lang.unit_t
    ~descr:"Run a function in a separate thread."
    (fun p ->
       let delay = Lang.to_float (List.assoc "delay" p) in
       let f = List.assoc "" p in
       let priority =
         if Lang.to_bool (List.assoc "fast" p) then
           Tutils.Maybe_blocking
         else
           Tutils.Blocking
       in
       let task =
         { Duppy.Task.
           priority = priority ;
           events   = [`Delay delay] ;
           handler  = fun _ -> Lang.to_unit (Lang.apply ~t:Lang.unit_t f []); []
         }
       in
       Duppy.Task.add Tutils.scheduler task;
       Lang.unit)
