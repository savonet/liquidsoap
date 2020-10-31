(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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
  add_builtin "on_shutdown" ~cat:Sys
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap shuts down."
    (fun p ->
      let f = List.assoc "" p in
      Lifecycle.before_core_shutdown (fun () -> ignore (Lang.apply f []));
      Lang.unit)

let () =
  add_builtin "on_start" ~cat:Sys
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap starts."
    (fun p ->
      let f = List.assoc "" p in
      let wrap_f () = ignore (Lang.apply f []) in
      Lifecycle.after_start wrap_f;
      Lang.unit)
