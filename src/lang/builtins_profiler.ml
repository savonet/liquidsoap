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
  add_builtin "profiler.enable" ~cat:Liq ~descr:"Record profiling statistics."
    [] Lang.unit_t (fun _ ->
      Lang_values.profile := true;
      Lang.unit)

let () =
  add_builtin "profiler.disable" ~cat:Liq ~descr:"Record profiling statistics."
    [] Lang.unit_t (fun _ ->
      Lang_values.profile := false;
      Lang.unit)

let () =
  add_builtin "profiler.run" ~cat:Liq
    ~descr:"Time a function with the profiler."
    [
      ("", Lang.string_t, None, Some "Name of the profiled function.");
      ("", Lang.fun_t [] Lang.unit_t, None, Some "Function to profile.");
    ]
    Lang.unit_t
    (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let f () = ignore (Lang.apply f [] ~t:Lang.unit_t) in
      Profiler.time name f ();
      Lang.unit)

let () =
  add_builtin "profiler.stats.string" ~cat:Liq ~descr:"Profiling statistics." []
    Lang.string_t (fun _ -> Lang.string (Profiler.stats ()))
