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

let profiler = Modules.profiler
let profiler_stats = Modules.profiler_stats

let _ =
  Lang.add_builtin ~base:profiler "enable" ~category:`Liquidsoap
    ~descr:"Record profiling statistics." [] Lang.unit_t (fun _ ->
      Term.profile := true;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:profiler "disable" ~category:`Liquidsoap
    ~descr:"Record profiling statistics." [] Lang.unit_t (fun _ ->
      Term.profile := false;
      Lang.unit)

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:profiler "run" ~category:`Liquidsoap
    ~descr:"Time a function with the profiler."
    [
      ("", Lang.string_t, None, Some "Name of the profiled function.");
      ("", Lang.fun_t [] a, None, Some "Function to profile.");
    ]
    a
    (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let f () = Lang.apply ~pos:(Lang.pos p) f [] in
      Profiler.time name f ())

let _ =
  Lang.add_builtin ~base:profiler_stats "string" ~category:`Liquidsoap
    ~descr:"Profiling statistics." [] Lang.string_t (fun _ ->
      Lang.string (Profiler.stats ()))
