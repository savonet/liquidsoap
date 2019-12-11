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
  add_builtin "on_shutdown" ~cat:Sys
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap shuts down."
    (fun p ->
      let f = List.assoc "" p in
      let wrap_f () = ignore (Lang.apply ~t:Lang.unit_t f []) in
      ignore (Dtools.Init.make ~before:[Tutils.scheduler_shutdown_atom] wrap_f) ;
      Lang.unit)

let () =
  add_builtin "on_start" ~cat:Sys
    [("", Lang.fun_t [] Lang.unit_t, None, None)]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap starts."
    (fun p ->
      let f = List.assoc "" p in
      let wrap_f () = ignore (Lang.apply ~t:Lang.unit_t f []) in
      (* TODO: this could happen after duppy and other threads are shut down, is that ok? *)
      ignore (Dtools.Init.at_start wrap_f) ;
      Lang.unit)

let () =
  add_builtin "source.on_shutdown" ~cat:Sys
    [ ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.fun_t [] Lang.unit_t, None, None) ]
    Lang.unit_t
    ~descr:"Register a function to be called when source shuts down."
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let wrap_f () = ignore (Lang.apply ~t:Lang.unit_t f []) in
      s#on_shutdown wrap_f ; Lang.unit)

let () =
  add_builtin "source.is_up" ~cat:Sys
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t ~descr:"Check whether a source is up."
    (fun p -> Lang.bool (Lang.to_source (Lang.assoc "" 1 p))#is_up)
