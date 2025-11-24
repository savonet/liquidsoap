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

let source = Muxer.source

let _ =
  let univ = Lang.univ_t () in
  Lang.add_builtin ~base:source "methods" ~category:(`Source `Liquidsoap)
    ~descr:"Returns the given source decorated with all its methods."
    [("", Lang.source_t ~methods:false univ, None, None)]
    (Lang.source_t ~methods:true univ)
    (fun p -> List.assoc "" p)

let _ =
  let univ = Lang.univ_t () in
  Lang.add_builtin ~base:source "effective" ~category:(`Source `Liquidsoap)
    ~descr:
      "Returns the effective source for the given source that is the source \
       effectively being animated during the current streaming cyc le. For \
       instance, this return the currently active source in a `switch` and \
       etc. The operator is recursive so it is applied through the whole \
       streaming graph until it finds a leaf source."
    [("", Lang.source_t ~methods:false univ, None, None)]
    (Lang.source_t ~methods:false univ)
    (fun p -> Lang.source (Lang.to_source (List.assoc "" p))#effective_source)

let _ =
  Lang.add_builtin ~base:source "set_id" ~category:(`Source `Liquidsoap)
    ~descr:"Set the id of an operator."
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.string_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let n = Lang.assoc "" 2 p |> Lang.to_string in
      s#set_id n;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:source "skip" ~category:(`Source `Liquidsoap)
    ~descr:"Skip to the next track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.unit_t
    (fun p ->
      (Lang.to_source (List.assoc "" p))#abort_track;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:source "seek" ~category:(`Source `Liquidsoap)
    ~descr:
      "Seek forward, in seconds. Returns the amount of time effectively seeked."
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.float_t, None, None);
    ]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let time = Lang.to_float (Lang.assoc "" 2 p) in
      let len = Frame.main_of_seconds time in
      let ret = s#seek len in
      Lang.float (Frame.seconds_of_main ret))

let _ =
  Lang.add_builtin ~base:source "id" ~category:(`Source `Liquidsoap)
    ~descr:"Get the identifier of a source."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.string_t
    (fun p -> Lang.string (Lang.to_source (List.assoc "" p))#id)

let _ =
  Lang.add_builtin ~base:source "fallible" ~category:(`Source `Liquidsoap)
    ~descr:"Indicate if a source may fail, i.e. may not be ready to stream."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#fallible)

let _ =
  Lang.add_builtin ~base:source "is_ready" ~category:(`Source `Liquidsoap)
    ~descr:
      "Indicate if a source is ready to stream (we also say that it is \
       available), or currently streaming."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#is_ready)

let _ =
  Lang.add_builtin ~base:source "is_up" ~category:`System
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t ~descr:"Check whether a source is up."
    (fun p -> Lang.bool (Lang.to_source (Lang.assoc "" 1 p))#is_up)

let _ =
  Lang.add_builtin ~base:source "remaining" ~category:(`Source `Liquidsoap)
    ~descr:"Estimation of remaining time in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let r = (Lang.to_source (List.assoc "" p))#remaining in
      let f = if r < 0 then infinity else Frame.seconds_of_main r in
      Lang.float f)

let _ =
  Lang.add_builtin ~base:source "elapsed" ~category:(`Source `Liquidsoap)
    ~descr:"Elapsed time in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let d = (Lang.to_source (List.assoc "" p))#elapsed in
      let f = if d < 0 then infinity else Frame.seconds_of_main d in
      Lang.float f)

let _ =
  Lang.add_builtin ~base:source "duration" ~category:(`Source `Liquidsoap)
    ~descr:"Estimation of the duration in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let d = (Lang.to_source (List.assoc "" p))#duration in
      let f = if d < 0 then infinity else Frame.seconds_of_main d in
      Lang.float f)

let _ =
  Lang.add_builtin ~category:(`Source `Liquidsoap) ~base:source "time"
    ~descr:"Get a source's time, based on its assigned clock"
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      let ticks = Clock.ticks s#clock in
      let frame_position = Lazy.force Frame.duration *. float ticks in
      Lang.float frame_position)

let _ =
  Lang.add_builtin ~base:source "on_shutdown" ~category:(`Source `Liquidsoap)
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.fun_t [] Lang.unit_t, None, None);
    ]
    Lang.unit_t
    ~descr:
      "Register a function to be called when source is not used anymore by \
       another source."
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let wrap_f () = ignore (Lang.apply f []) in
      s#on_sleep wrap_f;
      Lang.unit)
