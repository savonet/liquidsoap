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
  add_builtin "source.skip" ~cat:Liq ~descr:"Skip to the next track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.unit_t
    (fun p -> (Lang.to_source (List.assoc "" p))#abort_track ; Lang.unit)

let () =
  add_builtin "source.seek" ~cat:Liq
    ~descr:
      "Seek forward, in seconds. Returns the amount of time effectively seeked."
    [ ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.float_t, None, None) ]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let time = Lang.to_float (Lang.assoc "" 2 p) in
      let len = Frame.master_of_seconds time in
      let ret = s#seek len in
      Lang.float (Frame.seconds_of_master ret))

let () =
  add_builtin "source.id" ~cat:Liq ~descr:"Get one source's identifier."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.string_t
    (fun p -> Lang.string (Lang.to_source (List.assoc "" p))#id)

let () =
  add_builtin "source.fallible" ~cat:Liq
    ~descr:"Indicate if a source may fail, i.e. may not be ready to stream."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p ->
      Lang.bool ((Lang.to_source (List.assoc "" p))#stype == Source.Fallible))

let () =
  add_builtin "source.is_ready" ~cat:Liq
    ~descr:"Indicate if a source is ready to stream, or currently streaming."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#is_ready)

let () =
  add_builtin "source.remaining" ~cat:Liq
    ~descr:"Estimation of remaining time in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let r = (Lang.to_source (List.assoc "" p))#remaining in
      let f = if r = -1 then infinity else Frame.seconds_of_master r in
      Lang.float f)

let () =
  add_builtin "source.shutdown" ~cat:Liq ~descr:"Desactivate a source."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.unit_t
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      (Clock.get s#clock)#detach (fun (s' : Source.active_source) ->
          (s' :> Source.source) = s) ;
      Lang.unit)

let () =
  let s_t =
    let v = Lang.variable_t in
    Lang.source_t (Lang.frame_kind_t ~audio:v ~video:v ~midi:v)
  in
  add_builtin "source.init" ~cat:Liq
    ~descr:
      "Simultaneously initialize sources, return the sublist of sources that \
       failed to initialize."
    [("", Lang.list_t s_t, None, None)]
    (Lang.list_t s_t)
    (fun p ->
      let l = Lang.to_list (List.assoc "" p) in
      let l = List.map Lang.to_source l in
      let l =
        (* TODO this whole function should be about active sources,
         *   just like source.shutdown() but the language has no runtime
         *   difference between sources and active sources, so we use
         *   this trick to compare active sources and passive ones... *)
        Clock.force_init (fun x -> List.exists (fun y -> Oo.id x = Oo.id y) l)
      in
      Lang.list ~t:s_t (List.map (fun x -> Lang.source (x :> Source.source)) l))
