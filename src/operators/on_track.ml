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

class on_track ~kind f s =
  object
    inherit Source.operator ~name:"on_track" kind [s]

    method stype = s#stype

    method is_ready = s#is_ready

    method abort_track = s#abort_track

    method remaining = s#remaining

    method seek n = s#seek n

    method self_sync = s#self_sync

    val mutable called = false

    method private get_frame ab =
      let p = Frame.position ab in
      s#get ab ;
      if not called then (
        let m =
          match Frame.get_metadata ab p with
            | None ->
                Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) []
            | Some m ->
                Lang.metadata m
        in
        ignore (Lang.apply ~t:Lang.unit_t f [("", m)]) ;
        called <- true ) ;
      if Frame.is_partial ab then called <- false
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "on_track"
    [ ( "",
        Lang.fun_t
          [ ( false,
              "",
              Lang.list_t (Lang.product_t Lang.string_t Lang.string_t) ) ]
          Lang.unit_t,
        None,
        Some
          "Function called on every beginning of track in the stream, with \
           the corresponding metadata as argument. If there is no metadata at \
           the beginning of track, the empty list is passed. That function \
           should be fast because it is executed in the main streaming thread."
      );
      ("", Lang.source_t kind, None, None) ]
    ~category:Lang.TrackProcessing ~descr:"Call a given handler on new tracks."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let f = Lang.assoc "" 1 p in
      let s = Lang.to_source (Lang.assoc "" 2 p) in
      new on_track ~kind f s)
