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

class on_frame ~kind f s =
  object
    inherit Source.operator ~name:"on_frame" kind [s]

    method stype = s#stype

    method is_ready = s#is_ready

    method abort_track = s#abort_track

    method remaining = s#remaining

    method seek n = s#seek n

    method self_sync = s#self_sync

    method private get_frame ab =
      s#get ab;
      ignore (Lang.apply ~t:Lang.unit_t f [])
  end

let () =
  let kind = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "on_frame"
    [
      ( "",
        Lang.fun_t [] Lang.unit_t,
        None,
        Some
          "Function called on every frame. It should be fast because it is \
           executed in the main streaming thread." );
      ("", Lang.source_t kind, None, None);
    ]
    ~category:Lang.TrackProcessing ~descr:"Call a given handler on every frame."
    ~return_t:kind
    (fun p kind ->
      let f = Lang.assoc "" 1 p in
      let s = Lang.to_source (Lang.assoc "" 2 p) in
      new on_frame ~kind f s)
