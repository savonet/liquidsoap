(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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
      ignore (Lang.apply f [])
  end

let () =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "source.on_frame"
    [
      ("", Lang.source_t k, None, None);
      ( "",
        Lang.fun_t [] Lang.unit_t,
        None,
        Some
          "Function called on every frame. It should be fast because it is \
           executed in the main streaming thread." );
    ]
    ~category:Lang.TrackProcessing ~descr:"Call a given handler on every frame."
    ~return_t:k
    (fun p ->
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let f = Lang.assoc "" 2 p in
      new on_frame ~kind f s)
