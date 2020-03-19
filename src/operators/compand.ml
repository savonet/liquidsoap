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

open Source

class compand ~kind (source : source) mu =
  object
    inherit operator ~name:"compand" kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf in
      for c = offset to Array.length b - 1 do
        let b_c = b.(c) in
        for i = offset to AFrame.position buf - 1 do
          (* Cf. http://en.wikipedia.org/wiki/Mu-law *)
          let sign = if b_c.{i} < 0. then -1. else 1. in
          b_c.{i} <-
            sign *. log (1. +. (mu *. abs_float b_c.{i})) /. log (1. +. mu)
        done
      done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "compand"
    [
      ("mu", Lang.float_t, Some (Lang.float 1.), None);
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Compand the signal"
    (fun p kind ->
      let f v = List.assoc v p in
      let mu, src = (Lang.to_float (f "mu"), Lang.to_source (f "")) in
      new compand ~kind src mu)
