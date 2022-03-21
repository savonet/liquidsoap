(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Mm
open Source

class clip ~kind (source : source) =
  object
    inherit operator ~name:"clip" kind [source]
    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method is_ready = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      let position = AFrame.position buf in
      Audio.clip b offset (position - offset)
  end

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "clip"
    [("", Lang.source_t k, None, None)]
    ~return_t:k ~category:`Audio
    ~descr:
      "Clip samples, i.e. ensure that all values are between -1 and 1: values \
       lower than -1 become -1 and values higher than 1 become 1."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let kind = Kind.of_kind kind in
      new clip ~kind src)

class unnan ~kind (source : source) =
  object
    inherit operator ~name:"unnan" kind [source]
    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method is_ready = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      let position = AFrame.position buf in
      for c = 0 to Array.length b - 1 do
        let bc = b.(c) in
        for i = offset to position - 1 do
          if Float.is_nan bc.(i) then bc.(i) <- 0.
        done
      done
  end

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "unnan"
    [("", Lang.source_t k, None, None)]
    ~return_t:k ~category:`Audio
    ~descr:
      "Convert nan values (which correspond to undefined float values) to 0. \
       Those can come up rare situations with badly designed audio effects."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let kind = Kind.of_kind kind in
      new unnan ~kind src)
