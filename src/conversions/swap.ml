(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

class swap ~kind (source : source) =
  object
    inherit operator kind [source] ~name:"swap"

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      let buffer =
        source#get buf;
        AFrame.content buf offset
      in
      if offset = 0 then (
        let tmp = buffer.(1) in
        buffer.(1) <- buffer.(2);
        buffer.(2) <- tmp )
      else
        for i = offset to AFrame.position buf - 1 do
          let tmp = buffer.(0).{i} in
          buffer.(0).{i} <- buffer.(1).{i};
          buffer.(1).{i} <- tmp
        done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_stereo in
  Lang.add_operator "swap"
    [("", Lang.source_t k, None, None)]
    ~kind:(Lang.Unconstrained k) ~category:Lang.Conversions
    ~descr:"Swap two channels of a stereo source."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      new swap ~kind s)
