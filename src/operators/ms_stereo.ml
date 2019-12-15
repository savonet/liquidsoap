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

type mode = Encode | Decode

class msstereo ~kind (source : source) mode width =
  object
    inherit operator ~name:"stereo.ms.encode" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let buffer = AFrame.content buf offset in
      for i = offset to AFrame.position buf - 1 do
        match mode with
          | Encode ->
              let left = buffer.(0).{i} and right = buffer.(1).{i} in
              buffer.(0).{i} <- 0.5 *. (left +. right);
              (* mid *)
              buffer.(1).{i} <- 0.5 *. (left -. right)
              (* side *)
          | Decode ->
              let mid = buffer.(0).{i} and side = buffer.(1).{i} in
              buffer.(0).{i} <- mid +. (side *. width);
              (* left *)
              buffer.(1).{i} <- mid -. (side *. width)
        (* right *)
      done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_stereo in
  Lang.add_operator "stereo.ms.encode"
    [("", Lang.source_t k, None, None)]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Encode left+right stereo to mid+side stereo (M/S)."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      new msstereo ~kind s Encode 0.);
  Lang.add_operator "stereo.ms.decode"
    [
      ( "width",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Width of the stereo field." );
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Decode mid+side stereo (M/S) to left+right stereo."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let w = Lang.to_float (Lang.assoc "width" 1 p) in
      new msstereo ~kind s Decode w)
