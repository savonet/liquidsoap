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

open Source

type mode = Encode | Decode

class msstereo ~field (source : source) mode width =
  object
    inherit operator ~name:"stereo.ms.encode" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method abort_track = source#abort_track

    method private generate_frame =
      let buffer = Content.Audio.get_data (source#get_mutable_content field) in
      for i = 0 to source#frame_audio_position - 1 do
        match mode with
          | Encode ->
              let left = buffer.(0).(i) and right = buffer.(1).(i) in
              buffer.(0).(i) <- 0.5 *. (left +. right);

              (* mid *)
              buffer.(1).(i) <- 0.5 *. (left -. right)
              (* side *)
          | Decode ->
              let mid = buffer.(0).(i) and side = buffer.(1).(i) in
              buffer.(0).(i) <- mid +. (side *. width);

              (* left *)
              buffer.(1).(i) <- mid -. (side *. width)
        (* right *)
      done;
      source#set_frame_data field Content.Audio.lift_data buffer
  end

let stereo_ms = Lang.add_module ~base:Stereo.stereo "ms"

let _ =
  let return_t = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:stereo_ms "encode"
    [("", return_t, None, None)]
    ~return_t ~category:`Audio
    ~descr:"Encode left+right stereo to mid+side stereo (M/S)."
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      (field, new msstereo ~field s Encode 0.))

let _ =
  let return_t = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:stereo_ms "decode"
    [
      ( "width",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Width of the stereo field." );
      ("", return_t, None, None);
    ]
    ~return_t ~category:`Audio
    ~descr:"Decode mid+side stereo (M/S) to left+right stereo."
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      let w = Lang.to_float (Lang.assoc "width" 1 p) in
      (field, new msstereo ~field s Decode w))

class spatializer ~field ~width (source : source) =
  object
    inherit operator ~name:"stereo.width" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method abort_track = source#abort_track

    method private generate_frame =
      let position = source#frame_audio_position in
      let buf = Content.Audio.get_data (source#get_mutable_content field) in
      let width = width () in
      let width = (width +. 1.) /. 2. in
      let a =
        let w = width in
        let w' = 1. -. width in
        w /. sqrt ((w *. w) +. (w' *. w'))
      in
      for i = 0 to position - 1 do
        let left = buf.(0).(i) in
        let right = buf.(1).(i) in
        let mid = (left +. right) /. 2. in
        let side = (left -. right) /. 2. in
        buf.(0).(i) <- ((1. -. a) *. mid) -. (a *. side);
        buf.(1).(i) <- ((1. -. a) *. mid) +. (a *. side)
      done;
      source#set_frame_data field Content.Audio.lift_data buf
  end

let _ =
  let return_t = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:Stereo.stereo "width"
    [
      ( "",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.),
        Some "Width of the signal (-1: mono, 0.: original, 1.: wide stereo)." );
      ("", return_t, None, None);
    ]
    ~return_t ~category:`Audio
    ~descr:"Spacializer which allows controlling the width of the signal."
    (fun p ->
      let width = Lang.assoc "" 1 p |> Lang.to_float_getter in
      let field, s = Lang.assoc "" 2 p |> Lang.to_track in
      (field, new spatializer ~field ~width s))
