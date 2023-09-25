(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

class on_frame f s =
  object
    inherit Source.operator ~name:"on_frame" [s]
    method stype = s#stype
    method private _is_ready = s#is_ready
    method abort_track = s#abort_track
    method remaining = s#remaining
    method seek n = s#seek n
    method seek_source = s
    method self_sync = s#self_sync

    method private get_frame ab =
      s#get ab;
      ignore (Lang.apply f [])
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Muxer.source "on_frame"
    [
      ("", Lang.source_t frame_t, None, None);
      ( "",
        Lang.fun_t [] Lang.unit_t,
        None,
        Some
          "Function called on every frame. It should be fast because it is \
           executed in the main streaming thread." );
    ]
    ~category:`Track ~descr:"Call a given handler on every frame."
    ~return_t:frame_t
    (fun p ->
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let f = Lang.assoc "" 2 p in
      new on_frame f s)

(** Operations on frames. *)
class frame_op ~name f default s =
  object
    inherit Source.operator ~name [s]
    method stype = s#stype
    method private _is_ready = s#is_ready
    method abort_track = s#abort_track
    method remaining = s#remaining
    method seek n = s#seek n
    method seek_source = s
    method self_sync = s#self_sync
    val mutable value = default
    method value : Lang.value = value

    method private get_frame buf =
      let off = Frame.position buf in
      s#get buf;
      let pos = Frame.position buf in
      value <- f buf off (pos - off)
  end

let source_frame = Lang.add_module ~base:Muxer.source "frame"

let op name descr f_t f default =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  ignore
    (Lang.add_operator ~base:source_frame name
       [("", Lang.source_t frame_t, None, None)]
       ~category:`Track ~descr
       ~return_t:(Lang.method_t frame_t [("frame_" ^ name, ([], f_t), descr)])
       ~meth:[("frame_" ^ name, ([], f_t), descr, fun s -> s#value)]
       (fun p ->
         let s = List.assoc "" p |> Lang.to_source in
         new frame_op ~name f default s))

let () =
  op "duration" "Compute the duration of the last frame." Lang.float_t
    (fun _ _ len -> Lang.float (Frame.seconds_of_main len))
    (Lang.float 0.);
  op "rms" "Compute the rms of the last frame." Lang.float_t
    (fun buf off len ->
      let rms =
        Mm.Audio.Analyze.rms (AFrame.pcm buf) (Frame.audio_of_main off)
          (Frame.audio_of_main len)
      in
      let rms = Array.fold_left max 0. rms in
      Lang.float rms)
    (Lang.float 0.)
