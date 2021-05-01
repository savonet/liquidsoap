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

open Lang_values
open Lang_values.Ground
open Lang_encoders

(* Looks like this is how ffmpeg CLI does it.
   See: https://github.com/FFmpeg/FFmpeg/blob/4782124b90cf915ede2cebd871be82fc0267a135/fftools/ffmpeg_opt.c#L1567-L1570 *)
let set_global_quality q f =
  let flags =
    match Hashtbl.find_opt f.Ffmpeg_format.other_opts "flags" with
      | Some (`Int f) -> f
      | Some _ -> assert false
      | None -> 0
  in
  let flags = flags lor Avcodec.flag_qscale in
  Hashtbl.replace f.Ffmpeg_format.other_opts "flags" (`Int flags);
  Hashtbl.replace f.Ffmpeg_format.other_opts "global_quality"
    (`Float (float Avutil.qp2lambda *. q))

let ffmpeg_gen params =
  let defaults =
    {
      Ffmpeg_format.format = None;
      output = `Stream;
      channels = 2;
      samplerate = Frame.audio_rate;
      framerate = Frame.video_rate;
      width = Frame.video_width;
      height = Frame.video_height;
      pixel_format = None;
      audio_codec = None;
      video_codec = None;
      hwaccel = `Auto;
      hwaccel_device = None;
      audio_opts = Hashtbl.create 0;
      video_opts = Hashtbl.create 0;
      other_opts = Hashtbl.create 0;
    }
  in
  let to_int t =
    match t.term with
      | Ground (Int i) -> i
      | Ground (String s) -> int_of_string s
      | Ground (Float f) -> int_of_float f
      | _ -> raise (generic_error t)
  in
  let to_string t =
    match t.term with
      | Ground (Int i) -> Printf.sprintf "%i" i
      | Ground (String s) -> s
      | Ground (Float f) -> Printf.sprintf "%f" f
      | _ -> raise (generic_error t)
  in
  let to_float t =
    match t.term with
      | Ground (Int i) -> float i
      | Ground (String s) -> float_of_string s
      | Ground (Float f) -> f
      | _ -> raise (generic_error t)
  in
  let rec parse_args ~format ~mode f = function
    | [] -> f
    (* Audio options *)
    | ("channels", t) :: l when mode = `Audio ->
        parse_args ~format ~mode { f with Ffmpeg_format.channels = to_int t } l
    | ("ac", t) :: l when mode = `Audio ->
        parse_args ~format ~mode { f with Ffmpeg_format.channels = to_int t } l
    | ("samplerate", t) :: l when mode = `Audio ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.samplerate = Lazy.from_val (to_int t) }
          l
    | ("ar", t) :: l when mode = `Audio ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.samplerate = Lazy.from_val (to_int t) }
          l
    (* Video options *)
    | ("framerate", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.framerate = Lazy.from_val (to_int t) }
          l
    | ("r", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.framerate = Lazy.from_val (to_int t) }
          l
    | ("width", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.width = Lazy.from_val (to_int t) }
          l
    | ("height", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.height = Lazy.from_val (to_int t) }
          l
    | ("pixel_format", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.pixel_format = None } l
    | ("pixel_format", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.pixel_format = Some (to_string t) }
          l
    | ("hwaccel", { term = Var "auto"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.hwaccel = `Auto } l
    | ("hwaccel", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.hwaccel = `None } l
    | ("hwaccel_device", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.hwaccel_device = None }
          l
    | ("hwaccel_device", t) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.hwaccel_device = Some (to_string t) }
          l
    (* Shared options *)
    | ("codec", t) :: l ->
        let f =
          match (mode, format) with
            | `Audio, `Internal ->
                {
                  f with
                  Ffmpeg_format.audio_codec =
                    Some (`Internal (Some (to_string t)));
                }
            | `Audio, `Raw ->
                {
                  f with
                  Ffmpeg_format.audio_codec = Some (`Raw (Some (to_string t)));
                }
            | `Video, `Internal ->
                {
                  f with
                  Ffmpeg_format.video_codec =
                    Some (`Internal (Some (to_string t)));
                }
            | `Video, `Raw ->
                {
                  f with
                  Ffmpeg_format.video_codec = Some (`Raw (Some (to_string t)));
                }
        in
        parse_args ~format ~mode f l
    | ("q", t) :: l | ("qscale", t) :: l ->
        set_global_quality (to_float t) f;
        parse_args ~format ~mode f l
    | (k, { term = Ground (String s); _ }) :: l ->
        ( match mode with
          | `Audio -> Hashtbl.add f.Ffmpeg_format.audio_opts k (`String s)
          | `Video -> Hashtbl.add f.Ffmpeg_format.video_opts k (`String s) );
        parse_args ~format ~mode f l
    | (k, { term = Ground (Int i); _ }) :: l ->
        ( match mode with
          | `Audio -> Hashtbl.add f.Ffmpeg_format.audio_opts k (`Int i)
          | `Video -> Hashtbl.add f.Ffmpeg_format.video_opts k (`Int i) );
        parse_args ~format ~mode f l
    | (k, { term = Ground (Float fl); _ }) :: l ->
        ( match mode with
          | `Audio -> Hashtbl.add f.Ffmpeg_format.audio_opts k (`Float fl)
          | `Video -> Hashtbl.add f.Ffmpeg_format.video_opts k (`Float fl) );
        parse_args ~format ~mode f l
    | (_, t) :: _ -> raise (generic_error t)
  in
  List.fold_left
    (fun f -> function
      | `Audio_none -> { f with Ffmpeg_format.audio_codec = None; channels = 0 }
      | `Audio_copy -> { f with Ffmpeg_format.audio_codec = Some `Copy }
      | `Audio_raw l ->
          let f = { f with Ffmpeg_format.audio_codec = Some (`Raw None) } in
          parse_args ~format:`Raw ~mode:`Audio f l
      | `Audio l ->
          let f =
            { f with Ffmpeg_format.audio_codec = Some (`Internal None) }
          in
          parse_args ~format:`Internal ~mode:`Audio f l
      | `Video_none -> { f with Ffmpeg_format.video_codec = None }
      | `Video_copy -> { f with Ffmpeg_format.video_codec = Some `Copy }
      | `Video_raw l ->
          let f = { f with Ffmpeg_format.video_codec = Some (`Raw None) } in
          parse_args ~format:`Raw ~mode:`Video f l
      | `Video l ->
          let f =
            { f with Ffmpeg_format.video_codec = Some (`Internal None) }
          in
          parse_args ~format:`Internal ~mode:`Video f l
      | `Option ("format", { term = Var "none"; _ }) ->
          { f with Ffmpeg_format.format = None }
      | `Option ("format", { term = Ground (String fmt); _ }) ->
          { f with Ffmpeg_format.format = Some fmt }
      | `Option (k, { term = Ground (String s); _ }) ->
          Hashtbl.add f.Ffmpeg_format.other_opts k (`String s);
          f
      | `Option (k, { term = Ground (Int i); _ }) ->
          Hashtbl.add f.Ffmpeg_format.other_opts k (`Int i);
          f
      | `Option (k, { term = Ground (Float i); _ }) ->
          Hashtbl.add f.Ffmpeg_format.other_opts k (`Float i);
          f | `Option (_, t) -> raise (generic_error t))
    defaults params

let make params = Encoder.Ffmpeg (ffmpeg_gen params)
