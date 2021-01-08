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
  let rec parse_args ~format ~mode f = function
    | [] -> f
    (* Audio options *)
    | ("channels", { term = Ground (Int c); _ }) :: l when mode = `Audio ->
        parse_args ~format ~mode { f with Ffmpeg_format.channels = c } l
    | ("ac", { term = Ground (Int c); _ }) :: l when mode = `Audio ->
        parse_args ~format ~mode { f with Ffmpeg_format.channels = c } l
    | ("samplerate", { term = Ground (Int s); _ }) :: l when mode = `Audio ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.samplerate = Lazy.from_val s }
          l
    | ("ar", { term = Ground (Int s); _ }) :: l when mode = `Audio ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.samplerate = Lazy.from_val s }
          l
    (* Video options *)
    | ("framerate", { term = Ground (Int r); _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.framerate = Lazy.from_val r }
          l
    | ("r", { term = Ground (Int r); _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.framerate = Lazy.from_val r }
          l
    | ("width", { term = Ground (Int w); _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.width = Lazy.from_val w }
          l
    | ("height", { term = Ground (Int h); _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.height = Lazy.from_val h }
          l
    | ("pixel_format", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.pixel_format = None } l
    | ("pixel_format", { term = Ground (String p); _ }) :: l when mode = `Video
      ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.pixel_format = Some p }
          l
    | ("hwaccel", { term = Var "auto"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.hwaccel = `Auto } l
    | ("hwaccel", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode { f with Ffmpeg_format.hwaccel = `None } l
    | ("hwaccel_device", { term = Var "none"; _ }) :: l when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.hwaccel_device = None }
          l
    | ("hwaccel_device", { term = Ground (String d); _ }) :: l
      when mode = `Video ->
        parse_args ~format ~mode
          { f with Ffmpeg_format.hwaccel_device = Some d }
          l
    (* Shared options *)
    | ("codec", { term = Ground (String c); _ }) :: l ->
        let f =
          match (mode, format) with
            | `Audio, `Internal ->
                { f with Ffmpeg_format.audio_codec = Some (`Internal (Some c)) }
            | `Audio, `Raw ->
                { f with Ffmpeg_format.audio_codec = Some (`Raw (Some c)) }
            | `Video, `Internal ->
                { f with Ffmpeg_format.video_codec = Some (`Internal (Some c)) }
            | `Video, `Raw ->
                { f with Ffmpeg_format.video_codec = Some (`Raw (Some c)) }
        in
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
      | `Option ("format", { term = Ground (String s); _ })
      | `Option ("format", { term = Var s; _ })
        when s = "none" ->
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
