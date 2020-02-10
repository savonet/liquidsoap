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
      audio_codec = None;
      video_codec = None;
      audio_opts = Hashtbl.create 0;
      video_opts = Hashtbl.create 0;
      other_opts = Hashtbl.create 0;
    }
  in
  let rec parse_args opts = function
    | [] -> ()
    | (k, { term = Ground (String s); _ }) :: l ->
        Hashtbl.add opts k (`String s);
        parse_args opts l
    | (k, { term = Ground (Int i); _ }) :: l ->
        Hashtbl.add opts k (`Int i);
        parse_args opts l
    | (k, { term = Ground (Float i); _ }) :: l ->
        Hashtbl.add opts k (`Float i);
        parse_args opts l
    | (_, t) :: _ -> raise (generic_error t)
  in
  List.fold_left
    (fun f -> function
      | `Option ("format", { term = Var s; _ }) when s = "none" ->
          { f with Ffmpeg_format.format = None }
      | `Option ("format", { term = Ground (String fmt); _ }) ->
          { f with Ffmpeg_format.format = Some fmt }
      | `Option ("channels", { term = Ground (Int c); _ }) ->
          { f with Ffmpeg_format.channels = c }
      | `Option ("samplerate", { term = Ground (Int s); _ }) ->
          { f with Ffmpeg_format.samplerate = Lazy.from_val s }
      | `Option ("audio_codec", { term = Var s; _ }) when s = "none" ->
          { f with Ffmpeg_format.audio_codec = None; channels = 0 }
      | `Option ("audio_codec", { term = Ground (String c); _ }) ->
          { f with Ffmpeg_format.audio_codec = Some c }
      | `Option ("video_codec", { term = Ground (String c); _ }) ->
          { f with Ffmpeg_format.video_codec = Some c }
      | `Audio l ->
          parse_args f.Ffmpeg_format.audio_opts l;
          f
      | `Video l ->
          parse_args f.Ffmpeg_format.video_opts l;
          f
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
