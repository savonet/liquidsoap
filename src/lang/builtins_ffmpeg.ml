(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

let () =
  Lang.add_module "ffmpeg";
  Lang.add_module "ffmpeg.encode"

let () =
  let source_t =
    Lang.kind_type_of_kind_format
      Frame.{ audio = audio_pcm; video = video_yuv420p; midi = none }
  in
  let return_t =
    Lang.kind_type_of_kind_format
      Frame.
        {
          audio = `Kind Ffmpeg_copy_content.Audio.kind;
          video = `Kind Ffmpeg_copy_content.Video.kind;
          midi = none;
        }
  in
  let proto =
    [
      ("", Lang.format_t source_t, None, Some "Encoding format.");
      ("", Lang.source_t source_t, None, None);
    ]
  in
  Lang.add_operator "ffmpeg.encode.audio_video" proto ~return_t
    ~category:Lang.Conversions ~descr:"Encode a source's content" (fun p ->
      assert false)
