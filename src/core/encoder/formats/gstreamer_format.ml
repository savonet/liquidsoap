(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

type t = {
  channels : int;
  audio : string option;
  has_video : bool;
  video : string option;
  muxer : string option;
  metadata : string;
  pipeline : string option;
  log : int;
}

let audio_channels { channels; audio } = if audio <> None then channels else 0

let to_string m =
  let pipeline l name value =
    Option.value ~default:l
      (Option.map (fun value -> Printf.sprintf "%s=%S" name value :: l) value)
  in
  Printf.sprintf "%%gstreamer(%s,metadata=%S,has_video=%b,%slog=%d)"
    (String.concat ","
       (pipeline
          (pipeline
             (pipeline
                [Printf.sprintf "channels=%d" m.channels]
                "audio" m.audio)
             "video" m.video)
          "muxer" m.muxer))
    m.metadata m.has_video
    (Option.value ~default:""
       (Option.map (Printf.sprintf "pipeline=%S,") m.pipeline))
    m.log
