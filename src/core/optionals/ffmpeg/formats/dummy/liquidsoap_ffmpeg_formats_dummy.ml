(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** What a process without libav knows of an ffmpeg stream's parameters: the
    names a script wrote, and no more. Two streams are compatible when nothing
    they both name differs. *)
module Params = struct
  let implementation = "the stand-in for libav"

  type t = (string * string) list

  let default = []
  let parse label value = Some [(label, value)]

  let to_string params =
    String.concat ","
      (List.map (fun (label, value) -> label ^ "=" ^ value) params)

  let compatible p p' =
    List.for_all
      (fun (label, value) ->
        match List.assoc_opt label p' with
          | Some value' -> value = value'
          | None -> true)
      p

  let merge p p' =
    if not (compatible p p') then failwith "Incompatible format!";
    p @ List.filter (fun (label, _) -> not (List.mem_assoc label p)) p'
end

module Copy =
  Ffmpeg_content_type.Make
    (Params)
    (struct
      type kind = [ `Copy ]

      let kind = `Copy
      let name = "ffmpeg.copy"
      let kind_name = "ffmpeg.copy"
    end)

module Raw_audio =
  Ffmpeg_content_type.Make
    (Params)
    (struct
      type kind = [ `Raw ]

      let kind = `Raw
      let name = "ffmpeg.raw.audio"
      let kind_name = "ffmpeg.audio.raw"
    end)

module Raw_video =
  Ffmpeg_content_type.Make
    (Params)
    (struct
      type kind = [ `Raw ]

      let kind = `Raw
      let name = "ffmpeg.raw.video"
      let kind_name = "ffmpeg.video.raw"
    end)
