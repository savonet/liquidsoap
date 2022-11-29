(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

(** FFMPEG encoder *)

open Ffmpeg_encoder_common

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"" (function
    | Encoder.Ffmpeg m ->
        Some
          (fun _ ->
            let copy_count =
              Frame.Fields.fold
                (fun _ c cur -> match c with `Copy _ -> cur + 1 | _ -> cur)
                m.streams 0
            in
            let get_stream, remove_stream = mk_stream_store copy_count in
            let mk_streams output =
              Frame.Fields.mapi
                (fun field -> function
                  | `Copy keyframe_opt ->
                      Ffmpeg_copy_encoder.mk_stream_copy ~get_stream
                        ~remove_stream ~keyframe_opt ~field output
                  | `Encode Ffmpeg_format.{ codec = None } ->
                      Lang_encoder.raise_error ~pos:None
                        (Printf.sprintf
                           "Codec unspecified for %%ffmpeg stream %%%s!"
                           (Frame.Fields.string_of_field field))
                  | `Encode
                      Ffmpeg_format.
                        {
                          mode;
                          codec = Some codec;
                          options = `Audio params;
                          opts = options;
                        } ->
                      Ffmpeg_internal_encoder.mk_audio ~mode ~params ~options
                        ~codec ~field output
                  | `Encode
                      Ffmpeg_format.
                        {
                          mode;
                          codec = Some codec;
                          options = `Video params;
                          opts = options;
                        } ->
                      Ffmpeg_internal_encoder.mk_video ~mode ~params ~options
                        ~codec ~field output)
                m.streams
            in
            encoder ~mk_streams m)
    | _ -> None)
