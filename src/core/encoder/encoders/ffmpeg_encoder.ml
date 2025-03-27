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

(** FFMPEG encoder *)

open Ffmpeg_encoder_common

let replace_default opts name default =
  Hashtbl.replace opts name (Option.value ~default (Hashtbl.find_opt opts name))

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"" (function
    | Encoder.Ffmpeg ffmpeg ->
        Some
          (fun ?(hls = false) ~pos _ ->
            (* Inject hls params. *)
            let ffmpeg =
              if hls then (
                let opts = Hashtbl.copy ffmpeg.Ffmpeg_format.opts in
                replace_default opts "flush_packets" (`Int 1);
                (match ffmpeg.Ffmpeg_format.format with
                  | Some "mp4" ->
                      replace_default opts "movflags"
                        (`String "+dash+skip_sidx+skip_trailer+frag_custom");
                      replace_default opts "frag_duration" (`Int 10)
                  | _ -> ());
                let streams =
                  List.map
                    (function
                      | ( lbl,
                          `Encode
                            ({ Ffmpeg_format.opts } as stream :
                              Ffmpeg_format.encoded_stream) ) ->
                          let opts = Hashtbl.copy opts in
                          replace_default opts "flags"
                            (`String "+global_header");
                          (lbl, `Encode { stream with Ffmpeg_format.opts })
                      | s -> s)
                    ffmpeg.Ffmpeg_format.streams
                in
                { ffmpeg with Ffmpeg_format.streams; opts })
              else ffmpeg
            in
            let copy_count =
              List.fold_left
                (fun cur (_, c) -> match c with `Copy _ -> cur + 1 | _ -> cur)
                0 ffmpeg.streams
            in
            let get_stream, remove_stream = mk_stream_store copy_count in
            let mk_streams output =
              List.fold_left
                (fun streams (field, stream) ->
                  match stream with
                    | `Drop -> streams
                    | `Copy keyframe_opt ->
                        Frame.Fields.add field
                          (Ffmpeg_copy_encoder.mk_stream_copy ~on_keyframe:None
                             ~get_stream ~remove_stream ~keyframe_opt ~field
                             output)
                          streams
                    | `Encode Ffmpeg_format.{ codec = None } ->
                        Lang_encoder.raise_error ~pos
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
                        Frame.Fields.add field
                          (Ffmpeg_internal_encoder.mk_audio ~pos
                             ~on_keyframe:None ~mode ~params ~options ~codec
                             ~field output)
                          streams
                    | `Encode
                        Ffmpeg_format.
                          {
                            mode;
                            codec = Some codec;
                            options = `Video params;
                            opts = options;
                          } ->
                        Frame.Fields.add field
                          (Ffmpeg_internal_encoder.mk_video ~pos
                             ~on_keyframe:None ~mode ~params ~options ~codec
                             ~field output)
                          streams)
                Frame.Fields.empty ffmpeg.streams
            in
            encoder ~pos ~mk_streams ffmpeg)
    | _ -> None)
