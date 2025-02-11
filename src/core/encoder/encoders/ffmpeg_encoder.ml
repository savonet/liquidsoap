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

let () =
  Plug.register Encoder.plug "ffmpeg" ~doc:"" (function
    | Encoder.Ffmpeg ffmpeg ->
        Some
          (fun ?(hls = false) ~pos _ ->
            (* Inject hls params. *)
            let ffmpeg =
              if hls then (
                let opts =
                  match ffmpeg.Ffmpeg_format.format with
                    | Some "mp4" ->
                        let opts = Hashtbl.copy ffmpeg.Ffmpeg_format.opts in
                        let movflags =
                          Option.value
                            ~default:
                              (`String
                                 "+dash+skip_sidx+skip_trailer+frag_custom")
                            (Hashtbl.find_opt ffmpeg.Ffmpeg_format.opts
                               "movflags")
                        in
                        Hashtbl.replace opts "movflags" movflags;
                        let frag_duration =
                          Option.value ~default:(`Int 10)
                            (Hashtbl.find_opt ffmpeg.Ffmpeg_format.opts
                               "frag_duration")
                        in
                        Hashtbl.replace opts "frag_duration" frag_duration;
                        opts
                    | _ -> ffmpeg.Ffmpeg_format.opts
                in
                let streams =
                  List.map
                    (function
                      | ( lbl,
                          `Encode
                            ({ Ffmpeg_format.opts } as stream :
                              Ffmpeg_format.encoded_stream) ) ->
                          let opts = Hashtbl.copy opts in
                          let flags =
                            Option.value ~default:(`String "+global_header")
                              (Hashtbl.find_opt opts "flags")
                          in
                          Hashtbl.replace opts "flags" flags;
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
            let keyframes =
              List.map
                (fun (field, _) -> (field, Atomic.make false))
                ffmpeg.streams
            in
            let on_keyframe = Atomic.make (fun () -> ()) in
            let on_stream_keyframe field =
              if hls then
                Some
                  (fun () ->
                    let on_keyframe = Atomic.get on_keyframe in
                    on_keyframe ();
                    Atomic.set (List.assoc field keyframes) true)
              else None
            in
            let mk_streams output =
              List.fold_left
                (fun streams (field, stream) ->
                  match stream with
                    | `Drop -> streams
                    | `Copy keyframe_opt ->
                        Frame.Fields.add field
                          (Ffmpeg_copy_encoder.mk_stream_copy
                             ~on_keyframe:(on_stream_keyframe field) ~get_stream
                             ~remove_stream ~keyframe_opt ~field output)
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
                             ~on_keyframe:(on_stream_keyframe field) ~mode
                             ~params ~options ~codec ~field output)
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
                             ~on_keyframe:(on_stream_keyframe field) ~mode
                             ~params ~options ~codec ~field output)
                          streams)
                Frame.Fields.empty ffmpeg.streams
            in
            encoder ~pos ~on_keyframe ~keyframes ~mk_streams ffmpeg)
    | _ -> None)
