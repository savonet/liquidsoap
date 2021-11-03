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

open Mm
open Extralib

(* Video-only input. Ideally this should be merged with previous one. *)

exception Finished of string * bool

class video ~name ~kind ~restart ~bufferize ~log_overfull ~restart_on_error ~max
  ~on_data ?read_header command =
  let abg_max_len = Frame.main_of_seconds max in
  (* We need a temporary log until the source has an id *)
  let log_ref = ref (fun _ -> ()) in
  let log_error = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let abg = Generator.create ~log ~log_overfull `Both in
  (* Maximal difference between audio and video in seconds before a warning. *)
  let vadiff = 10. in
  let last_vadiff_warning = ref 0. in
  let on_data reader =
    on_data abg reader;

    (* Check that audio and video roughly get filled as the same speed. *)
    let lv = Frame.seconds_of_main (Generator.video_length abg) in
    let la = Frame.seconds_of_main (Generator.audio_length abg) in
    let d = abs_float (lv -. la) in
    if d > vadiff && d -. !last_vadiff_warning >= vadiff then (
      last_vadiff_warning := d;
      let v, a = if lv >= la then ("video", "audio") else ("audio", "video") in
      !log_error
        (Printf.sprintf
           "Got %f seconds more of %s than of %s. Are you sure that you are \
            producing the correct kind of data?"
           d v a));
    let buffered = Generator.length abg in
    if abg_max_len < buffered then
      `Delay (Frame.seconds_of_audio (buffered - (3 * abg_max_len / 4)))
    else `Continue
  in
  object (self)
    inherit
      External_input.base
        ~name ~kind ?read_header ~restart ~restart_on_error ~on_data command as base

    inherit Generated.source abg ~empty_on_abort:false ~bufferize

    method buffer_length_cmd =
      ( Frame.seconds_of_main (Generator.audio_length abg),
        Frame.seconds_of_main (Generator.video_length abg),
        Frame.seconds_of_main (Generator.length abg) )

    method wake_up x =
      (* Now we can create the log function *)
      log_ref := self#log#info "%s";
      log_error := self#log#severe "%s";
      if self#ctype.Frame.audio = Content.None.format then
        Generator.set_mode abg `Video;
      self#log#debug "Generator mode: %s."
        (match Generator.mode abg with
          | `Video -> "video"
          | `Both -> "both"
          | _ -> "???");
      base#wake_up x
  end

(***** AVI *****)

let log = Log.make ["input"; "external"; "video"]

let () =
  let kind = Frame.{ audio = audio_pcm; video = video_yuva420p; midi = none } in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "input.external.avi" ~category:`Input ~flags:[`Experimental]
    ~meth:
      [
        ( "buffer_length",
          ( [],
            Lang.fun_t []
              (Lang.tuple_t [Lang.float_t; Lang.float_t; Lang.float_t]) ),
          "Length of the buffer (in seconds).",
          fun s ->
            Lang.val_fun [] (fun _ ->
                let x, y, z = s#buffer_length_cmd in
                Lang.tuple [Lang.float x; Lang.float y; Lang.float z]) );
      ]
    ~descr:"Stream data from an external application."
    [
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Duration of the pre-buffered data." );
      ( "max",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." );
      ( "log_overfull",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Log when the source's buffer is overfull." );
      ( "restart",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Restart process when exited." );
      ( "restart_on_error",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Restart process when exited with error." );
      ("", Lang.string_t, None, Some "Command to execute.");
    ]
    ~return_t
    (fun p ->
      let command = Lang.to_string (List.assoc "" p) in
      let video_format = ref None in
      let width = ref None in
      let height = ref None in
      let audio_converter = ref None in
      let video_converter = ref None in
      let video_scaler = Video_converter.scaler () in
      let read_header read =
        (* Reset the state. *)
        video_format := None;
        width := None;
        height := None;
        audio_converter := None;
        video_converter := None;
        let h, _ = Avi.Read.headers_simple read in
        let check = function
          | `Video (fmt, w, h, fps) ->
              (* if w <> width then failwith (Printf.sprintf "Wrong video width (%d instead of %d)." w width); *)
              (* if h <> height then failwith (Printf.sprintf "Wrong video height (%d instead of %d)." h height); *)
              log#info "Format: %s."
                (match fmt with `RGB24 -> "RGB24" | `I420 -> "I420");
              video_format := Some fmt;
              width := Some w;
              height := Some h;
              if fps <> float (Lazy.force Frame.video_rate) then
                failwith
                  (Printf.sprintf
                     "Wrong video rate (%f instead of %d). Support for \
                      timestretching should be added some day in the future."
                     fps
                     (Lazy.force Frame.video_rate));
              let converter data =
                let video_format = Option.get !video_format in
                let of_string s =
                  match video_format with
                    | `RGB24 -> Image.YUV420.of_RGB24_string s w
                    | `I420 ->
                        (* TODO: can there be stride in avi videos? *)
                        let h = String.length s * 4 / 6 / w in
                        Image.YUV420.of_YUV420_string s w h
                in
                let src = of_string data in
                let in_width = Video.Image.width src in
                let in_height = Video.Image.height src in
                let out_width = Lazy.force Frame.video_width in
                let out_height = Lazy.force Frame.video_height in
                if
                  out_width = in_width && out_height = in_height
                  && video_format = `I420
                then src
                else (
                  let dst = Video.Image.create out_width out_height in
                  video_scaler src dst;
                  dst)
              in
              video_converter := Some converter
          | `Audio (channels, samplerate) ->
              if !audio_converter <> None then
                failwith "Only one audio track is supported for now.";
              let resampler = Decoder_utils.samplerate_converter () in
              let converter =
                Decoder_utils.from_iff ~format:`Wav ~channels ~samplesize:16
              in
              audio_converter :=
                Some (fun data -> resampler ~samplerate (converter data))
        in
        List.iter check h;
        `Continue
      in
      let on_data abg reader =
        match Avi.Read.chunk reader with
          | `Frame (_, _, data) when String.length data = 0 -> ()
          | `Frame (`Video, _, data) ->
              let width = Option.get !width in
              let height = Option.get !height in
              let video_format = Option.get !video_format in
              if
                video_format = `RGB24
                && String.length data <> width * height * 3
                || video_format = `I420
                   && String.length data <> width * height * 6 / 4
              then
                failwith
                  (Printf.sprintf "Wrong video frame size (%d instead of %d)"
                     (String.length data)
                     (width * height * 3));
              let data = (Option.get !video_converter) data in
              Generator.put_video abg
                (Content.Video.lift_data (Video.single data))
                0 (Frame.main_of_video 1)
          | `Frame (`Audio, _, data) ->
              let converter = Option.get !audio_converter in
              let data = converter data in
              Generator.put_audio abg
                (Content.Audio.lift_data data)
                0
                (Frame.main_of_audio (Audio.length data))
          | _ -> failwith "Invalid chunk."
      in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      let kind = Kind.of_kind kind in
      new video
        ~name:"input.external.avi" ~kind ~restart ~bufferize ~log_overfull
        ~restart_on_error ~max ~read_header ~on_data command)

(***** raw video *****)

let () =
  let kind = Lang.video_yuva420p in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "input.external.rawvideo" ~category:`Input
    ~flags:[`Experimental]
    ~meth:
      [
        ( "buffer_length",
          ( [],
            Lang.fun_t []
              (Lang.tuple_t [Lang.float_t; Lang.float_t; Lang.float_t]) ),
          "Length of the buffer (in seconds).",
          fun s ->
            Lang.val_fun [] (fun _ ->
                let x, y, z = s#buffer_length_cmd in
                Lang.tuple [Lang.float x; Lang.float y; Lang.float z]) );
      ]
    ~descr:"Stream data from an external application."
    [
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Duration of the pre-buffered data." );
      ( "max",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." );
      ( "log_overfull",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Log when the source's buffer is overfull." );
      ( "restart",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Restart process when exited." );
      ( "restart_on_error",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Restart process when exited with error." );
      ("", Lang.string_t, None, Some "Command to execute.");
    ]
    ~return_t
    (fun p ->
      let command = Lang.to_string (List.assoc "" p) in
      let width = Lazy.force Frame.video_width in
      let height = Lazy.force Frame.video_height in
      let buflen = width * height * 3 in
      let buf = Bytes.create buflen in
      let on_data abg reader =
        let ret = reader buf 0 buflen in
        let data =
          Image.YUV420.of_YUV420_string
            (Bytes.sub_string buf 0 ret)
            width height
        in
        (* Img.swap_rb data; *)
        (* Img.Effect.flip data; *)
        Generator.put_video abg
          (Content.Video.lift_data (Video.single data))
          0 (Frame.main_of_video 1)
      in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      let kind = Kind.of_kind kind in
      new video
        ~name:"input.external.rawvideo" ~kind ~restart ~bufferize ~log_overfull
        ~restart_on_error ~max ~on_data command)
