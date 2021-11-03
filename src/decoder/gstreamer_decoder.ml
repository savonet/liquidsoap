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

(** Decode files and streams using GStreamer. *)

open Extralib
module GU = Gstreamer_utils

let log = Log.make ["decoder"; "gstreamer"]

type gst = {
  bin : Gstreamer.Element.t;
  audio_sink : Gstreamer.App_sink.t option;
  video_sink : Gstreamer.App_sink.t option;
}

(** Generic decoder. *)
let create_decoder ?(merge_tracks = false) _ ~channels ~mode fname =
  let decode_audio, decode_video =
    match mode with
      | `Both -> (true, true)
      | `Audio -> (true, false)
      | `Video -> (false, true)
      | _ -> assert false
  in
  log#info "Using %s." (Gstreamer.version_string ());
  log#debug "Decode A/V: %B/%B." decode_audio decode_video;
  let gst_max_buffers = GU.max_buffers () in
  let gst =
    let audio_pipeline =
      if decode_audio then
        Printf.sprintf " d. ! queue ! %s ! %s"
          (GU.Pipeline.convert_audio ())
          (GU.Pipeline.audio_sink ~channels "audio_sink")
      else ""
    in
    let video_pipeline =
      if decode_video then
        Printf.sprintf " d. ! queue ! %s ! %s"
          (GU.Pipeline.convert_video ())
          (GU.Pipeline.video_sink "video_sink")
      else ""
    in
    let pipeline =
      Printf.sprintf "filesrc location=%s ! decodebin name=d%s%s"
        (Utils.quote_string fname) audio_pipeline video_pipeline
    in
    log#debug "GStreamer pipeline: %s." pipeline;
    let bin = Gstreamer.Pipeline.parse_launch pipeline in
    let audio_sink =
      if decode_audio then (
        let sink =
          Gstreamer.App_sink.of_element
            (Gstreamer.Bin.get_by_name bin "audio_sink")
        in
        Gstreamer.App_sink.set_max_buffers sink gst_max_buffers;
        Some sink)
      else None
    in
    let video_sink =
      if decode_video then (
        let sink =
          Gstreamer.App_sink.of_element
            (Gstreamer.Bin.get_by_name bin "video_sink")
        in
        Gstreamer.App_sink.set_max_buffers sink gst_max_buffers;
        Some sink)
      else None
    in
    { bin; audio_sink; video_sink }
  in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let started = ref false in
  let init ~reset buffer =
    if reset then
      (* We enforce that all contents end together, otherwise there will
       * be a lag between different content types in the next track. *)
      if not merge_tracks then
        Generator.add_break ~sync:true buffer.Decoder.generator;
    ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_playing)
  in
  let decode buffer =
    if not !started then (
      init ~reset:false buffer;
      started := true);
    let decode_audio, decode_video =
      if decode_audio && decode_video then (
        let gen = buffer.Decoder.generator in
        if Generator.audio_length gen < Generator.video_length gen then
          (true, false)
        else (false, true))
      else (decode_audio, decode_video)
    in
    if decode_audio then (
      let _, state, _ = Gstreamer.Element.get_state gst.bin in
      if state <> Gstreamer.Element.State_playing then
        failwith "Not in playing state!";
      let b =
        Gstreamer.App_sink.pull_buffer_string (Option.get gst.audio_sink)
      in
      let len = String.length b / (2 * channels) in
      let buf = Audio.create channels len in
      Audio.S16LE.to_audio b 0 buf;
      let samplerate = Lazy.force Frame.audio_rate in
      buffer.Decoder.put_pcm ~samplerate buf);
    if decode_video then (
      let _, state, _ = Gstreamer.Element.get_state gst.bin in
      if state <> Gstreamer.Element.State_playing then
        failwith "Not in playing state!";
      let buf = Gstreamer.App_sink.pull_buffer (Option.get gst.video_sink) in
      (* let vm = Gstreamer.Buffer.get_video_meta buf in *)
      let buf = Gstreamer.Buffer.to_data buf in
      (* GStreamer's lines are strided to multiples of 4. *)
      let round4 n = ((n + 3) lsr 2) lsl 2 in
      let y_stride = round4 width in
      let uv_stride = round4 (width / 2) in
      let img = Image.YUV420.make_data width height buf y_stride uv_stride in
      let stream = Video.single img in
      let fps = { Decoder.num = Lazy.force Frame.video_rate; den = 1 } in
      buffer.Decoder.put_yuva420p ~fps stream);
    GU.flush ~log gst.bin
  in
  let seek off =
    try
      let off = Gstreamer_utils.time_of_main off in
      let pos = Gstreamer.Element.position gst.bin Gstreamer.Format.Time in
      let new_pos =
        Gstreamer.Element.seek_simple gst.bin Gstreamer.Format.Time
          [
            Gstreamer.Event.Seek_flag_flush;
            Gstreamer.Event.Seek_flag_key_unit;
            Gstreamer.Event.Seek_flag_skip;
          ]
          (Int64.add pos off);
        ignore (Gstreamer.Element.get_state gst.bin);
        Gstreamer.Element.position gst.bin Gstreamer.Format.Time
      in
      GU.flush ~log gst.bin;
      Gstreamer_utils.main_of_time (Int64.sub new_pos pos)
    with exn ->
      let bt = Printexc.get_backtrace () in
      Utils.log_exception ~log ~bt
        (Printf.sprintf "Seek failed: %s" (Printexc.to_string exn));
      0
  in
  let close () =
    ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_null);
    GU.flush ~log gst.bin
  in
  ({ Decoder.decode; seek }, close, gst.bin)

let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "gstreamer")
    "Mime-types used for guessing format handled by GStreamer"
    ~d:["application/gstreamer"]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "gstreamer")
    "File extensions used for guessing format handled by GStreamer"
    ~d:["wma"; "wmv"; "avi"; "mp4"; "3gp"; "webm"; "mkv"]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "gstreamer")
    "Priority for the GStreamer decoder" ~d:0

let channels { Frame.audio } =
  if audio = Content.None.format then 0
  else Content.Audio.channels_of_format audio

let create_file_decoder filename content_type ctype =
  let mode =
    match (content_type.Frame.video, content_type.Frame.audio) with
      | video, _ when video = Content.None.format -> `Audio
      | _, audio when audio = Content.None.format -> `Video
      | _, _ -> `Both
  in
  let channels = channels content_type in
  let decoder, close, bin =
    create_decoder ~channels ~merge_tracks:true ~mode `File filename
  in
  let remaining () =
    let pos =
      Gstreamer_utils.main_of_time
        (Gstreamer.Element.position bin Gstreamer.Format.Time)
    in
    let duration =
      Gstreamer_utils.main_of_time
        (Gstreamer.Element.duration bin Gstreamer.Format.Time)
    in
    duration - pos
  in
  Decoder.file_decoder ~filename ~close ~ctype ~remaining decoder

(** Get the type of a file's content. For now it is a bit imprecise:
  * we always pretend that audio content has the expected number of
  * channels, which is passed as a parameter to get_type. *)
let get_type ~channels filename =
  let filesrc = Printf.sprintf "filesrc location=\"%s\"" filename in
  let audio =
    let pipeline =
      Printf.sprintf "%s ! %s ! fakesink" filesrc (GU.Pipeline.decode_audio ())
    in
    try
      let bin = Gstreamer.Pipeline.parse_launch pipeline in
      Tutils.finalize
        ~k:(fun () ->
          ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
          GU.flush ~log bin)
        (fun () ->
          ignore
            (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
          GU.flush ~log bin;
          let _, state, _ = Gstreamer.Element.get_state bin in
          if state = Gstreamer.Element.State_paused then (
            log#debug "File %s has audio." filename;
            channels)
          else 0)
    with Gstreamer.Failed -> 0
  in
  let video =
    try
      let pipeline =
        Printf.sprintf "%s ! %s ! fakesink" filesrc
          (GU.Pipeline.decode_video ())
      in
      let bin = Gstreamer.Pipeline.parse_launch pipeline in
      Tutils.finalize
        ~k:(fun () ->
          ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
          GU.flush ~log bin)
        (fun () ->
          ignore
            (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
          GU.flush ~log bin;
          let _, state, _ = Gstreamer.Element.get_state bin in
          if state = Gstreamer.Element.State_paused then (
            log#debug "File %s has video." filename;
            1)
          else 0)
    with Gstreamer.Failed -> 0
  in
  let audio =
    if audio = 0 then Content.None.format
    else
      Content.(
        Audio.lift_params
          {
            Contents.channel_layout =
              lazy (Audio_converter.Channel_layout.layout_of_channels audio);
          })
  in
  let video =
    if video = 0 then Content.None.format
    else Content.(default_format Video.kind)
  in
  { Frame.video; audio; midi = Content.None.format }

let file_decoder ~metadata:_ ~ctype filename =
  let channels = channels ctype in
  let content_type = get_type ~channels filename in
  create_file_decoder filename content_type ctype

let () =
  Decoder.decoders#register "GSTREAMER"
    ~sdoc:"Decode a file or stream using GStreamer."
    {
      Decoder.media_type = `Audio_video;
      priority = (fun () -> priority#get);
      file_extensions = (fun () -> Some file_extensions#get);
      mime_types = (fun () -> Some mime_types#get);
      file_type =
        (fun ~ctype:_ filename ->
          let channels = Lazy.force Frame.audio_channels in
          Some (get_type ~channels filename));
      file_decoder = Some file_decoder;
      stream_decoder = None;
    }

(** Metadata *)

(* See
   http://gstreamer.freedesktop.org/data/doc/gstreamer/head/manual/html/chapter-metadata.html *)
let get_tags file =
  if
    not
      (Decoder.test_file ~log ~mimes:mime_types#get
         ~extensions:file_extensions#get file)
  then raise Not_found;
  let pipeline =
    Printf.sprintf "filesrc location=\"%s\" ! decodebin ! fakesink" file
  in
  let bin = Gstreamer.Pipeline.parse_launch pipeline in
  let bus = Gstreamer.Bus.of_element bin in
  (* Go in paused state. *)
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
  GU.flush ~log bin;

  (* Wait for the state to complete. *)
  ignore (Gstreamer.Element.get_state bin);
  let ans = ref [] in
  try
    while true do
      let msg = Gstreamer.Bus.pop_filtered bus [`Error; `Tag] in
      let msg = match msg with Some msg -> msg | None -> raise Exit in
      match msg.Gstreamer.Bus.payload with
        | `Error _ ->
            GU.handler ~log ~on_error:(fun _ -> ()) msg;
            raise Exit
        | `Tag tags ->
            List.iter
              (fun (l, v) ->
                match v with [v] -> ans := (l, v) :: !ans | _ -> ())
              tags
        | _ -> assert false
    done;
    assert false
  with Exit ->
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
    GU.flush ~log bin;
    List.rev !ans

let () = Request.mresolvers#register "GSTREAMER" get_tags
