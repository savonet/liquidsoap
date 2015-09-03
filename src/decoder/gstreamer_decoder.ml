(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Decode files and streams using GStreamer. *)

open Stdlib

module GU = Gstreamer_utils
module Img = Image.RGBA32
let log = Dtools.Log.make ["decoder";"gstreamer"]

type gst =
  {
    bin : Gstreamer.Element.t;
    src : Gstreamer.App_src.t;
    src_mutex : Mutex.t; (** Mutex for feeding data. *)
    audio_sink : Gstreamer.App_sink.t option;
    video_sink : Gstreamer.App_sink.t option;
  }

(** Generic decoder. *)
(* TODO: we should share some code with Ogg_decoder... *)
module Make (Generator : Generator.S_Asio) = struct
  let create_decoder ?(merge_tracks=false) _ ~channels mode input =
    GU.init ();

    let decode_audio = mode = `Both || mode = `Audio in
    let decode_video = mode = `Both || mode = `Video in

    log#f 4 "Using %s." (Gstreamer.version_string ());
    log#f 5 "Decode A/V: %B/%B." decode_audio decode_video;

    let gst_max_buffers = GU.max_buffers () in

    let gst =
      let audio_pipeline =
        if decode_audio then
          Printf.sprintf " t. ! queue ! %s ! %s"
            (GU.Pipeline.decode_audio ())
            (GU.Pipeline.audio_sink ~channels "audio_sink")
        else
          ""
      in
      let video_pipeline =
        if decode_video then
          Printf.sprintf
            " t. ! queue ! %s ! %s"
            (GU.Pipeline.decode_video ())
            (GU.Pipeline.video_sink "video_sink")
        else
          ""
      in
      let pipeline =
        Printf.sprintf "appsrc name=src ! tee name=t%s%s"
          audio_pipeline video_pipeline
      in
      log#f 5 "Gstreamer pipeline: %s." pipeline;
      let bin = Gstreamer.Pipeline.parse_launch pipeline in
      let src = Gstreamer.App_src.of_element (Gstreamer.Bin.get_by_name bin "src") in
      let audio_sink =
        if decode_audio then
          let sink = Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "audio_sink") in
          Gstreamer.App_sink.set_max_buffers sink gst_max_buffers;
          Some sink
        else
          None
      in
      let video_sink =
        if decode_video then
          let sink = Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "video_sink") in
          Gstreamer.App_sink.set_max_buffers sink gst_max_buffers;
          Some sink
        else
          None
      in
      let src_mutex = Mutex.create () in
      { bin; src; src_mutex; audio_sink; video_sink }
    in

    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let started = ref false in

    let init ~reset buffer =
      if reset then
        (
          (* We enforce that all contents end together, otherwise there will
           * be a lag between different content types in the next track. *)
          if not merge_tracks then Generator.add_break ~sync:`Drop buffer
        );
      Generator.set_mode buffer mode;
      ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_playing)
    in

    let feed_data buflen =
      Mutex.lock gst.src_mutex;
      let buf,buflen = input.Decoder.read buflen in
      let buf = String.sub buf 0 buflen in
      if buflen = 0 then
        (
          log#f 5 "End of stream.";
          Gstreamer.App_src.end_of_stream gst.src
        )
      else
        Gstreamer.App_src.push_buffer_string gst.src buf;
      Mutex.unlock gst.src_mutex
    in
    Gstreamer.App_src.on_need_data gst.src feed_data;

    let decode buffer =
      if not !started then
        (
          init ~reset:false buffer;
          started := true
        );
      let decode_audio, decode_video =
        if decode_audio && decode_video then
          if Generator.audio_length buffer < Generator.video_length buffer then
            true, false
          else
            false, true
        else
          decode_audio, decode_video
      in
      if decode_audio then
        (
          let _, state, _ = Gstreamer.Element.get_state gst.bin in
          if state <> Gstreamer.Element.State_playing then
            failwith "Not in playing state!";
          let b = Gstreamer.App_sink.pull_buffer_string (Utils.get_some gst.audio_sink) in
          let len = String.length b / (2*channels) in
          let buf = Audio.create channels len in
          Audio.S16LE.to_audio b 0 buf 0 len;
          Generator.put_audio buffer buf 0 len
        );
      if decode_video then
        (
          let _, state, _ = Gstreamer.Element.get_state gst.bin in
          if state <> Gstreamer.Element.State_playing then
            failwith "Not in playing state!";
          let b = Gstreamer.App_sink.pull_buffer_data (Utils.get_some gst.video_sink) in
          let img = Img.make width  height b in
          let stream = [|img|] in
          Generator.put_video buffer [|stream|] 0 (Array.length stream)
        )
    in

    let seek off =
      try
        (* TODO: fix seek at some point... *)
        ignore (raise Gstreamer.Failure);
        let off =
          Int64.of_float (Frame.seconds_of_master off *. 1000000000.)
        in
        let pos =
          let pos = Gstreamer.Element.position gst.bin Gstreamer.Format.Time in
          Gstreamer.Element.seek_simple
            gst.bin
            Gstreamer.Format.Time
            [Gstreamer.Event.Seek_flag_flush;
             Gstreamer.Event.Seek_flag_key_unit;
             Gstreamer.Event.Seek_flag_skip]
            (Int64.add pos off);
          Gstreamer.Element.position gst.bin Gstreamer.Format.Time
        in
        Frame.master_of_seconds (Int64.to_float pos /. 1000000000.)
      with
      | Gstreamer.Failure ->
        log#f 3 "Seek failed!";
        0
    in

    { Decoder.
      decode = decode;
      seek = seek;
    }
end

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered(G)
module D = Make(G)

let mime_types =
  Dtools.Conf.list ~p:(Decoder.conf_mime_types#plug "gstreamer")
    "Mime-types used for guessing format handled by GStreamer"
    ~d:["video/x-ms-asf";"video/x-msvideo";
        "video/mp4";"video/3gpp";"video/webm";
        "video/x-matroska";"video/mp2t";"video/MP2T"]
let file_extensions =
  Dtools.Conf.list ~p:(Decoder.conf_file_extensions#plug "gstreamer")
    "File extensions used for guessing format handled by GStreamer"
    ~d:["wmv";"avi";"mp4";"3gp";"webm";"mkv"]

let create_file_decoder filename content_type kind =
  let mode =
    match content_type.Frame.video, content_type.Frame.audio with
    | 0, _ -> `Audio
    | _, 0 -> `Video
    | _, _ -> `Both
  in
  let channels = content_type.Frame.audio in
  let generator = G.create mode in
  Buffered.file_decoder
    filename kind
    (D.create_decoder ~channels ~merge_tracks:true `File mode)
    generator

(** Get the type of a file's content. For now it is a bit imprecise:
  * we always pretend that audio content has the expected number of
  * channels, which is passed as a parameter to get_type. *)
let get_type ~channels filename =
  GU.init ();
  let filesrc = Printf.sprintf "filesrc location=\"%s\"" filename in
  let audio =
    let pipeline =
      Printf.sprintf
        "%s ! %s ! fakesink"
        filesrc (GU.Pipeline.decode_audio ())
    in
    let bin = Gstreamer.Pipeline.parse_launch pipeline in
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
    let _, state, _ = Gstreamer.Element.get_state bin in
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
    if state = Gstreamer.Element.State_paused then
      (
        log#f 5 "File %s has audio." filename;
        channels
      )
    else
      0
  in
  let video =
    try
      let pipeline =
        Printf.sprintf "%s ! %s ! fakesink" filesrc (GU.Pipeline.decode_video ())
      in
      let bin = Gstreamer.Pipeline.parse_launch pipeline in
      ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
      let _, state, _ = Gstreamer.Element.get_state bin in
      ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
      if state = Gstreamer.Element.State_paused then
        (
          log#f 5 "File %s has video." filename;
          1
        )
      else
        0
    with
    | Gstreamer.Failure -> 0
  in
  { Frame. video; audio; midi = 0 }

let () =
  Decoder.file_decoders#register "GSTREAMER"
    ~sdoc:"Decode a file using GStreamer."
    (fun ~metadata:_ filename kind ->
      if not (Decoder.test_file
                ~mimes:mime_types#get
                ~extensions:file_extensions#get
                ~log filename)
      then
        None
      else
        let channels =
          (* Get the default expected number of audio channels *)
          (Frame.type_of_kind kind).Frame.audio
        in
        let content_type = get_type ~channels filename in
        let content_type =
          (* If the kind doesn't allow audio, or video, pretend that we don't
           * have any: it will be dropped anyway. A more fine-grained approach
           * might or might not be possible, based on the number of channels. *)
          if kind.Frame.video = Frame.Zero then
            { content_type with Frame.video = 0 }
          else if kind.Frame.audio = Frame.Zero then
            { content_type with Frame.audio = 0 }
          else
            content_type
        in
        if Frame.type_has_kind content_type kind then
          Some (fun () -> create_file_decoder filename content_type kind)
        else
          None
    )

(** Stream decoder *)

module D_stream = Make(Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register
    "GSTREAMER"
    ~sdoc:"Decode any stream with an appropriate MIME type using GStreamer."
    (fun mime kind ->
      if List.mem mime mime_types#get then
        let content_type = Frame.type_of_kind kind in
        let channels = content_type.Frame.audio in
        let mode =
          match content_type.Frame.video, content_type.Frame.audio with
          | 0, _ -> `Audio
          | _, 0 -> `Video
          | _, _ -> `Both
        in
        Some (D_stream.create_decoder ~channels `Stream mode)
      else
        None)

(** Metadata *)

(* See
   http://gstreamer.freedesktop.org/data/doc/gstreamer/head/manual/html/chapter-metadata.html *)
let get_tags file =
  if not
    (Decoder.test_file ~mimes:mime_types#get
       ~extensions:file_extensions#get
       ~log file)
  then raise Not_found;
  GU.init ();
  let pipeline =
    Printf.sprintf "filesrc location=\"%s\" ! decodebin ! fakesink" file
  in
  let bin = Gstreamer.Pipeline.parse_launch pipeline in
  (* Go in paused state. *)
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_paused);
  (* Wait for the state to complete. *)
  ignore (Gstreamer.Element.get_state bin);
  let ans = ref [] in
  try
    while true do
      let msg =
        Gstreamer.Bus.pop_filtered
          (Gstreamer.Bus.of_element bin)
          [Gstreamer.Message.Error; Gstreamer.Message.Tag]
      in
      let msg = match msg with Some msg -> msg | None -> raise Exit in
      let typ = Gstreamer.Message.message_type msg in
      if typ <> Gstreamer.Message.Tag then raise Exit;
      let tags = Gstreamer.Message.parse_tag msg in
      List.iter
        (fun (l,v) ->
          match v with
          | [v] -> ans := (l,v) :: !ans
          | _ -> ()
        ) tags
    done;
    assert false
  with
  | Exit ->
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
    List.rev !ans

let () = Request.mresolvers#register "GSTREAMER" get_tags
