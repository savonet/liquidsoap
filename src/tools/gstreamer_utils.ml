open Dtools

module Img = Image.RGBA32

let conf_gstreamer =
  Conf.void ~p:(Configure.conf#plug "gstreamer")
    "Media decoding/endcoding through gstreamer."

let conf_debug =
  Conf.int ~p:(conf_gstreamer#plug "debug_level") ~d:0
    "Debug level (bewteen 0 and 5)."

let conf_max_buffers =
  Conf.int ~p:(conf_gstreamer#plug "max_buffers") ~d:10
    "Maximal number of buffers."
let max_buffers () = conf_max_buffers#get

let conf_add_borders =
  Conf.bool ~p:(conf_gstreamer#plug "add_borders") ~d:true
    "Add borders in order to keep video aspect ratio."
let add_borders () = conf_add_borders#get

let () =
  ignore (Dtools.Init.at_start (fun () ->
    Gstreamer.init ~argv:[|
      "Liquidsoap";
      "--gst-debug-spew";
      Printf.sprintf "--gst-debug-level=%d" conf_debug#get
    |] ();
  let major, minor, micro, nano_str = Gstreamer.version () in
  let log = Dtools.Log.make ["gstreamer";"loader"] in
  log#f 3 "Loaded GStreamer %d.%d.%d %d" major minor micro nano_str))

module Pipeline = struct
  let convert_audio () =
    "audioconvert ! audioresample"

  let decode_audio () =
    Printf.sprintf "decodebin ! %s" (convert_audio ())

  let convert_video () =
    let add_borders = add_borders () in
    Printf.sprintf "videoconvert ! videoscale add-borders=%B ! videorate"
      add_borders

  let audio_format channels =
    let rate = Lazy.force Frame.audio_rate in
    Printf.sprintf "audio/x-raw,format=S16LE,layout=interleaved,channels=%d,rate=%d"
      channels rate

  let audio_src ~channels ?(block=true) ?(format=Gstreamer.Format.Time) name =
    Printf.sprintf "appsrc name=\"%s\" block=%B caps=\"%s\" format=%s"
      name block (audio_format channels) (Gstreamer.Format.to_string format)

  let audio_sink ?(drop=false) ?(sync=false) ?max_buffers ~channels name =
    let max_buffers =
      match max_buffers with
      | None -> conf_max_buffers#get
      | Some m -> m
    in
    Printf.sprintf "appsink max-buffers=%d drop=%B sync=%B name=\"%s\" caps=\"%s\""
      max_buffers drop sync name
      (audio_format channels)

  let video_format () =
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let fps = Lazy.force Frame.video_rate in
    Printf.sprintf
      "video/x-raw,format=RGBA,width=%d,height=%d,framerate=%d/1,pixel-aspect-ratio=1/1"
      width height fps

  let video_src ?(block=true) ?(format=Gstreamer.Format.Time) name =
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let blocksize = width * height * 4 in
    Printf.sprintf "appsrc name=\"%s\" block=%B caps=\"%s\" format=%s blocksize=%d"
      name block (video_format ()) (Gstreamer.Format.to_string format) blocksize

  let video_sink ?(drop=false) ?(sync=false) ?max_buffers name =
    let max_buffers =
      match max_buffers with
      | None -> conf_max_buffers#get
      | Some m -> m
    in
    Printf.sprintf
      "appsink name=\"%s\" drop=%B sync=%B max-buffers=%d caps=\"%s\""
      name drop sync max_buffers
      (video_format ())

  let decode_video () =
    Printf.sprintf "decodebin ! %s" (convert_video ())
end

let render_image pipeline =
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let pipeline = Printf.sprintf "%s ! %s ! %s" pipeline (Pipeline.convert_video ()) (Pipeline.video_sink ~drop:false ~max_buffers:1 "sink") in
  (* Printf.printf "render_image pipeline: %s\n%!" pipeline; *)
  let bin = Gstreamer.Pipeline.parse_launch pipeline in
  let sink = Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "sink") in
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_playing);
  ignore (Gstreamer.Element.get_state bin);
  let buf = Gstreamer.App_sink.pull_buffer_data sink in
  let img = Img.make width height buf in
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
  img
