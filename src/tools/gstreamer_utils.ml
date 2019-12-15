let conf_gstreamer =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "gstreamer")
    "Media decoding/endcoding through gstreamer."

let conf_max_buffers =
  Dtools.Conf.int
    ~p:(conf_gstreamer#plug "max_buffers")
    ~d:10 "Maximal number of buffers."

let max_buffers () = conf_max_buffers#get

let conf_add_borders =
  Dtools.Conf.bool
    ~p:(conf_gstreamer#plug "add_borders")
    ~d:true "Add borders in order to keep video aspect ratio."

let add_borders () = conf_add_borders#get

let () =
  Configure.at_init (fun () ->
      let debug =
        try int_of_string (Sys.getenv "LIQ_GST_DEBUG_LEVEL") with _ -> 0
      in
      Gstreamer.init
        ~argv:
          [|
            "Liquidsoap";
            "--gst-debug-spew";
            Printf.sprintf "--gst-debug-level=%d" debug;
          |]
        ();
      let major, minor, micro, nano = Gstreamer.version () in
      let log = Log.make ["gstreamer"; "loader"] in
      log#important "Loaded GStreamer %d.%d.%d %d" major minor micro nano)

module Pipeline = struct
  let convert_audio () = "audioconvert ! audioresample"
  let decode_audio () = Printf.sprintf "decodebin ! %s" (convert_audio ())

  let convert_video () =
    let add_borders = add_borders () in
    Printf.sprintf "videoconvert ! videoscale add-borders=%B ! videorate"
      add_borders

  let audio_format channels =
    let rate = Lazy.force Frame.audio_rate in
    Printf.sprintf
      "audio/x-raw,format=S16LE,layout=interleaved,channels=%d,rate=%d" channels
      rate

  let audio_src ~channels ?(maxBytes = 10 * Utils.pagesize) ?(block = true)
      ?(format = Gstreamer.Format.Time) name =
    Printf.sprintf
      "appsrc name=\"%s\" block=%B caps=\"%s\" format=%s max-bytes=%d" name
      block (audio_format channels)
      (Gstreamer.Format.to_string format)
      maxBytes

  let audio_sink ?(drop = false) ?(sync = false) ?max_buffers ~channels name =
    let max_buffers =
      match max_buffers with None -> conf_max_buffers#get | Some m -> m
    in
    Printf.sprintf
      "appsink max-buffers=%d drop=%B sync=%B name=\"%s\" caps=\"%s\""
      max_buffers drop sync name (audio_format channels)

  let video_format () =
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let fps = Lazy.force Frame.video_rate in
    Printf.sprintf
      "video/x-raw,format=I420,width=%d,height=%d,framerate=%d/1,pixel-aspect-ratio=1/1"
      width height fps

  let video_src ?(block = true) ?(maxBytes = 10 * Utils.pagesize)
      ?(format = Gstreamer.Format.Time) name =
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let blocksize = width * height * 4 in
    Printf.sprintf
      "appsrc name=\"%s\" block=%B caps=\"%s\" format=%s blocksize=%d \
       max-bytes=%d"
      name block (video_format ())
      (Gstreamer.Format.to_string format)
      blocksize maxBytes

  let video_sink ?(drop = false) ?(sync = false) ?max_buffers name =
    let max_buffers =
      match max_buffers with None -> conf_max_buffers#get | Some m -> m
    in
    Printf.sprintf
      "appsink name=\"%s\" drop=%B sync=%B max-buffers=%d caps=\"%s\"" name drop
      sync max_buffers (video_format ())

  let decode_video () = Printf.sprintf "decodebin ! %s" (convert_video ())
end

let render_image pipeline =
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let pipeline =
    Printf.sprintf "%s ! %s ! %s" pipeline
      (Pipeline.convert_video ())
      (Pipeline.video_sink ~drop:false ~max_buffers:1 "sink")
  in
  (* Printf.printf "render_image pipeline: %s\n%!" pipeline; *)
  let bin = Gstreamer.Pipeline.parse_launch pipeline in
  let sink =
    Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "sink")
  in
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_playing);
  ignore (Gstreamer.Element.get_state bin);
  let buf = Gstreamer.App_sink.pull_buffer_data sink in
  let img =
    Image.YUV420.make_data width height buf (Image.Data.round 4 width)
      (Image.Data.round 4 (width / 2))
  in
  ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_null);
  img

let master_of_time time =
  Frame.master_of_seconds (Int64.to_float (Int64.div time 100000L) *. 0.0001)

let time_of_master tick =
  Int64.mul (Int64.of_float (Frame.seconds_of_master tick *. 10000.)) 100000L

let time_of_audio tick =
  Int64.mul (Int64.of_float (Frame.seconds_of_audio tick *. 10000.)) 100000L

let time_of_video tick =
  Int64.mul (Int64.of_float (Frame.seconds_of_video tick *. 10000.)) 100000L

let handler ~(log : Log.t) ~on_error msg =
  let source = msg.Gstreamer.Bus.source in
  match msg.Gstreamer.Bus.payload with
    | `Error err ->
        log#severe "[%s] Error: %s" source err;
        on_error err
    | `Warning err -> log#important "[%s] Warning: %s" source err
    | `Info err -> log#info "[%s] Info: %s" source err
    | `State_changed (o, n, p) ->
        let f = Gstreamer.Element.string_of_state in
        let o = f o in
        let n = f n in
        let p =
          match p with
            | Gstreamer.Element.State_void_pending -> ""
            | _ -> Printf.sprintf " (pending: %s)" (f p)
        in
        log#debug "[%s] State change: %s -> %s%s" source o n p
    | _ -> assert false

let flush ~log ?(types = [`Error; `Warning; `Info; `State_changed])
    ?(on_error = fun _ -> ()) bin =
  let bus = Gstreamer.Bus.of_element bin in
  let rec f () =
    match Gstreamer.Bus.pop_filtered bus types with
      | Some msg ->
          handler ~log ~on_error msg;
          f ()
      | None -> ()
  in
  f ()

let () =
  let loop = Gstreamer.Loop.create () in
  let main () = Gstreamer.Loop.run loop in
  ignore
    (Dtools.Init.at_start (fun () ->
         ignore (Tutils.create main () "gstreamer_main_loop")));
  ignore (Dtools.Init.at_stop (fun () -> Gstreamer.Loop.quit loop))
