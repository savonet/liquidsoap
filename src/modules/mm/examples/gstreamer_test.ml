open Gstreamer
open Mm_audio
open Mm_video

let width = 320
let height = 240
let fps = 24
let audio_channels = 2
let audio_rate = 44100
let src = "filesrc location=../test.wmv"

let pipeline =
  Printf.sprintf
    "%s ! decodebin name=decode decode. ! ffmpegcolorspace ! videoscale ! \
     videorate ! appsink max-buffers=2 drop=true name=videosink \
     caps=\"video/x-raw-rgb,width=%d,height=%d,pixel-aspect-ratio=1/1,bpp=(int)24,depth=(int)24,endianness=(int)4321,red_mask=(int)0xff0000,green_mask=(int)0x00ff00,blue_mask=(int)0x0000ff,framerate=(fraction)%d/1\" \
     decode. ! audioconvert ! audioresample ! appsink max-buffers=2 drop=true \
     name=audiosink \
     caps=\"audio/x-raw-int,width=16,channels=%d,rate=%d,signed=true\""
    src width height fps audio_channels audio_rate

let () =
  Gstreamer.init ();
  Printf.printf "%s\n%!" (version_string ());
  Printf.printf "%s\n%!" pipeline;
  let bin = Pipeline.parse_launch pipeline in

  let _ = Bin.get_by_name (Bin.of_element bin) "videosink" in
  let audiosink = Bin.get_by_name (Bin.of_element bin) "audiosink" in

  let sdl = new Mm_sdl.writer_to_screen width height in
  let oss = new Mm_oss.writer audio_channels audio_rate in

  ignore (Element.set_state bin State_playing);
  while true do
    (* Video *)
    (* let b = App_sink.pull_buffer (App_sink.of_element videosink) in *)
    (* let img = Image.Generic.make Image.Generic.Pixel.YUVJ420 width height b in *)
    (* let out = Video.Image.create width height in *)
    let out = assert false in
    (* Image.Generic.convert ~copy:true ~proportional:true img (Image.Generic.of_YUV420 out); *)
    let vid = Video.single out in
    sdl#write vid 0 1;

    (* Audio *)
    let b = App_sink.pull_buffer_string (App_sink.of_element audiosink) in
    let samples = Audio.S16LE.length audio_channels (String.length b) in
    let buf = Audio.create audio_channels samples in
    Audio.S16LE.to_audio b 0 buf 0 (Audio.length buf);
    oss#write buf 0 (Audio.length buf)
  done;
  ignore (Element.set_state bin State_null)
