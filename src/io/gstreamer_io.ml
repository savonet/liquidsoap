open Gstreamer

module I = Image.RGBA8
module G = Image.Generic

(* TODO.... *)
let () =
  Gstreamer.init ()

class v4l2_input ~kind dev =
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let vfps = 10 in
object (self)
  inherit Source.active_source kind

  method stype = Source.Infallible
  method remaining = -1
  method is_ready = true

  method abort_track = ()

  method output = if AFrame.is_partial memo then self#get_frame memo

  val mutable sink = None

  method output_get_ready =
    let pipeline = Printf.sprintf "v4l2src device=%s ! ffmpegcolorspace ! videoscale ! appsink drop=true name=sink caps=\"video/x-raw-rgb,width=%d,height=%d,pixel-aspect-ratio=1/1,bpp=(int)24,depth=(int)24,endianness=(int)4321,red_mask=(int)0xff0000,green_mask=(int)0x00ff00,blue_mask=(int)0x0000ff,framerate=(fraction)%d/1\"" dev width height vfps in
    let bin = Pipeline.parse_launch pipeline in
    let s = Bin.get_by_name (Bin.of_element bin) "sink" in
    sink <- Some s;
    Element.set_state bin State_playing;

  method output_reset = ()
  method is_active = true

  val mutable image = I.create 0 0

  method get_frame frame =
    assert (0 = AFrame.position frame);
    let sink = Utils.get_some sink in
    let buf = VFrame.content_of_type ~channels:1 frame 0 in
    let buf = buf.(0) in
    let b = App_sink.pull_buffer (App_sink.of_element sink) in
    let vimg = G.make_rgb G.Pixel.RGB24 width height b in
    let img = I.create width height in
    G.convert ~copy:true ~proportional:true vimg (G.of_RGBA8 img);
    for i = 0 to VFrame.size frame - 1 do
      I.Scale.onto ~proportional:true img buf.(i)
    done;
    AFrame.add_break frame (AFrame.size ())
end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained
         { Frame. audio = Lang.Any_fixed 0 ; video = Lang.Fixed 1 ; midi = Lang.Fixed 0 })
  in
  Lang.add_operator "input.gstreamer.v4l2"
    [
      "device", Lang.string_t, Some (Lang.string "/dev/video0"),
      Some "V4L device to use.";
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[Lang.Experimental;Lang.Hidden]
    ~descr:"Stream from a V4L2 (= video 4 linux) input device, such as a webcam."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let device = e Lang.to_string "device" in
         ((new v4l2_input ~kind device):>Source.source)
    )
