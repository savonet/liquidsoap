(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Gstreamer

module I = Image.RGBA32

(* TODO.... *)
let () =
  Gstreamer.init ()

let v4l_clock =
  Tutils.lazy_cell (fun () -> new Clock.self_sync "v4l")

class v4l_input v4l_version p kind =
  let e f v = f (List.assoc v p) in
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
  let dev = e Lang.to_string "device" in
  let drop = e Lang.to_bool "drop" in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let size = VFrame.size () in
  let blank () = Array.init size
                            (fun _ -> I.create width height) 
  in
  let vfps = Lazy.force Frame.video_rate in
object (self)
  inherit Source.active_source kind as active_source
  inherit [I.t array] IoRing.input ~nb_blocks ~blank as ioring

  method set_clock =
    active_source#set_clock ;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((v4l_clock ()):>Clock.clock))

  method private wake_up l =
    active_source#wake_up l ;
    if clock_safe then
      (v4l_clock ())#register_blocking_source

  method private sleep =
    ioring#sleep ;
    if clock_safe then
      (v4l_clock ())#unregister_blocking_source

  method stype = Source.Infallible
  method remaining = -1
  method is_ready = true

  method abort_track = ()

  val mutable device = None

  method pull_block block = 
    let sink = self#get_device in
    for i = 0 to size - 1 do 
      let b = App_sink.pull_buffer (App_sink.of_element sink) in
      let img = I.make width height b in
      block.(i) <- img
    done

  (* ocaml-gstreamer does not have
   * a close API for now.. *)
  method close = ()

  method get_device =
    match device with
      | Some s -> s
      | None ->
      let pipeline =
        Printf.sprintf
          "v4l%ssrc device=%s ! ffmpegcolorspace ! videoscale ! \
           videorate ! video/x-raw-rgb,width=%d,height=%d,\
           pixel-aspect-ratio=1/1,bpp=(int)32,depth=(int)24,\
           endianness=(int)4321,red_mask=(int)0xff000000,\
           green_mask=(int)0x00ff0000,blue_mask=(int)0x0000ff00,\
           framerate=(fraction)%d/1 ! \
           appsink max-buffers=2 drop=%b name=sink"
          (if v4l_version = 1 then "" else "2")
          dev width height vfps drop
      in
      let bin = Pipeline.parse_launch pipeline in
      let s = Bin.get_by_name (Bin.of_element bin) "sink" in
      device <- Some s;
      Element.set_state bin State_playing;
      s
    

  method output = if AFrame.is_partial memo then self#get_frame memo
  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = Frame.position frame);
    let buf = VFrame.content_of_type ~channels:1 frame in
    let buf = buf.(0) in
    let img = ioring#get_block in
      for i = 0 to size - 1 do
        buf.(i) <- img.(i)
      done;
    VFrame.add_break frame size;
end

let proto = 
    ["clock_safe",
      Lang.bool_t, Some (Lang.bool true),
      Some "Force the use of the dedicated v4l clock." ;
     "buffer_size",
      Lang.int_t, Some (Lang.int 2),
      Some "Set buffer size, in frames. Must be >= 1.";
     "drop",
      Lang.bool_t, Some (Lang.bool true),
      Some "Drop frames when execution is too slow.";
      "device", Lang.string_t, Some (Lang.string "/dev/video0"),
      Some "V4L device to use.";
    ]

let add ~descr ~name ~version =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained
         { Frame. audio = Lang.Any_fixed 0 ;
           video = Lang.Fixed 1 ;
           midi = Lang.Fixed 0 })
  in
  Lang.add_operator name proto
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr
    (fun p kind ->
      ((new v4l_input version p kind):>Source.source))

let () =
  add ~descr:"Stream from a video4linux input device, such as a webcam. \
            The most recent version of this library is provided by \
            input.v4l2, only use this one if the other does \
            not work."
      ~name:"input.v4l"
      ~version:1 ;
  add ~descr:"Stream from a video4linux 2 input device, such as a webcam."
      ~name:"input.v4l2"
      ~version:2
