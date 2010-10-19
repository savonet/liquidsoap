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

open Source

module Img = Image.RGBA8

class visu ~kind source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  (* [vol] contains the pixel coordinates to be displayed for each channel:
   * each pixel corresponds to the volume (rms) of one frame (rather, one
   * output chunk, which is bad because they don't all have the same
   * duration.. TODO). *)
  val vol = Array.init channels (fun _ -> Array.make width 0.)

  method private add_vol v =
    for c = 0 to channels - 1 do
      (* Shift old values *)
      for i = 1 to width - 1 do
        vol.(c).(i - 1) <- vol.(c).(i)
      done;
      (* Add the new one *)
      vol.(c).(width - 1) <- v.(c)
    done

  method private render frame offset len =
    let clip y =
      max 0 (min (height-1) y)
    in
    let pts =
      Array.mapi
        (fun j v ->
           Array.mapi
             (fun i x ->
                i,
                clip (height -
                      (int_of_float (x *. float height) / channels +
                       j * height / channels)))
             v)
        vol
    in
    let pts = Array.concat (Array.to_list pts) in

    (* Add a video channel to the frame contents. *)
    let _,src = Frame.content frame offset in
    let src_type = Frame.type_of_content src in
    let dst_type = { src_type with Frame.video = 1 } in
    let dst = Frame.content_of_type frame offset dst_type in

    (* Reproduce audio data in the new contents. *)
    Audio.blit
      src.Frame.audio (Frame.audio_of_master offset)
      dst.Frame.audio (Frame.audio_of_master offset)
      (Frame.audio_of_master len);

    (* Fill-in video information. *)
    let buf = dst.Frame.video.(0) in
    let start = Frame.video_of_master offset in
    let stop = start + Frame.video_of_master len in
      for f = start to stop - 1 do
        Img.blank_all buf.(f) ;
        Array.iter
          (fun (i,j) -> Img.set_pixel buf.(f) i j (0xff, 0xff, 0xff, 0xff))
          pts
      done

  method private get_frame frame =
    let offset = Frame.position frame in
    let len = source#get frame ; Frame.position frame - offset in
    (* If the data has duration=0 don't do anything as there might
     * not even be a content layer of the right type to look at. *)
    if len > offset then
      let rms =
        AFrame.rms
          frame (Frame.audio_of_master offset) (Frame.audio_of_master len)
      in
        self#add_vol rms ;
        self#render frame offset len

end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  let fmt =
    { Frame. audio = Lang.Any_fixed 1;
             video = Lang.Fixed 1; midi = Lang.Fixed 0 }
  in
  Lang.add_operator "video.volume"
    [ "", Lang.source_t k, None, None ]
    ~kind:(Lang.Constrained fmt)
    ~category:Lang.Visualization
    ~descr:"Graphical visualization of the sound."
    (fun p kind ->
       let f v = List.assoc v p in
       let src =
         Lang.to_source (f "")
       in
         new visu ~kind src)
