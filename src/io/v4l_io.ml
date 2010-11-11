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

module Img = Image.RGBA32

external caps :
  Unix.file_descr -> string * int * int * int * int * int * int
  = "caml_v4l_caps"
external init : Unix.file_descr -> unit = "caml_v4l_init"
external get_dims : Unix.file_descr -> int * int = "caml_v4l_get_dims"
external capture : Unix.file_descr -> int -> int -> string = "caml_v4l_capture"

(* TODO WTF? should this module disappear anyway? *)
let every = 5

class input ~kind dev =
object (self)
  inherit Source.active_source kind

  val mutable fd = None

  method stype = Source.Infallible
  method remaining = -1
  method is_ready = true

  method abort_track = ()

  method output = if VFrame.is_partial memo then self#get_frame memo

  val mutable width = 0
  val mutable height = 0

  method output_get_ready =
    fd <- Some (Unix.openfile dev [Unix.O_RDWR] 0);
    let fd = Utils.get_some fd in
    let _, _, _, _, _, _, _ = caps fd in
      init fd;
      let w, h = get_dims fd in
        width <- w;
        height <- h

  method output_reset = ()
  method is_active = true

  val mutable image = Img.create 0 0
  val mutable count = every

  method get_frame frame =
    assert (0 = Frame.position frame);
    let fd = Utils.get_some fd in
    let buf = VFrame.content_of_type ~channels:1 frame in
    let buf = buf.(0) in
    let img =
      (*
      let buflen = width * height * 3 in
      let buf = String.make buflen '\000' in
        ignore (Unix.read fd buf 0 buflen);
        buf
       *)
      if count = every then
        (
          count <- 0;
          Img.of_RGB24_string (capture fd width height) width
        )
      else
        (
          count <- count + 1;
          image
        )
    in
      image <- img;
      for i = 0 to VFrame.size frame - 1 do
        Img.Scale.onto ~proportional:true img buf.(i)
      done;
      VFrame.add_break frame (VFrame.size ())
end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained
         { Frame. audio = Lang.Any_fixed 0 ;
                  video = Lang.Fixed 1 ;
                  midi = Lang.Fixed 0 })
  in
  Lang.add_operator "input.v4l"
    [
      "device", Lang.string_t, Some (Lang.string "/dev/video0"),
      Some "V4L device to use.";
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[Lang.Experimental;Lang.Hidden]
    ~descr:"Stream from a V4L (Video for Linux) input device, such as a webcam."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let device = e Lang.to_string "device" in
         ((new input ~kind device):>Source.source))
