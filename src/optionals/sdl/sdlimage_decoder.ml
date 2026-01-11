(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
open Tsdl
module P = Image.Generic.Pixel

let log = Log.make ["decoder"; "sdlimage"]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_image_priorities#plug "sdl")
    "Priority for the sdl image decoder" ~d:5

let decode_image filename =
  let surface = Sdl_utils.check Tsdl_image.Image.load filename in
  Fun.protect
    (fun () -> Sdl_utils.Surface.to_img surface)
    ~finally:(fun () -> Sdl.free_surface surface)

let check_image filename =
  try
    ignore (decode_image filename);
    true
  with exn ->
    log#info "Failed to decode %s: %s"
      (Lang_string.quote_string filename)
      (Printexc.to_string exn);
    false

let () =
  Plug.register Decoder.image_file_decoders "sdl"
    ~doc:"Use SDL to decode images."
    {
      Decoder.image_decoder_priority = (fun () -> priority#get);
      check_image;
      decode_image;
    }
