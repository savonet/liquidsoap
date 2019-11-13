(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Tsdl

module Gen = Image.Generic

type event = 
        [ `AUDIO
        | `CDROM
        | `EVENTTHREAD
        | `EVERYTHING
        | `JOYSTICK
        | `NOPARACHUTE
        | `TIMER
        | `VIDEO ]

let options : Sdl.Init.t option ref = ref None
let start_ttf = ref false

let check f x =
  match f x with
  | Error (`Msg err) -> failwith err
  | Ok ans -> ans

let init l =
  if !options = None then options := Some Sdl.Init.nothing;
  List.iter (fun e -> options := Some Sdl.Init.(Option.get !options + e)) l

let ttf_init () = start_ttf := true

let () = 
  ignore (Dtools.Init.at_start (fun () ->
      if !start_ttf then check Tsdl_ttf.Ttf.init ();
      match !options with
      | Some o -> check Sdl.init o
      | None -> ()
    ))

module Surface = struct
  let to_img surface =
    let width, height = Sdl.get_surface_size surface in
    let pitch = Sdl.get_surface_pitch surface in
    let fmt = Sdl.get_surface_format_enum surface in
    let img = Video.Image.create width height in
    (
      match fmt with
      | fmt when fmt = Sdl.Pixel.format_rgb888 ->
        let pitch = pitch / 4 in
        let pix = Sdl.get_surface_pixels surface Bigarray.Int32 in
        for j = 0 to height-1 do
          for i = 0 to width-1 do
            let p = Int32.to_int pix.{i+j*pitch} in
            let r = p lsr 16 land 0xff in
            let g = p lsr 8  land 0xff in
            let b = p        land 0xff in
            let a = 0xff               in
            Video.Image.set_pixel_rgba img i j (r,g,b,a)
          done
        done
      | fmt when fmt = Sdl.Pixel.format_index8 ->
        let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
        for j = 0 to height-1 do
          for i = 0 to width-1 do
            let p = pix.{i+j*pitch} in
            let a = 0xff in
            Video.Image.set_pixel_rgba img i j (p,p,p,a)
          done
        done
      | _ -> failwith ("img_of_surface: unhandled format " ^ string_of_int (Int32.to_int (Sdl.Pixel.to_uint32 fmt)))
    );
    img

  let of_img surface img =
    let width, height = Sdl.get_surface_size surface in
    let fmt = Sdl.get_surface_format_enum surface in
    match fmt with
    | fmt when fmt = Sdl.Pixel.format_rgb888 ->
      let pix = Sdl.get_surface_pixels surface Bigarray.Int32 in
      for i = 0 to width-1 do
        for j = 0 to height-1 do
          let r,g,b,_ = Video.Image.get_pixel_rgba img i j in
          pix.{i+j*width} <- Int32.of_int (r lsl 16 + g lsl 8 + b);
        done
      done
    | _ -> failwith ("img_of_surface: unhandled format " ^ string_of_int (Int32.to_int (Sdl.Pixel.to_uint32 fmt)))
end
