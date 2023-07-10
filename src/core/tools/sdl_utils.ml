(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let check f x =
  match f x with Error (`Msg err) -> failwith err | Ok ans -> ans

let initialized = ref false

let init l =
  if !options = None then options := Some Sdl.Init.nothing;
  List.iter (fun e -> options := Some Sdl.Init.(Option.get !options + e)) l

let () =
  Lifecycle.on_start (fun () ->
      match !options with Some o -> check Sdl.init o | None -> ())

module Surface = struct
  let to_img surface =
    let width, height = Sdl.get_surface_size surface in
    let finalize = ref (fun () -> ()) in
    let surface =
      let fmt = Sdl.get_surface_format_enum surface in
      (* It seems that we cannot decode those properly (see #3194),
         converting. *)
      if fmt = Sdl.Pixel.format_index8 then (
        let fmt = Sdl.alloc_format Sdl.Pixel.format_rgb24 |> Result.get_ok in
        let surface' = Sdl.convert_surface surface fmt |> Result.get_ok in
        Sdl.free_format fmt;
        (finalize := fun () -> Sdl.free_surface surface');
        surface')
      else surface
    in
    let pitch = Sdl.get_surface_pitch surface in
    let fmt = Sdl.get_surface_format_enum surface in
    let img = Video.Image.create width height in
    let finalize = !finalize in
    (match fmt with
      | fmt when fmt = Sdl.Pixel.format_rgb888 ->
          assert (Sdl.lock_surface surface = Ok ());
          let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
          for j = 0 to height - 1 do
            for i = 0 to width - 1 do
              let r, g, b, a =
                if Sys.big_endian then
                  ( pix.{(j * pitch) + (4 * i) + 0},
                    pix.{(j * pitch) + (4 * i) + 1},
                    pix.{(j * pitch) + (4 * i) + 2},
                    0xff )
                else
                  ( pix.{(j * pitch) + (4 * i) + 2},
                    pix.{(j * pitch) + (4 * i) + 1},
                    pix.{(j * pitch) + (4 * i) + 0},
                    0xff )
              in
              Image.YUV420.set_pixel_rgba img i j (r, g, b, a)
            done
          done;
          Sdl.unlock_surface surface;
          finalize ()
      | fmt when fmt = Sdl.Pixel.format_argb8888 ->
          assert (Sdl.lock_surface surface = Ok ());
          let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
          for j = 0 to height - 1 do
            for i = 0 to width - 1 do
              let r, g, b, a =
                if Sys.big_endian then
                  ( pix.{(j * pitch) + (4 * i) + 1},
                    pix.{(j * pitch) + (4 * i) + 2},
                    pix.{(j * pitch) + (4 * i) + 3},
                    pix.{(j * pitch) + (4 * i) + 0} )
                else
                  ( pix.{(j * pitch) + (4 * i) + 2},
                    pix.{(j * pitch) + (4 * i) + 1},
                    pix.{(j * pitch) + (4 * i) + 0},
                    pix.{(j * pitch) + (4 * i) + 3} )
              in
              Image.YUV420.set_pixel_rgba img i j (r, g, b, a)
            done
          done;
          Sdl.unlock_surface surface;
          finalize ()
      | fmt when fmt = Sdl.Pixel.format_rgb24 ->
          assert (Sdl.lock_surface surface = Ok ());
          let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
          for j = 0 to height - 1 do
            for i = 0 to width - 1 do
              let r, g, b =
                ( pix.{(j * pitch) + (3 * i) + 0},
                  pix.{(j * pitch) + (3 * i) + 1},
                  pix.{(j * pitch) + (3 * i) + 2} )
              in
              let a = 0xff in
              Image.YUV420.set_pixel_rgba img i j (r, g, b, a)
            done
          done;
          Sdl.unlock_surface surface;
          finalize ()
      | fmt when fmt = Sdl.Pixel.format_index8 ->
          assert (Sdl.lock_surface surface = Ok ());
          let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
          for j = 0 to height - 1 do
            for i = 0 to width - 1 do
              let p = pix.{i + (j * pitch)} in
              let a = 0xff in
              Image.YUV420.set_pixel_rgba img i j (p, p, p, a)
            done
          done;
          Sdl.unlock_surface surface;
          finalize ()
      | _ ->
          finalize ();
          failwith
            ("img_of_surface: unhandled format "
            ^ string_of_int (Int32.to_int (Sdl.Pixel.to_uint32 fmt))));
    img

  let of_img surface img =
    let width, height = Sdl.get_surface_size surface in
    assert (width = Image.YUV420.width img);
    assert (height = Image.YUV420.height img);
    let pitch = Sdl.get_surface_pitch surface in
    let fmt = Sdl.get_surface_format_enum surface in
    match fmt with
      | fmt when fmt = Sdl.Pixel.format_rgb888 ->
          assert (Sdl.lock_surface surface = Ok ());
          let pix = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
          for i = 0 to width - 1 do
            for j = 0 to height - 1 do
              let r, g, b, _ = Image.YUV420.get_pixel_rgba img i j in
              pix.{(j * pitch) + (4 * i) + 0} <- b;
              pix.{(j * pitch) + (4 * i) + 1} <- g;
              pix.{(j * pitch) + (4 * i) + 2} <- r;
              pix.{(j * pitch) + (4 * i) + 3} <- 0xff
            done
          done;
          Sdl.unlock_surface surface
      | _ ->
          failwith
            ("img_of_surface: unhandled format "
            ^ string_of_int (Int32.to_int (Sdl.Pixel.to_uint32 fmt)))
end
