(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

open Mm_video
open Mm_midi

let init () = Sdl.init [`VIDEO]

(*
(** 8bit surfaces always use a palette *)
let from_8 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let image = Sdlvideo.pixel_data_8 surface in
  let a = I.create width height in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b = Sdlvideo.get_palette_color surface image.{i+j*pitch} in
      I.set_pixel a i j (r,g,b,0xff)
    done
  done;
  a

(** 16bits surfaces contain specially packed RGB *)
let to_16 rgb surface =
  let s = Sdlvideo.pixel_data_16 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/2 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  assert (width = I.width rgb && height = I.height rgb);
  assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette) ;
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b,_ = I.get_pixel rgb i j in
      let color =
        ((r lsr fmt.Sdlvideo.rloss) lsl fmt.Sdlvideo.rshift) lor
          ((g lsr fmt.Sdlvideo.gloss) lsl fmt.Sdlvideo.gshift) lor
          ((b lsr fmt.Sdlvideo.bloss) lsl fmt.Sdlvideo.bshift)
      in
      (* let color = Int32.to_int (Sdlvideo.map_RGB surface (r,g,b)) in *)
      s.{i+j*pitch} <- color
    done
  done
*)

(*
(** 24bits surfaces are standard RGB stored in three different bytes, but the
    order might vary. *)
let from_24 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let rgb = Sdlvideo.pixel_data_24 surface in
  let a = I.create width height in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      for c = 0 to 2 do
        let c' = if fmt.Sdlvideo.rshift = 0 then c else 2-c in
        rgba.{c+i*4+j*a.RGB.stride} <- rgb.{c'+i*3+j*pitch}
      done ;
      rgba.{3+i*4+j*a.RGB.stride} <- 0xff
    done
  done ;
  a
*)

(** 32bits surfaces are standard RGBA However, the RGB components are (at least
    sometimes) packed in a different order as in liquidsoap: 0xAARRGGBB.

    An alternative implementation, which is surprisingly not sensibly faster,
    uses SDL blitting directly by casting a char* into an int*. The alpha is
    masked out because we don't want to see video frames on top of each other on
    screen. This hack might not work the same on different platforms. let s =
    Sdlvideo.create_RGB_surface_from_32 (Obj.magic rgb.RGB.data)
    ~w:rgb.RGB.width ~h:rgb.RGB.height ~pitch:rgb.RGB.stride (* The masks might
    be endianness dependent *) ~rmask:0xffl ~gmask:0xff00l ~bmask:0xff0000l
    ~amask:0l in Sdlvideo.blit_surface ~src:s ~dst:surface () *)

(*
let to_32 rgb surface =
  let s = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/4 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  assert (width = I.width rgb && height = I.height rgb);
  assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette);
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b,_ = I.get_pixel rgb i j in
      let color =
        Int32.of_int
          ((r lsl fmt.Sdlvideo.rshift) lor
              (g lsl fmt.Sdlvideo.gshift) lor
              (b lsl fmt.Sdlvideo.bshift))
      in
      s.{i+j*pitch} <- color
    done
  done
*)

external to_32 :
  Video.Image.t ->
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int * int * int ->
  unit = "caml_sdl_rgb_to32"

let to_32 rgb surface =
  let sbuf = Sdlvideo.pixel_data_32 surface in
  let fmt = Sdlvideo.surface_format surface in
  to_32 rgb sbuf (fmt.Sdlvideo.rshift, fmt.Sdlvideo.gshift, fmt.Sdlvideo.bshift)

(*
let from_32 surface =
  let img = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let pitch = pitch/4 in (* pitch is in bytes, convert for int32 array *)
  let a = RGB.create width height in
  let rgba = a.RGB.data in
  assert (fmt.Sdlvideo.rloss = 0 &&
          fmt.Sdlvideo.gloss = 0 &&
          fmt.Sdlvideo.bloss = 0) ;
  let (&&) = Int32.logand in
  let (>>) = Int32.shift_right in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let pixel = img.{i+j*pitch} in
        let pos = i*4+j*a.RGB.stride in
        rgba.{0+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.rmask) >> fmt.Sdlvideo.rshift) ;
        rgba.{1+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.gmask) >> fmt.Sdlvideo.gshift) ;
        rgba.{2+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.bmask) >> fmt.Sdlvideo.bshift) ;
        rgba.{3+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.amask) >> fmt.Sdlvideo.ashift)
      done
    done ;
    a
*)

class writer_to_screen w h =
  object
    initializer
      Sdlevent.enable_events Sdlevent.quit_mask;
      (* Try to get 32bpp because it's faster (twice as fast here), but accept
       * other formats too. *)
      ignore (Sdlvideo.set_video_mode ~w ~h ~bpp:32 [`ANYFORMAT; `DOUBLEBUF])

    method write buf ofs len =
      if Sdlevent.poll () = Some Sdlevent.QUIT then Sdl.quit ()
      else if len > 0 then (
        let surface = Sdlvideo.get_video_surface () in
        (* We only display the last image of each frame *)
        let rgb = buf.(ofs + len - 1) in
        begin
          match Sdlvideo.surface_bpp surface with
            (* | 16 -> to_16 rgb surface *)
            | 32 -> to_32 rgb surface
            | i -> failwith (Printf.sprintf "Unsupported format %dbpp" i)
        end;
        Sdlvideo.flip surface)

    method close = Sdl.quit ()
  end

class midi_keyboard : MIDI.IO.Reader.t =
  let knotes2 =
    [|
      '&';
      'a';
      '\233';
      'z';
      '"';
      'e';
      'r';
      '(';
      't';
      '-';
      'y';
      '\232';
      'u';
      'i';
      '\231';
      'o';
      '\224';
      'p';
    |]
  in
  let knotes1 =
    [|
      'q';
      'w';
      's';
      'x';
      'd';
      'c';
      'v';
      'g';
      'b';
      'h';
      'n';
      'j';
      ',';
      ';';
      'l';
      ':';
      'm';
      '!';
    |]
  in
  let array_index a x =
    let ans = ref None in
    for i = 0 to Array.length a - 1 do
      if a.(i) = x then ans := Some i
    done;
    match !ans with Some i -> i | None -> raise Not_found
  in
  let note_of_char c =
    try array_index knotes2 c + 71
    with Not_found -> array_index knotes1 c + 59
  in
  object
    initializer
      Sdl.init [`EVENTTHREAD; `VIDEO];
      Sdlevent.disable_events Sdlevent.all_events_mask;
      Sdlevent.enable_events
        (Sdlevent.make_mask
           [Sdlevent.KEYDOWN_EVENT; Sdlevent.KEYUP_EVENT; Sdlevent.QUIT_EVENT]);
      ignore (Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [])

    val mutable velocity = 1.
    val channel = 0

    method read _ buf ofs len =
      MIDI.Multitrack.clear buf ofs len;
      Sdlevent.pump ();
      while Sdlevent.has_event () do
        try
          match Sdlevent.poll () with
            | Some (Sdlevent.KEYDOWN k) ->
                let c = Sdlkey.char_of_key k.Sdlevent.keysym in
                if c = '+' || c = '*' then velocity <- min 1. (velocity +. 0.1)
                else if c = '-' || c = '/' then
                  velocity <- max 0. (velocity -. 0.1)
                else (
                  let n = note_of_char c in
                  (* Printf.printf "Playing note %d.\n%!" n; *)
                  MIDI.insert buf.(channel) (ofs, MIDI.Note_on (n, velocity)))
            | Some (Sdlevent.KEYUP k) ->
                let c = Sdlkey.char_of_key k.Sdlevent.keysym in
                let n = note_of_char c in
                (* Printf.printf "Stopping note %d.\n%!" n; *)
                MIDI.insert buf.(channel) (ofs, MIDI.Note_off (n, velocity))
            | _ -> ()
        with Not_found | Invalid_argument _ -> ()
      done;
      len

    method close = Sdl.quit ()
  end
