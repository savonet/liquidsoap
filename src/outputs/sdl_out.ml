(*****************************************************************************

  Copyright 2003-2008 Savonet team

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

(** Output using SDL lib. *)

open Sdl

let chunksize = 4096

class output source start =
  (*
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
  let bytes_per_sample = 4 in
   *)
  let video_channels = Fmt.video_channels () in
  let video_width = Fmt.video_width () in
  let video_height = Fmt.video_height () in
object (self)
  inherit Output.output ~name:"sdl" ~kind:"output.sdl" source start

  initializer
    (* TODO: do not initialize twice *)
    Sdl.init [`VIDEO] (* `AUDIO *)

  val mutable sleep = false
  method output_stop =
    sleep <- true

  val mutable surface = None
  method output_start =
    (*
    Sdlmixer.open_audio
      ~freq:(Fmt.samples_per_second ())
      ~format:Sdlmixer.AUDIO_FORMAT_S16LSB
      ~chunksize
      ~channels:(if (Fmt.channels ()) = 2 then Sdlmixer.STEREO else Sdlmixer.MONO)
      ();
    *)
    if video_channels > 0 then begin
      Sdlevent.enable_events Sdlevent.quit_mask ;
      surface <-
        Some (Sdlvideo.set_video_mode ~w:video_width ~h:video_height
                                      ~bpp:16 [`DOUBLEBUF])
    end ;
    sleep <- false

  method output_send buf =
    if Sdlevent.poll () = Some Sdlevent.QUIT then Tutils.shutdown () ;
    if video_channels > 0 then
      let surface = Utils.get_some surface in
      let rgb = VFrame.get_rgb buf in
      let frame = 0 in (* we only display the first frame... *)
      let bmp = RGB.to_bmp rgb.(0).(frame) in
      let sbmp = Sdlvideo.load_BMP_from_mem bmp in
        Sdlvideo.blit_surface ~src:sbmp ~dst:surface ();
        (*
        for x = 0 to video_width - 1 do
          for y = 0 to video_height - 1 do
            Sdlvideo.put_pixel_color surface ~x ~y (RGB.get rgb.(0).(frame) x y)
          done
        done;
        *)
        Sdlvideo.flip surface
    (* Sdlmixer is not binded for raw support.
     * This API is meant for playing sounds,
     * it generates horribles ticks when 
     * used like this.. Hence disabled for now.. *)
    (*
    let audio = AFrame.to_s16le buf in
    let header =
      Wav.header ~len:(String.length audio) ~channels:(Fmt.channels ())
                 ~sample_rate:(Fmt.samples_per_second ())
                 ~sample_size:16
                 ~big_endian:false ~signed:true ()
    in
    let chunk = Sdlmixer.load_string (header^audio) in
    Sdlmixer.play_sound chunk ()
    *)

  method output_reset = ()
end

let () =
  Lang.add_operator "output.sdl"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Display a video using SDL."
    (fun p ->
       let start = Lang.to_bool (List.assoc "start" p) in
       let source = List.assoc "" p in
         ((new output source start):>Source.source))
