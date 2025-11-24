(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
open Source

(* TODO: share code with visu.volume. *)

let backpoints = 200
let group_size = 1764
let f_group_size = float group_size

class visu source =
  object (self)
    inherit operator ~name:"video.volume" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    method effective_source = source#effective_source

    (* Ringbuffer for previous values, with its current position. *)
    val mutable vol = [||]
    val mutable pos = 0

    (* Another buffer for accumulating RMS over [group_size] samples, with its
     * current position. *)
    val mutable cur_rms = [||]
    val mutable group = 0

    initializer
      self#on_wake_up (fun () ->
          vol <-
            Array.init self#audio_channels (fun _ -> Array.make backpoints 0.);
          cur_rms <- Array.make self#audio_channels 0.)

    method private add_vol v =
      for c = 0 to self#audio_channels - 1 do
        cur_rms.(c) <- cur_rms.(c) +. v.(c)
      done;
      group <- (group + 1) mod group_size;
      if group = 0 then (
        for c = 0 to self#audio_channels - 1 do
          vol.(c).(pos) <- sqrt (cur_rms.(c) /. f_group_size);
          cur_rms.(c) <- 0.
        done;
        pos <- (pos + 1) mod backpoints)

    method private create_image ~pos:_ ~volwidth ~volheight ~width ~height () =
      let img = ref (Video.Canvas.Image.create width height) in
      let line c p q =
        img := Video.Canvas.Image.add (Video.Canvas.Image.Draw.line c p q) !img
      in
      for i = 0 to self#audio_channels - 1 do
        let y = int_of_float (volheight *. float i) in
        line (90, 90, 90, 0xff) (0, y) (width - 1, y);
        for chan = 0 to self#audio_channels - 1 do
          let vol = vol.(chan) in
          let chan_height = int_of_float (volheight *. float chan) in
          let x0 = 0 in
          let y0 =
            height - (int_of_float (volheight *. vol.(pos)) + chan_height) - 1
          in
          let pt0 = ref (x0, y0) in
          for i = 1 to backpoints - 1 do
            let pt1 =
              ( int_of_float (volwidth *. float i),
                height
                - (chan_height
                  + int_of_float (volheight *. vol.((i + pos) mod backpoints)))
                - 1 )
            in
            line (0, 0xff, 0, 0xff) !pt0 pt1;
            pt0 := pt1
          done
        done
      done;
      !img

    method private generate_frame =
      let frame = source#get_frame in

      (* Add a video channel to the frame contents. *)

      (* Feed the volume buffer. *)
      let acontent = AFrame.pcm frame in
      for i = Frame.audio_of_main 0 to source#frame_audio_position - 1 do
        self#add_vol
          (Array.map
             (fun c ->
               let x = c.(i) in
               x *. x)
             acontent)
      done;

      (* Fill-in video information. *)
      let width, height = self#video_dimensions in
      let volwidth = float width /. float backpoints in
      let volheight = float height /. float self#audio_channels in
      let buf =
        self#generate_video ~field:Frame.Fields.video
          ~create:(self#create_image ~volwidth ~volheight)
          (Frame.position frame)
      in
      Frame.set_data frame Frame.Fields.video Content.Video.lift_data buf
  end

let _ =
  let base_t = Lang.univ_t () in
  let audio_t = Format_type.audio () in
  let frame_t = Lang.frame_t base_t (Frame.Fields.make ~audio:audio_t ()) in
  let video_t = Format_type.video () in
  let return_t =
    Lang.frame_t base_t (Frame.Fields.make ~audio:audio_t ~video:video_t ())
  in
  Lang.add_operator ~base:Modules.video "volume"
    [("", Lang.source_t frame_t, None, None)]
    ~return_t ~category:`Visualization
    ~descr:"Graphical visualization of the sound."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      (new visu src :> Source.source))
