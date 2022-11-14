(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
open Liquidsoap_lang.Modules

(* TODO: share code with visu.volume. *)

let backpoints = 200
let group_size = 1764
let f_group_size = float group_size

class visu source =
  object (self)
    inherit operator ~name:"video.volume" [source] as super
    method stype = source#stype
    method is_ready = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    method seek = source#seek

    (* Ringbuffer for previous values, with its current position. *)
    val mutable vol = [||]
    val mutable pos = 0

    (* Another buffer for accumulating RMS over [group_size] samples, with its
     * current position. *)
    val mutable cur_rms = [||]
    val mutable group = 0

    method wake_up a =
      super#wake_up a;
      vol <- Array.init self#audio_channels (fun _ -> Array.make backpoints 0.);
      cur_rms <- Array.make self#audio_channels 0.

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

    method private get_frame frame =
      let offset = Frame.position frame in
      let len =
        source#get frame;
        Frame.position frame - offset
      in
      (* If the data has duration=0 don't do anything as there might
       * not even be a content layer of the right type to look at. *)
      if len > 0 then (
        (* Add a video channel to the frame contents. *)
        let vFrame =
          Frame.(
            create
              (Frame.Fields.make ~video:Content.(default_format Video.kind) ()))
        in
        Frame.set_video frame (VFrame.content vFrame);

        (* Feed the volume buffer. *)
        let acontent = AFrame.pcm frame in
        for i = Frame.audio_of_main offset to AFrame.position frame - 1 do
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
        let buf = VFrame.data frame in
        let start = Frame.video_of_main offset in
        let stop = start + Frame.video_of_main len in
        for f = start to stop - 1 do
          let img = ref (Video.Canvas.Image.create width height) in
          let line c p q =
            img :=
              Video.Canvas.Image.add (Video.Canvas.Image.Draw.line c p q) !img
          in
          for i = 0 to self#audio_channels - 1 do
            let y = int_of_float (volheight *. float i) in
            line (90, 90, 90, 0xff) (0, y) (width - 1, y);
            for chan = 0 to self#audio_channels - 1 do
              let vol = vol.(chan) in
              let chan_height = int_of_float (volheight *. float chan) in
              let x0 = 0 in
              let y0 =
                height
                - (int_of_float (volheight *. vol.(pos)) + chan_height)
                - 1
              in
              let pt0 = ref (x0, y0) in
              for i = 1 to backpoints - 1 do
                let pt1 =
                  ( int_of_float (volwidth *. float i),
                    height
                    - (chan_height
                      + int_of_float
                          (volheight *. vol.((i + pos) mod backpoints)))
                    - 1 )
                in
                line (0, 0xff, 0, 0xff) !pt0 pt1;
                pt0 := pt1
              done
            done
          done;
          Video.Canvas.set buf f !img
        done)
  end

let () =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "video.volume"
    [("", Lang.source_t frame_t, None, None)]
    ~return_t:frame_t ~category:`Visualization
    ~descr:"Graphical visualization of the sound."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      (new visu src :> Source.source))
