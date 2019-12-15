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

open Source

(* TODO: share code with visu.volume. *)

let backpoints = 200
let group_size = 1764
let f_group_size = float group_size

class visu ~kind source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  object (self)
    inherit operator ~name:"video.volume" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    (* Ringbuffer for previous values, with its current position. *)
    val vol = Array.init channels (fun _ -> Array.make backpoints 0.)

    val mutable pos = 0

    (* Another buffer for accumulating RMS over [group_size] samples, with its
     * current position. *)
    val mutable cur_rms = Array.make channels 0.

    val mutable group = 0

    method private add_vol v =
      for c = 0 to channels - 1 do
        cur_rms.(c) <- cur_rms.(c) +. v.(c)
      done;
      group <- (group + 1) mod group_size;
      if group = 0 then (
        for c = 0 to channels - 1 do
          vol.(c).(pos) <- sqrt (cur_rms.(c) /. f_group_size);
          cur_rms.(c) <- 0.
        done;
        pos <- (pos + 1) mod backpoints )

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
        let _, src = Frame.content frame offset in
        let src_type = Frame.type_of_content src in
        let dst_type = { src_type with Frame.video = 1 } in
        let dst = Frame.content_of_type frame offset dst_type in
        (* Reproduce audio data in the new contents. *)
        Audio.blit
          (Audio.sub src.Frame.audio
             (Frame.audio_of_master offset)
             (Frame.audio_of_master len))
          (Audio.sub dst.Frame.audio
             (Frame.audio_of_master offset)
             (Frame.audio_of_master len));
        (* Feed the volume buffer. *)
        let acontent = AFrame.content frame (Frame.audio_of_master offset) in
        for i = Frame.audio_of_master offset to AFrame.position frame - 1 do
          self#add_vol
            (Array.map
               (fun c ->
                 let x = c.{i} in
                 x *. x)
               acontent)
        done;
        (* Fill-in video information. *)
        let volwidth = float width /. float backpoints in
        let volheight = float height /. float channels in
        let buf = dst.Frame.video.(0) in
        let start = Frame.video_of_master offset in
        let stop = start + Frame.video_of_master len in
        let line img c p q =
          let f i j =
            if
              0 <= i
              && i < Image.YUV420.width img
              && 0 <= j
              && j < Image.YUV420.height img
            then Image.YUV420.set_pixel_rgba img i j c
          in
          Image.Draw.line f p q
        in
        for f = start to stop - 1 do
          let buf = Video.get buf f in
          Video.Image.blank buf;
          for i = 0 to channels - 1 do
            let y = int_of_float (volheight *. float i) in
            line buf (90, 90, 90, 0xff) (0, y) (width - 1, y);
            for chan = 0 to channels - 1 do
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
                    - ( chan_height
                      + int_of_float
                          (volheight *. vol.((i + pos) mod backpoints)) )
                    - 1 )
                in
                line buf (0, 0xff, 0, 0xff) !pt0 pt1;
                pt0 := pt1
              done
            done
          done
        done )
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  let fmt =
    {
      Frame.audio = Lang.Any_fixed 1;
      video = Lang.Fixed 1;
      midi = Lang.Fixed 0;
    }
  in
  Lang.add_operator "video.volume"
    [("", Lang.source_t k, None, None)]
    ~kind:(Lang.Constrained fmt) ~category:Lang.Visualization
    ~descr:"Graphical visualization of the sound."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new visu ~kind src)
