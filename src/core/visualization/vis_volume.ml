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

(** VU meter. *)

open Source

let backpoints = 200
let group_size = 1764
let f_group_size = float group_size

class vumeter ~kind source =
  object (self)
    inherit operator ~name:"visu.volume" kind [source] as super
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private sleep =
      super#sleep;
      Graphics.close_graph ()

    (* Ringbuffer for previous values, with its current position *)
    val mutable vol = [||]
    val mutable pos = 0

    (* Another buffer for accumulating RMS over [group_size] samples,
     * with its current position. *)
    val mutable cur_rms = [||]
    val mutable group = 0

    method private wake_up act =
      super#wake_up act;
      vol <- Array.init self#audio_channels (fun _ -> Array.make backpoints 0.);
      cur_rms <- Array.make self#audio_channels 0.;
      Graphics.open_graph "";
      Graphics.set_window_title "Liquidsoap's volume";
      Graphics.auto_synchronize false

    method private add_vol v =
      let channels = self#audio_channels in
      for c = 0 to channels - 1 do
        cur_rms.(c) <- cur_rms.(c) +. v.(c)
      done;
      group <- (group + 1) mod group_size;
      if group = 0 then (
        for c = 0 to channels - 1 do
          vol.(c).(pos) <- sqrt (cur_rms.(c) /. f_group_size);
          cur_rms.(c) <- 0.
        done;
        pos <- (pos + 1) mod backpoints)

    method private get_frame buf =
      let channels = self#audio_channels in
      let offset = AFrame.position buf in
      let end_pos =
        source#get buf;
        AFrame.position buf
      in
      if offset < end_pos then (
        let content = AFrame.pcm buf in
        for i = offset to AFrame.position buf - 1 do
          self#add_vol
            (Array.map
               (fun c ->
                 let x = c.(i) in
                 x *. x)
               content)
        done;
        let volwidth = float (Graphics.size_x ()) /. float backpoints in
        let volheight = float (Graphics.size_y ()) /. float channels in
        Graphics.clear_graph ();
        Graphics.set_color Graphics.black;
        for i = 0 to channels - 1 do
          let y = int_of_float (volheight *. float i) in
          Graphics.moveto 0 y;
          Graphics.lineto (Graphics.size_x () - 1) y
        done;
        Graphics.set_color Graphics.red;
        for chan = 0 to channels - 1 do
          let vol = vol.(chan) in
          let chan_height = int_of_float (volheight *. float chan) in
          let x0 = 0 in
          let y0 = int_of_float (volheight *. vol.(pos)) + chan_height in
          Graphics.moveto x0 y0;
          for i = 1 to backpoints - 1 do
            Graphics.lineto
              (int_of_float (volwidth *. float i))
              (chan_height
              + int_of_float (volheight *. vol.((i + pos) mod backpoints)))
          done
        done;
        Graphics.synchronize ())
  end

let () =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "visu.volume"
    [("", Lang.source_t frame_t, None, None)]
    ~return_t:frame_t ~category:`Visualization
    ~descr:"Graphical visualization of the volume."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let kind = Lang.frame_kind_t kind in
      new vumeter ~kind src)
