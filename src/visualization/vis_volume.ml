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

(** VU meter. *)

open Source

let backpoints = 200

let group_size = 1764

let f_group_size = float group_size

class vumeter ~kind source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  object (self)
    inherit operator ~name:"visu.volume" kind [source] as super

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private wake_up act =
      super#wake_up act ;
      Graphics.open_graph "" ;
      Graphics.set_window_title "Liquidsoap's volume" ;
      Graphics.auto_synchronize false

    method private sleep = super#sleep ; Graphics.close_graph ()

    (* Ringbuffer for previous values, with its current position *)
    val vol = Array.init channels (fun _ -> Array.make backpoints 0.)

    val mutable pos = 0

    (* Another buffer for accumulating RMS over [group_size] samples,
     * with its current position. *)
    val mutable cur_rms = Array.make channels 0.

    val mutable group = 0

    method private add_vol v =
      for c = 0 to channels - 1 do
        cur_rms.(c) <- cur_rms.(c) +. v.(c)
      done ;
      group <- (group + 1) mod group_size ;
      if group = 0 then (
        for c = 0 to channels - 1 do
          vol.(c).(pos) <- sqrt (cur_rms.(c) /. f_group_size) ;
          cur_rms.(c) <- 0.
        done ;
        pos <- (pos + 1) mod backpoints )

    method private get_frame buf =
      let offset = AFrame.position buf in
      let end_pos = source#get buf ; AFrame.position buf in
      if offset < end_pos then (
        let content = AFrame.content buf offset in
        for i = offset to AFrame.position buf - 1 do
          self#add_vol
            (Array.map
               (fun c ->
                 let x = c.{i} in
                 x *. x)
               content)
        done ;
        let volwidth = float (Graphics.size_x ()) /. float backpoints in
        let volheight = float (Graphics.size_y ()) /. float channels in
        Graphics.clear_graph () ;
        Graphics.set_color Graphics.black ;
        for i = 0 to channels - 1 do
          let y = int_of_float (volheight *. float i) in
          Graphics.moveto 0 y ;
          Graphics.lineto (Graphics.size_x () - 1) y
        done ;
        Graphics.set_color Graphics.red ;
        for chan = 0 to channels - 1 do
          let vol = vol.(chan) in
          let chan_height = int_of_float (volheight *. float chan) in
          let x0 = 0 in
          let y0 = int_of_float (volheight *. vol.(pos)) + chan_height in
          Graphics.moveto x0 y0 ;
          for i = 1 to backpoints - 1 do
            Graphics.lineto
              (int_of_float (volwidth *. float i))
              ( chan_height
              + int_of_float (volheight *. vol.((i + pos) mod backpoints)) )
          done
        done ;
        Graphics.synchronize () )
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "visu.volume"
    [("", Lang.source_t k, None, None)]
    ~kind:(Lang.Unconstrained k) ~category:Lang.Visualization
    ~descr:"Graphical visualization of the volume."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new vumeter ~kind src)
