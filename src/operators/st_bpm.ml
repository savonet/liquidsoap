(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

class bpm ~kind (source : source) =
  object (self)
    inherit operator ~name:"bpm" kind [source] as super
    method stype = source#stype
    method is_ready = source#is_ready
    method self_sync = source#self_sync
    method remaining = source#remaining
    method seek = source#seek
    method abort_track = source#abort_track
    val mutable bpm = None

    method wake_up a =
      super#wake_up a;
      bpm <-
        Some
          (Soundtouch.BPM.make
             (Frame_content.Audio.channels_of_format self#ctype.Frame.audio)
             (Lazy.force Frame.audio_rate))

    method private get_frame buf =
      let bpm = Option.get bpm in
      let offset = AFrame.position buf in
      source#get buf;
      let len = AFrame.position buf - offset in
      let buf = AFrame.pcm buf in
      let ibuf = Audio.interleave (Audio.sub buf offset len) in
      Soundtouch.BPM.put_samples_ba bpm ibuf

    method bpm =
      match bpm with Some bpm -> Soundtouch.BPM.get_bpm bpm | None -> 0.
  end

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "bpm"
    [("", Lang.source_t k, None, None)]
    ~return_t:k ~category:`Visualization
    ~descr:
      "Detect the BPM (number of beats per minute). The returned source has a \
       method `bpm`, which can be called to compute it."
    ~meth:
      [
        ( "bpm",
          ([], Lang.fun_t [] Lang.float_t),
          "Compute the current BPM.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#bpm) );
      ]
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      let kind = Kind.of_kind kind in
      new bpm ~kind s)
