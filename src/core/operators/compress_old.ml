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

(** See http://www.musicdsp.org/archive.php?classid=4#169 *)

class compress (source : source) attack release threshold ratio knee rms_window
  gain =
  let samplerate = Lazy.force Frame.audio_rate in
  object (self)
    inherit operator ~name:"compress" [source] as super
    val mutable effect = None

    method private wake_up a =
      super#wake_up a;
      effect <-
        Some
          (new Audio.Effect.compress
             ~attack:(attack ()) ~release:(release ()) ~threshold:(threshold ())
             ~ratio:(ratio ()) ~knee:(knee ()) ~rms_window ~gain:(gain ())
             self#audio_channels samplerate)

    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method self_sync = source#self_sync
    method is_ready = source#is_ready
    method abort_track = source#abort_track

    method private get_frame buf =
      let ofs = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      let pos = AFrame.position buf in
      let len = pos - ofs in
      let effect = Option.get effect in
      effect#set_gain (gain ());
      effect#set_attack (attack ());
      effect#set_release (release ());
      effect#set_threshold (threshold ());
      effect#set_ratio (ratio ());
      effect#set_knee (knee ());
      effect#process b ofs len;

      (* Reset values if it is the end of the track. *)
      if AFrame.is_partial buf then effect#reset
  end

let proto frame_t =
  [
    ( "attack",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 100.),
      Some "Attack time (ms)." );
    ( "release",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 400.),
      Some "Release time (ms)." );
    ( "threshold",
      Lang.getter_t Lang.float_t,
      Some (Lang.float (-10.)),
      Some "Threshold level (dB)." );
    ( "knee",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 1.),
      Some "Knee radius (dB)." );
    ( "window",
      Lang.float_t,
      Some (Lang.float 0.1),
      Some "Window for computing RMS (in sec)." );
    ( "gain",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 0.),
      Some "Additional gain (dB)." );
    ("", Lang.source_t frame_t, None, None);
  ]

let compress p =
  let f v = List.assoc v p in
  let attack, release, threshold, ratio, knee, rmsw, gain, src =
    ( Lang.to_float_getter (f "attack"),
      Lang.to_float_getter (f "release"),
      Lang.to_float_getter (f "threshold"),
      Lang.to_float_getter (f "ratio"),
      Lang.to_float_getter (f "knee"),
      Lang.to_float (f "window"),
      Lang.to_float_getter (f "gain"),
      Lang.to_source (f "") )
  in
  new compress
    src
    (fun () -> attack () /. 1000.)
    (fun () -> release () /. 1000.)
    threshold ratio knee rmsw
    (fun () -> Audio.lin_of_dB (gain ()))

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Compress.compress "old"
    (( "ratio",
       Lang.getter_t Lang.float_t,
       Some (Lang.float 2.),
       Some "Gain reduction ratio (n:1)." )
    :: proto frame_t)
    ~return_t:frame_t ~category:`Audio ~descr:"Compress the signal." compress

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "limit"
    (( "ratio",
       Lang.getter_t Lang.float_t,
       Some (Lang.float 20.),
       Some "Gain reduction ratio (n:1)." )
    :: proto frame_t)
    ~return_t:frame_t ~category:`Audio ~descr:"Limit the signal." compress
