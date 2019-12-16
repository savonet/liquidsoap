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

(** See http://www.musicdsp.org/archive.php?classid=4#169 *)

class compress ~kind (source : source) attack release threshold ratio knee
  rms_window gain =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samplerate = Lazy.force Frame.audio_rate in
  object
    inherit operator ~name:"compress" kind [source]

    val effect =
      new Audio.Effect.compress
        ~attack:(attack ()) ~release:(release ()) ~threshold:(threshold ())
        ~ratio:(ratio ()) ~knee:(knee ()) ~rms_window ~gain:(gain ()) channels
        samplerate

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let ofs = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf ofs in
      let pos = AFrame.position buf in
      let len = pos - ofs in
      effect#set_gain (gain ());
      effect#set_attack (attack ());
      effect#set_release (release ());
      effect#set_threshold (threshold ());
      effect#set_ratio (ratio ());
      effect#set_knee (knee ());
      effect#process (Audio.sub b ofs len);

      (* Reset values if it is the end of the track. *)
      if AFrame.is_partial buf then effect#reset
  end

(* The five first variables ('a,'b...) are used for getters. *)
let k = Lang.kind_type_of_kind_format Lang.any_fixed

let proto =
  [
    ( "attack",
      Lang.float_getter_t (),
      Some (Lang.float 100.),
      Some "Attack time (ms)." );
    ( "release",
      Lang.float_getter_t (),
      Some (Lang.float 400.),
      Some "Release time (ms)." );
    ( "threshold",
      Lang.float_getter_t (),
      Some (Lang.float (-10.)),
      Some "Threshold level (dB)." );
    ( "knee",
      Lang.float_getter_t (),
      Some (Lang.float 1.),
      Some "Knee radius (dB)." );
    ( "window",
      Lang.float_t,
      Some (Lang.float 0.1),
      Some "Window for computing RMS (in sec)." );
    ( "gain",
      Lang.float_getter_t (),
      Some (Lang.float 0.),
      Some "Additional gain (dB)." );
    ("", Lang.source_t k, None, None);
  ]

let compress p kind =
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
    ~kind src
    (fun () -> attack () /. 1000.)
    (fun () -> release () /. 1000.)
    threshold ratio knee rmsw
    (fun () -> Audio.lin_of_dB (gain ()))

let () =
  Lang.add_operator "compress"
    ( ( "ratio",
        Lang.float_getter_t (),
        Some (Lang.float 2.),
        Some "Gain reduction ratio (n:1)." )
    :: proto )
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Compress the signal." compress;
  Lang.add_operator "limit"
    ( ( "ratio",
        Lang.float_getter_t (),
        Some (Lang.float 20.),
        Some "Gain reduction ratio (n:1)." )
    :: proto )
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Limit the signal." compress
