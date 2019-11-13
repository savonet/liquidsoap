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

class biquad ~kind (source : source) filter_type freq fparam db_gain =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samplerate = Frame.audio_of_seconds 1. in
  object
    inherit operator ~name:"biquad_filter" kind [source]

    val effect =
      new Audio.Effect.biquad_filter
        channels samplerate filter_type ~gain:db_gain freq fparam

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.content buf offset in
      let pos = AFrame.position buf in
      let len = pos - offset in
      effect#process (Audio.sub b offset len)
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.lowshelf"
    [ ("frequency", Lang.float_t, None, Some "Corner frequency");
      ( "slope",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Shelf slope (dB/octave)" );
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Low shelf biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "slope"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `Low_shelf freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.highshelf"
    [ ("frequency", Lang.float_t, None, Some "Center frequency");
      ( "slope",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Shelf slope (in dB/octave)" );
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"High shelf biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "slope"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `High_shelf freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.low"
    [ ("frequency", Lang.float_t, None, Some "Corner frequency");
      ("q", Lang.float_t, Some (Lang.float 1.), Some "Q");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Low pass biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "q"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `Low_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.high"
    [ ("frequency", Lang.float_t, None, Some "Corner frequency");
      ("q", Lang.float_t, Some (Lang.float 1.), Some "Q");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"High pass biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "q"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `High_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.bandpass"
    [ ("frequency", Lang.float_t, None, Some "Center frequency");
      ("q", Lang.float_t, Some (Lang.float 1.), Some "Q");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "q"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `Band_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.allpass"
    [ ("frequency", Lang.float_t, None, Some "Center frequency");
      ( "bandwidth",
        Lang.float_t,
        Some (Lang.float (1. /. 3.)),
        Some "Bandwidth (in octaves)" );
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"All pass biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "bandwidth"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `All_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.notch"
    [ ("frequency", Lang.float_t, None, Some "Center frequency");
      ("q", Lang.float_t, Some (Lang.float 1.), Some "Q");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "q"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `Notch freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.peak"
    [ ("frequency", Lang.float_t, None, Some "Center frequency");
      ("q", Lang.float_t, Some (Lang.float 1.), Some "Q");
      ("gain", Lang.float_t, Some (Lang.float 1.), Some "Gain (in dB)");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Peak EQ biquad filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, param, gain, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "q"),
          Lang.to_float (f "gain"),
          Lang.to_source (f "") )
      in
      new biquad ~kind src `Peaking freq param gain)
