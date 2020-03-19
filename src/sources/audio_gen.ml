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

(** Generate a square *)

open Source

class gen ~kind ~seek name g freq duration ampl =
  let g = g freq ampl in
  object
    inherit Synthesized.source ~seek ~name kind duration

    method private synthesize frame off len =
      let off = Frame.audio_of_master off in
      let len = Frame.audio_of_master len in
      let buf = AFrame.content frame in
      g#fill (Audio.sub buf off len)
  end

let add name g =
  Lang.add_operator name ~category:Lang.Input
    ~descr:("Generate a " ^ name ^ " wave.")
    ~kind:Lang.audio_any
    [
      ( "duration",
        Lang.float_t,
        Some (Lang.float 0.),
        Some "Duration in seconds (0. means infinite)." );
      ( "amplitude",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Maximal value of the waveform." );
      ( "",
        Lang.float_t,
        Some (Lang.float 440.),
        Some ("Frequency of the " ^ name ^ ".") );
    ]
    (fun p kind ->
      ( new gen
          ~seek:true ~kind name g
          (Lang.to_float (List.assoc "" p))
          (Lang.to_float (List.assoc "duration" p))
          (Lang.to_float (List.assoc "amplitude" p))
        :> source ))

let sine f volume =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.sine (Lazy.force Frame.audio_rate) ~volume f)

let square f volume =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.square (Lazy.force Frame.audio_rate) ~volume f)

let saw f volume =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.saw (Lazy.force Frame.audio_rate) ~volume f)

let () =
  add "sine" sine;
  add "square" square;
  add "saw" saw
