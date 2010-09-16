(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Generate a square *)

open Source

class gen ~kind name g freq duration =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let g = g freq 1. in
object
  inherit Synthesized.source ~name kind duration

  method private synthesize frame off len =
    let off = Frame.audio_of_master off in
    let len = Frame.audio_of_master len in
    let buf = AFrame.content_of_type ~channels frame off in
    g#fill buf off len
end

let add name g =
  Lang.add_operator name
    ~category:Lang.Input
    ~descr:("Generate a " ^ name ^ " wave.")
    ~kind:Lang.audio_any
    [
      "duration", Lang.float_t, Some (Lang.float 0.), None;
      "", Lang.float_t, Some (Lang.float 440.), Some ("Frequency of the " ^ name ^ ".")
    ]
    (fun p kind ->
      (new gen ~kind name g
         (Lang.to_float (List.assoc "" p))
         (Lang.to_float (List.assoc "duration" p)) :> source))

let sine f volume = new Audio.Generator.of_mono (new Audio.Mono.Generator.sine (Lazy.force Frame.audio_rate) ~volume f)
let square f volume = new Audio.Generator.of_mono (new Audio.Mono.Generator.square (Lazy.force Frame.audio_rate) ~volume f)
let saw f volume = new Audio.Generator.of_mono (new Audio.Mono.Generator.saw (Lazy.force Frame.audio_rate) ~volume f)

let () =
  add "sine" sine;
  add "square" square;
  add "saw" saw
