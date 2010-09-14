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

class gen ~kind g freq duration =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let nb_samples = Frame.audio_of_seconds duration in
  let g = g freq 1. in
object
  inherit source kind

  method stype = Infallible
  method is_ready = true

  val mutable remaining = nb_samples
  method remaining = Frame.master_of_audio remaining

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  val mutable pos = 0

  method get_frame ab =
    if must_fail then
      (
        AFrame.add_break ab (AFrame.position ab);
        remaining <- nb_samples;
        must_fail <- false
      )
    else
      let off = AFrame.position ab in
      let buf = AFrame.content_of_type ~channels ab off in
      let size = AFrame.size () in
      g#fill buf off (size - off);
      AFrame.add_break ab size;
      remaining <- remaining - size - off;
      if remaining <= 0 then must_fail <- true
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
      (new gen ~kind g
         (Lang.to_float (List.assoc "" p))
         (Lang.to_float (List.assoc "duration" p)) :> source))

let sine f volume = Audio.Generator.of_mono (Audio.Mono.Generator.sine (Lazy.force Frame.audio_rate) ~volume f)
let square f volume = Audio.Generator.of_mono (Audio.Mono.Generator.square (Lazy.force Frame.audio_rate) ~volume f)
let saw f volume = Audio.Generator.of_mono (Audio.Mono.Generator.saw (Lazy.force Frame.audio_rate) ~volume f)

let () =
  add "sine" sine;
  add "square" square;
  add "saw" saw
