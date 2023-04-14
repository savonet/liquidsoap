(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** Generate a square *)

open Source

let to_audio = function
  | c when Content.Audio.is_data c -> Content.Audio.get_data c
  | c when Content_pcm_s16.is_data c -> Content_pcm_s16.(to_audio (get_data c))
  | c when Content_pcm_f32.is_data c -> Content_pcm_f32.(to_audio (get_data c))
  | _ -> raise Content.Invalid

class gen ~seek name g freq duration ampl =
  let g = g (freq ()) in
  object
    inherit Synthesized.source ~seek ~name duration

    method private synthesize frame off len =
      let off = Frame.audio_of_main off in
      let len = Frame.audio_of_main len in
      let content = AFrame.content frame in
      let buf = to_audio content in
      g#set_frequency (freq ());
      g#set_volume (ampl ());
      g#fill buf off len;
      match content with
        | _ when Content.Audio.is_data content -> ()
        | _ when Content_pcm_s16.is_data content ->
            let pcm = Content_pcm_s16.get_data content in
            Content_pcm_s16.blit_audio buf off pcm off len;
            Frame.set_audio frame (Content_pcm_s16.lift_data pcm)
        | _ when Content_pcm_f32.is_data content ->
            let pcm = Content_pcm_f32.get_data content in
            Content_pcm_f32.blit_audio buf off pcm off len;
            Frame.set_audio frame (Content_pcm_f32.lift_data pcm)
        | _ -> raise Content.Invalid
  end

let add name g =
  let return_t =
    Lang.frame_t Lang.unit_t (Frame.Fields.make ~audio:(Lang.pcm_audio_t ()) ())
  in
  Lang.add_operator name ~category:`Input
    ~descr:("Generate a " ^ name ^ " wave.")
    ~return_t
    [
      ( "duration",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Duration in seconds (`null` means infinite)." );
      ( "amplitude",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Maximal value of the waveform." );
      ( "",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 440.),
        Some ("Frequency of the " ^ name ^ ".") );
    ]
    (fun p ->
      (new gen
         ~seek:true name g
         (Lang.to_float_getter (List.assoc "" p))
         (Lang.to_valued_option Lang.to_float (List.assoc "duration" p))
         (Lang.to_float_getter (List.assoc "amplitude" p))
        :> source))

let sine f =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.sine (Lazy.force Frame.audio_rate) f)

let square f =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.square (Lazy.force Frame.audio_rate) f)

let saw f =
  new Audio.Generator.of_mono
    (new Audio.Mono.Generator.saw (Lazy.force Frame.audio_rate) f)

let sine = add "sine" sine

let () =
  ignore (add "square" square);
  ignore (add "saw" saw)
