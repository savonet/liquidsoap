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

open Mm

(** Generate a square *)

open Source

let get_params = function
  | format when Content.Audio.is_format format -> format
  | format when Content_pcm_s16.is_format format ->
      Content.Audio.lift_params (Content_pcm_s16.get_params format)
  | format when Content_pcm_f32.is_format format ->
      Content.Audio.lift_params (Content_pcm_f32.get_params format)
  | _ -> raise Content.Invalid

let to_content ~format c =
  match format with
    | _ when Content.Audio.is_format format -> Content.Audio.lift_data c
    | _ when Content_pcm_s16.is_format format ->
        Content_pcm_s16.(lift_data (from_audio c))
    | _ when Content_pcm_f32.is_format format ->
        Content_pcm_f32.(lift_data (from_audio c))
    | _ -> raise Content.Invalid

class gen ~seek name g freq duration ampl =
  let g = g (freq ()) in
  object (self)
    inherit Synthesized.source ~seek ~name duration

    method private synthesize length =
      let frame = Frame.create ~length Frame.Fields.empty in
      let format = Frame.Fields.find Frame.Fields.audio self#content_type in
      let content = Content.make ~length (get_params format) in
      let buf = Content.Audio.get_data content in
      g#set_frequency (freq ());
      g#set_volume (ampl ());
      g#fill buf 0 (Frame.audio_of_main length);
      Frame.Fields.add Frame.Fields.audio (to_content ~format buf) frame
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
