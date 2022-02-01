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

class synth ~kind (synth : Synth.synth) (source : source) chan volume =
  object (self)
    inherit operator ~name:"synth" kind [source]
    initializer synth#set_volume volume
    method stype = source#stype
    method self_sync = source#self_sync
    method remaining = source#remaining
    method is_ready = source#is_ready
    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      let midi = MFrame.midi buf in
      if chan >= Array.length midi then (
        self#log#important
          "Cannot read MIDI channel %d, stream only has %d channels." chan
          (Array.length midi);
        source#get buf)
      else (
        let evs = midi.(chan) in
        source#get buf;
        let b = AFrame.pcm buf in
        let position = AFrame.position buf in
        let len = position - offset in
        synth#play evs offset (Audio.sub b offset len))
  end

let register obj name descr =
  let kind =
    { Frame.audio = Frame.audio_mono; video = `Any; midi = Frame.midi_n 1 }
  in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator ("synth." ^ name)
    [
      ("channel", Lang.int_t, Some (Lang.int 0), Some "MIDI channel to handle.");
      ("volume", Lang.float_t, Some (Lang.float 0.3), Some "Initial volume.");
      ("envelope", Lang.bool_t, Some (Lang.bool true), Some "Use envelope.");
      ( "attack",
        Lang.float_t,
        Some (Lang.float 0.02),
        Some "Envelope attack (in seconds)." );
      ( "decay",
        Lang.float_t,
        Some (Lang.float 0.01),
        Some "Envelope decay (in seconds)." );
      ( "sustain",
        Lang.float_t,
        Some (Lang.float 0.9),
        Some "Envelope sustain level." );
      ( "release",
        Lang.float_t,
        Some (Lang.float 0.05),
        Some "Envelope release (in seconds)." );
      ("", Lang.source_t k, None, None);
    ]
    ~return_t:k ~category:`Synthesis ~descr
    (fun p ->
      let f v = List.assoc v p in
      let chan = Lang.to_int (f "channel") in
      let volume = Lang.to_float (f "volume") in
      let adsr =
        ( Lang.to_float (f "attack"),
          Lang.to_float (f "decay"),
          Lang.to_float (f "sustain"),
          Lang.to_float (f "release") )
      in
      let adsr =
        if Lang.to_bool (f "envelope") then
          Some (Audio.Mono.Effect.ADSR.make (Lazy.force Frame.audio_rate) adsr)
        else None
      in
      let src = Lang.to_source (f "") in
      let kind = Kind.of_kind kind in
      (new synth ~kind (obj adsr) src chan volume :> Source.source));
  let kind =
    { Frame.audio = Frame.audio_mono; video = `Any; midi = Frame.midi_n 16 }
  in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator ("synth.all." ^ name)
    [
      ("envelope", Lang.bool_t, Some (Lang.bool true), Some "Use envelope.");
      ( "attack",
        Lang.float_t,
        Some (Lang.float 0.02),
        Some "Envelope attack (in seconds)." );
      ( "decay",
        Lang.float_t,
        Some (Lang.float 0.01),
        Some "Envelope decay (in seconds)." );
      ( "sustain",
        Lang.float_t,
        Some (Lang.float 0.9),
        Some "Envelope sustain level." );
      ( "release",
        Lang.float_t,
        Some (Lang.float 0.01),
        Some "Envelope release (in seconds)." );
      ("", Lang.source_t k, None, None);
    ]
    ~return_t:k ~category:`Synthesis
    ~descr:(descr ^ " It creates one synthesizer for each channel.")
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let adsr =
        ( Lang.to_float (f "attack"),
          Lang.to_float (f "decay"),
          Lang.to_float (f "sustain"),
          Lang.to_float (f "release") )
      in
      let adsr =
        if Lang.to_bool (f "envelope") then
          Some (Audio.Mono.Effect.ADSR.make (Lazy.force Frame.audio_rate) adsr)
        else None
      in
      let synths =
        Array.init (Lazy.force Frame.midi_channels) (fun c ->
            ( (fun () -> 1.),
              let kind = Kind.of_kind kind in
              new synth ~kind (obj adsr) src c 1. ))
      in
      let synths = Array.to_list synths in
      let kind = Kind.of_kind kind in
      (new Add.add
         ~kind
         ~renorm:(fun () -> false)
         ~power:(fun () -> false)
         synths
         (fun _ -> ())
         (* TODO: restor *)
         (* (fun _ buf tmp -> Video.Image.add buf tmp) *)
           (fun _ _ _ -> ())
        :> Source.source))

let () =
  register
    (fun adsr -> new Synth.sine ?adsr (Lazy.force Frame.audio_rate))
    "sine" "Sine synthesizer."

let () =
  register
    (fun adsr -> new Synth.square ?adsr (Lazy.force Frame.audio_rate))
    "square" "Square synthesizer."

let () =
  register
    (fun adsr -> new Synth.saw ?adsr (Lazy.force Frame.audio_rate))
    "saw" "Saw synthesizer."

(*
let () = register (fun adsr -> (new Synth.hammond ?adsr (* [|4.; 6.; 8.; 3.; 6.; 4.; 8.; 7.; 6.|] *) [|0.; 0.; 6.; 5.; 4.; 5.; 4.; 5.; 6.|] :> Synth.synth)) "hammond" "Hammond synthsizer."
*)
