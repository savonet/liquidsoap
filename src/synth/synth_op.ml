(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

open Source

class synth ~kind (synth:Synth.synth) (source:source) chan volume =
object (self)
  inherit operator kind [source] as super

  initializer
    synth#set_volume volume

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = AFrame.position buf in
    let evs = (MFrame.content buf (MFrame.position buf)).(chan) in
    let evs = !evs in
    source#get buf;
    let b = AFrame.content buf offset in
    let position = AFrame.position buf in
    let sps = float (Lazy.force Frame.audio_rate) in
    let rec process evs off =
      match evs with
        | (t,e)::tl ->
            let t = Frame.audio_of_master t in
              synth#synth sps b off (t - off);
              (
                match e with
                  | Midi.Note_on (n, v) ->
                      synth#note_on n v
                  | Midi.Note_off (n, v) ->
                      synth#note_off n v
                  | Midi.Control_change (0x7, v) ->
                      synth#set_volume (float v /. 127.)
                  | _ -> ()
              );
              process tl t
        | [] ->
            synth#synth sps b off (position - off)
    in
      process evs offset
end

let register obj name descr =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ~midi:1 ()) in
  Lang.add_operator ("synth." ^ name)
    [
      "channel", Lang.int_t, Some (Lang.int 0), Some "MIDI channel to handle.";
      "volume", Lang.float_t, Some (Lang.float 0.3), Some "Initial volume.";
      "envelope", Lang.bool_t, Some (Lang.bool true), Some "Use envelope.";
      "attack", Lang.float_t, Some (Lang.float 0.02), Some "Envelope attack (in seconds).";
      "decay", Lang.float_t, Some (Lang.float 0.01), Some "Envelope decay (in seconds).";
      "sustain", Lang.float_t, Some (Lang.float 0.9), Some "Envelope sustain level.";
      "release", Lang.float_t, Some (Lang.float 0.05), Some "Envelope release (in seconds).";
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundSynthesis
    ~descr
    (fun p kind ->
       let f v = List.assoc v p in
       let chan = Mutils.to_chan (f "channel") in
       let volume = Lang.to_float (f "volume") in
       let adsr =
         Lang.to_float (f "attack"),
         Lang.to_float (f "decay"),
         Lang.to_float (f "sustain"),
         Lang.to_float (f "release")
       in
       let adsr = if Lang.to_bool (f "envelope") then Some adsr else None in
       let src = Lang.to_source (f "") in
         new synth ~kind (obj adsr) src chan volume);
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ~midi:16 ()) in
  Lang.add_operator ("synth.all." ^ name)
    [
      "envelope", Lang.bool_t, Some (Lang.bool true), Some "Use envelope.";
      "attack", Lang.float_t, Some (Lang.float 0.02), Some "Envelope attack (in seconds).";
      "decay", Lang.float_t, Some (Lang.float 0.01), Some "Envelope decay (in seconds).";
      "sustain", Lang.float_t, Some (Lang.float 0.9), Some "Envelope sustain level.";
      "release", Lang.float_t, Some (Lang.float 0.01), Some "Envelope release (in seconds).";
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundSynthesis
    ~descr:(descr ^ " It creates one synthesizer for each channel.")
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let adsr =
         Lang.to_float (f "attack"),
         Lang.to_float (f "decay"),
         Lang.to_float (f "sustain"),
         Lang.to_float (f "release")
       in
       let adsr = if Lang.to_bool (f "envelope") then Some adsr else None in
       let synths = Array.init ((Frame.type_of_kind kind).Frame.midi) (fun c -> 1, new synth ~kind (obj adsr) src c 1.) in
       let synths = Array.to_list synths in
         new Add.add ~kind ~renorm:false synths
           (fun _ -> ())
           (fun _ buf tmp -> RGB.add buf tmp)
    )

let () = register (fun adsr -> (new Synth.sine ?adsr () :> Synth.synth)) "sine" "Sine synthesizer."
let () = register (fun adsr -> (new Synth.square ?adsr () :> Synth.synth)) "square" "Square synthesizer."
let () = register (fun adsr -> (new Synth.saw ?adsr () :> Synth.synth)) "saw" "Saw synthesizer."
let () = register (fun adsr -> (new Synth.hammond ?adsr (* [|4.; 6.; 8.; 3.; 6.; 4.; 8.; 7.; 6.|] *) [|0.; 0.; 6.; 5.; 4.; 5.; 4.; 5.; 6.|] :> Synth.synth)) "hammond" "Hammond synthsizer."
