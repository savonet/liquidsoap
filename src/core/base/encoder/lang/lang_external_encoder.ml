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

open Value

let type_of_encoder p =
  let channels = Lang_encoder.channels_of_params p in
  match
    List.find_map (function `Labelled ("video", p) -> Some p | _ -> None) p
  with
    | Some { Term.term = `Bool true } ->
        Encoder.audio_video_type ~pcm_kind:Content.Audio.kind channels
    | Some ({ t = { Type.pos } } as tm) ->
        Lang_encoder.raise_error ~pos
          (Printf.sprintf
             "Invalid value %s for value mode. Only `true` or `false is \
              allowed."
             (Term.to_string tm))
    | _ -> Encoder.audio_type ~pcm_kind:Content.Audio.kind channels

let make params =
  let defaults =
    {
      (* We use a hardcoded value in order not to force the evaluation of the
         number of channels too early, see #933. *)
      External_encoder_format.channels = 2;
      samplerate = Frame.audio_rate;
      video = None;
      header = true;
      restart_on_crash = false;
      restart = External_encoder_format.No_condition;
      process = "";
    }
  in
  let ext =
    List.fold_left
      (fun f -> function
        | `Labelled ("stereo", Value.Bool { value = b; _ }) ->
            { f with External_encoder_format.channels = (if b then 2 else 1) }
        | `Labelled ("mono", Value.Bool { value = b; _ }) ->
            { f with External_encoder_format.channels = (if b then 1 else 2) }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with External_encoder_format.channels = 1 }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with External_encoder_format.channels = 2 }
        | `Labelled ("channels", Value.Int { value = c }) ->
            { f with External_encoder_format.channels = c }
        | `Labelled ("samplerate", Value.Int { value = i; _ }) ->
            { f with External_encoder_format.samplerate = Lazy.from_val i }
        | `Labelled ("video", Value.Bool { value = b; _ }) ->
            let w, h =
              match f.External_encoder_format.video with
                | None -> Frame.video_dimensions ()
                | Some (w, h) -> (w, h)
            in
            {
              f with
              External_encoder_format.video = (if b then Some (w, h) else None);
            }
        | `Labelled ("width", Int { value = w; _ }) ->
            let _, h =
              match f.External_encoder_format.video with
                | None -> Frame.video_dimensions ()
                | Some (w, h) -> (w, h)
            in
            let w = Lazy.from_val w in
            { f with External_encoder_format.video = Some (w, h) }
        | `Labelled ("height", Int { value = h }) ->
            let w, _ =
              match f.External_encoder_format.video with
                | None -> Frame.video_dimensions ()
                | Some (w, h) -> (w, h)
            in
            let h = Lazy.from_val h in
            { f with External_encoder_format.video = Some (w, h) }
        | `Labelled ("header", Bool { value = h }) ->
            { f with External_encoder_format.header = h }
        | `Labelled ("restart_on_crash", Bool { value = h }) ->
            { f with External_encoder_format.restart_on_crash = h }
        | `Anonymous s when String.lowercase_ascii s = "restart_on_metadata" ->
            {
              f with
              External_encoder_format.restart = External_encoder_format.Metadata;
            }
        | `Labelled ("restart_after_delay", Value.Int { value = i; _ }) ->
            {
              f with
              External_encoder_format.restart = External_encoder_format.Delay i;
            }
        | `Labelled ("process", String { value = s; _ }) ->
            { f with External_encoder_format.process = s }
        | `Anonymous s -> { f with External_encoder_format.process = s }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  if ext.External_encoder_format.process = "" then
    raise External_encoder_format.No_process;
  Encoder.External ext

let () = Lang_encoder.register "external" type_of_encoder make
