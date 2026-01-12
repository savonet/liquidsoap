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
  Encoder.audio_type ~pcm_kind:Content.Audio.kind
    (Lang_encoder.channels_of_params p)

let allowed_bitrates =
  [
    8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160; 192; 224; 256; 320;
  ]

let check_samplerate ~pos i =
  Lazy.from_fun (fun () ->
      let i = Lazy.force i in
      let allowed =
        [8000; 11025; 12000; 16000; 22050; 24000; 32000; 44100; 48000]
      in
      if not (List.mem i allowed) then
        Lang_encoder.raise_error ~pos "invalid samplerate value";
      i)

let mp3_base_defaults () =
  {
    Mp3_format.stereo = true;
    stereo_mode = Mp3_format.Joint_stereo;
    samplerate = check_samplerate ~pos:None Frame.audio_rate;
    bitrate_control = Mp3_format.CBR 128;
    internal_quality = 2;
    id3v2 = None;
  }

let mp3_base f = function
  | `Labelled ("stereo", Value.Bool { value = b; _ }) ->
      { f with Mp3_format.stereo = b }
  | `Labelled ("mono", Value.Bool { value = b; _ }) ->
      { f with Mp3_format.stereo = not b }
  | `Labelled ("stereo_mode", String { value = m; pos }) ->
      let mode =
        match m with
          | "default" -> Mp3_format.Default
          | "joint_stereo" -> Mp3_format.Joint_stereo
          | "stereo" -> Mp3_format.Stereo
          | _ -> Lang_encoder.raise_error ~pos "invalid stereo mode"
      in
      { f with Mp3_format.stereo_mode = mode }
  | `Labelled ("internal_quality", Int { value = q; pos }) ->
      if q < 0 || q > 9 then
        Lang_encoder.raise_error ~pos
          "internal quality must be a value between 0 and 9";
      { f with Mp3_format.internal_quality = q }
  | `Labelled ("samplerate", Value.Int { value = i; pos }) ->
      { f with Mp3_format.samplerate = check_samplerate ~pos (Lazy.from_val i) }
  | `Labelled ("id3v2", Bool { value = true; _ }) ->
      { f with Mp3_format.id3v2 = Some 3 }
  | `Labelled ("id3v2", Bool { value = false; _ }) ->
      { f with Mp3_format.id3v2 = None }
  | `Labelled ("id3v2", Int { value = v }) ->
      { f with Mp3_format.id3v2 = Some v }
  | `Labelled ("id3v2", String { value = "none"; _ }) ->
      { f with Mp3_format.id3v2 = None }
  | `Anonymous s when String.lowercase_ascii s = "mono" ->
      { f with Mp3_format.stereo = false }
  | `Anonymous s when String.lowercase_ascii s = "stereo" ->
      { f with Mp3_format.stereo = true }
  | t -> Lang_encoder.raise_generic_error t

let make_cbr params =
  let defaults =
    {
      (mp3_base_defaults ()) with
      Mp3_format.bitrate_control = Mp3_format.CBR 128;
    }
  in
  let set_bitrate f b =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.CBR _ ->
          { f with Mp3_format.bitrate_control = Mp3_format.CBR b }
      | _ -> assert false
  in
  let mp3 =
    List.fold_left
      (fun f -> function
        | `Labelled ("bitrate", Value.Int { value = i; pos }) ->
            if not (List.mem i allowed_bitrates) then
              Lang_encoder.raise_error ~pos "invalid bitrate value";
            set_bitrate f i
        | x -> mp3_base f x)
      defaults params
  in
  Encoder.MP3 mp3

let make_abr_vbr ~default params =
  let set_min_bitrate f b =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.VBR vbr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.VBR { vbr with Mp3_format.min_bitrate = b };
          }
      | Mp3_format.ABR abr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.ABR { abr with Mp3_format.min_bitrate = b };
          }
      | _ -> assert false
  in
  let set_max_bitrate f b =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.VBR vbr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.VBR { vbr with Mp3_format.max_bitrate = b };
          }
      | Mp3_format.ABR abr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.ABR { abr with Mp3_format.max_bitrate = b };
          }
      | _ -> assert false
  in
  let set_mean_bitrate f b =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.VBR vbr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.VBR { vbr with Mp3_format.mean_bitrate = b };
          }
      | Mp3_format.ABR abr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.ABR { abr with Mp3_format.mean_bitrate = b };
          }
      | _ -> assert false
  in
  let set_hard_min f b =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.VBR vbr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.VBR { vbr with Mp3_format.hard_min = b };
          }
      | Mp3_format.ABR abr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.ABR { abr with Mp3_format.hard_min = b };
          }
      | _ -> assert false
  in
  let set_quality f q =
    match f.Mp3_format.bitrate_control with
      | Mp3_format.VBR vbr ->
          {
            f with
            Mp3_format.bitrate_control =
              Mp3_format.VBR { vbr with Mp3_format.quality = q };
          }
      | _ -> assert false
  in
  let mp3 =
    let is_vbr f =
      match f.Mp3_format.bitrate_control with
        | Mp3_format.VBR _ -> true
        | _ -> false
    in
    List.fold_left
      (fun f -> function
        | `Labelled ("quality", Int { value = q; pos }) when is_vbr f ->
            if q < 0 || q > 9 then
              Lang_encoder.raise_error ~pos "quality should be in [0..9]";
            set_quality f (Some q)
        | `Labelled ("hard_min", Value.Bool { value = b; _ }) ->
            set_hard_min f (Some b)
        | `Labelled ("bitrate", Value.Int { value = i; pos }) ->
            if not (List.mem i allowed_bitrates) then
              Lang_encoder.raise_error ~pos "invalid bitrate value";
            set_mean_bitrate f (Some i)
        | `Labelled ("min_bitrate", Value.Int { value = i; pos }) ->
            if not (List.mem i allowed_bitrates) then
              Lang_encoder.raise_error ~pos "invalid bitrate value";
            set_min_bitrate f (Some i)
        | `Labelled ("max_bitrate", Value.Int { value = i; pos }) ->
            if not (List.mem i allowed_bitrates) then
              Lang_encoder.raise_error ~pos "invalid bitrate value";
            set_max_bitrate f (Some i)
        | x -> mp3_base f x)
      default params
  in
  Encoder.MP3 mp3

let make_abr p =
  make_abr_vbr
    ~default:
      {
        (mp3_base_defaults ()) with
        Mp3_format.bitrate_control =
          Mp3_format.ABR
            {
              Mp3_format.quality = None;
              min_bitrate = None;
              mean_bitrate = Some 128;
              max_bitrate = None;
              hard_min = Some false;
            };
      }
    p

let make_vbr p =
  make_abr_vbr
    ~default:
      {
        (mp3_base_defaults ()) with
        Mp3_format.bitrate_control =
          Mp3_format.VBR
            {
              Mp3_format.quality = Some 4;
              min_bitrate = None;
              mean_bitrate = None;
              max_bitrate = None;
              hard_min = None;
            };
      }
    p

let () =
  Lang_encoder.register "mp3" type_of_encoder make_cbr;
  Lang_encoder.register "mp3.cbr" type_of_encoder make_cbr;
  Lang_encoder.register "mp3.abr" type_of_encoder make_abr;
  Lang_encoder.register "mp3.vbr" type_of_encoder make_vbr
