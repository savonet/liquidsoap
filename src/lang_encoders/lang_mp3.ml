(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

open Lang_values
open Lang_values.Ground
open Lang_encoders

let allowed_bitrates =
  [
    8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160; 192; 224; 256; 320;
  ]

let check_samplerate ?t i =
  lazy
    (let i = Lazy.force i in
     let allowed =
       [8000; 11025; 12000; 16000; 22050; 24000; 32000; 44100; 48000]
     in
     if not (List.mem i allowed) then (
       match t with
         | Some t -> raise (Error (t, "invalid samplerate value"))
         | None -> failwith "invalid samplerate value" );
     i)

let mp3_base_defaults () =
  {
    Mp3_format.stereo = true;
    stereo_mode = Mp3_format.Joint_stereo;
    samplerate = check_samplerate Frame.audio_rate;
    bitrate_control = Mp3_format.CBR 128;
    internal_quality = 2;
    id3v2 = None;
    msg_interval = 0.1;
    msg = "";
  }

let mp3_base f = function
  | "stereo", { term = Ground (Bool b); _ } -> { f with Mp3_format.stereo = b }
  | "mono", { term = Ground (Bool b); _ } ->
      { f with Mp3_format.stereo = not b }
  | "stereo_mode", ({ term = Ground (String m); _ } as t) ->
      let mode =
        match m with
          | "default" -> Mp3_format.Default
          | "joint_stereo" -> Mp3_format.Joint_stereo
          | "stereo" -> Mp3_format.Stereo
          | _ -> raise (Error (t, "invalid stereo mode"))
      in
      { f with Mp3_format.stereo_mode = mode }
  | "internal_quality", ({ term = Ground (Int q); _ } as t) ->
      if q < 0 || q > 9 then
        raise (Error (t, "internal quality must be a value between 0 and 9"));
      { f with Mp3_format.internal_quality = q }
  | "msg_interval", { term = Ground (Float i); _ } ->
      { f with Mp3_format.msg_interval = i }
  | "msg", { term = Ground (String m); _ } -> { f with Mp3_format.msg = m }
  | "samplerate", ({ term = Ground (Int i); _ } as t) ->
      { f with Mp3_format.samplerate = check_samplerate ~t (Lazy.from_val i) }
  | "id3v2", ({ term = Ground (Bool true); _ } as t) -> (
      match !Mp3_format.id3v2_export with
        | None ->
            raise (Error (t, "no id3v2 support available for the mp3 encoder"))
        | Some g -> { f with Mp3_format.id3v2 = Some g } )
  | "id3v2", { term = Ground (Bool false); _ } ->
      { f with Mp3_format.id3v2 = None }
  | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
      { f with Mp3_format.stereo = false }
  | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
      { f with Mp3_format.stereo = true }
  | _, t -> raise (generic_error t)

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
        | "bitrate", ({ term = Ground (Int i); _ } as t) ->
            if not (List.mem i allowed_bitrates) then
              raise (Error (t, "invalid bitrate value"));
            set_bitrate f i | x -> mp3_base f x)
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
        | "quality", ({ term = Ground (Int q); _ } as t) when is_vbr f ->
            if q < 0 || q > 9 then
              raise (Error (t, "quality should be in [0..9]"));
            set_quality f (Some q)
        | "hard_min", { term = Ground (Bool b); _ } -> set_hard_min f (Some b)
        | "bitrate", ({ term = Ground (Int i); _ } as t) ->
            if not (List.mem i allowed_bitrates) then
              raise (Error (t, "invalid bitrate value"));
            set_mean_bitrate f (Some i)
        | "min_bitrate", ({ term = Ground (Int i); _ } as t) ->
            if not (List.mem i allowed_bitrates) then
              raise (Error (t, "invalid bitrate value"));
            set_min_bitrate f (Some i)
        | "max_bitrate", ({ term = Ground (Int i); _ } as t) ->
            if not (List.mem i allowed_bitrates) then
              raise (Error (t, "invalid bitrate value"));
            set_max_bitrate f (Some i) | x -> mp3_base f x)
      default params
  in
  Encoder.MP3 mp3

let make_abr =
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

let make_vbr =
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
