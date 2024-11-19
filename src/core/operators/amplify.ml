(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let parse_db s =
  try Scanf.sscanf s " %f dB" Audio.lin_of_dB with _ -> float_of_string s

class amplify ~field ~override_field (source : source) coeff =
  object (self)
    inherit operator ~name:"track.audio.amplify" [source]
    val mutable override = None
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek_source = source#seek_source
    method self_sync = source#self_sync

    method private amplify k c offset len =
      match Content.format c with
        | f when Content.Audio.is_format f ->
            let data = Content.Audio.get_data c in
            Audio.amplify k data offset len
        | f when Content_pcm_s16.is_format f ->
            let data = Content_pcm_s16.get_data c in
            Content_pcm_s16.amplify
              (Array.map (fun c -> Bigarray.Array1.sub c offset len) data)
              k
        | f when Content_pcm_f32.is_format f ->
            let data = Content_pcm_f32.get_data c in
            Content_pcm_f32.amplify
              (Array.map (fun c -> Bigarray.Array1.sub c offset len) data)
              k
        | _ -> assert false

    method private process buf =
      let k = match override with Some o -> o | None -> coeff () in
      if k <> 1. then (
        let content = Frame.get buf field in
        self#amplify k content 0 (Frame.audio_of_main (Content.length content));
        Frame.set buf field content)
      else buf

    method private set_override buf =
      match Option.map (fun f -> f ()) override_field with
        | Some f ->
            if override <> None then
              self#log#info "End of the current overriding.";
            override <- None;
            List.iter
              (fun (_, m) ->
                try
                  let k = parse_db (Frame.Metadata.find f m) in
                  self#log#info "Overriding amplification: %f." k;
                  override <- Some k
                with _ -> ())
              (Frame.get_all_metadata buf)
        | _ -> ()

    method private generate_frame =
      let buf = source#get_mutable_frame field in
      match self#split_frame buf with
        | buf, None -> self#process buf
        | buf, Some new_track ->
            let buf = self#process buf in
            self#set_override new_track;
            Frame.append buf (self#process new_track)
  end

let _ =
  let frame_t = Lang.pcm_audio_t () in
  Lang.add_track_operator ~base:Modules.track_audio "amplify"
    [
      ( "override",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        Some (Lang.string "liq_amplify"),
        Some
          "Specify the name of a metadata field that, when present and \
           well-formed, overrides the amplification factor for the current \
           track. Well-formed values are floats in decimal notation (e.g. \
           `0.7`) which are taken as normal/linear multiplicative factors; \
           values can be passed in decibels with the suffix `dB` (e.g. `-8.2 \
           dB`, but the spaces do not matter). Defaults to \
           `settings.amplify.metadata`. Set to `null` to disable." );
      ("", Lang.getter_t Lang.float_t, None, Some "Multiplicative factor.");
      ("", frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio
    ~descr:"Multiply the amplitude of the signal."
    (fun p ->
      let c = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let field, s = Lang.to_track (Lang.assoc "" 2 p) in
      let override_field =
        Lang.to_valued_option Lang.to_string_getter (Lang.assoc "override" 1 p)
      in
      (field, new amplify ~field ~override_field s c))
