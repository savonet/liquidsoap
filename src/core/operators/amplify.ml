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
open Source

class amplify ~field (source : source) override_field coeff =
  object (self)
    inherit operator ~name:"track.audio.amplify" [source]
    val mutable override = None
    method stype = source#stype
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek_source = source#seek_source
    method self_sync = source#self_sync

    method private process content =
      let k = match override with Some o -> o | None -> coeff () in
      if k <> 1. then (
        let data = Content.Audio.get_data content in
        Audio.amplify k data 0 (Frame.audio_of_main (Content.length content));
        Content.Audio.lift_data data)
      else content

    method private generate_frame =
      let c = source#get_mutable_field field in
      let track_mark = source#track_mark in
      let c, end_c =
        match track_mark with
          | Some p -> (Content.sub c 0 p, Content.sub c p (Content.length c - p))
          | None -> (c, Content.make ~length:0 (Content.format c))
      in
      let c = self#process c in
      begin
        match (track_mark, override_field) with
          | Some p, Some f ->
              if override <> None then
                self#log#info "End of the current overriding.";
              override <- None;
              List.iter
                (fun (pos, m) ->
                  if p <= pos then (
                    try
                      let s = Frame.Metadata.find f m in
                      let k =
                        try Scanf.sscanf s " %f dB" Audio.lin_of_dB
                        with _ -> float_of_string s
                      in
                      self#log#info "Overriding amplification: %f." k;
                      override <- Some k
                    with _ -> ()))
                source#metadata
          | _ -> ()
      end;
      let end_c = self#process end_c in
      Frame.set source#get_frame field (Content.append c end_c)
  end

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "amplify"
    [
      ("", Lang.getter_t Lang.float_t, None, Some "Multiplicative factor.");
      ( "override",
        Lang.nullable_t Lang.string_t,
        Some (Lang.string "liq_amplify"),
        Some
          "Specify the name of a metadata field that, when present and \
           well-formed, overrides the amplification factor for the current \
           track. Well-formed values are floats in decimal notation (e.g. \
           `0.7`) which are taken as normal/linear multiplicative factors; \
           values can be passed in decibels with the suffix `dB` (e.g. `-8.2 \
           dB`, but the spaces do not matter)." );
      ("", frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio
    ~descr:"Multiply the amplitude of the signal."
    (fun p ->
      let c = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let field, s = Lang.to_track (Lang.assoc "" 2 p) in
      let o = Lang.to_option (Lang.assoc "override" 1 p) in
      let o = Option.map Lang.to_string o in
      (field, new amplify ~field s o c))
