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

open Source

class map_subtitles ~field (source : source) callback =
  object
    inherit operator ~name:"track.subtitles.map" [source]
    val mutable stream_position = 0
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method effective_source = source#effective_source
    method self_sync = source#self_sync

    method private generate_frame =
      let buf = source#get_frame in
      let length = Frame.position buf in
      let content = Frame.get buf field in
      let subtitles = Subtitle_content.get_data content in
      let mapped =
        List.filter_map
          (fun (pos, (sub : Subtitle_content.subtitle)) ->
            let position = stream_position + pos in
            let absolute_start = position + sub.start_time in
            let absolute_end = position + sub.end_time in
            let v =
              Lang.record
                [
                  ("position", Lang.int position);
                  ("start_time", Lang.int sub.start_time);
                  ("end_time", Lang.int sub.end_time);
                  ( "absolute_start_time",
                    Lang.float (Frame.seconds_of_main absolute_start) );
                  ( "absolute_end_time",
                    Lang.float (Frame.seconds_of_main absolute_end) );
                  ("text", Lang.string sub.text);
                  ( "format",
                    Lang.string
                      (match sub.format with `Ass -> "ass" | `Text -> "text") );
                  ("forced", Lang.bool sub.forced);
                ]
            in
            match Lang.to_option (Lang.apply callback [("", v)]) with
              | None -> None
              | Some result ->
                  let result, _ = Lang.split_meths result in
                  let text =
                    match List.assoc_opt "text" result with
                      | Some v -> Lang.to_string v
                      | None -> sub.text
                  in
                  let format =
                    match List.assoc_opt "format" result with
                      | Some v -> (
                          match Lang.to_string v with
                            | "ass" -> `Ass
                            | _ -> `Text)
                      | None -> sub.format
                  in
                  let forced =
                    match List.assoc_opt "forced" result with
                      | Some v -> Lang.to_bool v
                      | None -> sub.forced
                  in
                  let new_sub : Subtitle_content.subtitle =
                    { sub with text; format; forced }
                  in
                  Some (pos, new_sub))
          subtitles
      in
      stream_position <- stream_position + length;
      if mapped <> subtitles then (
        let data = Subtitle_content.lift_data ~length mapped in
        Frame.set buf field data)
      else buf
  end

let subtitle_out_t =
  Lang.optional_record_t
    [
      ("text", Lang.string_t); ("format", Lang.string_t); ("forced", Lang.bool_t);
    ]

let _ =
  let frame_t = Format_type.subtitle () in
  Lang.add_track_operator ~base:Modules.track_subtitles "map"
    [
      ( "",
        Lang.fun_t
          [(false, "", On_subtitle.subtitle_t)]
          (Lang.nullable_t subtitle_out_t),
        None,
        Some
          "Callback function called on each subtitle. Returns a record with \
           optional fields to update (text, format, forced), or null to remove \
           the subtitle." );
      ("", frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Track
    ~descr:"Map a function over each subtitle in the track."
    (fun p ->
      let callback = Lang.assoc "" 1 p in
      let field, s = Lang.to_track (Lang.assoc "" 2 p) in
      (field, new map_subtitles ~field s callback))
