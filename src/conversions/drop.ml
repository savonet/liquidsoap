(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

class drop ?(audio = false) ?(video = false) ?(midi = false) ~name source =
  let f x = if x then Some Frame.none else None in
  let audio_in = f audio in
  let video_in = f video in
  let midi_in = f midi in
  object
    inherit Source.operator ?audio_in ?video_in ?midi_in Lang.any [source] ~name

    inherit
      Conversion.base
        ~audio ~video ~midi
        ~converter:(fun ~frame:_ _ -> ())
        source
  end

let () =
  List.iter
    (fun content ->
      let input = Lang.kind_type_of_kind_format Lang.any in
      let { Frame.audio; video; midi } = Lang.of_frame_kind_t input in
      let name, descr, output, source =
        match content with
          | `Audio ->
              ( "drop_audio",
                "Drop all audio content of a stream.",
                Lang.frame_kind_t ~audio:(Lang.kind_t Frame.none) ~video ~midi,
                fun p ->
                  let source = Lang.to_source (List.assoc "" p) in
                  new drop ~audio:true ~name:"drop_audio" source )
          | `Video ->
              ( "drop_video",
                "Drop all video content of a stream.",
                Lang.frame_kind_t ~audio ~video:(Lang.kind_t Frame.none) ~midi,
                fun p ->
                  let source = Lang.to_source (List.assoc "" p) in
                  new drop ~video:true ~name:"drop_video" source )
          | `Midi ->
              ( "drop_midi",
                "Drop all midi content of a stream.",
                Lang.frame_kind_t ~audio ~video ~midi:(Lang.kind_t Frame.none),
                fun p ->
                  let source = Lang.to_source (List.assoc "" p) in
                  new drop ~midi:true ~name:"drop_midi" source )
      in
      Lang.add_operator name ~category:Lang.Conversions ~descr ~return_t:output
        [("", Lang.source_t input, None, None)]
        (fun p -> (source p :> Source.source)))
    [`Audio; `Video; `Midi]
