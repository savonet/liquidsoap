(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

class drop_video ~kind source =
object
  inherit Source.operator kind [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining
  method seek = source#seek

  method private get_frame frame =
    let start = Frame.position frame in
    let len = source#get frame ; Frame.position frame - start in
    let layer_end,src = Frame.content frame start in
      assert (layer_end = Lazy.force Frame.size) ;
      if Array.length src.Frame.video > 0 then
        let new_type = { (Frame.type_of_content src) with Frame.video = 0 } in
        let dst = Frame.content_of_type frame start new_type in
          for i = 0 to Array.length src.Frame.audio - 1 do
            let (!) = Frame.audio_of_master in
            Audio.Mono.blit
              src.Frame.audio.(i) !start
              dst.Frame.audio.(i) !start
              !len
          done ;
          for i = 0 to Array.length src.Frame.midi - 1 do
            MIDI.blit
              src.Frame.midi.(i) start
              dst.Frame.midi.(i) start
              len
          done
end

let () =
  let input = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  let {Frame.audio=audio;video=video;midi=midi} = Lang.of_frame_kind_t input in
  let output = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi in
  Lang.add_operator "drop_video"
    ~category:Lang.Conversions
    ~descr:"Drop all video channels of a stream."
    ~kind:(Lang.Unconstrained output)
    [ "", Lang.source_t input, None, None ]
    (fun p kind ->
       new drop_video ~kind (Lang.to_source (List.assoc "" p)))

class drop_audio ~kind source =
object
  inherit Source.operator kind [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining
  method seek = source#seek

  method private get_frame frame =
    let start = Frame.position frame in
    let len = source#get frame ; Frame.position frame - start in
    let layer_end,src = Frame.content frame start in
      assert (layer_end = Lazy.force Frame.size) ;
      if Array.length src.Frame.audio > 0 then
        let new_type = { (Frame.type_of_content src) with Frame.audio = 0 } in
        let dst = Frame.content_of_type frame start new_type in
          for i = 0 to Array.length src.Frame.video - 1 do
            let (!) = Frame.video_of_master in
              for j = 0 to !len-1 do
                Image.RGBA32.blit
                  src.Frame.video.(i).(!start+j)
                  dst.Frame.video.(i).(!start+j)
              done
          done ;
          for i = 0 to Array.length src.Frame.midi - 1 do
            MIDI.blit
              src.Frame.midi.(i) start
              dst.Frame.midi.(i) start
              len
          done
end

let () =
  let input = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  let {Frame.audio=audio;video=video;midi=midi} = Lang.of_frame_kind_t input in
  let output = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi in
  Lang.add_operator "drop_audio"
    ~category:Lang.Conversions
    ~descr:"Drop all audio channels of a stream."
    ~kind:(Lang.Unconstrained output)
    [ "", Lang.source_t input, None, None ]
    (fun p kind ->
       new drop_audio ~kind (Lang.to_source (List.assoc "" p)))

class drop_midi ~kind source =
object
  inherit Source.operator kind [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining
  method seek = source#seek

  method private get_frame frame =
    let start = Frame.position frame in
    let len = source#get frame ; Frame.position frame - start in
    let layer_end,src = Frame.content frame start in
      assert (layer_end = Lazy.force Frame.size) ;
      if Array.length src.Frame.midi > 0 then
        let new_type = { (Frame.type_of_content src) with Frame.midi = 0 } in
        let dst = Frame.content_of_type frame start new_type in
          for i = 0 to Array.length src.Frame.audio - 1 do
            let (!) = Frame.audio_of_master in
              Audio.Mono.blit
                src.Frame.audio.(i) !start
                dst.Frame.audio.(i) !start
                !len
          done ;
          for i = 0 to Array.length src.Frame.video - 1 do
            let (!) = Frame.video_of_master in
              for j = 0 to !len-1 do
                Image.RGBA32.blit
                  src.Frame.video.(i).(!start+j)
                  dst.Frame.video.(i).(!start+j)
              done
          done
end

let () =
  let input = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  let {Frame.audio=audio;video=video;midi=midi} = Lang.of_frame_kind_t input in
  let output = Lang.frame_kind_t ~audio ~video ~midi:Lang.zero_t in
  Lang.add_operator "drop_midi"
    ~category:Lang.Conversions
    ~descr:"Drop all midi channels of a stream."
    ~kind:(Lang.Unconstrained output)
    [ "", Lang.source_t input, None, None ]
    (fun p kind ->
       new drop_midi ~kind (Lang.to_source (List.assoc "" p)))
