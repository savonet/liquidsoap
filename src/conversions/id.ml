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

class id ~kind (source:source) =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method remaining = source#remaining
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method private get_frame buf = source#get buf
end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "id"
    ["", Lang.source_t kind, None, None]
    ~category:Lang.SoundProcessing
    ~descr:"Does not do anything."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new id ~kind src)

let () =
  let stream_kind = Lang.audio_mono in
  let kind_type = Lang.kind_type_of_kind_format ~fresh:1 stream_kind in
  Lang.add_operator "id.mono"
    ["", Lang.source_t kind_type, None, None]
    ~category:Lang.SoundProcessing
    ~descr:"Does not do anything, \
            but forces the stream type of the input source."
    ~kind:stream_kind
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new id ~kind src)

let () =
  let stream_kind = Lang.audio_stereo in
  let kind_type = Lang.kind_type_of_kind_format ~fresh:1 stream_kind in
  Lang.add_operator "id.stereo"
    ["", Lang.source_t kind_type, None, None]
    ~category:Lang.SoundProcessing
    ~descr:"Does not do anything, \
            but forces the stream type of the input source."
    ~kind:stream_kind
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new id ~kind src)

let () =
  let stream_kind = Lang.video_only in
  let kind_type = Lang.kind_type_of_kind_format ~fresh:1 stream_kind in
  Lang.add_operator "id.video_only"
    ["", Lang.source_t kind_type, None, None]
    ~category:Lang.SoundProcessing
    ~descr:"Does not do anything, \
            but forces the stream type of the input source."
    ~kind:stream_kind
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new id ~kind src)

let () =
  let kind =
    { Frame. audio = Frame.Zero ; video = Frame.Zero ; midi = Frame.Zero }
  in
  let kind_type = Lang.kind_type_of_frame_kind kind in
    Lang.add_operator "id.empty"
      ["", Lang.source_t kind_type, None, None]
      ~category:Lang.SoundProcessing
      ~descr:"Does not do anything, \
              but forces the stream type of the input source."
      ~kind:(Lang.Unconstrained kind_type)
      (fun p kind ->
         let f v = List.assoc v p in
         let src = Lang.to_source (f "") in
           new id ~kind src)
