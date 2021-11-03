(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

class resample ~kind ~ratio source_val =
  let source = Lang.to_source source_val in
  let write_frame_ref = ref (fun _ -> ()) in
  let consumer =
    new Producer_consumer.consumer
      ~write_frame:(fun frame -> !write_frame_ref frame)
      ~name:"stretch.consumer" ~kind ~source:source_val ()
  in
  let generator = Generator.create `Audio in
  object (self)
    inherit operator ~name:"stretch" kind [(consumer :> Source.source)] as super

    inherit
      Child_support.base ~check_self_sync:true [source_val] as child_support

    method self_sync = source#self_sync
    method stype = source#stype

    method remaining =
      let rem = source#remaining in
      if rem = -1 then rem
      else int_of_float (float (rem + Generator.length generator) *. ratio ())

    method abort_track = source#abort_track
    method is_ready = source#is_ready
    val mutable converter = None

    method wake_up a =
      super#wake_up a;
      converter <- Some (Audio_converter.Samplerate.create self#audio_channels);
      write_frame_ref := self#write_frame

    method before_output =
      super#before_output;
      child_support#before_output

    method after_output =
      super#after_output;
      child_support#after_output

    method private write_frame =
      function `Frame frame -> self#process_frame frame | `Flush -> ()

    method private process_frame frame =
      let ratio = ratio () in
      let content, len =
        let content = AFrame.pcm frame in
        let content = Audio.sub content 0 (AFrame.position frame) in
        let converter = Option.get converter in
        let pcm = Audio_converter.Samplerate.resample converter ratio content in
        (Content.Audio.lift_data pcm, Audio.length pcm)
      in
      let convert x = int_of_float (float x *. ratio) in
      Generator.put_audio generator content 0 len;
      List.iter
        (fun (i, m) -> Generator.add_metadata ~pos:(convert i) generator m)
        (Frame.get_all_metadata frame);
      if Frame.is_partial frame then Generator.add_break generator

    method private get_frame frame =
      while
        Generator.length generator < Lazy.force Frame.size && source#is_ready
      do
        self#child_tick
      done;
      needs_tick <- false;
      Generator.fill generator frame
  end

let () =
  let kind = Lang.audio_pcm in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "stretch" (* TODO better name *)
    [
      ( "ratio",
        Lang.getter_t Lang.float_t,
        None,
        Some "A value higher than 1 means slowing down." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Audio
    ~descr:
      "Slow down or accelerate an audio stream by stretching (sounds lower) or \
       squeezing it (sounds higher)."
    (fun p ->
      let f v = List.assoc v p in
      let src = f "" in
      let ratio = Lang.to_float_getter (f "ratio") in
      let kind = Kind.of_kind kind in
      new resample ~kind ~ratio src)
