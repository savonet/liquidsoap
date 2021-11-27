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

class soundtouch ~kind source_val rate tempo pitch =
  let abg = Generator.create `Audio in
  let source = Lang.to_source source_val in
  let write_frame_ref = ref (fun _ -> ()) in
  let consumer =
    new Producer_consumer.consumer
      ~write_frame:(fun frame -> !write_frame_ref frame)
      ~name:"soundtouch.consumer" ~kind ~source:source_val ()
  in
  object (self)
    inherit
      operator ~name:"soundtouch" kind [(consumer :> Source.source)] as super

    inherit
      Child_support.base ~check_self_sync:true [source_val] as child_support

    val mutable st = None
    method stype = source#stype
    method self_sync = source#self_sync
    method is_ready = source#is_ready
    method remaining = -1

    method abort_track =
      Generator.add_break abg;
      source#abort_track

    method private write_frame =
      function `Frame databuf -> self#process_frame databuf | `Flush -> ()

    method process_frame databuf =
      let st = Option.get st in
      Soundtouch.set_rate st (rate ());
      Soundtouch.set_tempo st (tempo ());
      Soundtouch.set_pitch st (pitch ());
      let db = AFrame.pcm databuf in
      let db = Audio.sub db 0 (AFrame.position databuf) in
      let db = Audio.interleave db in
      Soundtouch.put_samples_ba st db;
      let available = Soundtouch.get_available_samples st in
      if available > 0 then (
        let tmp =
          Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
            (self#audio_channels * available)
        in
        ignore (Soundtouch.get_samples_ba st tmp);
        let tmp = Audio.deinterleave self#audio_channels tmp in
        Generator.put_audio abg
          (Content.Audio.lift_data tmp)
          0
          (Frame.main_of_audio available));
      if AFrame.is_partial databuf then Generator.add_break abg;

      (* It's almost impossible to know where to add metadata,
       * b/c of tempo so we add then right here. *)
      List.iter
        (fun (_, m) -> Generator.add_metadata abg m)
        (AFrame.get_all_metadata databuf)

    method private get_frame buf =
      while Generator.length abg < Lazy.force Frame.size && source#is_ready do
        self#child_tick
      done;
      needs_tick <- false;
      Generator.fill abg buf

    method wake_up a =
      super#wake_up a;
      st <-
        Some (Soundtouch.make self#audio_channels (Lazy.force Frame.audio_rate));
      self#log#important "Using soundtouch %s."
        (Soundtouch.get_version_string (Option.get st));
      write_frame_ref := self#write_frame

    method before_output =
      super#before_output;
      child_support#before_output

    method private after_output =
      super#after_output;
      child_support#after_output
  end

let () =
  (* TODO: could we keep the video in some cases? *)
  let kind = Lang.audio_pcm in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "soundtouch"
    [
      ("rate", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("tempo", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("pitch", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("", Lang.source_t return_t, None, None);
    ]
    ~category:`Audio ~return_t
    ~descr:"Change the rate, the tempo or the pitch of the sound."
    ~flags:[`Experimental]
    (fun p ->
      let f v = List.assoc v p in
      let rate = Lang.to_float_getter (f "rate") in
      let tempo = Lang.to_float_getter (f "tempo") in
      let pitch = Lang.to_float_getter (f "pitch") in
      let s = f "" in
      let kind = Kind.of_kind kind in
      (new soundtouch ~kind s rate tempo pitch :> Source.source))
