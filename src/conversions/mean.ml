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

open Source

class mean ~kind source =
  let dst_type = Frame.type_of_kind kind in
  let src_type = Frame.type_of_kind source#kind in
  let channels = src_type.Frame.audio in
  let tmp_audio =
    Audio.create channels (Frame.audio_of_master (Lazy.force Frame.size))
  in
  let channels = float channels in
  object (self)
    inherit operator kind [source] ~name:"mean"

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame frame =
      let start = Frame.position frame in
      (* Get the final (mono) content layer first.
      * Doing it now (instead of after the source#get) avoids loosing
      * optimizations where we're supposed to write to a specific layer.
      * It's also useful to perform our own copy-avoiding optimization. *)
      let dst = Frame.content_of_type frame start dst_type in
      let src =
        (* Insert a special content layer with N audio channels
         * but the same video and midi channels, so that those do not
         * necessarily have to be copied -- the blit below will be
         * trivial if source#get does write to our special content layer. *)
        let content = { dst with Frame.audio = tmp_audio } in
        let restore = Frame.hide_contents frame in
        let _ = Frame.content_of_type ~force:content frame start src_type in
        source#get frame;
        let layer_end, src = Frame.content frame start in
        assert (layer_end = Lazy.force Frame.size);
        restore ();
        if src != content then
          self#log#info "Copy-avoiding optimization isn't working!";
        src
      in
      let len = Frame.position frame - start in
      let ( ! ) = Frame.audio_of_master in
      (* Compute the mean of audio channels *)
      for i = !start to !(start + len) - 1 do
        dst.Frame.audio.(0).{i} <-
          Array.fold_left (fun m b -> m +. b.{i}) 0. src.Frame.audio /. channels
      done;
      (* Finally, blit in case src_mono.Frame.midi/video is not already
       * the same as dst.Frame.midi/video. *)
      Frame.blit_content
        { src with Frame.audio = dst.Frame.audio }
        start dst start len
  end

let () =
  let in_kind = Lang.kind_type_of_kind_format Lang.any_fixed in
  let out_kind =
    let { Frame.audio = _; video = v; midi = m } =
      Lang.of_frame_kind_t in_kind
    in
    Lang.frame_kind_t ~audio:(Lang.succ_t Lang.zero_t) ~video:v ~midi:m
  in
  Lang.add_operator "mean"
    [("", Lang.source_t in_kind, None, None)]
    ~kind:(Lang.Unconstrained out_kind) ~category:Lang.Conversions
    ~descr:"Produce mono audio by taking the mean of all audio channels."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      new mean ~kind s)
