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

open Mm
include Content_base

module MkContent (C : ContentSpecs) = struct
  include MkContentBase (C)

  let () =
    Type.register_type (C.string_of_kind C.kind) (fun () ->
        Type.make
          (Type.Custom
             (Format_type.kind_handler
                (lift_kind C.kind, Liquidsoap_lang.Lang.univ_t ()))))
end

include Content_timed

module Audio = struct
  include Content_audio

  type audio_params = Content_audio.Specs.params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
  }
end

module Video = struct
  include Content_video

  type ('a, 'b) video_content = ('a, 'b) Content_video.Base.content = {
    length : int;
    mutable params : 'a;
    mutable data : (int * 'b) list;
  }

  type video_params = Content_video.Specs.params = {
    width : int Lazy.t option;
    height : int Lazy.t option;
  }

  let lift_image img =
    let width = Video.Canvas.Image.width img in
    let height = Video.Canvas.Image.height img in
    lift_data
      {
        length = Frame_settings.main_of_video 1;
        params =
          {
            height = Some (Lazy.from_val height);
            width = Some (Lazy.from_val width);
          };
        data = [(0, img)];
      }

  let get_data content =
    let buf = get_data content in
    {
      buf with
      data = List.stable_sort (fun (p, _) (p', _) -> Int.compare p p') buf.data;
    }

  type generator = {
    interval : int;
    params : params;
    width : int;
    height : int;
    mutable position : int64;
    mutable next_sample : int64;
  }

  let make_generator params =
    let default_width, default_height = Frame_settings.video_dimensions () in
    let width =
      Lazy.force
        (Option.value ~default:default_width params.Content_video.Specs.width)
    in
    let height =
      Lazy.force
        (Option.value ~default:default_height params.Content_video.Specs.height)
    in
    {
      params =
        {
          Content_video.Specs.width = Some (Lazy.from_val width);
          height = Some (Lazy.from_val height);
        };
      width;
      height;
      interval = Frame_settings.main_of_video 1;
      position = 0L;
      next_sample = 0L;
    }

  let generate
      ?(create =
        fun ~pos:_ ~width ~height () ->
          Mm.Video.Canvas.Image.create width height) gen length =
    let initial_pos = gen.position in
    gen.position <- Int64.add gen.position (Int64.of_int length);
    let rec f data pos =
      if length <= pos then List.rev data
      else (
        let data =
          if gen.next_sample <= Int64.add initial_pos (Int64.of_int pos) then (
            gen.next_sample <-
              Int64.add gen.next_sample (Int64.of_int gen.interval);
            (pos, create ~pos ~width:gen.width ~height:gen.height ()) :: data)
          else List.rev data
        in
        f data (pos + gen.interval))
    in
    { params = gen.params; length; data = f [] 0 }
end

module Midi = struct
  include Content_midi

  type midi_params = Content_midi.Specs.params = { channels : int }
end
