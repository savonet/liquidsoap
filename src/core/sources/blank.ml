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

class blank duration =
  object (self)
    inherit source ~name:"blank" ()

    val position : [ `New_track | `Elapsed of int ] Atomic.t =
      Atomic.make `New_track

    (** Remaining time, -1 for infinity. *)
    method remaining =
      match (Atomic.get position, duration ()) with
        | `New_track, _ -> 0
        | `Elapsed _, d when d < 0. -> -1
        | `Elapsed e, d -> max 0 (Frame.main_of_seconds d - e)

    method fallible = false
    method private can_generate_frame = true
    method self_sync = (`Static, None)
    method! seek x = x
    method effective_source = (self :> Source.source)
    method abort_track = Atomic.set position `New_track
    val mutable frame = None

    method private make_frame =
      let length = Lazy.force Frame.size in
      let audio_len = Frame.audio_of_main length in
      Frame.Fields.fold
        (fun field format frame ->
          match format with
            | _ when Content.Audio.is_format format ->
                let data =
                  Content.Audio.get_data (Content.make ~length format)
                in
                Audio.clear data 0 audio_len;
                Frame.set_data frame field Content.Audio.lift_data data
            | _ when Content_pcm_s16.is_format format ->
                let data =
                  Content_pcm_s16.get_data (Content.make ~length format)
                in
                Content_pcm_s16.clear data 0 audio_len;
                Frame.set_data frame field Content_pcm_s16.lift_data data
            | _ when Content_pcm_f32.is_format format ->
                let data =
                  Content_pcm_f32.get_data (Content.make ~length format)
                in
                Content_pcm_f32.clear data 0 audio_len;
                Frame.set_data frame field Content_pcm_f32.lift_data data
            | _ when Content.Video.is_format format ->
                let data =
                  self#generate_video ~field
                    ~create:(fun ~pos:_ ~width ~height () ->
                      let img = Video.Canvas.Image.create width height in
                      Video.Canvas.Image.iter Video.Image.blank img)
                    length
                in
                Frame.set_data frame field Content.Video.lift_data data
            | _
              when Content.Metadata.is_format format
                   || Content.Track_marks.is_format format ->
                frame
            | _ -> failwith "Invalid content type!")
        self#content_type
        (Frame.create ~length Frame.Fields.empty)

    method private blank_frame =
      match frame with
        | Some f -> f
        | None ->
            let f = self#make_frame in
            frame <- Some f;
            f

    method generate_frame =
      let frame = self#blank_frame in
      let length = Lazy.force Frame.size in
      match (Atomic.get position, self#remaining) with
        | `New_track, _ ->
            Atomic.set position (`Elapsed length);
            Frame.add_track_mark frame 0
        | `Elapsed d, -1 ->
            Atomic.set position (`Elapsed (d + length));
            frame
        | `Elapsed d, r ->
            if r < length then (
              Atomic.set position (`Elapsed (length - r));
              Frame.add_track_mark frame r)
            else (
              Atomic.set position (`Elapsed (d + length));
              frame)
  end

let blank =
  let return_t = Lang.internal_tracks_t () in
  Lang.add_operator "blank" ~category:`Input
    ~descr:"Produce silence and blank images." ~return_t
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-1.)),
        Some
          "Duration of blank tracks in seconds, Negative value means forever."
      );
    ]
    (fun p ->
      let d = Lang.to_float_getter (List.assoc "duration" p) in
      (new blank d :> source))
