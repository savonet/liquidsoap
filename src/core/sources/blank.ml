(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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
      Atomic.make (`Elapsed 0)

    method remaining =
      match (Atomic.get position, duration ()) with
        | `New_track, _ -> 0
        | `Elapsed _, d when d < 0. -> -1
        | `Elapsed e, d -> max 0 (Frame.main_of_seconds d - e)

    method stype = `Infallible
    method private _is_ready ?frame:_ _ = true
    method self_sync = (`Static, false)
    method! seek x = x
    method seek_source = (self :> Source.source)
    method abort_track = Atomic.set position `New_track

    method get_frame ab =
      match (Atomic.get position, self#remaining) with
        | `New_track, _ -> Frame.add_break ab (Frame.position ab)
        | `Elapsed elapsed, rem ->
            let pos = Frame.position ab in
            let length =
              if rem < 0 then Lazy.force Frame.size - pos
              else min rem (Lazy.force Frame.size - pos)
            in
            let audio_pos = Frame.audio_of_main pos in
            let audio_len = Frame.audio_of_main length in
            let video_pos = Frame.video_of_main pos in
            let video_len = Frame.video_of_main length in

            Frame.Fields.iter
              (fun field typ ->
                match typ with
                  | _ when Content.Audio.is_format typ ->
                      Audio.clear
                        (Content.Audio.get_data (Frame.get ab field))
                        audio_pos audio_len
                  | _ when Content_pcm_s16.is_format typ ->
                      Content_pcm_s16.clear
                        (Content_pcm_s16.get_data (Frame.get ab field))
                        audio_pos audio_len
                  | _ when Content_pcm_f32.is_format typ ->
                      Content_pcm_f32.clear
                        (Content_pcm_f32.get_data (Frame.get ab field))
                        audio_pos audio_len
                  | _ when Content.Video.is_format typ ->
                      Video.Canvas.blank
                        (Content.Video.get_data (Frame.get ab field))
                        video_pos video_len
                  | _ -> failwith "Invalid content type!")
              self#content_type;

            Frame.add_break ab (pos + length);
            Atomic.set position
              (`Elapsed (if Frame.is_partial ab then 0 else elapsed + length))
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
