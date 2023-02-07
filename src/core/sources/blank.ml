(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  let ticks = if duration < 0. then -1 else Frame.main_of_seconds duration in
  object (self)
    inherit source ~name:"blank" ()

    (** Remaining time, -1 for infinity. *)
    val mutable remaining = ticks

    method remaining = remaining
    method stype = `Infallible
    method is_ready = true
    method self_sync = (`Static, false)
    method seek x = x
    method abort_track = remaining <- 0

    method get_frame ab =
      let position = Frame.position ab in
      let length =
        if remaining < 0 then Multicore.force Frame.size - position
        else min remaining (Multicore.force Frame.size - position)
      in
      let audio_pos = Frame.audio_of_main position in
      let audio_len = Frame.audio_of_main length in
      let video_pos = Frame.video_of_main position in
      let video_len = Frame.video_of_main length in

      Frame.Fields.iter
        (fun field typ ->
          match typ with
            | _ when Content.Audio.is_format typ ->
                Audio.clear
                  (Content.Audio.get_data (Frame.get ab field))
                  audio_pos audio_len
            | _ when Content.Video.is_format typ ->
                Video.Canvas.blank
                  (Content.Video.get_data (Frame.get ab field))
                  video_pos video_len
            | _ -> failwith "Invalid content type!")
        self#content_type;

      Frame.add_break ab (position + length);
      if Frame.is_partial ab then remaining <- ticks
      else if remaining > 0 then remaining <- remaining - length
  end

let blank =
  let return_t = Lang.internal_t () in
  Lang.add_operator "blank" ~category:`Input
    ~descr:"Produce silence and blank images." ~return_t
    [
      ( "duration",
        Lang.float_t,
        Some (Lang.float (-1.)),
        Some
          "Duration of blank tracks in seconds, Negative value means forever."
      );
    ]
    (fun p ->
      let d = Lang.to_float (List.assoc "duration" p) in
      (new blank d :> source))
