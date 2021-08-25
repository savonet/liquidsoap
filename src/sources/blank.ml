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

class blank ~kind duration =
  let ticks = if duration < 0. then -1 else Frame.main_of_seconds duration in
  object
    inherit source ~name:"blank" kind

    (** Remaining time, -1 for infinity. *)
    val mutable remaining = ticks

    method remaining = remaining
    method stype = Infallible
    method is_ready = true
    method self_sync = (`Static, false)
    method seek x = x
    method abort_track = remaining <- 0

    method get_frame ab =
      let position = Frame.position ab in
      let length =
        if remaining < 0 then Lazy.force Frame.size - position
        else min remaining (Lazy.force Frame.size - position)
      in
      let video_pos = Frame.video_of_main position in
      (* Audio *)
      (try
         Audio.clear
           (Audio.sub (AFrame.pcm ab)
              (Frame.audio_of_main position)
              (Frame.audio_of_main length))
       with Frame_content.Invalid -> ());

      (* Video *)
      (try
         Video.blank (VFrame.yuva420p ab) video_pos (Frame.video_of_main length)
       with Frame_content.Invalid -> ());

      Frame.add_break ab (position + length);
      if Frame.is_partial ab then remaining <- ticks
      else if remaining > 0 then remaining <- remaining - length
  end

let () =
  let kind = Lang.internal in
  let return_t = Lang.kind_type_of_kind_format kind in
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
      let kind = Source.Kind.of_kind kind in
      (new blank ~kind d :> source))
