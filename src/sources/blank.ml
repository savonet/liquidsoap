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

class blank ~kind duration =
  let ticks = if duration < 0. then -1 else Frame.master_of_seconds duration in
  object
    inherit source ~name:"blank" kind

    (** Remaining time, -1 for infinity. *)
    val mutable remaining = ticks

    method remaining = remaining

    method stype = Infallible

    method is_ready = true

    method self_sync = false

    method seek x = x

    method abort_track = remaining <- 0

    method get_frame ab =
      let position = Frame.position ab in
      let length =
        if remaining < 0 then Lazy.force Frame.size - position
        else min remaining (Lazy.force Frame.size - position)
      in
      let content = ab.Frame.content in
      let video_pos = Frame.video_of_master position in
      (* Audio *)
      Audio.clear
        (Audio.sub content.Frame.audio
           (Frame.audio_of_master position)
           (Frame.audio_of_master length));

      (* Video *)
      Array.iter
        (fun a -> Video.blank a video_pos (Frame.video_of_master length))
        content.Frame.video;
      Frame.add_break ab (position + length);
      if Frame.is_partial ab then remaining <- ticks
      else if remaining > 0 then remaining <- remaining - length
  end

let () =
  Lang.add_operator "blank" ~category:Lang.Input
    ~descr:"Produce silence and blank images."
    ~kind:(Lang.Unconstrained (Lang.univ_t ()))
    [
      ( "duration",
        Lang.float_t,
        Some (Lang.float (-1.)),
        Some
          "Duration of blank tracks in seconds, Negative value means forever."
      );
    ]
    (fun p kind ->
      let d = Lang.to_float (List.assoc "duration" p) in
      (new blank ~kind d :> source))

class empty ~kind =
  object
    inherit source ~name:"empty" kind

    method stype = Fallible

    method is_ready = false

    method self_sync = false

    method remaining = 0

    method abort_track = ()

    method get_frame _ = assert false
  end

let empty kind = (new empty ~kind :> source)

let () =
  let audio = Lang.univ_t () in
  let video = Lang.univ_t () in
  let midi = Lang.univ_t () in
  Lang.add_operator "empty" ~category:Lang.Input
    ~descr:
      "A source that does not produce anything. No silence, no track at all."
    ~kind:(Lang.Unconstrained (Lang.frame_kind_t ~audio ~video ~midi))
    []
    (fun _ kind -> (new empty ~kind :> source))
