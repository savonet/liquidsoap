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

class echo (source : source) delay feedback ping_pong =
  object (self)
    inherit operator ~name:"echo" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method seek_source = source#seek_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    val mutable effect_ = None

    initializer
      self#on_wake_up (fun () ->
          effect_ <-
            Some
              (Audio.Effect.delay self#audio_channels
                 (Lazy.force Frame.audio_rate)
                 ~ping_pong (delay ()) (feedback ())))

    val mutable past_pos = 0

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let position = source#frame_audio_position in
      let effect_ = Option.get effect_ in
      effect_#set_delay (delay ());
      effect_#set_feedback (feedback ());
      effect_#process b 0 position;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "echo"
    [
      ( "delay",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.5),
        Some "Delay in seconds." );
      ( "feedback",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-6.)),
        Some "Feedback coefficient in dB (negative)." );
      ( "ping_pong",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Use ping-pong delay." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"Add echo."
    (fun p ->
      let f v = List.assoc v p in
      let duration, feedback, pp, src =
        ( Lang.to_float_getter (f "delay"),
          Lang.to_float_getter (f "feedback"),
          Lang.to_bool (f "ping_pong"),
          Lang.to_source (f "") )
      in
      let feedback =
        (* Check the initial value, wrap the getter with a converter. *)
        if feedback () > 0. then
          raise
            (Error.Invalid_value (f "feedback", "feedback should be negative"));
        fun () -> Audio.lin_of_dB (feedback ())
      in
      new echo src duration feedback pp)
