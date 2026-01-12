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
open Source

(* See http://en.wikipedia.org/wiki/Comb_filter *)

class comb ~field (source : source) delay feedback =
  let past_len = Frame.audio_of_seconds delay in
  object (self)
    inherit operator ~name:"comb" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    val mutable past = Audio.make 0 0 0.

    initializer
      self#on_wake_up (fun () ->
          past <- Audio.make self#audio_channels past_len 0.)

    val mutable past_pos = 0

    method private generate_frame =
      let b = Content.Audio.get_data (source#get_mutable_content field) in
      let position = source#frame_audio_position in
      let feedback = feedback () in
      for i = 0 to position - 1 do
        for c = 0 to Array.length b - 1 do
          let oldin = b.(c).(i) in
          b.(c).(i) <- b.(c).(i) +. (past.(c).(past_pos) *. feedback);
          past.(c).(past_pos) <- oldin
        done;
        past_pos <- (past_pos + 1) mod past_len
      done;
      source#set_frame_data field Content.Audio.lift_data b
  end

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "comb"
    [
      ("delay", Lang.float_t, Some (Lang.float 0.001), Some "Delay in seconds.");
      ( "feedback",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-6.)),
        Some "Feedback coefficient in dB." );
      ("", frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"Comb filter."
    (fun p ->
      let f v = List.assoc v p in
      let duration, feedback, (field, src) =
        ( Lang.to_float (f "delay"),
          Lang.to_float_getter (f "feedback"),
          Lang.to_track (f "") )
      in
      ( field,
        new comb ~field src duration (fun () -> Audio.lin_of_dB (feedback ()))
      ))
