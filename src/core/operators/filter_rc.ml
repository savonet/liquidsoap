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

open Source

type mode = Low_pass | High_pass

class filter (source : source) freq wet mode =
  let rate = float (Lazy.force Frame.audio_rate) in
  let dt = 1. /. rate in
  object (self)
    inherit operator ~name:"filter.rc" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    val mutable prev = [||]
    val mutable prev_in = [||]

    initializer
      self#on_wake_up (fun () ->
          prev <- Array.make self#audio_channels 0.;
          prev_in <- Array.make self#audio_channels 0.)

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let position = source#frame_audio_position in
      let rc = 1. /. freq () in
      let alpha =
        match mode with
          | Low_pass -> dt /. (rc +. dt)
          | High_pass -> rc /. (rc +. dt)
      in
      let alpha' = 1. -. alpha in
      let wet = wet () in
      let wet' = 1. -. wet in
      (match mode with
        | Low_pass ->
            let alpha = dt /. (rc +. dt) in
            for c = 0 to Array.length b - 1 do
              let b_c = b.(c) in
              for i = 0 to position - 1 do
                prev.(c) <- (alpha *. b_c.(i)) +. (alpha' *. prev.(c));
                b_c.(i) <- (wet *. prev.(c)) +. (wet' *. b_c.(i))
              done
            done
        | High_pass ->
            let alpha = dt /. (rc +. dt) in
            for c = 0 to Array.length b - 1 do
              let b_c = b.(c) in
              for i = 0 to position - 1 do
                prev.(c) <- alpha *. (prev.(c) +. b_c.(i) -. prev_in.(c));
                prev_in.(c) <- b_c.(i);
                b_c.(i) <- (wet *. prev.(c)) +. (wet' *. b_c.(i))
              done
            done);
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Filter.filter "rc"
    [
      ("frequency", Lang.getter_t Lang.float_t, None, Some "Cutoff frequency.");
      ( "mode",
        Lang.string_t,
        None,
        Some
          "Available modes are 'low' (for low-pass filter), 'high' (for \
           high-pass filter)." );
      ( "wetness",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some
          "How much of the original signal should be added (1. means only \
           filtered and 0. means only original signal)." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"First-order filter (RC filter)."
    (fun p ->
      let f v = List.assoc v p in
      let freq, wet, mode, src =
        ( Lang.to_float_getter (f "frequency"),
          Lang.to_float_getter (f "wetness"),
          f "mode",
          Lang.to_source (f "") )
      in
      let mode =
        match Lang.to_string mode with
          | "low" -> Low_pass
          | "high" -> High_pass
          | _ -> raise (Error.Invalid_value (mode, "valid values are low|high"))
      in
      (new filter src freq wet mode :> Source.source))
