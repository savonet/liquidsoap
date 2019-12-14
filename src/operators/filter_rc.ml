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

type mode = Low_pass | High_pass

class filter ~kind (source : source) rc wet mode =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let rate = float (Lazy.force Frame.audio_rate) in
  let dt = 1. /. rate in
  object
    inherit operator ~name:"filter.rc" kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.content buf offset in
      let position = AFrame.position buf in
      let rc = rc () in
      let alpha =
        match mode with
          | Low_pass ->
              dt /. (rc +. dt)
          | High_pass ->
              rc /. (rc +. dt)
      in
      let alpha' = 1. -. alpha in
      let wet = wet () in
      let wet' = 1. -. wet in
      let prev = Array.make channels 0. in
      let prev_in = Array.make channels 0. in
      match mode with
        | Low_pass ->
            let alpha = dt /. (rc +. dt) in
            for c = 0 to Array.length b - 1 do
              let b_c = b.(c) in
              for i = offset to position - 1 do
                prev.(c) <- (alpha *. b_c.{i}) +. (alpha' *. prev.(c)) ;
                b_c.{i} <- (wet *. prev.(c)) +. (wet' *. b_c.{i})
              done
            done
        | High_pass ->
            let alpha = dt /. (rc +. dt) in
            for c = 0 to Array.length b - 1 do
              let b_c = b.(c) in
              for i = offset to position - 1 do
                prev.(c) <- alpha *. (prev.(c) +. b_c.{i} -. prev_in.(c)) ;
                prev_in.(c) <- b_c.{i} ;
                b_c.{i} <- (wet *. prev.(c)) +. (wet' *. b_c.{i})
              done
            done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.rc"
    [ ("rc", Lang.float_getter_t (), None, Some "Time constant (in seconds).");
      ( "mode",
        Lang.string_t,
        None,
        Some
          "Available modes are 'low' (for low-pass filter), 'high' (for \
           high-pass filter)." );
      ( "wetness",
        Lang.float_getter_t (),
        Some (Lang.float 1.),
        Some
          "How much of the original signal should be added (1. means only \
           filtered and 0. means only original signal)." );
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"First-order filter (RC filter)."
    (fun p kind ->
      let f v = List.assoc v p in
      let rc, wet, mode, src =
        ( Lang.to_float_getter (f "rc"),
          Lang.to_float_getter (f "wetness"),
          f "mode",
          Lang.to_source (f "") )
      in
      let mode =
        match Lang.to_string mode with
          | "low" ->
              Low_pass
          | "high" ->
              High_pass
          | _ ->
              raise
                (Lang_errors.Invalid_value (mode, "valid values are low|high"))
      in
      new filter ~kind src rc wet mode)
