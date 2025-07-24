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

class gate ~threshold ~attack ~release ~hold ~range ~window (source : source) =
  object (self)
    inherit operator ~name:"gate" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method seek_source = source#seek_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    (* Position of the gate between 0. and 1. *)
    val mutable gate = 1.
    method gate = gate

    (* Smoothed peak. *)
    val mutable peak = 0.

    (* Current state. *)
    val mutable state = `Open

    (* Time remaining before closing. *)
    val mutable hold_delay =
      int_of_float (hold () *. float (Lazy.force Frame.audio_rate))

    method private generate_frame =
      let buf =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let position = self#frame_audio_position in
      let chans = self#audio_channels in
      let samplerate = float (Lazy.force Frame.audio_rate) in
      let attack = attack () in
      let attack_rate = 1. /. (attack *. samplerate) in
      let release = release () in
      let release_rate = 1. /. (release *. samplerate) in
      let threshold = threshold () in
      let threshold_lin = Audio.lin_of_dB threshold in
      let window_coef = 1. -. exp (-1. /. (window () *. samplerate)) in
      let range = range () in
      let hold = int_of_float (hold () *. samplerate) in
      for i = 0 to position - 1 do
        let x =
          let x = ref 0. in
          for c = 0 to chans - 1 do
            x := max !x (Utils.abs_float buf.(c).(i))
          done;
          peak <- peak +. (window_coef *. (!x -. peak));
          peak
        in
        (match state with
          | `Closed -> if x > threshold_lin then state <- `Opening
          | `Opening ->
              gate <- gate +. attack_rate;
              if gate >= 1. then (
                gate <- 1.;
                hold_delay <- hold;
                state <- `Open)
          | `Open ->
              if x < threshold_lin then
                if hold_delay <= 0 then state <- `Closing
                else hold_delay <- hold_delay - 1
              else hold_delay <- hold
          | `Closing ->
              gate <- gate -. release_rate;
              if x >= threshold_lin then state <- `Opening
              else if gate <= 0. then (
                gate <- 0.;
                state <- `Closed));
        let gain = Audio.lin_of_dB (range *. (1. -. gate)) in
        for c = 0 to chans - 1 do
          buf.(c).(i) <- buf.(c).(i) *. gain
        done
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data buf
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "gate"
    [
      ( "attack",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 10.),
        Some "Time to fully open the gate (ms)." );
      ( "release",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 2000.),
        Some "Time to fully close the gate (ms)." );
      ( "threshold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-30.)),
        Some "Threshold at which the gate will open (dB)." );
      ( "hold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1000.),
        Some "Minimum amount of time the gate stays open (ms)." );
      ( "range",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-30.)),
        Some "Difference between closed and open level (dB)." );
      ( "window",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Duration for computing peak (ms)." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio
    ~descr:
      "Reduce the volume when the stream is silent (typically in order to \
       avoid low intensity noise)."
    ~meth:
      Lang.
        [
          {
            name = "gate";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Position of the gate (0. means closed, 1. means open).";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#gate));
          };
        ]
    (fun p ->
      let threshold = List.assoc "threshold" p |> Lang.to_float_getter in
      let attack = List.assoc "attack" p |> Lang.to_float_getter in
      let attack () = attack () /. 1000. in
      let release = List.assoc "release" p |> Lang.to_float_getter in
      let release () = release () /. 1000. in
      let hold = List.assoc "hold" p |> Lang.to_float_getter in
      let hold () = hold () /. 1000. in
      let range = List.assoc "range" p |> Lang.to_float_getter in
      let window = List.assoc "window" p |> Lang.to_float_getter in
      let window () = window () /. 1000. in
      let src = List.assoc "" p |> Lang.to_source in
      new gate ~threshold ~attack ~release ~hold ~range ~window src)
