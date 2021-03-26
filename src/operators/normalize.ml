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

class normalize ~kind ~track_sensitive (source : source) (* RMS target. *) rmst
  (* Number of samples for computing rms. *)
    window (* Spring coefficient when the sound is going louder. *) kup
  (* Spring coefficient when the sound is going less loud. *)
    kdown threshold gmin gmax =
  let rmsi = Frame.audio_of_seconds window in
  object (self)
    inherit operator ~name:"normalize" kind [source] as super

    (** Current squares of RMS. *)
    val mutable rms = [||]

    (** Current number of samples used to compute [rmsl] and [rmsr]. *)
    val mutable rmsc = 0

    (** Volume coefficients. *)
    val mutable v = [||]

    (** Previous volume coefficients. *)
    val mutable vold = [||]

    method wake_up a =
      super#wake_up a;
      let channels = self#audio_channels in
      rms <- Array.make channels 0.;
      v <- Array.make channels 1.;
      vold <- Array.make channels 1.

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      let rmst = rmst () in
      let kup = kup () in
      let kdown = kdown () in
      let threshold = threshold () in
      let gmin = gmin () in
      let gmax = gmax () in
      for i = offset to AFrame.position buf - 1 do
        for c = 0 to self#audio_channels - 1 do
          let bc = b.(c) in
          let x = bc.{i} in
          rms.(c) <- rms.(c) +. (x *. x);
          bc.{i} <-
            x
            *. ((float rmsc *. vold.(c)) +. (float (rmsi - rmsc) *. v.(c)))
            /. float rmsi
        done;
        rmsc <- rmsc + 1;
        if rmsc >= rmsi then (
          (* TODO: adapt this to channels chans. *)
          (*
              begin
                let rmsl = sqrt (rms.(0) /. (float_of_int rmsi)) in
                let rmsr = sqrt (rms.(1) /. (float_of_int rmsi)) in
                  self#log#info
                    "%6.02f  *  %4.02f  ->  %6.02f    |    \
                    %6.02f  *  %4.02f  ->  %6.02f"
                    (Sutils.dB_of_lin rmsl)
                    v.(0)
                    (Sutils.dB_of_lin (rmsl *. v.(0)))
                    (Sutils.dB_of_lin rmsr)
                    v.(1)
                    (Sutils.dB_of_lin (rmsr *. v.(1)))
              end ;
              *)
          (* TODO: do all the computations in dB? *)
          for c = 0 to self#audio_channels - 1 do
            let r = sqrt (rms.(c) /. float_of_int rmsi) in
            if r > threshold then
              if r *. v.(c) > rmst then
                v.(c) <- v.(c) +. (kdown *. ((rmst /. r) -. v.(c)))
              else v.(c) <- v.(c) +. (kup *. ((rmst /. r) -. v.(c)));
            vold.(c) <- v.(c);
            v.(c) <- max gmin (min gmax v.(c));
            rms.(c) <- 0.
          done;
          rmsc <- 0 )
      done;

      (* Reset values if it is the end of the track. *)
      if track_sensitive && AFrame.is_partial buf then (
        for c = 0 to self#audio_channels - 1 do
          vold.(c) <- 1.;
          v.(c) <- 1.;
          rms.(c) <- 0.
        done;
        rmsc <- 0 )
  end

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "normalize"
    [
      ( "target",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-13.)),
        Some "Desired RMS (dB)." );
      ( "window",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Duration of the window used to compute the current RMS power \
           (second)." );
      ( "k_up",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.005),
        Some
          "Coefficient when the power must go up (between 0 and 1, slowest to \
           fastest)." );
      ( "k_down",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Coefficient when the power must go down (between 0 and 1, slowest \
           to fastest)." );
      ( "threshold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-40.)),
        Some "Minimal RMS for activaing gain control (dB)." );
      ( "gain_min",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-6.)),
        Some "Minimal gain value (dB)." );
      ( "gain_max",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 6.),
        Some "Maximal gain value (dB)." );
      ( "track_sensitive",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Reset values on every track." );
      ("", Lang.source_t k, None, None);
    ]
    ~return_t:k ~category:Lang.SoundProcessing
    ~descr:
      "Normalize the signal. Dynamic normalization of the signal is sometimes \
       the only option, and can make a listening experience much nicer. \
       However, its dynamic aspect implies some limitations which can go as \
       far as creating saturation in some extreme cases. If possible, consider \
       using some track-based normalization techniques such as those based on \
       replay gain. See the documentation for more details."
    (fun p ->
      let f v = List.assoc v p in
      let target, window, kup, kdown, threshold, gmin, gmax, src =
        ( Lang.to_float_getter (f "target"),
          Lang.to_float (f "window"),
          Lang.to_float_getter (f "k_up"),
          Lang.to_float_getter (f "k_down"),
          Lang.to_float_getter (f "threshold"),
          Lang.to_float_getter (f "gain_min"),
          Lang.to_float_getter (f "gain_max"),
          Lang.to_source (f "") )
      in
      let track_sensitive = Lang.to_bool (f "track_sensitive") in
      ( new normalize
          ~kind ~track_sensitive src
          (fun () -> Audio.lin_of_dB (target ()))
          window kup kdown
          (fun () -> Audio.lin_of_dB (threshold ()))
          (fun () -> Audio.lin_of_dB (gmin ()))
          (fun () -> Audio.lin_of_dB (gmax ()))
        :> Source.source ))
