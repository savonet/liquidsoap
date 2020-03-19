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

class normalize ~kind (source : source) (* RMS target. *) rmst
  (* Number of samples for computing rms. *)
    window (* Spring coefficient when the sound is going louder. *) kup
  (* Spring coefficient when the sound is going less loud. *)
    kdown threshold gmin gmax =
  let channels = AFrame.channels_of_kind kind in
  let rmsi = Frame.audio_of_seconds window in
  object
    inherit operator ~name:"normalize" kind [source]

    (** Current squares of RMS. *)
    val rms = Array.make channels 0.

    (** Current number of samples used to compute [rmsl] and [rmsr]. *)
    val mutable rmsc = 0

    (** Volume coefficients. *)
    val v = Array.make channels 1.

    (** Previous volume coefficients. *)
    val vold = Array.make channels 1.

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf in
      let rmst = rmst () in
      let kup = kup () in
      let kdown = kdown () in
      let threshold = threshold () in
      let gmin = gmin () in
      let gmax = gmax () in
      for i = offset to AFrame.position buf - 1 do
        for c = 0 to channels - 1 do
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
          for c = 0 to channels - 1 do
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
      if AFrame.is_partial buf then (
        for c = 0 to channels - 1 do
          vold.(c) <- 1.;
          v.(c) <- 1.;
          rms.(c) <- 0.
        done;
        rmsc <- 0 )
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "normalize"
    [
      ( "target",
        Lang.float_getter_t (),
        Some (Lang.float (-13.)),
        Some "Desired RMS (dB)." );
      ( "window",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Duration of the window used to compute the current RMS power \
           (second)." );
      ( "k_up",
        Lang.float_getter_t (),
        Some (Lang.float 0.005),
        Some
          "Coefficient when the power must go up (between 0 and 1, slowest to \
           fastest)." );
      ( "k_down",
        Lang.float_getter_t (),
        Some (Lang.float 0.1),
        Some
          "Coefficient when the power must go down (between 0 and 1, slowest \
           to fastest)." );
      ( "threshold",
        Lang.float_getter_t (),
        Some (Lang.float (-40.)),
        Some "Minimal RMS for activaing gain control (dB)." );
      ( "gain_min",
        Lang.float_getter_t (),
        Some (Lang.float (-6.)),
        Some "Minimal gain value (dB)." );
      ( "gain_max",
        Lang.float_getter_t (),
        Some (Lang.float 6.),
        Some "Maximal gain value (dB)." );
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:
      "Normalize the signal. Dynamic normalization of the signal is sometimes \
       the only option, and can make a listening experience much nicer. \
       However, its dynamic aspect implies some limitations which can go as \
       far as creating saturation in some extreme cases. If possible, consider \
       using some track-based normalization techniques such as those based on \
       replay gain. See the documentation for more details."
    (fun p kind ->
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
      new normalize
        ~kind src
        (fun () -> Audio.lin_of_dB (target ()))
        window kup kdown
        (fun () -> Audio.lin_of_dB (threshold ()))
        (fun () -> Audio.lin_of_dB (gmin ()))
        (fun () -> Audio.lin_of_dB (gmax ())))
