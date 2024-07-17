(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

class normalize (source:source)
           rmst  (** RMS target. *)
           window  (** Number of samples for computing rms. *)
           kup   (** Spring coefficient when the sound is going louder. *)
           kdown (** Spring coefficient when the sound is going less loud. *)
           threshold
           gmin
           gmax
           debug (** Show RMS and amplification coeffs. *)
           =
let rmsi = Fmt.samples_of_seconds window in
object (self)
  inherit operator [source] as super

  (** Current squares of RMS. *)
  val rms = [|0.; 0.|]
  (** Current number of samples used to compute [rmsl] and [rmsr]. *)
  val mutable rmsc = 0

  (** Volume coefficients. *)
  val v = [|1.; 1.|]
  (** Previous volume coefficients. *)
  val vold = [|1.; 1.|]

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.get_float_pcm buf in
      let rmst = rmst () in
      let kup = kup () in
      let kdown = kdown () in
      let threshold = threshold () in
      let gmin = gmin () in
      let gmax = gmax () in
        for i = offset to AFrame.position buf - 1 do
          for c = 0 to 1 do
            rms.(c) <- rms.(c) +. b.(c).(i) *. b.(c).(i);
            b.(c).(i) <-
              b.(c).(i) *.
              ((float rmsc) *. vold.(c) +. (float (rmsi - rmsc)) *. v.(c)) /.
              (float rmsi)
          done;
          rmsc <- rmsc + 1;
          if rmsc >= rmsi then
            (
              if debug then
                (
                  let rmsl = sqrt (rms.(0) /. (float_of_int rmsi)) in
                  let rmsr = sqrt (rms.(1) /. (float_of_int rmsi)) in
                    Printf.printf
                      "%6.02f  *  %4.02f  ->  %6.02f    |    \
                       %6.02f  *  %4.02f  ->  %6.02f\n%!"
                      (Sutils.dB_of_lin rmsl)
                      v.(0)
                      (Sutils.dB_of_lin (rmsl *. v.(0)))
                      (Sutils.dB_of_lin rmsr)
                      v.(1)
                      (Sutils.dB_of_lin (rmsr *. v.(1)))
                );
              (* TODO: do all the computations in dB? *)
              for c = 0 to 1 do
                let r = sqrt (rms.(c) /. (float_of_int rmsi)) in
                  if r > threshold then
                    if r *. v.(c) > rmst then
                      v.(c) <- v.(c) +. kdown *. (rmst /. r -. v.(c))
                    else
                      v.(c) <- v.(c) +. kup *. (rmst /. r -. v.(c));
                  vold.(c) <- v.(c);
                  v.(c) <- max gmin (min gmax v.(c));
                  rms.(c) <- 0.
              done;
              rmsc <- 0
            )
        done;
        (* Reset values if it is the end of the track. *)
        if AFrame.is_partial buf then
          (
            for c = 0 to 1 do
              vold.(c) <- 1.;
              v.(c) <- 1.;
              rms.(c) <- 0.
            done;
            rmsc <- 0
          )
end

let () =
  Lang.add_operator "normalize"
    [
      "target", Lang.float_getter_t 1, Some (Lang.float (-13.)),
      Some "Desired RMS (dB).";
      "window", Lang.float_t, Some (Lang.float 0.1),
      Some "Duration of the window used to compute \
            the current RMS power (second).";
      "k_up", Lang.float_getter_t 2, Some (Lang.float 0.005),
      Some "Coefficient when the power must go up \
            (between 0 and 1, slowest to fastest).";
      "k_down", Lang.float_getter_t 3, Some (Lang.float 0.1),
      Some "Coefficient when the power must go down \
            (between 0 and 1, slowest to fastest).";
      "threshold", Lang.float_getter_t 4, Some (Lang.float (-40.)),
      Some "Minimal RMS for activaing gain control (dB).";
      "gain_min", Lang.float_getter_t 5, Some (Lang.float (-6.)),
      Some "Minimal gain value (dB).";
      "gain_max", Lang.float_getter_t 6, Some (Lang.float 6.),
      Some "Maximal gain value (dB).";
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Show coefficients.";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Normalize the signal. Dynamic normalization of the signal \
            is sometimes the only option, and can make a listening experience \
            much nicer. However, its dynamic aspect implies some limitations \
            which can go as far as creating saturation in some extreme cases. \
            If possible, consider using some track-based normalization \
            techniques such as those based on replay gain. See the \
            documentation for more details."
    (fun p ->
       let f v = List.assoc v p in
       let target, window, kup, kdown, threshold, gmin, gmax, debug, src =
         Lang.to_float_getter (f "target"),
         Lang.to_float (f "window"),
         Lang.to_float_getter (f "k_up"),
         Lang.to_float_getter (f "k_down"),
         Lang.to_float_getter (f "threshold"),
         Lang.to_float_getter (f "gain_min"),
         Lang.to_float_getter (f "gain_max"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "")
       in
         ((new normalize
             src
             (fun () -> Sutils.lin_of_dB (target ()))
             window
             kup
             kdown
             (fun () -> Sutils.lin_of_dB (threshold ()))
             (fun () -> Sutils.lin_of_dB (gmin ()))
             (fun () -> Sutils.lin_of_dB (gmax ()))
             debug):>source)
    )
