(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

let clip x = min 1. (max (-.1.) x)

class normalize (source:source)
           rmst  (** RMS target. *)
           rmsi  (** Number of samples for computing rms. *)
           kup   (** Spring coefficient when the sound is going louder. *)
           kdown (** Spring coefficient when the sound is going less loud. *)
           vmax  (** Maximum amplification coefficient. *)
           debug (** Show RMS and amplification coeffs. *)
           =
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
        for i = offset to AFrame.position buf - 1 do
          for c = 0 to 1 do
            rms.(c) <- rms.(c) +. b.(c).(i) *. b.(c).(i);
            b.(c).(i) <- clip (b.(c).(i) *. ((float rmsc) *. vold.(c) +. (float (rmsi - rmsc)) *. v.(c)) /. (float rmsi))
          done;
          rmsc <- rmsc + 1;
          if rmsc >= rmsi then
            (
              if debug then
                (
                  let rmsl = sqrt (rms.(0) /. (float_of_int rmsi)) in
                  let rmsr = sqrt (rms.(1) /. (float_of_int rmsi)) in
                    Printf.printf "%.02f\t%.02f\t->\t%0.02f\t|\t%.02f\t%.02f\t->\t%.02f\n%!" rmsl v.(0) (rmsl *. v.(0)) rmsr v.(1) (rmsr *. v.(1))
                );
              for c = 0 to 1 do
                let r = sqrt (rms.(c) /. (float_of_int rmsi)) in
                  if r > 0.01 then
                    if r *. v.(c) > rmst then
                      v.(c) <- v.(c) +. kdown *. (rmst /. r -. v.(c))
                    else
                      v.(c) <- v.(c) +. kup *. (rmst /. r -. v.(c));
                  vold.(c) <- v.(c);
                  v.(c) <- min vmax v.(c);
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
      "rms_target", Lang.float_t, Some (Lang.float 0.2), Some "Desired RMS power";
      "rms_interval", Lang.int_t, Some (Lang.int 4410), Some "Number of samples used to compute the current RMS power";
      "k_up", Lang.float_t, Some (Lang.float 0.005), Some "Coefficient when the power must go up (0 < k_up <1)";
      "k_down", Lang.float_t, Some (Lang.float 0.1), Some "Coefficient when the power must go down (0 < kd_won < 1)";
      "v_max", Lang.float_t, Some (Lang.float 2.), Some "Maximal amplification coefficient";
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Show coefficients";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Normalize the signal"
    (fun p ->
       let f v = List.assoc v p in
       let rmst, rmsi, kup, kdown, vmax, debug, src =
         Lang.to_float (f "rms_target"),
         Lang.to_int (f "rms_interval"),
         Lang.to_float (f "k_up"),
         Lang.to_float (f "k_down"),
         Lang.to_float (f "v_max"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "")
       in
         ((new normalize src rmst rmsi kup kdown vmax debug):>source)
    )
