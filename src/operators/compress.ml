(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

(** See http://www.musicdsp.org/archive.php?classid=4#169 *)

class compress (source:source) attack release threshold ratio knee rmsw gn debug =
  (** Number of samples for computing rms. *)
  let rmsn = Fmt.samples_of_seconds rmsw in
object (self)
  inherit operator [source] as super

  (** [rmsn] last squares. *)
  val rmsv = Array.make rmsn 0.
  (** Current position in [rmsv]. *)
  val mutable rmsp = 0
  (** Current squares of RMS. *)
  val mutable rms = 0.

  (* Processing variables. *)
  val mutable amp = 0.
  (** Envelope. *)
  val mutable env = 0.
  (** Current gain. *)
  val mutable gain = 1.

  (** Counter for debugging purposes. *)
  val mutable count = 0

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.get_float_pcm buf in
      let chans = Array.length b in
      let gn = gn () in
      let attack = attack () in
      let release = release () in
      let threshold = threshold () in
      (** Compression ratio. *)
      let ratio = ratio () in
      let ratio = (ratio -. 1.) /. ratio in
      (** Knee. *)
      let knee = knee () in
      (* Attack and release "per sample decay". *)
      let g_attack =
        if attack = 0. then
          0.
        else
          exp (-1. /. (float (Fmt.samples_per_second ()) *. attack));
      in
      let ef_a = g_attack *. 0.25 in
      let g_release =
        if release = 0. then
          0.
        else
          exp (-1. /. (float (Fmt.samples_per_second ()) *. release));
      in
      let ef_ai = 1. -. ef_a in
      (* Knees. *)
      let knee_min = Sutils.lin_of_dB (threshold -. knee) in
      let knee_max = Sutils.lin_of_dB (threshold +. knee) in
        for i = offset to AFrame.position buf - 1 do

          (* Input level. *)
          let lev_in =
            let ans = ref 0. in
              for c = 0 to chans - 1 do
                ans := !ans +. b.(c).(i) *. b.(c).(i)
              done;
              !ans /. (float chans)
          in

            (* RMS *)
            rms <- rms -. rmsv.(rmsp) +. lev_in;
            rms <- abs_float rms; (* Sometimes the rms was -0., avoid that. *)
            rmsv.(rmsp) <- lev_in;
            rmsp <- (rmsp + 1) mod rmsn;
            amp <- sqrt (rms /. float rmsn);

            (* Dynamic selection: attack or release? *)
            (* Smoothing with capacitor, envelope extraction... Here be aware of
             * pIV denormal numbers glitch. *)
            if amp > env then
              env <- env *. g_attack +. amp *. (1. -. g_attack)
            else
              env <- env *. g_release +. amp *. (1. -. g_release);

            (* Compute the gain. *)
            let gain_t =
              if env < knee_min then
                (* Do not compress. *)
                1.
              else
                  if env < knee_max then
                    (
                      (* Knee: compress smoothly. *)
                      let x = (knee +. Sutils.dB_of_lin env -. threshold) /. (2. *. knee) in
                        Sutils.lin_of_dB (0. -. knee *. ratio *. x *. x)
                    )
                  else
                    (* Maximal (n:1) compression. *)
                    Sutils.lin_of_dB ((threshold -. Sutils.dB_of_lin env) *. ratio)
            in
              gain <- gain *. ef_a +. gain_t *. ef_ai;

              (* Apply the gain. *)
              let g = gain *. gn in
                for c = 0 to chans - 1 do
                  b.(c).(i) <- b.(c).(i) *. g
                done;

                (* Debug messages. *)
                count <- count + 1;
                if debug && count mod 10000 = 0 then
                  Printf.printf "RMS:%7.02f     Env:%7.02f     Gain: %4.02f\r%!" (Sutils.dB_of_lin amp) (Sutils.dB_of_lin env) gain

        done;
        (* Reset values if it is the end of the track. *)
        if AFrame.is_partial buf then
          (
            rms <- 0.;
            rmsp <- 0;
            for i = 0 to rmsn - 1 do
              rmsv.(i) <- 0.
            done;
            gain <- 1.;
            env <- 0.;
            amp <- 0.
          )
end

let proto =
  [
    "attack", Lang.float_getter_t 1, Some (Lang.float 100.), Some "Attack time (ms).";
    "release", Lang.float_getter_t 2, Some (Lang.float 400.), Some "Release time (ms).";
    "threshold", Lang.float_getter_t 3, Some (Lang.float (-10.)), Some "Threshold level (dB).";
    "knee", Lang.float_getter_t 4, Some (Lang.float 1.), Some "Knee radius (dB).";
    "rms_window", Lang.float_t, Some (Lang.float 0.1), Some "Window for computing RMS (in sec).";
    "gain", Lang.float_getter_t 5, Some (Lang.float 0.), Some "Additional gain (dB).";
    "debug", Lang.bool_t, Some (Lang.bool false), None;
    "", Lang.source_t, None, None
    ]

let compress p =
  let f v = List.assoc v p in
  let attack, release, threshold, ratio, knee, rmsw, gain, debug, src =
    Lang.to_float_getter (f "attack"),
    Lang.to_float_getter (f "release"),
    Lang.to_float_getter (f "threshold"),
    Lang.to_float_getter (f "ratio"),
    Lang.to_float_getter (f "knee"),
    Lang.to_float (f "rms_window"),
    Lang.to_float_getter (f "gain"),
    Lang.to_bool (f "debug"),
    Lang.to_source (f "")
  in
    ((new compress
        src
        (fun () -> attack () /. 1000.)
        (fun () -> release () /. 1000.)
        threshold
        ratio
        knee
        rmsw
        (fun () -> Sutils.lin_of_dB (gain ()))
        debug):>source)

let () =
  Lang.add_operator "compress"
    (("ratio", Lang.float_t, Some (Lang.float 2.), Some "Gain reduction ratio (n:1).")::proto)
    ~category:Lang.SoundProcessing
    ~descr:"Compress the signal."
    compress;
  Lang.add_operator "limit"
    (("ratio", Lang.float_t, Some (Lang.float 20.), Some "Gain reduction ratio (n:1).")::proto)
    ~category:Lang.SoundProcessing
    ~descr:"Limit the signal."
    compress
