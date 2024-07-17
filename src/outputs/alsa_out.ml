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

(* See sources/alsa_in.ml for details.
 * Basically, we thread because alsa calls are blocking and can have a bad
 * interaction with the root scheduling. *)

open Alsa

exception Error of string

(** Size of the ringbuffer in seconds *)
let ringtime = Dtools.Conf.get_float ~default:4. "alsa.buffer_duration"

(** ALSA should be quiet *)
let () = no_stderr_report ()

class output dev start source =
  let channels = Fmt.channels () in
  let samples_per_second = Fmt.samples_per_second () in
  let seconds_per_frame = Fmt.seconds_per_frame () in
object (self)
  inherit Output.output ~name:"output.alsa" ~kind:"output.alsa" source start

  val mutable alsa_rate = 0
  val mutable write =
    (fun pcm buf ofs len -> Pcm.writen_float pcm buf ofs len)
  val mutable ring = Ringbuffer.create channels 0

  val mutable sleep = false
  method output_stop = sleep <- true

  method output_start =
    sleep <- false ;
    self#log 3 ("Using ALSA " ^ Alsa.get_version () ^ ".");
    try
      let dev = Pcm.open_pcm dev [Pcm.Playback] [] in
      let params = Pcm.get_params dev in
      let periods,bufsize =
        (
          try
            Pcm.set_access dev params Pcm.Access_rw_noninterleaved ;
            Pcm.set_format dev params Pcm.Format_float
          with
            | _ ->
                (* If we can't get floats we fallback on interleaved s16le *)
                self#log 2 "Falling back on interleaved S16LE";
                Pcm.set_access dev params Pcm.Access_rw_interleaved ;
                Pcm.set_format dev params Pcm.Format_s16_le ;
                write <-
                (fun pcm buf ofs len ->
                   let sbuf = String.create (2 * len * Array.length buf) in
                   let _ =  Float_pcm.to_s16le buf ofs len sbuf 0 in
                     Pcm.writei pcm sbuf 0 len
                )
        );
        Pcm.set_channels dev params channels ;
        alsa_rate <- Pcm.set_rate_near dev params samples_per_second Dir_eq ;
        (* Size in frames, must be set after the samplerate.
         * This setting is critical as a too small bufsize will easily result in
         * underruns when the thread isn't fast enough.
         * TODO make it customizable *)
        let periods = Dtools.Conf.get_int ~default:5 "alsa.periods" in
          Pcm.set_periods dev params periods Dir_eq;
          periods, Pcm.set_buffer_size_near dev params 65536
      in
        self#log 3
          (Printf.sprintf "Samplefreq=%dHz, Bufsize=%dB, Frame=%dB, Periods=%d"
             alsa_rate bufsize (Pcm.get_frame_size params) periods) ;
        Pcm.set_params dev params ;
	let ringsize = Fmt.samples_of_seconds ringtime in
        ring <- Ringbuffer.create channels ringsize;
        (* Now feed half of the ringbuffer with blank *)
        Ringbuffer.write_advance ring (ringsize/2);
        ignore
          (Tutils.create (fun () -> self#reader dev bufsize) () "alsa_playback")
    with
      | Unknown_error n when false -> raise (Error (string_of_error n))

  method reader dev bufsize =
    (* TODO: static buffers *)
    let blank = Array.init channels (fun _ -> Array.create bufsize 0.) in
    try
      Pcm.prepare dev ;
      while not sleep && not !Root.shutdown do
        try
          if Ringbuffer.transmit ring (Pcm.writen_float dev) = 0
          then (
            self#log 3 "Writer is late!" ;
            Thread.delay (seconds_per_frame/.2.)
          )
        with
          | Buffer_xrun ->
              Dtools.Log.log ~label:"alsa.out" 2 "Underrun!" ;
              Pcm.prepare dev ;
              ignore (write dev blank 0 bufsize)
      done ;
      Pcm.close dev
    with
      | Unknown_error n -> raise (Error (string_of_error n))

  method output_send buf =
    let buf = AFrame.get_float_pcm buf in
    let ratio = float alsa_rate /. float samples_per_second in
    let buf = Float_pcm.resample ratio buf 0 (Array.length buf.(0)) in
      if Ringbuffer.write_space ring < Array.length buf.(0) then begin
        if false then self#log 3 "Reader is late!" ;
        (* Thread.delay (Mixer.Buffer.length/.2.) *)
      end else
        Ringbuffer.write ring buf 0 (Array.length buf.(0))

  method output_reset = ()
end

(* It is registered in Alsa_io. *)
