(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(* Buffered ALSA output *)

open Alsa
open Dtools

(** ALSA should be quiet *)
let () = no_stderr_report ()

class output dev start source =
  let buffer_length = Fmt.samples_per_frame () in
  let buffer_chans = Fmt.channels () in
  let blank () = Array.init buffer_chans (fun _ -> Array.make buffer_length 0.) in
  let nb_blocks = Alsa_settings.conf_buffer_length#get in
  let samples_per_second = Fmt.samples_per_second () in
  let periods = Alsa_settings.periods#get in
object (self)
  inherit Output.output ~name:"output.alsa" ~kind:"output.alsa" source start
  inherit [float array array] IoRing.output ~nb_blocks ~blank
                                 ~blocking:true () as ioring

  val mutable device = None

  val mutable alsa_rate = samples_per_second
  val samplerate_converter = Audio_converter.Samplerate.create buffer_chans
  val mutable alsa_write =                                                                       
    (fun pcm buf ofs len -> Pcm.writen_float pcm buf ofs len) 

  method get_device =
    match device with
      | Some d -> d
      | None -> 
          self#log#f 3 "Using ALSA %s." (Alsa.get_version ()) ;
          let dev = Pcm.open_pcm dev [Pcm.Playback] [] in
          let params = Pcm.get_params dev in
          let bufsize =
             (
               try
                 Pcm.set_access dev params Pcm.Access_rw_noninterleaved ;
                 Pcm.set_format dev params Pcm.Format_float
               with
                 | _ ->
                    (* If we can't get floats we fallback on interleaved s16le *)
                    self#log#f 2 "Falling back on interleaved S16LE";
                    Pcm.set_access dev params Pcm.Access_rw_interleaved ;
                    Pcm.set_format dev params Pcm.Format_s16_le ;
                    alsa_write <-
                      (fun pcm buf ofs len ->
                         let sbuf = String.create (2 * len * Array.length buf) in
                         let _ =  Float_pcm.to_s16le buf ofs len sbuf 0 in
                         Pcm.writei pcm sbuf 0 len
                      )     
             );
             Pcm.set_channels dev params buffer_chans ;
             alsa_rate <- Pcm.set_rate_near dev params samples_per_second Dir_eq ;
             (* Size in frames, must be set after the samplerate.
              * This setting is critical as a too small bufsize will easily result in
              * underruns when the thread isn't fast enough.
              * TODO make it customizable *)
             Pcm.set_periods dev params periods Dir_eq;
             Pcm.set_buffer_size_near dev params 65536
          in
          self#log#f 3 "Samplefreq=%dHz, Bufsize=%dB, Frame=%dB, Periods=%d"
            alsa_rate bufsize (Pcm.get_frame_size params) periods ;
          Pcm.set_params dev params ;
          device <- Some dev ;
          dev

  method close = 
    match device with
      | Some d ->
          Pcm.close d ;
          device <- None
      | None -> ()

  method push_block data = 
    let dev = self#get_device in
    try
      let len = Array.length data.(0) in
      let rec f pos = 
        if pos < len then
          let ret = alsa_write dev data pos (len - pos) in
          f (pos+ret) 
      in
      f 0
    with
      | Buffer_xrun ->
           self#log#f 2 "Underrun!" ;
           Pcm.prepare dev

  method output_send buf =
    let buf = AFrame.get_float_pcm buf in
    let ratio = float alsa_rate /. float samples_per_second in
    let buf = Audio_converter.Samplerate.resample samplerate_converter 
                  ratio buf 0 (Array.length buf.(0)) 
    in
    let f data = 
      for c = 0 to Array.length buf - 1 do
        Float_pcm.float_blit buf.(c) 0 data.(c) 0 (Array.length buf.(0))
      done
    in
    ioring#put_block f

  method output_reset = ()
end

(* It is registered in Alsa_io. *)
