(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(* Alsa calls are blocking AND they do not respond in a regular fashion.
 * Basically, it waits two buffer lengths, then give you one buffer,
 * then waits epsilon and gives you the second one.
 * To make it smooth and avoid the root scheduling to be affected by alsa
 * problems (and vice versa), we thread and bufferize a LITTLE bit. *)

open Alsa
open Source

class mic ~kind ~clock_safe device =
  let buffer_length = AFrame.size () in
  let buffer_chans = (Frame.type_of_kind kind).Frame.audio in
  let alsa_device = device in
  let nb_blocks = Alsa_settings.conf_buffer_length#get in
  let blank () = Array.init buffer_chans (fun _ -> Array.make buffer_length 0.) in
object (self)
  inherit active_source kind as active_source
  inherit [float array array] IoRing.input ~nb_blocks ~blank as ioring

  method set_clock =
    active_source#set_clock ;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((Alsa_settings.get_clock ()):>Clock.clock))

  method private wake_up l =
    active_source#wake_up l ;
    if clock_safe then
      (Alsa_settings.get_clock ())#register_blocking_source

  method private sleep =
    ioring#sleep ;
    if clock_safe then
      (Alsa_settings.get_clock ())#unregister_blocking_source

  method stype = Infallible
  method is_ready = true
  method abort_track = ()
  method remaining = -1

  (* val mutable alsa_fmt = *)
  val mutable sample_freq = Lazy.force Frame.audio_rate

  val mutable read_fun =
    (fun pcm buf ofs len -> Pcm.readn_float pcm buf ofs len)

  val mutable device = None

  method close =
    match device with
      | Some d ->
          Pcm.close d ;
          device <- None
      | None -> ()

  method get_device =
    match device with
      | Some d -> d
      | None ->
          self#log#f 3 "Using ALSA %s." (Alsa.get_version ()) ;
          let dev = Pcm.open_pcm alsa_device [Pcm.Capture] [] in
          let params = Pcm.get_params dev in
          begin try
            Pcm.set_access dev params Pcm.Access_rw_noninterleaved ;
            Pcm.set_format dev params Pcm.Format_float
          with
            | _ ->
                (* If we can't get floats we fallback on interleaved s16le *)
                self#log#f 2 "Falling back on interleaved S16LE";
                Pcm.set_access dev params Pcm.Access_rw_interleaved ;
                Pcm.set_format dev params Pcm.Format_s16_le ;
                read_fun <-
                  (fun pcm buf ofs len ->
                     let sbuf = String.create (2 * 2 * len) in
                     let r = Pcm.readi pcm sbuf 0 len in
                     Audio.S16LE.to_audio sbuf 0 buf ofs r;
                     r)
          end ;
          sample_freq <-
            Pcm.set_rate_near dev params sample_freq Dir_eq; (* TODO: resample *)
          Pcm.set_channels dev params buffer_chans ;
          Pcm.set_params dev params ;
          Pcm.prepare dev ;
          device <- Some dev;
          dev

  method pull_block block =
    let dev = self#get_device in
    try
      let pos = ref 0 in
      while !pos < buffer_length do
        let len = buffer_length - !pos in
        let ret = read_fun dev block !pos len in
        assert (ret <= len);
        pos := !pos + ret;
      done;
    with
      | e ->
        begin
         match e with
           | Buffer_xrun ->
               self#log#f 2 "Overrun!"
             | _ -> self#log#f 2 "Alsa error: %s" (string_of_error e)
        end ;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then
         begin
          self#log#f 2 "Trying to recover.." ;
          Pcm.recover dev e
         end
        else raise e

  method get_frame buf =
    assert (0 = AFrame.position buf) ;
    let buffer = ioring#get_block in
    let fbuf = AFrame.content_of_type ~channels:buffer_chans buf 0 in
      for c = 0 to Array.length fbuf - 1 do
        Array.blit buffer.(c) 0 fbuf.(c) 0 buffer_length
      done;
      AFrame.add_break buf buffer_length

  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_reset = ()

  method is_active = true
end

(* This source is registered in Alsa_io. *)
