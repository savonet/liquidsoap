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

(* Alsa calls are blocking AND they do not respond in a regular fashion.
 * Basically, it waits two buffer lengths, then give you one buffer,
 * then waits epsilon and gives you the second one.
 * To make it smooth and avoid the root scheduling to be affected by alsa
 * problems (and vice versa), we thread and bufferize a LITTLE bit. *)

open Alsa
open Source

exception Error of string

let log = Dtools.Log.make ["input";"alsa"]

class mic device =
  let buffer_length = Fmt.samples_per_frame () in
  let buffer_chans = Fmt.channels () in
  (* Only one block is OK, but 10 doesn't cost much and could allow
   * some timing errors to be absorbed.. I can't hear any difference anyway. *)
  let nb_blocks = 10 in
  let blank = Array.init buffer_chans (fun _ -> Array.make buffer_length 0.) in
object (self)
  inherit active_source

  method stype = Infallible
  method is_ready = true
  method abort_track = ()
  method remaining = -1

  (* val mutable alsa_fmt = *)
  val mutable sample_freq = Fmt.samples_per_second ()

  val buffer = Array.make nb_blocks [||]
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <-
        Array.init buffer_chans (fun _ -> Array.make buffer_length 0.)
    done
  val mutable read = 0
  val mutable write = 0
  (* Read and write are stored modulo 2*nb_blocks,
   * because we must be able to distinguish the case where the sched is late
   * from the one where the capture is late.
   * And we don't need more than modulo 2*nb_blocks. *)

  val mutable read_fun =
    (fun pcm buf ofs len -> Pcm.readn_float pcm buf ofs len)

  val mutable sleep = false
  method sleep = sleep <- true

  method output_get_ready =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    ignore (Tutils.create (fun () -> self#writer) () "alsa_capture")

  method writer =
    self#log#f 3 "Using ALSA %s." (Alsa.get_version ()) ;
    let dev = Pcm.open_pcm device [Pcm.Capture] [] in
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
                   Float_pcm.from_s16le buf ofs sbuf 0 r;
                   r)
      end ;
      sample_freq <-
        Pcm.set_rate_near dev params sample_freq Dir_eq; (* TODO: resample *)
      Pcm.set_channels dev params buffer_chans ;
      Pcm.set_params dev params ;
      Pcm.prepare dev ;
      let fill block =
        try
          let pos = ref 0 in
            while !pos < buffer_length do
              let len = buffer_length - !pos in
              let ret = read_fun dev buffer.(block mod nb_blocks) !pos len in
                assert (ret <= len);
                pos := !pos + ret;
            done;
        with
          | Unknown_error n -> raise (Error (string_of_error n))
          | Buffer_xrun ->
              (* Restart. Try again ... *)
              log#f 2 "Overrun!" ;
              Pcm.prepare dev
      in
        (* Fill the first block *)
        fill 0 ;
        write <- 1 ;
        (* Filling loop *)
        while not sleep do
          if
            read <> write &&
            write mod nb_blocks = read mod nb_blocks
          then begin
            (* Wait for the reader to read the block we fancy *)
            Thread.delay (Fmt.seconds_per_frame () /. 2.)
          end else begin
            fill write ; write <- (write + 1) mod (2*nb_blocks)
          end
        done

  method get_frame buf =
    assert (0 = AFrame.position buf) ;
    let buffer =
      (* Check that the writer still has an advance.
       * Otherwise play blank for waiting.. *)
      if write = read then begin
        log#f 2 "No available frame!" ;
        blank
      end else
        let b = buffer.(read mod nb_blocks) in
          read <- (read + 1) mod (2*nb_blocks) ;
          b
    in
    let fbuf = AFrame.get_float_pcm buf in
      for c = 0 to Array.length fbuf - 1 do
        Array.blit buffer.(c) 0 fbuf.(c) 0 buffer_length
      done;
      AFrame.add_break buf buffer_length

  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_reset = ()

end

(* This source is registered in Alsa_io. *)
