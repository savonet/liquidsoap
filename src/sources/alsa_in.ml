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

(* Alsa calls are blocking AND they do not respond in a regular fashion.
 * Basically, it waits two buffer lengths, then give you one buffer,
 * then waits epsilon and gives you the second one.
 * To make it smooth and avoid the root scheduling to be affected by alsa
 * problems (and vice versa), we thread and bufferize a LITTLE bit. *)

open Alsa
open Types

(* Only one block is OK, but 10 doesn't cost much and could allow
 * some timing errors to be absorbed.. I can't ear any difference anyway. *)
let nb_blocks = 10
let blank = String.make Mixer.Buffer.size '\000'

class mic device =
object (self)
  inherit active_source

  method stype = Infallible
  method is_ready = true
  method abort_track = ()

  val buffer = Array.create nb_blocks ""
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <- String.create Mixer.Buffer.size
    done
  val mutable read = 0
  val mutable write = 0
  (* Read and write are stored modulo 2*nb_blocks,
   * because we must be able to distinguish the case where the sched is late
   * from the one where the capture is late.
   * And we don't need more than modulo 2*nb_blocks. *)

  val mutable sleep = false
  method sleep = sleep <- true

  method wake_up _ =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    let reader = Mutex.create () in
      Mutex.lock reader ;
      ignore (Tutils.create (fun l -> self#writer l) reader "alsa_capture") ;
      (* Wait for the first buffer input. *)
      Mutex.lock reader

  method writer reader_lock =
    let dev = Pcm.open_pcm device [Pcm.Capture] [] in
    let params = Pcm.get_params dev in
      Pcm.set_access dev params Pcm.Access_rw_interleaved ;
      Pcm.set_format dev params Pcm.Format_s16_le ;
      assert (Mixer.Buffer.format.Mixer.sample_freq =
                Pcm.set_rate_near dev params
                  Mixer.Buffer.format.Mixer.sample_freq Dir_eq) ;
      Pcm.set_channels dev params Mixer.Buffer.format.Mixer.channels ;
      Pcm.set_params dev params ;
      Pcm.prepare dev ;
      let fill block =
        try
          assert (Mixer.Buffer.size/4 =
                    Pcm.readi dev buffer.(block mod nb_blocks) 0
                      (Mixer.Buffer.size/4))
        with
          | Buffer_xrun ->
              (* Restart. Try again ... *)
              Dtools.Log.log ~label:"alsa.in" 2 "Overrun!" ;
              Pcm.prepare dev
      in
        (* Fill the first block *)
        fill 0 ;
        write <- 1 ;
        Mutex.unlock reader_lock ;
        (* Filling loop *)
        while not sleep do
          if read <> write &&
             write mod nb_blocks = read mod nb_blocks then begin
               (* Wait for the reader to read the block we fancy *)
               Thread.delay (Mixer.Buffer.length/.2.)
          end else
            ( fill write ; write <- (write + 1) mod (2*nb_blocks) )
        done

  method get_frame buf =
    assert (0 = Mixer.Buffer.position buf) ;
    let buffer =
      (* Check that the writer still has an advance.
       * Otherwise play blank for waiting.. *)
      if write = read then begin
        Dtools.Log.log ~label:"alsa.in" 2 "No available frame!" ;
        blank
      end else
        let b = buffer.(read mod nb_blocks) in
          read <- (read + 1) mod (2*nb_blocks) ;
          b
    in
      String.blit
        buffer 0
        (Mixer.Buffer.to_string buf) 0
        Mixer.Buffer.size ;
      Mixer.Buffer.add_break buf Mixer.Buffer.size

  method output = if Mixer.Buffer.is_partial memo then self#get_frame memo

end

let _ =
  Lang.add_operator "input.alsa"
    ~descr:"Alsa microphone input"
    [ "device", Lang.string_t, Some (Lang.string "default"), None ]
    (fun p -> ((new mic (Lang.to_string (List.assoc "device" p))):>source))
