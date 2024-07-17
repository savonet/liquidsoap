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

let nb_blocks = 10
let blank = String.make Mixer.Buffer.size '\000'
let _ = no_stderr_report ()

class output dev start source =
object (self)
  inherit Output.output ~name:"speaker" ~kind:"output.alsa" source start

  (* See sources/alsa_in.ml, it's the same producer/consumer system. *)
  val buffer = Array.create nb_blocks ""
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <- String.create Mixer.Buffer.size
    done
  val mutable read = 0
  val mutable write = 0

  val mutable sleep = false
  method output_stop = sleep <- true

  method output_start =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    ignore (Tutils.create (fun () -> self#reader) () "alsa_playback")

  method reader =
    try
    let dev = Pcm.open_pcm dev [Pcm.Playback] [] in
    let params = Pcm.get_params dev in
      Pcm.set_access dev params Pcm.Access_rw_interleaved ;
      Pcm.set_format dev params Pcm.Format_s16_le ;
      ignore (Pcm.set_rate_near dev params
                Mixer.Buffer.format.Mixer.sample_freq Dir_eq) ;
      Pcm.set_channels dev params Mixer.Buffer.format.Mixer.channels ;
      Pcm.set_buffer_size dev params (Mixer.Buffer.size*4) ;
      Pcm.set_params dev params ;
      Pcm.prepare dev ;
      (* The output loop *)
      while not sleep do
        while write = read do
          Thread.delay (Mixer.Buffer.length/.2.)
        done ;
        begin try
          ignore
            (Pcm.writei dev buffer.(read mod nb_blocks) 0 (Mixer.Buffer.size/4))
        with
          | Buffer_xrun ->
              Dtools.Log.log ~label:"alsa.out" 2 "Underrun!" ;
              Pcm.prepare dev ;
              ignore (Pcm.writei dev blank 0 (Mixer.Buffer.size/4))
        end ;
        read <- (read + 1) mod (2*nb_blocks)
      done ;
      Pcm.close dev
    with
      | Unknown_error n -> raise (Error (string_of_error n))

  method output_send wav =
    if read <> write &&
       write mod nb_blocks = read mod nb_blocks then
      Thread.delay (Mixer.Buffer.length/.2.)
    else begin
      String.blit
        (Mixer.Buffer.to_string wav) 0
        buffer.(write mod nb_blocks) 0
        Mixer.Buffer.size ;
      write <- (write + 1) mod (2*nb_blocks)
    end

end

let _ =
  Lang.add_operator "output.alsa"
    [ "device", Lang.string_t, Some (Lang.string "hw:0,0"), None;
      "start", Lang.bool_t, Some (Lang.bool true), None;
      "", Lang.source_t, None, None ]
    ~descr:"Output the source's stream to an ALSA output device."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let device = e Lang.to_string  "device" in
       let source = List.assoc "" p in
         ((new output device autostart source):>Types.source))
