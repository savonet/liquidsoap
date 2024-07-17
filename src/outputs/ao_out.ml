(*****************************************************************************

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

(** Output using ao lib. *)

open Ao

exception Error of string

let nb_blocks = 10

let bytes_per_sample = 2

class output ~driver ~options source start =
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
object (self)
  inherit Output.output ~name:"ao" ~kind:"output.ao" source start

  (* See sources/alsa_in.ml, it's the same producer/consumer system. *)
  val buffer = Array.create nb_blocks ""
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <- String.create (samples_per_frame * channels * bytes_per_sample)
    done
  val mutable read = 0
  val mutable write = 0

  val mutable sleep = false
  method output_stop = sleep <- true

  method output_start =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    ignore (Tutils.create (fun () -> self#reader) () "ao_playback")

  method reader =
    let driver = if driver = "" then
      get_default_driver ()
    else
      find_driver driver
    in
    let device = open_live ~driver ~options
                   ~rate:samples_per_second
                   ~bits:(bytes_per_sample * 8)
                   ~channels:channels ()
    in
      (* Wait for things to settle *)
      Thread.delay (5. *. (Fmt.seconds_per_frame ()));
      (* The output loop *)
      while not sleep do
        while write = read do
          Thread.delay ((Fmt.seconds_per_frame ()) /. 2.)
        done ;
        play device buffer.(read mod nb_blocks);
        read <- (read + 1) mod (2*nb_blocks)
      done ;
      close device

  method output_send wav =
    if read <> write &&
       write mod nb_blocks = read mod nb_blocks then
      self#log#f 4 "Reader not ready!"
    else begin
      ignore (Float_pcm.to_s16le (AFrame.get_float_pcm wav) 0 (AFrame.size wav)
                buffer.(write mod nb_blocks) 0) ;
      write <- (write + 1) mod (2*nb_blocks)
    end

  method output_reset = ()
end

let () =
  Lang.add_operator "output.ao"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "driver",
      Lang.string_t, Some (Lang.string ""),
      Some "libao driver to use." ;

      "options",
      Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
      Some (Lang.list []),
      Some "List of parameters, depends on driver.";

      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Output stream to local sound card using libao."
    (fun p ->
       let start = Lang.to_bool (List.assoc "start" p) in
       let driver = Lang.to_string (List.assoc "driver" p) in
       let options = List.map (fun x -> let a,b = Lang.to_product x in Lang.to_string a, Lang.to_string b) ( Lang.to_list (List.assoc "options" p)) in
       let source = List.assoc "" p in
         ((new output ~driver ~options source start):>Source.source))
