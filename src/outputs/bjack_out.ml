(*****************************************************************************

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

(** Output using ocaml-jack. *)

let bytes_per_sample = 2

class output ~nb_blocks ~server source =
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
  let bufferize = (nb_blocks <> 0) in
  let nb_blocks = if not bufferize then 1 else nb_blocks in
object (self)
  inherit Output.output ~name:"output.jack" ~kind:"output.jack" source true

  (* See sources/alsa_in.ml, it's the same producer/consumer system. *)
  val buffer = Array.create nb_blocks ""
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <- String.create (samples_per_frame * channels * bytes_per_sample)
    done
  val mutable read = 0
  val mutable write = 0

  val mutable sleep = false
  val mutable device = None
  method output_stop = sleep <- true

  method get_device = 
    match device with
      | None -> 
          let server_name =
            match server with "" -> None | s -> Some s
          in
	  let dev = 
            Bjack.open_t 
	      ~rate:samples_per_second ~bits_per_sample:(bytes_per_sample * 8)
              ~input_channels:0 ~output_channels:channels ~flags:[] ?server_name
              ~ringbuffer_size:(nb_blocks*samples_per_frame*bytes_per_sample) 
              ~client_name:self#id () in
          Bjack.set_all_volume dev 75 ;
          device <- Some dev ;
          dev
      | Some d -> d

  method output_start =
    let dev = self#get_device in
    if bufferize then
    begin
      sleep <- false ;
      read <- 0 ; write <- 0 ;
      ignore (Tutils.create (fun () -> self#reader dev) () "jack_playback")
    end

  method write_block dev data = 
    let len = String.length data in
    let remaining = ref (len - (Bjack.write dev data)) in
    while !remaining > 0 do
      Thread.delay (Fmt.seconds_per_frame () /. 2.) ;
      let tmp = Str.string_after data (len - !remaining) in
      let written = Bjack.write dev tmp in
      remaining := !remaining - written
    done

  method reader device =
      (* Wait for things to settle *)
      Thread.delay (5. *. (Fmt.seconds_per_frame ()));
      (* The output loop *)
      while not sleep do
        while write = read do
          Thread.delay ((Fmt.seconds_per_frame ()) /. 2.)
        done ;
        let data = buffer.(read mod nb_blocks) in
        self#write_block device data ;
        read <- (read + 1) mod (2*nb_blocks)
      done ;
      Bjack.close device

  method output_send wav =
    if bufferize then
      if read <> write &&
         write mod nb_blocks = read mod nb_blocks then
        self#log#f 4 "Reader not ready!"
      else begin
        ignore (Float_pcm.to_s16le (AFrame.get_float_pcm wav) 0 (AFrame.size wav)
                  buffer.(write mod nb_blocks) 0) ;
        write <- (write + 1) mod (2*nb_blocks)
      end
    else begin
      let dev = self#get_device in
      let s = String.create (samples_per_frame * channels * bytes_per_sample) in
      ignore (Float_pcm.to_s16le (AFrame.get_float_pcm wav) 0 (AFrame.size wav)
                  s 0) ;
      self#write_block dev s
    end

  method output_reset = ()
end

let () =
  Lang.add_operator "output.jack"
    [ "buffer_size",
      Lang.int_t, Some (Lang.int 2),
      Some "Set buffer size, in frames. 0 means unbuffered output.";
     "server",
      Lang.string_t, Some (Lang.string ""),
      Some "Jack server to connect to.";
      "", Lang.source_t, None, None ]
    ~category:Lang.Output
    ~descr:"Output stream to jack."
    (fun p ->
       let source = List.assoc "" p in
       let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
       let server = Lang.to_string (List.assoc "server" p) in
         ((new output ~nb_blocks ~server source):>Source.source))
