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

let log = Dtools.Log.make ["input";"jack"]

class jack_in ~nb_blocks ~server =
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
  let bytes_per_sample = 2 in
  let bufferize = (nb_blocks <> 0) in
  let nb_blocks = if not bufferize then 1 else nb_blocks in
  let blank = String.make (samples_per_frame * channels * bytes_per_sample) '0' in
object (self)
  inherit active_source

  method stype = Infallible
  method is_ready = true
  method abort_track = ()
  method remaining = -1

  val mutable sample_freq = Fmt.samples_per_second ()

  (* See sources/alsa_in.ml, it's the same producer/consumer system. *)
  val buffer = Array.create nb_blocks ""
  initializer
    for i = 0 to nb_blocks - 1 do
      buffer.(i) <-blank
    done
  val mutable read = 0
  val mutable write = 0
  (* Read and write are stored modulo 2*nb_blocks,
   * because we must be able to distinguish the case where the sched is late
   * from the one where the capture is late.
   * And we don't need more than modulo 2*nb_blocks. *)

  val mutable sleep = false
  val mutable device = None
  method sleep = sleep <- true

  method get_device = 
    match device with
      | None -> 
          let server_name = 
            match server with "" -> None | s -> Some s
          in
	  let dev = 
	    Bjack.open_t 
              ~rate:samples_per_second ~bits_per_sample:(bytes_per_sample * 8)
              ~input_channels:channels ~output_channels:0 ~flags:[] ?server_name
              ~ringbuffer_size:(nb_blocks*samples_per_frame*bytes_per_sample) 
              ~client_name:self#id () in
          Bjack.set_all_volume dev 75 ;
          device <- Some dev ;
          dev
      | Some d -> d

  method output_get_ready =
    let dev = self#get_device in
    if bufferize then
      begin
        sleep <- false ;
        read <- 0 ; write <- 0 ;
        let reader = Mutex.create () in
        ignore (Tutils.create (fun l -> self#writer dev l) reader "jack_capture") ;
        (* Wait for the first buffer input. *)
        Mutex.lock reader
      end

  method get_block dev = 
        let length = samples_per_frame * channels * bytes_per_sample in
        let ans = ref (Bjack.read dev length) in
          while String.length !ans < length do
            Thread.delay (Fmt.seconds_per_frame () /. 2.) ;
            let len = length - (String.length !ans) in
            let tmp = Bjack.read dev len in
            ans := !ans ^ tmp 
         done;
         !ans

  method writer dev reader_lock =
      let fill block =
         buffer.(block mod nb_blocks) <- self#get_block dev 
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
               Thread.delay (Fmt.seconds_per_frame () /. 2.)
          end else
            ( fill write ; write <- (write + 1) mod (2*nb_blocks) )
        done

  method get_frame buf =
    assert (0 = AFrame.position buf) ;
    let buffer =
      if bufferize then
      begin
        (* Check that the writer still has an advance.
         * Otherwise play blank for waiting.. *)
        if write = read then begin
          log#f 4 "No available frame!" ;
          blank
        end else
          let b = buffer.(read mod nb_blocks) in
            read <- (read + 1) mod (2*nb_blocks) ;
            b
      end
      else 
        let dev = self#get_device in
        self#get_block dev
    in
    let fbuf = AFrame.get_float_pcm buf in
      for c = 0 to Array.length fbuf - 1 do
        Float_pcm.from_s16le fbuf 0 buffer 0 samples_per_frame
      done;
      AFrame.add_break buf samples_per_frame

  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_reset = ()

end

let () =
  Lang.add_operator "input.jack"
    ["buffer_size",
      Lang.int_t, Some (Lang.int 2),
      Some "Set buffer size, in frames. 0 means unbuffered input.";
     "server",
      Lang.string_t, Some (Lang.string ""),
      Some "Jack server to connect to." ]
    ~category:Lang.Input
    ~descr:"Get stream from jack."
    (fun p ->
       let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
       let server = Lang.to_string (List.assoc "server" p) in
         ((new jack_in ~nb_blocks ~server):>Source.source))

