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

open Source

let log = Dtools.Log.make ["input";"jack"]

class jack_in ~nb_blocks ~server =
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
  let bytes_per_sample = 2 in
  let blank () = 
    String.make (samples_per_frame * channels * bytes_per_sample) '0' 
  in
object (self)
  inherit active_source
  inherit [string] IoRing.input 
      ~nb_blocks ~blank () as ioring

  method stype = Infallible
  method is_ready = true
  method abort_track = ()
  method remaining = -1

  val mutable sample_freq = Fmt.samples_per_second ()

  val mutable device = None

  method close = 
    match device with
      | Some d -> 
          Bjack.close d ;
          device <- None
      | None -> ()

  method private get_device = 
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

  method private pull_block block =
        let dev = self#get_device in 
        let length = String.length block in
        let ans = ref (Bjack.read dev length) in
          while String.length !ans < length do
            Thread.delay (Fmt.seconds_per_frame () /. 2.) ;
            let len = length - (String.length !ans) in
            let tmp = Bjack.read dev len in
            ans := !ans ^ tmp 
         done;
         String.blit !ans 0 block 0 length

  method private get_frame buf =
    assert (0 = AFrame.position buf) ;
    let buffer = ioring#get_block in
    let fbuf = AFrame.get_float_pcm buf in
      Float_pcm.from_s16le fbuf 0 buffer 0 samples_per_frame ;
      AFrame.add_break buf samples_per_frame

  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_reset = ()

  method is_active = true
end

let () =
  Lang.add_operator "input.jack"
    ["buffer_size",
      Lang.int_t, Some (Lang.int 2),
      Some "Set buffer size, in frames. Must be >= 1.";
     "server",
      Lang.string_t, Some (Lang.string ""),
      Some "Jack server to connect to." ]
    ~category:Lang.Input
    ~descr:"Get stream from jack."
    (fun p _ ->
       let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
       let server = Lang.to_string (List.assoc "server" p) in
         ((new jack_in ~nb_blocks ~server):>Source.source))

