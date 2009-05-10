(*****************************************************************************

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

(** Output using ao lib. *)

open Ao

class output ~nb_blocks ~driver 
             ~options source start =
  let channels = Fmt.channels () in
  let samples_per_frame = Fmt.samples_per_frame () in
  let samples_per_second = Fmt.samples_per_second () in
  let bytes_per_sample = 2 in
  let blank () =
    String.make (samples_per_frame * channels * bytes_per_sample) '0'
  in
object (self)
  inherit Output.output ~name:"ao" ~kind:"output.ao" source start
  inherit [string] IoRing.output ~nb_blocks ~blank 
                                 ~blocking:true () as ioring

  val mutable device = None

  method get_device = 
    match device with
      | Some d -> d
      | None ->
          (* Wait for things to settle *)                                           
          Thread.delay (5. *. (Fmt.seconds_per_frame ())); 
          let driver = 
            if driver = "" then
              get_default_driver ()
            else
              find_driver driver
           in
           let dev = open_live ~driver ~options
                        ~rate:samples_per_second
                        ~bits:(bytes_per_sample * 8)
                        ~channels:channels ()
           in
           device <- Some dev ;
           dev

  method close = 
    match device with 
      | Some d -> 
          Ao.close d ;
          device <- None
      | None -> ()

  method push_block data =
    let dev = self#get_device in
    play dev data

  method output_send wav =
    let push data = 
      ignore (Float_pcm.to_s16le (AFrame.get_float_pcm wav) 0 (AFrame.size wav)
                data 0) 
    in
    ioring#put_block push

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

      "buffer_size",
      Lang.int_t, Some (Lang.int 2),
      Some "Set buffer size, in frames.";

      "options",
      Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
      Some (Lang.list []),
      Some "List of parameters, depends on driver.";

      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Output stream to local sound card using libao."
    (fun p _ ->
       let start = Lang.to_bool (List.assoc "start" p) in
       let driver = Lang.to_string (List.assoc "driver" p) in
       let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
       let options =
         List.map
           (fun x ->
              let a,b = Lang.to_product x in
                Lang.to_string a, Lang.to_string b)
           (Lang.to_list (List.assoc "options" p))
       in
       let source = List.assoc "" p in
         ((new output ~nb_blocks ~driver 
                      ~options source start):>Source.source))
