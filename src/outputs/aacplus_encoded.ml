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

(** Outputs using the Aacplus encoder for AAC+. *)

open Source
open Dtools

let () = Aacplus.init ()

let create_encoder ~samplerate ~bitrate =
  let bitrate = bitrate*1000 in
  Aacplus.create ~channels:(Fmt.channels ()) ~samplerate ~bitrate ()

class virtual base ~bitrate ~samplerate =
object (self)

  val mutable encoder : (Aacplus.t*string) option = None

  val mutable rem = Array.create (Fmt.channels ()) [||]

  val encoded = Buffer.create 1024

  method encode frame start len =
    let (e,tmp) = Utils.get_some encoder in
    let b = AFrame.get_float_pcm frame in
    let start = Fmt.samples_of_ticks start in
    let len = Fmt.samples_of_ticks len in
    let f s l x = 
      Array.sub x s l
    in
    let block_size = (String.length tmp) / (2 * (Fmt.channels ())) in
    let b = Array.map (f start len) b in
    let g i x =
      Array.append rem.(i) x
    in
    rem <- Array.mapi g b;
    let rec put () = 
      let len = Array.length rem.(0) in
      if len > block_size then
       begin
        let b = Array.map (f 0 block_size) rem in
        rem <- Array.map (f block_size (len-block_size)) rem ;
        ignore(Float_pcm.to_s16le b 0 block_size tmp 0);
        Buffer.add_string encoded (Aacplus.encode e tmp) ;
        put ()
       end
    in
    put () ;
    let ret = Buffer.contents encoded in
    Buffer.reset encoded ;
    ret

  method output_start =
    let enc = create_encoder ~bitrate ~samplerate in
    let len = Aacplus.data_length enc in
    let tmp = String.create len in
    encoder <- Some (enc,tmp) 
end

(** Output in an AAC+ file *)

class to_file
  ~infallible ~on_start ~on_stop
  ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~filename ~samplerate ~bitrate ~autostart source =
object (self)
  inherit
    Output.encoded ~name:filename ~kind:"output.file" ~autostart source
                   ~infallible ~on_start ~on_stop
  inherit
     File_output.to_file ~reload_delay ~reload_predicate ~reload_on_metadata
                         ~append ~perm ~dir_perm filename
     as to_file
  inherit base ~bitrate ~samplerate as base

  method reset_encoder m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    ""

  method output_start =
      base#output_start ;
      to_file#file_start

  method output_stop =
    to_file#file_stop

  method output_reset = ()
end

let () =
  Lang.add_operator "output.file.aacplus"
    ([ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

      "samplerate",
      Lang.int_t,
      Some (Lang.int 44100),
      None ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 64),
      None 
    ]
      @ File_output.proto @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:"Output the source's stream as an AAC+ file."
    (fun p _ ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let samplerate = e Lang.to_int "samplerate" in
       let bitrate = e Lang.to_int "bitrate" in
       let filename = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
       let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
       let on_start =
         let f = List.assoc "on_start" p in
           fun () -> ignore (Lang.apply f [])
       in
       let on_stop =
         let f = List.assoc "on_stop" p in
           fun () -> ignore (Lang.apply f [])
       in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
         ((new to_file ~filename
             ~infallible ~on_start ~on_stop
             ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~bitrate ~samplerate ~autostart source):>source))
