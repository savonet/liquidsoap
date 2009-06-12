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

(** Outputs using the LAME encoder for MP3. *)

open Source
open Dtools
open Lame

let create_encoder ~samplerate ~bitrate ~quality ~stereo =
  let enc = Lame.create_encoder () in
    (* Input settings *)
    Lame.set_in_samplerate enc (Fmt.samples_per_second ()) ;
    Lame.set_num_channels enc (Fmt.channels ()) ;
    (* Output settings *)
    Lame.set_mode enc (if stereo then Lame.Stereo else Lame.Mono) ;
    Lame.set_quality enc quality ;
    Lame.set_out_samplerate enc samplerate ;
    Lame.set_brate enc bitrate ;
    Lame.init_params enc ;
    enc

class virtual base ~quality ~bitrate ~stereo ~samplerate =
object (self)

  val mutable encoder : Lame.encoder option = None

  method encode frame start len =
    let e = Utils.get_some encoder in
    let b = AFrame.get_float_pcm frame in
    let start = Fmt.samples_of_ticks start in
    let len = Fmt.samples_of_ticks len in
    if Fmt.channels () = 1 then
      Lame.encode_buffer_float_part e b.(0) b.(0) start len
    else
      Lame.encode_buffer_float_part e b.(0) b.(1) start len

  method output_start =
    let enc = create_encoder ~quality ~bitrate ~stereo ~samplerate in
      encoder <- Some enc ;
end

(** Output in an MP3 file *)

class to_file
  ~infallible ~on_start ~on_stop
  ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~filename ~samplerate ~bitrate ~quality ~stereo ~autostart source =
object (self)
  inherit
    Output.encoded ~name:filename ~kind:"output.file" ~autostart source
                   ~infallible ~on_start ~on_stop
  inherit
     File_output.to_file ~reload_delay ~reload_predicate ~reload_on_metadata
                         ~append ~perm ~dir_perm filename
     as to_file
  inherit base ~quality ~bitrate ~stereo ~samplerate as base

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
  Lang.add_operator "output.file.mp3"
    ([ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

      "samplerate",
      Lang.int_t,
      Some (Lang.int 44100),
      None ;

      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      None ;

      "quality",
      Lang.int_t,
      Some (Lang.int 5),
      None ;

      "stereo",
      Lang.bool_t,
      Some (Lang.bool true),
      None]

      @ File_output.proto @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:"Output the source's stream as an MP3 file."
    (fun p _ ->
       let e f v = f (List.assoc v p) in
       let quality = e Lang.to_int "quality" in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
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
             ~quality ~bitrate ~samplerate ~stereo ~autostart source):>source))
