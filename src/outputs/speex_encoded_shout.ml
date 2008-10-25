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

open Speex_encoded

(** Send Speex through a shout connection *)

let no_mount = "Use [name].spx"
let no_name = "Use [mount]"

let proto =
  speex_proto @ Icecast2.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "", Lang.source_t, None, None ]

class to_shout ~mode p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let name = s  "name" in
  let mount = s "mount" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 ((List.assoc "mount" p),
                  "Either name or mount must be defined"))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then name ^ ".ogg" else mount
  in

  let stereo = e Lang.to_bool "stereo" in

  let bitrate = (e Lang.to_int "bitrate") in
  let bitrate = 
    if bitrate > 0 then
      bitrate * 1000
    else
      bitrate
  in
  let ibitrate = string_of_int bitrate in
  let freq = e Lang.to_int "samplerate" in
  let fpp = e Lang.to_int "frames_per_packet" in
  let vbr = e Lang.to_bool "vbr" in
  let abr = (e Lang.to_int "abr") * 1000 in
  let complexity = e Lang.to_int "complexity" in
  let quality = e Lang.to_int "quality" in

  let source = List.assoc "" p in

object (self)
  inherit [Speex.Encoder.t] Icecast2.output
    ~bitrate:ibitrate ~mount ~name ~source p as super
  inherit base freq stereo mode bitrate vbr fpp complexity abr quality as base

  method set_encoder e = encoder <- Some e

  method output_start =
    self#new_encoder stereo ;
    super#output_start 

  method output_stop =
    let b = base#end_of_os in
    super#send b;
    super#output_stop
end

let () = 
  Lang.add_operator "output.icecast.speex"
    proto 
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Speex stream to an \
             Icecast-compatible server.")
    (fun p ->
       let e f v = f (List.assoc v p) in 
       let mode =
         match e Lang.to_string "mode" with
           | "narrowband" -> Speex.Narrowband
           | "wideband" -> Speex.Wideband
           | "ultra-wideband" -> Speex.Ultra_wideband
           | _ -> failwith "Unknown speex mode"
       in
       ((new to_shout ~mode p):>Source.source))



