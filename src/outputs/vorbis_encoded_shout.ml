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

open Vorbis_encoded

(** Send Vorbis through a shout connection *)

let no_mount = "Use [name].ogg"
let no_name = "Use [mount]"

let proto =
  vorbis_proto @ Icecast2.proto @
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

  let bitrate =
    match mode with
      | ABR
      | CBR -> (e Lang.to_int "bitrate") * 1000
      | VBR -> 0
  in
  let min_bitrate =
    match mode with
      | ABR -> (e Lang.to_int "min_bitrate") * 1000
      | CBR -> bitrate
      | VBR -> 0
  in
  let max_bitrate =
    match mode with
      | ABR -> (e Lang.to_int "max_bitrate") * 1000
      | CBR -> bitrate
      | VBR -> 0
  in
  let quality =
    match mode with
      | ABR
      | CBR -> 0.
      | VBR -> (e Lang.to_float "quality") *. 0.1
  in
  let freq = e Lang.to_int "samplerate" in
  let ibitrate =
    match mode with
      | ABR
      | CBR -> string_of_int bitrate
      | VBR -> Printf.sprintf "Quality %.1f" (quality *. 10.)
  in

  let source = List.assoc "" p in

object (self)
  inherit [Vorbis.Encoder.t] Icecast2.output
    ~bitrate:ibitrate ~mount ~name ~source p as super
  inherit base freq stereo as base

  method new_encoder stereo =
    let channels = 
      if not stereo then 1 else Fmt.channels () in
    let enc =
      (* TODO: log message when the creation of the encoder fails *)
      match mode with
        | ABR
        | CBR -> Vorbis.Encoder.create channels freq
                   (max_bitrate) (bitrate) (min_bitrate)(* max nom min *)
        | VBR -> Vorbis.Encoder.create_vbr channels freq quality
    in
      encoder <- Some enc;
      enc

  method output_start =
    ignore(self#new_encoder stereo) ;
    super#output_start 

  method output_stop =
    super#output_stop ;
    ignore(base#end_of_os encoder)
end

let () = (* Average BitRate *)
  Lang.add_operator "output.icecast.vorbis.abr"
    (proto @ [
      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      Some "Target bitrate (in kbps).";

      "min_bitrate",
      Lang.int_t,
      Some (Lang.int 118),
      Some "Minimum bitrate (in kbps).";

      "max_bitrate",
      Lang.int_t,
      Some (Lang.int 138),
      Some "Maximum bitrate (in kbps)." ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis stream to an "
            ^ "Icecast-compatible server in Average BitRate mode.")
    (fun p -> ((new to_shout ~mode:ABR p):>Source.source))

let () = (* Constant BitRate *)
  Lang.add_operator "output.icecast.vorbis.cbr"
    (proto @ [
      "bitrate",
      Lang.int_t,
      Some (Lang.int 128),
      Some "Bitrate (in kbps)." ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis stream to an "
            ^ "Icecast-compatible server in Constant BitRate mode.")
    (fun p -> ((new to_shout ~mode:CBR p):>Source.source))

let () = (* Variable BitRate *)
  Lang.add_operator "output.icecast.vorbis"
    (proto @ [
      "quality",
      Lang.float_t,
      Some (Lang.float 2.),
      Some ("Desired quality level, currently from -1. to 10. (low to high).")])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Vorbis stream to an "
            ^ "Icecast-compatible server in Variable BitRate mode.")
    (fun p -> ((new to_shout ~mode:VBR p):>Source.source))


