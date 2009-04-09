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

open Vorbis_encoded

(** Send Vorbis through a shout connection *)

let proto = vorbis_proto @ Ogg_output_shout.proto

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
    (fun p _ -> 
       let e f v = f (List.assoc v p) in
       let bitrate = (e Lang.to_int "bitrate") * 1000 in
       let min_bitrate = (e Lang.to_int "min_bitrate") * 1000 in
       let max_bitrate = (e Lang.to_int "max_bitrate") * 1000 in
       let freq = e Lang.to_int "samplerate" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let streams =
         ["vorbis",create ~quality:0. ~mode:ABR
                          ~bitrate:(bitrate, min_bitrate, max_bitrate)
                          freq stereo]
       in
       let channels =
         if not stereo then 1 else Fmt.channels () 
       in
       let icecast_info =
         {
          Icecast2.
            quality    = None;
            bitrate    = Some (bitrate / 1000);
            channels   = Some channels;
            samplerate = Some freq
         }
       in
      ((new Ogg_output_shout.to_shout ~skeleton ~icecast_info ~streams p):>Source.source))

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
    (fun p _ -> 
       let e f v = f (List.assoc v p) in
       let bitrate = (e Lang.to_int "bitrate") * 1000 in
       let freq = e Lang.to_int "samplerate" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let streams =
         ["vorbis",create ~quality:0. ~mode:CBR
                          ~bitrate:(bitrate, bitrate, bitrate)
                          freq stereo]
       in
       let channels =
         if not stereo then 1 else Fmt.channels ()
       in
       let icecast_info =
         {
          Icecast2.
            quality    = None;
            bitrate    = Some (bitrate / 1000);
            channels   = Some channels;
            samplerate = Some freq
         }
       in
      ((new Ogg_output_shout.to_shout ~skeleton ~icecast_info ~streams p):>Source.source))

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
    (fun p _ -> 
       let e f v = f (List.assoc v p) in
       let quality = (e Lang.to_float "quality") *. 0.1 in
       let freq = e Lang.to_int "samplerate" in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let streams =
         ["vorbis",create ~quality ~mode:VBR
                          ~bitrate:(0, 0, 0)
                          freq stereo]
       in
       let channels =
         if not stereo then 1 else Fmt.channels ()
       in
       let icecast_info =
         {
          Icecast2.
            bitrate    = None;
            quality    = Some (string_of_float quality);
            channels   = Some channels;
            samplerate = Some freq
         }
       in
      ((new Ogg_output_shout.to_shout ~skeleton ~icecast_info ~streams p):>Source.source))


