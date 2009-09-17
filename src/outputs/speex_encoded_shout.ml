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

open Speex_encoded

(** Send Speex through a shout connection *)

let proto = Speex_encoded.speex_proto @ Ogg_output_shout.proto

let () =
  Lang.add_operator "output.icecast.speex"
    proto
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Speex stream to an \
             Icecast-compatible server.")
    (fun p _ ->
       let e f v = f (List.assoc v p) in
       let mode =
         match e Lang.to_string "mode" with
           | "narrowband" -> Speex.Narrowband
           | "wideband" -> Speex.Wideband
           | "ultra-wideband" -> Speex.Ultra_wideband
           | _ -> failwith "Unknown speex mode"
       in
       let stereo = e Lang.to_bool "stereo" in
       let skeleton = e Lang.to_bool "skeleton" in
       let bitrate = (e Lang.to_int "bitrate") in
       let bitrate =
         if bitrate > 0 then
           bitrate * 1000
         else
           bitrate
       in
       let freq = e Lang.to_int "samplerate" in
       let fpp = e Lang.to_int "frames_per_packet" in
       let vbr = e Lang.to_bool "vbr" in
       let abr = (e Lang.to_int "abr") * 1000 in
       let complexity = e Lang.to_int "complexity" in
       let quality = e Lang.to_int "quality" in
       let streams =
        ["speex",Speex_encoded.create
                        ~freq ~stereo ~mode
                        ~bitrate ~vbr ~fpp
                        ~complexity ~abr ~quality ()]
       in
       let channels =
         if not stereo then 1 else Fmt.channels ()
       in
       let bitrate =
         if bitrate > 0 then
           Some (bitrate / 1000)
         else
           None
       in
       let quality =
         if quality > 0 then
           Some (string_of_int quality)
         else
           None
       in
       let icecast_info =
         {
          Icecast2.
            bitrate    = bitrate;
            quality    = quality;
            channels   = Some channels;
            samplerate = Some freq
         }
       in
      ((new Ogg_output_shout.to_shout
          ~skeleton ~icecast_info ~streams p):>Source.source))
