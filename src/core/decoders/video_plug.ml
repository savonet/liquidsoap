(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let log = Log.make ["decoder"; "video"; "metadata"]

(** Configuration keys. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "video_metadata")
    "Mime-types used for decoding metadata using native parser."
    ~d:["video/x-msvideo"; "video/mp4"]

let conf_video =
  Dtools.Conf.void
    ~p:(Decoder.conf_decoder#plug "video_metadata")
    "Native video metadata parser settings."

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "video_metadata")
    "File extensions used for decoding metadata using native parser."
    ~d:["avi"; "mp4"]

let priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "video_metadata")
    "Priority for the native video metadata decoder" ~d:1

let get_tags ~metadata:_ ~extension ~mime fname =
  try
    if
      not
        (Decoder.test_file ~log ~mime ~extension ~mimes:(Some mime_types#get)
           ~extensions:(Some file_extensions#get) fname)
    then raise Metadata.Invalid;
    Metadata.Video.parse_file fname
  with
    | Metadata.Invalid -> []
    | e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Error while decoding file tags: %s"
             (Printexc.to_string e));
        raise Not_found

let () =
  Plug.register Request.mresolvers "video-metadata"
    ~doc:"Native metadata decoder for videos."
    { Request.priority = (fun () -> priority#get); resolver = get_tags }
