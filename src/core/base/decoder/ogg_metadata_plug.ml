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

let log = Log.make ["decoder"; "ogg"; "metadata"]

let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ogg_metadata")
    "Mime-types used for decoding metadata using native ogg metadata parser."
    ~d:["audio/ogg"]

let conf_ogg =
  Dtools.Conf.void
    ~p:(Decoder.conf_decoder#plug "ogg_metadata")
    "Native ogg metadata parser settings."

let conf_separator =
  Dtools.Conf.string ~d:", "
    ~p:(conf_ogg#plug "separator")
    "Separator used to join metadata field with several entries."

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ogg_metadata")
    "File extensions used for decoding metadata using native ogg parser."
    ~d:["ogg"]

let priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "ogg_metadata")
    "Priority for the native ogg metadata decoder" ~d:1

let get_tags ~metadata:_ ~extension ~mime parse fname =
  try
    if
      not
        (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mime_types#get)
           ~extensions:(Some file_extensions#get) fname)
    then raise Metadata.Invalid;
    let m = parse fname in
    let sep = conf_separator#get in
    List.fold_left
      (fun m (key, new_entry) ->
        try
          let old_entry = List.assoc key m in
          (key, old_entry ^ sep ^ new_entry) :: List.remove_assoc key m
        with Not_found -> (key, new_entry) :: m)
      [] m
  with
    | Metadata.Invalid -> []
    | e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Error while decoding file tags: %s"
             (Printexc.to_string e));
        raise Not_found

let () =
  Plug.register Request.mresolvers "ogg_native"
    ~doc:"Native ogg metadata resolver."
    {
      Request.priority = (fun () -> priority#get);
      resolver = get_tags Metadata.OGG.parse_file;
    }
