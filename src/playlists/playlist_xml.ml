(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

let log = Log.make ["playlist"; "xml"]

let conf_xml =
  Dtools.Conf.list
    ~p:(Playlist_parser.conf_mime_types#plug "xml")
    ~d:
      [
        "video/x-ms-asf";
        "audio/x-ms-asx";
        "text/xml";
        "application/xml";
        "application/smil";
        "application/smil+xml";
        "application/xspf+xml";
        "application/rss+xml";
      ]
    "Mime types associated to XML-based playlist formats"

let tracks ?pwd s =
  try
    let recode_metas m =
      let f = Configure.recode_tag in
      List.map (fun (a, b) -> (f a, f b)) m
    in
    List.map
      (fun (a, b) -> (recode_metas a, Playlist_parser.get_file ?pwd b))
      (Xmlplaylist.tracks s)
  with Xmlplaylist.Error e ->
    log#debug "Parsing failed: %s" (Xmlplaylist.string_of_error e);
    raise (Xmlplaylist.Error e)

let register mimetype =
  Playlist_parser.parsers#register mimetype
    { Playlist_parser.strict = true; Playlist_parser.parser = tracks }

let () =
  ignore (Dtools.Init.at_start (fun () -> List.iter register conf_xml#get))
