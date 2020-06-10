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

let conf =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "encoder")
    "Encoder settings"
    ~comments:["Settings for the encoder"]

let conf_meta =
  Dtools.Conf.void ~p:(conf#plug "encoder") "Metadata settings"
    ~comments:["Settings for the encoded metadata."]

(** The list of metadata fields that should be exported when encoding. *)
let conf_export_metadata =
  Dtools.Conf.list ~p:(conf_meta#plug "export") "Exported metdata"
    ~d:
      [
        "artist";
        "title";
        "album";
        "genre";
        "date";
        "tracknumber";
        "comment";
        "track";
        "year";
        "dj";
        "next";
      ]
    ~comments:["The list of labels of exported metadata."]

let string_of_stereo s = if s then "stereo" else "mono"
