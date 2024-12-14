(* -*- mode: tuareg; -*- *)
(*****************************************************************************

    Liquidsoap, a programmable stream generator.
    Copyright 2003-2016 Savonet team

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

let conf_harbor =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "harbor")
    "Harbor settings (Icecast/shoutcast stream receiver)."

let conf_harbor_bind_addrs =
  Dtools.Conf.list
    ~p:(conf_harbor#plug "bind_addrs")
    ~d:["0.0.0.0"] "IP addresses on which the harbor should listen."

let conf_harbor_max_conn =
  Dtools.Conf.int
    ~p:(conf_harbor#plug "max_connections")
    ~d:128 "Maximum of pending source requests per port."

let conf_pass_verbose =
  Dtools.Conf.bool
    ~p:(conf_harbor#plug "verbose")
    ~d:false "Display passwords, for debugging."

let conf_revdns =
  Dtools.Conf.bool
    ~p:(conf_harbor#plug "reverse_dns")
    ~d:false
    "Perform reverse DNS lookup to get the client's hostname from its IP."

let conf_icy_metadata =
  Dtools.Conf.list
    ~p:(conf_harbor#plug "icy_formats")
    ~d:
      [
        "audio/mpeg";
        "audio/aacp";
        "audio/aac";
        "audio/x-aac";
        "audio/wav";
        "audio/wave";
        "audio/flac";
        "audio/x-flac";
      ]
    "Content-type (mime) of formats which allow shout metadata update."

let conf_map_song_metadata =
  Dtools.Conf.bool
    ~p:(conf_harbor#plug "map_song_metadata")
    ~d:true
    "If `true`, `\"song\"` metadata in icecast metadata update is mapped to \
     `\"title\"` unless on of: `\"artist\"` or `\"title\"` metadata is also \
     passed in which case `\"song\"` metadata is removed as it usually \
     contains redundant info that confuses the system. Metadata are passed \
     as-is when `false`."

let conf_timeout =
  Dtools.Conf.float
    ~p:(conf_harbor#plug "timeout")
    ~d:120. "Timeout for network operations (in seconds)."

let conf_accept_timeout =
  Dtools.Conf.float
    ~p:(conf_harbor#plug "accept_timeout")
    ~d:3. "Timeout for network accept operations (in seconds)."

let log = Log.make ["harbor"]
