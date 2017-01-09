(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*****************************************************************************)

open Dtools

let conf_harbor =
  Conf.void ~p:(Configure.conf#plug "harbor")
    "Harbor settings (Icecast/shoutcast stream receiver)."
let conf_harbor_bind_addr =
  Conf.string ~p:(conf_harbor#plug "bind_addr") ~d:"0.0.0.0"
    "IP address on which the harbor should listen."
let conf_harbor_max_conn =
  Conf.int ~p:(conf_harbor#plug "max_connections") ~d:2
    "Maximun of pending source requests per port."
let conf_pass_verbose =
  Conf.bool ~p:(conf_harbor#plug "verbose") ~d:false
    "Display passwords, for debugging."
let conf_revdns =
  Conf.bool ~p:(conf_harbor#plug "reverse_dns") ~d:false
    "Perform reverse DNS lookup to get the client's hostname from its IP."
let conf_icy_metadata =
  Conf.list ~p:(conf_harbor#plug "icy_formats")
  ~d:["audio/mpeg"; "audio/aacp"; "audio/aac"; "audio/x-aac";
      "audio/wav"; "audio/wave"; "audio/x-flac"]
  "Content-type (mime) of formats which allow shout metadata update."
(* 300 sec timeout is the default value in Apache.. *)
let conf_timeout =
  Conf.float ~p:(conf_harbor#plug "timeout") ~d:300.
    "Timeout for network operations."

let log = Log.make ["harbor"]
