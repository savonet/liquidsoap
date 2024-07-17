(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

let lastfm req ~log maxtime =
  try
    match Lastfm.get (Printf.sprintf "lastfm:%s" req) with
      | (metadata,uri) :: _ ->
            [Request.indicator ~metadata:(Utils.hashtbl_of_list metadata) uri]
      | _ -> log "empty playlist"; []
  with
    | Lastfm.Error e -> log (Printf.sprintf "lastfm: Error %s" (Lastfm.string_of_error e)); []
    | _ -> log "lastfm request failed: unknown reason"; []

let () =
  Request.protocols#register "lastfm"
    ~sdoc:("Play a song on last.fm.")
    { Request.resolve = lastfm ; Request.static = false }
