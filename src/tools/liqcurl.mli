(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

val parse_response_headers :
  string -> string * int * string * (string * string) list

type after_write = [ `Continue | `Pause ]

val http_connection :
  ?headers:(string * string) list ->
  ?http_version:string ->
  ?timeout:int ->
  ?interface:string ->
  url:string ->
  request:[< `Delete | `Get | `Head | `Post of string | `Put of string ] ->
  on_response_header_data:(string -> unit) ->
  on_body_data:(string -> after_write) ->
  unit ->
  Curl.handle

val http_request :
  ?headers:(string * string) list ->
  ?http_version:string ->
  ?timeout:int ->
  ?interface:string ->
  follow_redirect:bool ->
  url:string ->
  request:[< `Delete | `Get | `Head | `Post of string | `Put of string ] ->
  on_body_data:(string -> after_write) ->
  unit ->
  string * int * string * (string * string) list
