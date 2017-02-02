(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

let () =
  let add_http_request = Lang_builtins.add_http_request (module Https_secure_transport) in
  add_http_request
    "https.get"
    "Perform a full https GET request and return (status,headers),data."
    Lang_builtins.Get;
  add_http_request
    "https.post"
    "Perform a full https POST request and return (status,headers),data."
    Lang_builtins.Post;
  add_http_request
    "https.put"
    "Perform a full https PUT request and return (status,headers),data."
    Lang_builtins.Put;
  add_http_request
    "https.head"
    "Perform a full https HEAD request and return (status,headers),data."
    Lang_builtins.Head;
  add_http_request
    "https.delete"
    "Perform a full https DELETE request and return (status,headers),data."
    Lang_builtins.Delete
