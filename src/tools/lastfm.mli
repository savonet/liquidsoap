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

(* liquidsoap API to
 * lastfm protocol *)

(* API is:
 * sessionid,request,options = Lastfm.init uri
 * adjust sessionid request
 * tracks sessionid
 * The module takes care of redundant calls.
 *
 * Another alternative usage is:
 * sessionid,request,options = Lastfm.init uri
 * adjust sessionid request
 * url sessionid opt
 * (optional: clear sessionid)
 * This will return the url of a playlist for this
 * lastfm:// uri
 * When using this API, you should call clear sessionid
 * if anything fails with the playlist
 *
 * You can also use directly Lastfm.get uri
 * that will perform all required actions
 * on its own and return a list of metadatas,uri *) 

type error = Http of string | Init of string | Adjust of string*string | Playlist | Empty
exception Error of error

(* Get meaning of Error e *)
val string_of_error : error -> string

(* [init uri] initiate lastfm session with the required uri
 * Parses and removes optional user, password and options
 * returns session ID,parsed uri,uri options *)
val init : string -> string*string*string

(* [adjust id uri] adjusts lastfm station 
 * for given session ID *)
val adjust : string -> string -> unit

(* [tracks id options] 
 * returns a list of metadatas,uri 
 * from given session *)
val tracks : string -> string -> ((string * string) list * string) list

(* [get uri] performs whole process and
 * outputs a list of metadatas,uri
 * from given lastfm uri *)
val get : string -> ((string * string) list * string) list

(* [url id options] returns the url of the playlist
 * from a lastfm:// uri *)
val url : string -> string -> string

(* [clear id] closes and clear all 
 * informations about the given session ID *)
val clear : string -> unit

