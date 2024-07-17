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

(** Plug for playlist parsing, in which [src/playlists] plugins come. *)

open Dtools

let log = Log.make ["playlist parser"]

let conf_playlists =
  Conf.void ~p:(Configure.conf#plug "playlists")
    "Playlist formats"
let conf_mime_types =
  Conf.void ~p:(conf_playlists#plug "mime_types")
    "Mime-types used for guessing formats"
    ~comments:[
      "When a mime-type is available (e.g. with input.http), it can be used";
      "to guess which playlist format to use.";
      "This section contains the listings used for that detection, which you";
      "might want to tweak if you encounter a new mime-type.";
      "If you feel that new mime-types should be permanently added, please";
      "contact the developpers."
    ]

(** A playlist is list of metadatas,uri *)
type playlist = ((string * string) list * string) list

(** A plugin is a boolean and a parsing function *)
type plugin = { 
  strict : bool; (* true is the format can be detected *)
  parser: string -> playlist
}

(** Parsers are given a string and return a list of metadatas,uri, if possible. *)
let parsers : plugin Plug.plug =
  Plug.create
    ~doc:"Method to parse playlist." "playlist formats"


exception Exit of (string*playlist)
(** Get a valid parser for [string].
  * The validity is not based on file type but only on success of the
  * parser instantiation.
  * Being based on file extension is weak, and troublesome when accessing a
  * remote file -- that would force us to create a local temporary file with the
  * same extension. *)
let search_valid string =
  try
    let plugins = parsers#get_all in
    (* Try strict plugins first *)
    let compare (_,a) (_,b) = compare b.strict a.strict in
    let plugins = List.sort compare plugins in
    List.iter 
      (fun (format,plugin) ->
           log#f 4 "Trying %s parser" format ;
	   match try Some (plugin.parser string) with _ -> None
	      with
	        | Some d -> raise (Exit (format,d))
		| None -> () )
      plugins;
    log#f 3 "No format found";
    raise Not_found
  with
    | Exit (format,d) -> format,d
	    
