(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Plug for playlist parsing, in which [src/playlists] plugins come. *)

module Http = Liq_http

let log = Log.make ["playlist parser"]

let conf_playlist =
  Dtools.Conf.void ~p:(Configure.conf#plug "playlist") "Playlist formats"

let conf_cue_in_metadata =
  Dtools.Conf.string
    ~p:(conf_playlist#plug "cue_in_metadata")
    ~d:"liq_cue_in" "Cue in metadata for playlists with track index."
    ~comments:
      [
        "Some playlists format, such as CUE files specify index points to start";
        "tracks playback. In this case, tracks are resolved to a annotate: \
         request with";
        "a cue-in metadata containing the index. If you want to make use of \
         this index,";
        "you should specify here what label you want for this metadata.";
      ]

let conf_cue_out_metadata =
  Dtools.Conf.string
    ~p:(conf_playlist#plug "cue_out_metadata")
    ~d:"liq_cue_out" "Cue out metadata for playlists with track index."
    ~comments:
      [
        "Some playlists format, such as CUE files specify index points to start";
        "tracks playback. In this case, tracks are resolved to a annotate: \
         request with";
        "a cue-in metadata containing the index. If you want to make use of \
         this index,";
        "you should specify here what label you want for this metadata.";
      ]

(** A playlist is list of metadatas,uri *)
type playlist = ((string * string) list * string) list

type parser = ?pwd:string -> string -> playlist

(** A plugin is a boolean and a parsing function *)
type plugin = {
  mimes : string list;
  (* true if the format can be automatically detected *)
  strict : bool;
  (* The parser is expected to respect the order
     of the files in the playlist. *)
  parser : parser;
}

(** Parsers are given a string and return a list of metadatas,uri, if possible.
*)
let parsers : plugin Plug.t =
  Plug.create ~doc:"Method to parse playlist." "playlist formats"

let get_file ?pwd file =
  match pwd with
    | Some pwd ->
        if Http.is_url pwd && not (Http.is_url file) then pwd ^ file
        else (
          let f = Filename.concat pwd file in
          if Sys.file_exists f then f else file)
    | None -> file

exception Exit of (string * playlist)

(** Get a valid parser for [string]. The validity is not based on file type but
    only on success of the parser instantiation. Being based on file extension
    is weak, and troublesome when accessing a remote file -- that would force us
    to create a local temporary file with the same extension. *)
let search_valid ?pwd string =
  try
    let plugins = Plug.list parsers in
    (* Try strict plugins first *)
    let compare (_, a) (_, b) = compare b.strict a.strict in
    let plugins = List.sort compare plugins in
    List.iter
      (fun (format, plugin) ->
        log#info "Trying %s parser" format;
        match try Some (plugin.parser ?pwd string) with _ -> None with
          | Some d -> raise (Exit (format, d))
          | None -> ())
      plugins;
    log#important "No format found";
    raise Not_found
  with Exit (format, d) -> (format, d)
