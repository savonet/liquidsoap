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

open Unix

let log = Dtools.Log.make ["parser";"http"]

let split_lines buf =
  Pcre.split ~pat:"[\r\n]+" buf

(* This parser cannot detect the format !! *)
let parse_mpegurl string =
  let lines = split_lines string in
  let urls =
    List.filter
      (fun s -> String.length s > 0 && s.[0] <> '#')
      lines
  in
  List.rev (List.map (fun t -> [],t) urls)

let parse_scpls string =
  let string = Pcre.replace ~pat:"#[^\\r\\n]*[\\n\\r]+" string in
  (* Format check, raise Not_found if invalid *)
  ignore(Pcre.exec ~pat:"^[\\r\\n\\s]*\\[playlist\\]" (String.lowercase string)) ;
  let lines = split_lines string in
  let urls =
    List.map
      (fun s ->
           try
	     let rex = Pcre.regexp ~flags:[`CASELESS] "file\\d*\\s*=\\s*(.*)\\s*" in
             let sub = Pcre.exec ~rex:rex s in
             Pcre.get_substring sub 1
	   with Not_found -> ""
      )
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
  List.rev (List.map (fun t -> [],t) urls)

let () =
  Playlist_parser.parsers#register "audio/x-scpls" { Playlist_parser.strict = true; Playlist_parser.parser = parse_scpls } ;
  Playlist_parser.parsers#register "audio/x-mpegurl" { Playlist_parser.strict = false; Playlist_parser.parser = parse_mpegurl }
