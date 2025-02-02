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

let log = Log.make ["playlist"; "basic"]
let split_lines buf = Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r\n]+") buf

let parse_meta =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised
      Liquidsoap_lang.Parser.annotate_metadata_entry
  in
  let rec f cur s =
    try
      let lexbuf = Sedlexing.Utf8.from_string s in
      let tokenizer = Liquidsoap_lang.Preprocessor.mk_tokenizer lexbuf in
      let metadata = processor tokenizer in
      let b = Buffer.create 10 in
      let rec g () =
        match Sedlexing.next lexbuf with
          | Some c ->
              Buffer.add_utf_8_uchar b c;
              g ()
          | None -> Buffer.contents b
      in
      f (metadata :: cur) (g ())
    with _ -> if cur <> [] then (List.rev cur, s) else ([], "")
  in
  f []

let parse_extinf s =
  try
    let rex = Re.Pcre.regexp "#EXTINF:(\\d*)\\s*(.*)" in
    let sub = Re.Pcre.exec ~rex s in
    let meta, song =
      match Re.Pcre.get_substring sub 2 with
        | "" -> ([], "")
        | s when s.[0] = ',' -> ([], String.sub s 1 (String.length s - 1))
        | s -> parse_meta s
    in
    let meta =
      match Re.Pcre.get_substring sub 1 with
        | "" -> meta
        | duration -> ("extinf_duration", duration) :: meta
    in
    let lines = Re.Pcre.split ~rex:(Re.Pcre.regexp "\\s*-\\s*") song in
    meta
    @
    match lines with
      | [] | [""; ""] -> []
      | [""; song] -> [("song", String.trim song)]
      | [artist; title] ->
          [("artist", String.trim artist); ("title", String.trim title)]
      | _ when song = "" -> []
      | _ -> [("song", String.trim song)]
  with Not_found -> []

(* This parser cannot detect the format !! *)
let parse_mpegurl ?pwd string =
  let lines = List.filter (fun x -> x <> "") (split_lines string) in
  let is_info line = Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "^#EXTINF") line in
  let skip_line line = line.[0] == '#' in
  let rec get_urls cur lines =
    match lines with
      | x :: y :: lines when is_info x && not (skip_line y) ->
          let metadata = parse_extinf x in
          get_urls ((metadata, Playlist_parser.get_file ?pwd y) :: cur) lines
      | x :: lines when not (skip_line x) ->
          get_urls (([], Playlist_parser.get_file ?pwd x) :: cur) lines
      | _ :: lines -> get_urls cur lines
      | [] -> List.rev cur
  in
  get_urls [] lines

let parse_scpls ?pwd string =
  let string =
    Re.Pcre.substitute
      ~rex:(Re.Pcre.regexp "#[^\\r\\n]*[\\n\\r]+")
      ~subst:(fun _ -> "")
      string
  in
  (* Format check, raise Not_found if invalid *)
  ignore
    (Re.Pcre.exec
       ~rex:(Re.Pcre.regexp "^[\\r\\n\\s]*\\[playlist\\]")
       (String.lowercase_ascii string));
  let lines = split_lines string in
  let urls =
    List.map
      (fun s ->
        try
          let rex =
            Re.Pcre.regexp ~flags:[`CASELESS] "file\\d*\\s*=\\s*(.*)\\s*"
          in
          let sub = Re.Pcre.exec ~rex s in
          Re.Pcre.get_substring sub 1
        with Not_found -> "")
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
  List.map (fun t -> ([], Playlist_parser.get_file ?pwd t)) urls

let _ =
  Builtins_resolvers.add_playlist_parser ~format:"SCPLS" "scpls" parse_scpls

let _ = Builtins_resolvers.add_playlist_parser ~format:"M3U" "m3u" parse_mpegurl
