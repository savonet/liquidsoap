(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

let split_lines buf = Pcre.split ~pat:"[\r\n]+" buf

let test_text s =
  match Configure.data_mime with
    | None ->
        ()
    | Some get_mime ->
        let mime = get_mime s in
        if not (Pcre.pmatch ~pat:"text/.*" mime) then (
          log#important "Wrong mime type %s for playlist!" mime ;
          (* TODO this shouldn't be an assert false, it can happen *)
          assert false )

let parse_extinf s =
  try
    let rex = Pcre.regexp "#EXTINF:(\\d+),(.*)" in
    let sub = Pcre.exec ~rex s in
    let duration = Pcre.get_substring sub 1 in
    let song = Pcre.get_substring sub 2 in
    let lines = Pcre.split ~pat:" - " song in
    match lines with
      | [artist; title] ->
          [ ("extinf_duration", duration);
            ("artist", Utils.trim artist);
            ("title", Utils.trim title) ]
      | _ ->
          [("extinf_duration", duration); ("song", Utils.trim song)]
  with Not_found -> []

(* This parser cannot detect the format !! *)
let parse_mpegurl ?pwd string =
  test_text string ;
  let lines = List.filter (fun x -> x <> "") (split_lines string) in
  let is_info line = Pcre.pmatch ~pat:"^#EXTINF" line in
  let skip_line line = line.[0] == '#' in
  let rec get_urls cur lines =
    match lines with
      | x :: y :: lines when is_info x && not (skip_line y) ->
          let metadata = parse_extinf x in
          get_urls ((metadata, Playlist_parser.get_file ?pwd y) :: cur) lines
      | x :: lines when not (skip_line x) ->
          get_urls (([], Playlist_parser.get_file ?pwd x) :: cur) lines
      | _ :: lines ->
          get_urls cur lines
      | [] ->
          List.rev cur
  in
  get_urls [] lines

let parse_scpls ?pwd string =
  test_text string ;
  let string = Pcre.replace ~pat:"#[^\\r\\n]*[\\n\\r]+" string in
  (* Format check, raise Not_found if invalid *)
  ignore
    (Pcre.exec ~pat:"^[\\r\\n\\s]*\\[playlist\\]"
       (String.lowercase_ascii string)) ;
  let lines = split_lines string in
  let urls =
    List.map
      (fun s ->
        try
          let rex =
            Pcre.regexp ~flags:[`CASELESS] "file\\d*\\s*=\\s*(.*)\\s*"
          in
          let sub = Pcre.exec ~rex s in
          Pcre.get_substring sub 1
        with Not_found -> "")
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
  List.map (fun t -> ([], Playlist_parser.get_file ?pwd t)) urls

type cue_track = {
  number: int;
  track_performer: string option;
  track_title: string option;
  indexes: (int, float) Hashtbl.t;
}

type cue_sheet = {
  file: string;
  performer: string option;
  title: string option;
  tracks: cue_track list;
}

let parse_optional parser f =
  try parser (fun x -> f (Some x)) with _ -> f None

let parse_maybe_escaped escaped non_escaped f =
  try escaped f with _ -> non_escaped f

let parse_file s f =
  let ret =
    try
      parse_maybe_escaped (Scanf.sscanf s "FILE %S %s")
        (Scanf.sscanf s "FILE %s %s") (fun x _ -> x)
    with _ -> raise Not_found
  in
  f ret

let parse_title s =
  parse_optional
    (parse_maybe_escaped
       (Scanf.sscanf s "TITLE %S")
       (Scanf.sscanf s "TITLE %s"))

let parse_performer s =
  parse_optional
    (parse_maybe_escaped
       (Scanf.sscanf s "PERFORMER %S")
       (Scanf.sscanf s "PERFORMER %s"))

let parse_track s f =
  try Scanf.sscanf s "TRACK %i %s" (fun i _ -> f i) with _ -> raise Not_found

let parse_index s f =
  try
    Scanf.sscanf s "INDEX %i %i:%i:%i" (fun index min sec frames ->
        let position =
          (float_of_int min *. 60.)
          +. float_of_int sec
          +. (float_of_int frames /. 75.)
        in
        f index position)
  with _ -> raise Not_found

let find_file l =
  let rec find cur rem =
    match rem with
      | x :: rem -> (
        try parse_file x (fun title -> (title, cur, rem))
        with Not_found -> find (x :: cur) rem )
      | [] ->
          raise Not_found
  in
  let title, cur, rem = find [] l in
  (List.rev cur @ rem, title)

let parse_tracks index lines =
  let rec parse tracks track rem =
    match rem with
      | [] ->
          List.rev (track :: tracks)
      | x :: rem -> (
        try
          parse_track x (fun index ->
              let tracks = track :: tracks in
              let track =
                {
                  number= index;
                  track_title= None;
                  track_performer= None;
                  indexes= Hashtbl.create 1;
                }
              in
              parse tracks track rem)
        with _ ->
          let track =
            parse_title x (fun title ->
                if title <> None then {track with track_title= title}
                else track)
          in
          let track =
            parse_performer x (fun performer ->
                if performer <> None then
                  {track with track_performer= performer}
                else track)
          in
          begin
            try parse_index x (Hashtbl.add track.indexes)
            with Not_found -> ()
          end ;
          parse tracks track rem )
  in
  let track =
    {
      number= index;
      track_title= None;
      track_performer= None;
      indexes= Hashtbl.create 1;
    }
  in
  parse [] track lines

let parse_cue ?pwd string =
  test_text string ;
  let strings = split_lines string in
  let strings =
    List.map
      (fun string -> Pcre.replace ~rex:(Pcre.regexp "^\\s+") string)
      strings
  in
  let strings = List.filter (fun s -> s <> "") strings in
  let strings, file = find_file strings in
  let rec parse sheet rem =
    match rem with
      | [] ->
          sheet
      | x :: rem -> (
          let sheet =
            parse_title x (fun title ->
                if title <> None then {sheet with title} else sheet)
          in
          let sheet =
            parse_performer x (fun performer ->
                if performer <> None then {sheet with performer} else sheet)
          in
          try
            parse_track x (fun index ->
                {sheet with tracks= parse_tracks index rem})
          with Not_found -> parse sheet rem )
  in
  let sheet = {file; performer= None; title= None; tracks= []} in
  let sheet = parse sheet strings in
  let export_track ?cue_out track =
    let metadata = [("track", string_of_int track.number)] in
    let maybe label value metadata =
      match value with
        | Some value ->
            (label, value) :: metadata
        | None ->
            metadata
    in
    let metadata = maybe "artist" sheet.performer metadata in
    let metadata = maybe "album" sheet.title metadata in
    let metadata = maybe "artist" track.track_performer metadata in
    let metadata = maybe "title" track.track_title metadata in
    let metadata =
      maybe Playlist_parser.conf_cue_in_metadata#get
        ( try Some (string_of_float (Hashtbl.find track.indexes 1))
          with _ -> None )
        metadata
    in
    ( maybe Playlist_parser.conf_cue_out_metadata#get cue_out metadata,
      Playlist_parser.get_file ?pwd sheet.file )
  in
  let rec export_tracks cur tracks =
    match tracks with
      | [] ->
          assert (cur == []) ;
          []
      | [track] ->
          List.rev (export_track track :: cur)
      | track :: track' :: tracks ->
          let cue_out =
            try Some (string_of_float (Hashtbl.find track'.indexes 0))
            with _ -> (
              try Some (string_of_float (Hashtbl.find track'.indexes 1))
              with _ -> None )
          in
          export_tracks (export_track ?cue_out track :: cur) (track' :: tracks)
  in
  export_tracks [] sheet.tracks

let () =
  Playlist_parser.parsers#register "audio/x-scpls"
    {Playlist_parser.strict= true; Playlist_parser.parser= parse_scpls} ;
  Playlist_parser.parsers#register "application/x-cue"
    {Playlist_parser.strict= true; Playlist_parser.parser= parse_cue} ;
  Playlist_parser.parsers#register "audio/x-mpegurl"
    {Playlist_parser.strict= false; Playlist_parser.parser= parse_mpegurl} ;
  Playlist_parser.parsers#register "audio/mpegurl"
    {Playlist_parser.strict= false; Playlist_parser.parser= parse_mpegurl} ;
  Playlist_parser.parsers#register "application/x-mpegURL"
    {Playlist_parser.strict= false; Playlist_parser.parser= parse_mpegurl}
