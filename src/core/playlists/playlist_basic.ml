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

type cue_track = {
  number : int;
  track_performer : string option;
  track_title : string option;
  indexes : (int, float) Hashtbl.t;
}

type cue_sheet = {
  file : string;
  performer : string option;
  title : string option;
  tracks : cue_track list;
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
          with Not_found -> find (x :: cur) rem)
      | [] -> raise Not_found
  in
  let title, cur, rem = find [] l in
  (List.rev cur @ rem, title)

let parse_tracks index lines =
  let rec parse tracks track rem =
    match rem with
      | [] -> List.rev (track :: tracks)
      | x :: rem -> (
          try
            parse_track x (fun index ->
                let tracks = track :: tracks in
                let track =
                  {
                    number = index;
                    track_title = None;
                    track_performer = None;
                    indexes = Hashtbl.create 1;
                  }
                in
                parse tracks track rem)
          with _ ->
            let track =
              parse_title x (fun title ->
                  if title <> None then { track with track_title = title }
                  else track)
            in
            let track =
              parse_performer x (fun performer ->
                  if performer <> None then
                    { track with track_performer = performer }
                  else track)
            in
            begin
              try parse_index x (Hashtbl.replace track.indexes)
              with Not_found -> ()
            end;
            parse tracks track rem)
  in
  let track =
    {
      number = index;
      track_title = None;
      track_performer = None;
      indexes = Hashtbl.create 1;
    }
  in
  parse [] track lines

let parse_cue ?pwd string =
  let strings = split_lines string in
  let strings =
    List.map
      (fun string ->
        Re.Pcre.substitute ~rex:(Re.Pcre.regexp "^\\s+")
          ~subst:(fun _ -> "")
          string)
      strings
  in
  let strings = List.filter (fun s -> s <> "") strings in
  let strings, file = find_file strings in
  let rec parse sheet rem =
    match rem with
      | [] -> sheet
      | x :: rem -> (
          let sheet =
            parse_title x (fun title ->
                if title <> None then { sheet with title } else sheet)
          in
          let sheet =
            parse_performer x (fun performer ->
                if performer <> None then { sheet with performer } else sheet)
          in
          try
            parse_track x (fun index ->
                { sheet with tracks = parse_tracks index rem })
          with Not_found -> parse sheet rem)
  in
  let sheet = { file; performer = None; title = None; tracks = [] } in
  let sheet = parse sheet strings in
  let export_track ?cue_out track =
    let metadata = [("track", string_of_int track.number)] in
    let maybe label value metadata =
      match value with
        | Some value -> (label, value) :: metadata
        | None -> metadata
    in
    let metadata = maybe "artist" sheet.performer metadata in
    let metadata = maybe "album" sheet.title metadata in
    let metadata = maybe "artist" track.track_performer metadata in
    let metadata = maybe "title" track.track_title metadata in
    let metadata =
      maybe Playlist_parser.conf_cue_in_metadata#get
        (try Some (string_of_float (Hashtbl.find track.indexes 1))
         with _ -> None)
        metadata
    in
    ( maybe Playlist_parser.conf_cue_out_metadata#get cue_out metadata,
      Playlist_parser.get_file ?pwd sheet.file )
  in
  let rec export_tracks cur tracks =
    match tracks with
      | [] ->
          assert (cur == []);
          []
      | [track] -> List.rev (export_track track :: cur)
      | track :: track' :: tracks ->
          let cue_out =
            try Some (string_of_float (Hashtbl.find track'.indexes 0))
            with _ -> (
              try Some (string_of_float (Hashtbl.find track'.indexes 1))
              with _ -> None)
          in
          export_tracks (export_track ?cue_out track :: cur) (track' :: tracks)
  in
  export_tracks [] sheet.tracks

let _ =
  Builtins_resolvers.add_playlist_parser ~format:"SCPLS" "scpls" parse_scpls

let _ = Builtins_resolvers.add_playlist_parser ~format:"CUE" "cue" parse_cue
let _ = Builtins_resolvers.add_playlist_parser ~format:"M3U" "m3u" parse_mpegurl
