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


(* Generic xml parsing module
 * Uses xml-light library *)

type error = XmlError of string | Empty | UnknownType | Internal
type format = Podcast | Xspf | Smil | Asx
exception Error of error

let string_of_error e =
  match e with
    | XmlError s -> Printf.sprintf "xml error: %s" s
    | Empty -> "no interesting data in Xml"
    | UnknownType -> "unknown Xml type"
    | Internal -> "xmliq internal error"

let raise e = raise (Error e)

let rec lowercase_tags xml =
  match xml with
    | Xml.Element(s,l,x) ->
        Xml.Element(String.lowercase s,
                    List.map
                      (fun (a,b) -> (String.lowercase a,b))
                      l,
                    List.map lowercase_tags x)
    | _ -> xml


let rec get_format x =
   (* Assume any rss is a podcast due to broken
    * implementation.. *)
   (* let rec match_rss l =
     match l with
       | (s,s') :: l' when s = "xmlns:itunes" -> Podcast
       | _ :: l' -> match_rss l'
       | [] -> raise UnknownType
   in *)
  match x with
    | Xml.Element(s,l,x) :: l' when s = "playlist" -> Xspf
    | Xml.Element(s,l,x) :: l' when s = "rss" -> Podcast (* match_rss l *)
    | Xml.Element(s,l,x) :: l' when s = "smil" -> Smil
    | Xml.Element(s,l,x) :: l' when s = "asx" -> Asx
    | Xml.Element(s,l,x) :: l' -> get_format (l' @ x)
    | _ :: l' -> get_format l'
    | [] -> raise UnknownType

let podcast_uri l x =
  try
    List.assoc "url" l
  with
    | _ -> raise Empty

let xspf_uri l x =
  match x with
    | Xml.PCData(v) :: [] -> v
    | _ -> raise Empty

let asx_uri l x =
  try
    List.assoc "href" l
  with
    | _ -> raise Empty

(* Should return usefull markup values for parsing:
 * author,location,track,extract loc function *)
let xml_spec f =
  match f with
    | Podcast -> "itunes:author","enclosure","item",podcast_uri 
    | Xspf -> "creator","location","track",xspf_uri
    | Asx -> "author","ref","entry",asx_uri
    | _ -> raise Internal

let xml_tracks t xml =
  try
    let author,location,track,extract = xml_spec t in
    let rec get_tracks l r =
      match l with
        | Xml.Element (s,_,x) :: l' when s = track -> get_tracks l' (x :: r)
        | Xml.Element (s,_,x) :: l' -> get_tracks (l' @ x) r
        | _ :: l' -> get_tracks l' r
        | [] -> r
    in
    let tracks = get_tracks [xml] [] in
    let rec parse_uri l =
      match l with
        | Xml.Element (s,l,x) :: l' when s = location -> extract l x
        | Xml.Element (_,_,_) :: l' -> parse_uri l'
        | _ :: l' -> parse_uri l'
        | [] -> raise Empty
    in
    let counter = ref 0 in
    let custom_link a =
      try
        Printf.sprintf "link:%s" (List.assoc "rel" a)
      with
        | _ -> incr counter; Printf.sprintf "link:%d" (!counter)
    in
    let rec parse_metadatas m l =
      match l with
        | Xml.Element (s,_,Xml.PCData(x) :: []) :: l' when s = author -> ("artist",Configure.recode_tag x) :: (parse_metadatas m l')
        | Xml.Element (s,a,Xml.PCData(x) :: []) :: l' when s = "link" -> (custom_link a, Configure.recode_tag x) :: (parse_metadatas m l')
        | Xml.Element (s,_,Xml.PCData(x) :: []) :: l' -> (s,Configure.recode_tag x) :: (parse_metadatas m l')
        | _ :: l' -> (parse_metadatas m l')
        | [] -> m
    in
    let rec parse_tracks t =
      match t with
        | track :: l -> (try (parse_metadatas [] track, parse_uri track) :: parse_tracks l with Error Empty -> parse_tracks l)
        | [] -> []
    in
    parse_tracks tracks
  with
    | Xml.Error(e) -> raise (XmlError (Xml.error e))

let smil_tracks xml =
  let rec get_tracks r l =
    match l with
      | Xml.Element (s,l',x) :: l'' when s = "audio" -> get_tracks (l' :: r) l''
      | Xml.Element (s,_,x) :: l' -> get_tracks r (l' @ x)
      | _ :: l' -> get_tracks r l'
      | [] -> r
  in
  let tracks = get_tracks [] [xml] in
  let smil_uri l =
    try
      List.assoc "src" l
    with _ -> raise Internal
  in
  let rec smil_meta m l =
    match l with
      | (s,s') :: l' when s = "author" -> ("artist",Configure.recode_tag s') :: (smil_meta m l')
      | (s,s') :: l' when s = "src" -> smil_meta m l'
      | (s,s') :: l' -> (s, Configure.recode_tag s') :: (smil_meta m l')
      | [] -> m
  in
  let rec parse_tracks t =
    match t with
      | track :: l -> (try (smil_meta [] track, smil_uri track) :: parse_tracks l with Error Empty -> parse_tracks l)
      | [] -> []
  in
    parse_tracks tracks


let tracks xml =
  try
    let xml = lowercase_tags (Xml.parse_string xml) in
    let t =  get_format [xml] in
    let tracks =
      match t with
        | Podcast | Xspf | Asx -> xml_tracks t xml
        | Smil -> smil_tracks xml
    in
      List.rev tracks
  with
    | Xml.Error(e) -> raise (XmlError (Xml.error e))
