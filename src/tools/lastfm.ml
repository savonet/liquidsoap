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

(* Liquidsoap support for lastfm
 * protocols. *)

module Conf = Dtools.Conf
type error = Http of string | Init of string | Adjust of string*string | Playlist | Empty
exception Error of error

let string_of_error e = 
  match e with
    | Http s -> Printf.sprintf "http connection failed: %s" s
    | Init s -> Printf.sprintf "could not open session:\n%s" s
    | Adjust (s,s') -> Printf.sprintf "could not adjust station to %s:\n%s\nIs the URI valid ?" s s'
    | Playlist -> "error while parsinf the playlist"
    | Empty -> "no files available"

let raise e = raise (Error e)

let conf =
  Conf.void ~p:(Configure.conf#plug "lastfm")
    "Parameters for the lastfm protocol."

let conf_user =
  Conf.string ~p:(conf#plug "user") ~d:"" "last.fm user."

let conf_pass =
  Conf.string ~p:(conf#plug "password") ~d:"" "last.fm password."


let host = "ws.audioscrobbler.com"
let port = 80
let sessions = Hashtbl.create 1
let stations = Hashtbl.create 1

let playlist_req id opt = 
  let opt = if opt <> "" then Printf.sprintf "&%s" opt else "" in
  Printf.sprintf "/radio/xspf.php?sk=%s%s&desktop=1" id opt

let request ?(post="") ?(headers=[]) ?(host=host) ?(port=port) req =
  try
    let socket = Http.connect host port in
    let http_req = 
      if post <> "" then 
        Http.post ~headers:headers post 
      else 
        Http.get ~headers:headers 
    in
    let (_, status, status_msg), _ = http_req socket host port req in
      if status <> 200 then
        (
  	Http.disconnect socket;
  	raise (Http (Printf.sprintf "Http request returned %d: %s" status status_msg))
        )
      else
        let ans = Http.read socket None in
          Http.disconnect socket;
          ans
  with
    | Http.Error e -> raise (Http (Http.string_of_error e))

(* Some parsing functions *)

let handshake_rex = Pcre.regexp "session=([0-9a-z]+).*"
let parse_handshake s = 
    try
      let sub = Pcre.exec ~rex:handshake_rex s in
      Pcre.get_substring sub 1
    with
      | Not_found -> raise (Init s)

let adjust_pat = "response=OK"
let check_adjust s =
    Pcre.pmatch ~pat:adjust_pat s

let auth_split_rex = Pcre.regexp "^lastfm://([^:]+):([^@]+)@(.+)$"
let auth_parse s = 
    try
      let sub = Pcre.exec ~rex:auth_split_rex s in
      Pcre.get_substring sub 1, Pcre.get_substring sub 2, Printf.sprintf "lastfm://%s" (Pcre.get_substring sub 3)
    with
      | Not_found -> conf_user#get,conf_pass#get,s

let opt_split_rex = Pcre.regexp "^([^?]+)\\?(.+)$"
let opt_parse s =
    try
      let sub = Pcre.exec ~rex:opt_split_rex s in
      Pcre.get_substring sub 1, Pcre.get_substring sub 2
    with
      | Not_found -> s,""

(* Let's forgot about xml parsing.. *)
let anon_split_rex = Pcre.regexp "<value><string>(.+)</string></value>\n<value><string>(.+)</string></value>"
let anon_parse s = 
   try
     let sub = Pcre.exec ~rex:anon_split_rex s in 
     Pcre.get_substring sub 1, Pcre.get_substring sub 2
   with
     | Not_found -> raise (Init s)

(* Core stuff.. *)

let clear sessionid =
    let keys = Hashtbl.fold (fun a b r -> if b = sessionid then a::r else r)
                  sessions []
    in
    ignore (List.map (fun x -> Hashtbl.remove sessions x) keys)

let anon_session () = 
  let req = "//1.0/webclient/xmlrpc.php" in
  let post = "<methodCall><methodName>getSession</methodName><params /></methodCall>" in
  let headers = [("Content-Type","text/xml")] in
  let ret = request ~post:post ~headers:headers req in
  anon_parse ret

let init req =
  let user,password,req = auth_parse req in
  let req,opt = opt_parse req in
  try
    Hashtbl.find sessions user,req,opt 
  with
    | Not_found -> 
      if user = "" || password = "" then
        let user,id = anon_session () in
        let ret = request (Printf.sprintf "/1.0/radio/webclient/handshake.php?sessionKey=%s&user=%s" id (Http.url_encode user)) in
        let sessionid = parse_handshake ret in
        Hashtbl.replace sessions user sessionid;
        sessionid,req,opt
      else
        let password = Digest.to_hex (Digest.string password) in
        let ret = request (Printf.sprintf "/radio/handshake.php?username=%s&passwordmd5=%s" (Http.url_encode user) password) in
        let sessionid = parse_handshake ret in
        Hashtbl.replace sessions user sessionid;
        parse_handshake ret,req,opt
  

let adjust sessionid req = 
  try 
    assert (Hashtbl.find stations sessionid = req)
  with 
    | _ ->
    let http_req = Printf.sprintf
                 "/radio/adjust.php?session=%s&url=%s"
                 sessionid (Http.url_encode req)
    in
    let ret = request http_req in
    if check_adjust ret then
       Hashtbl.replace stations sessionid req
    else
       begin
         Hashtbl.remove stations sessionid; 
         clear sessionid;
         raise (Adjust (req,ret))
       end

let tracks sessionid opt = 
  try
    let req = playlist_req sessionid opt in
    let playlist = request req in
    Xmliq.tracks playlist
  with
    | Xmliq.Error e -> clear sessionid; raise Playlist 
    | Error e -> clear sessionid; raise e

let get uri = 
  let id,req,opt = init uri in
  adjust id req;
  tracks id opt

let url id opt = 
 Printf.sprintf "http://%s:%d%s" host port (playlist_req id opt)

