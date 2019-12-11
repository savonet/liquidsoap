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

module Conf = Dtools.Conf

let conf =
  Conf.void ~p:(Configure.conf#plug "mpd") "Parameters for the mpd protocol."

let conf_host = Conf.string ~p:(conf#plug "host") ~d:"127.0.0.1" "MPD host."

let conf_port = Conf.int ~p:(conf#plug "port") ~d:6600 "MPD port."

let conf_path_prefix =
  Conf.string ~p:(conf#plug "path") ~d:"/var/lib/mpd/music"
    "Directory where MPD's music is located."

let conf_randomize =
  Conf.bool ~p:(conf#plug "randomize") ~d:true
    "Randomize order of MPD's results."

let conf_debug =
  Conf.bool ~p:(conf#plug "debug") ~d:false
    "Debug communications with MPD server."

exception Error of string

let connect () =
  let host =
    try Unix.gethostbyname conf_host#get
    with Not_found -> raise (Error "Host not found")
  in
  let sockaddr = Unix.ADDR_INET (host.Unix.h_addr_list.(0), conf_port#get) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let read () =
    let buflen = Utils.pagesize in
    let buf = Bytes.create buflen in
    let ans = ref "" in
    let n = ref buflen in
    while !n = buflen do
      n := Unix.recv socket buf 0 buflen [] ;
      ans := !ans ^ Bytes.sub_string buf 0 !n
    done ;
    if conf_debug#get then Printf.printf "R: %s%!" !ans ;
    !ans
  in
  let write s =
    let len = String.length s in
    if conf_debug#get then Printf.printf "W: %s%!" s ;
    let l = Unix.send socket (Bytes.of_string s) 0 len [] in
    assert (l = len)
  in
  Unix.connect socket sockaddr ;
  (socket, read, write)

let re_newline = Str.regexp "[\r\n]+"

let cmd read write name args =
  let args = List.map (fun s -> "\"" ^ s ^ "\"") args in
  write (name ^ " " ^ String.concat " " args ^ "\n") ;
  let ans = read () in
  let ans = Str.split re_newline ans in
  let ans = List.rev ans in
  (List.hd ans, List.rev (List.tl ans))

let re_file = Str.regexp "^file: \\(.*\\)$"

let re_metadata = Str.regexp "^\\([^:]+\\): \\(.*\\)$"

let valid_metadata = ["artist"; "title"; "album"; "genre"; "date"; "track"]

let search read write field v =
  let l =
    let ret, l = cmd read write "search" [field; v] in
    assert (ret = "OK") ;
    l
  in
  let ans = ref [] in
  let file = ref "" in
  let metadata = ref [] in
  let add () =
    if !file <> "" then
      ans :=
        Request.indicator ~metadata:(Utils.hashtbl_of_list !metadata) !file
        :: !ans
  in
  List.iter
    (fun s ->
      if Str.string_match re_file s 0 then (
        let f = Str.matched_group 1 s in
        let prefix = conf_path_prefix#get in
        let f = prefix ^ "/" ^ f in
        if conf_debug#get then Printf.printf "Found: %s\n%!" f ;
        add () ;
        file := f )
      else if Str.string_match re_metadata s 0 then (
        let field = Str.matched_group 1 s in
        let field = String.lowercase_ascii field in
        let value = Str.matched_group 2 s in
        if List.mem field valid_metadata then
          metadata := (field, value) :: !metadata ))
    l ;
  add () ;
  if conf_randomize#get then (
    let ans = Array.of_list !ans in
    Utils.randomize ans ; Array.to_list ans )
  else List.rev !ans

let re_request = Str.regexp "^\\([^=]+\\)=\\(.*\\)$"

let re_version = Str.regexp "OK MPD \\([0-9\\.]+\\)"

let mpd s ~log _ =
  if not (Str.string_match re_request s 0) then raise (Error "Invalid request") ;
  let field = Str.matched_group 1 s in
  let value = Str.matched_group 2 s in
  let value =
    let len = String.length value in
    if len > 0 && value.[0] = '"' && value.[len - 1] = '"' then
      String.sub value 1 (len - 2)
    else value
  in
  let _, read, write = connect () in
  let search = search read write in
  let version =
    let v = read () in
    if Str.string_match re_version v 0 then Str.matched_group 1 v
    else raise (Error "Not an MPD server")
  in
  log (Printf.sprintf "Connected to MPD version %s\n" version) ;
  let files = search field value in
  write "close\n" ; files

let () =
  Lang.add_protocol
    ~doc:"Finds all files with a tag equal to a given value using mpd."
    ~syntax:"mpd:tag=value" ~static:false "mpd" mpd
