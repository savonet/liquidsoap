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
open Dtools
open Http_source
open Server

let conf_harbor =
  Conf.void ~p:(Configure.conf#plug "harbor")
    "Builtin HTTP stream receiver."
let conf_harbor_port =
  Conf.int ~p:(conf_harbor#plug "port") ~d:8005
    "Port on which the HTTP stream receiver should listen."
let conf_harbor_bind_addr =
  Conf.string ~p:(conf_harbor#plug "bind_addr") ~d:"127.0.0.1"
    "IP address on which the HTTP stream receiver should listen."
let conf_harbor_user =
  Conf.string ~p:(conf_harbor#plug "username") ~d:"source"
    "Default username for source connection."
let conf_harbor_pass =
  Conf.string ~p:(conf_harbor#plug "password") ~d:"hackme"
    "Default password for source connection."

let log = Log.make ["harbor"]

exception Internal

(* Define what we need as a source *)
class virtual source  = 
object(self)
  inherit Source.source

  method virtual relay : Unix.file_descr -> unit
  method virtual insert_metadata : (string, string) Hashtbl.t -> unit
  method virtual is_taken : bool
  method virtual register_decoder : string -> unit

end

let sources : (string,source) Hashtbl.t = Hashtbl.create 1

(* Add your sources *)
let add_source mountpoint source =
  log#f 3 "Adding mountpoint '%s' to list..." mountpoint ;
  Hashtbl.add sources mountpoint source

(* ... maybe remove them *)
let remove_source mountpoint =
  Hashtbl.remove sources mountpoint

let find_source mountpoint =
  Hashtbl.find sources mountpoint

(** {1 Handling of a client} *)

exception Exit
exception Too_many_sources
exception Not_authenticated
exception Not_supported
exception Unknown_codec
exception Mount_taken
exception Success

type http_request_type =
  | Source
  | Get
  | Unhandled

type http_protocol =
  | Http_10
  | Http_11
  | Icy
  | Unknown

let http_error_page code status msg =
  ( "HTTP/1.0 " ^ (string_of_int code) ^ " " ^ status ^ "\r\n" ^ 
    "Content-Type: text/html\r\n\r\n" ^
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ^
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" " ^
    "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n" ^
    "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">" ^
    "<head><title>Liquidsoap source harbor</title></head>" ^ 
    "<body><p>" ^ msg ^ "</p></body></html>\n" )

let parse_http_request_line r =
  let data = Str.split (Str.regexp "[ \t]+") r in
    (
      (match (String.uppercase (List.nth data 0)) with
        | "SOURCE" -> Source
        | "GET" -> Get
        | _ -> Unhandled),
      (List.nth data 1),
      (match (String.uppercase (List.nth data 2)) with
        | "HTTP/1.0" -> Http_10
        | "HTTP/1.1" -> Http_11
        | _ -> Unknown)
    )

let write_answer c a = ignore (Unix.write c a 0 (String.length a))

let parse_headers headers = 
  let split_header h =
    let index = String.index h ':' in
    let rec trim s =
      if s.[0] = ' ' || s.[0] = '\t' then
        trim (String.sub s 1 ((String.length s)-1))
      else if s.[(String.length s) - 1] = '\r' ||
              s.[(String.length s) - 1] = '\n' ||
              s.[(String.length s) - 1] = '\t' ||
              s.[(String.length s) - 1] = ' ' then
        trim (String.sub s 0 ((String.length s)-1))
      else
        s
    in
      (String.uppercase (String.sub h 0 index), trim (String.sub h (index + 1) (String.length h - index - 1)))
  in
  List.map split_header headers

let auth_check c uri headers =
    List.iter (fun (h, v) -> log#f 4 "Header: %s, value: %s." h v) headers ;
    try
      (* Authentication *)
      let auth = List.assoc "AUTHORIZATION" headers in
      let data = Str.split (Str.regexp "[ \t]+") auth in
        if ((List.nth data 0) <> "Basic")
        then
          ( raise Not_supported ) ;
        (* TODO: per-source auth... *)
        let auth_data = Str.split (Str.regexp ":")
                          (Utils.decode64 (List.nth data 1))
        in
          let user, pass = (List.nth auth_data 0), (List.nth auth_data 1)
          in
            log#f 4 "Username: %s, password: %s." user pass ;
            if (user <> (conf_harbor_user#get)
                || pass <> (conf_harbor_pass#get))
            then
              ( raise Not_authenticated ) ;
      (* OK *)
      log#f 4 "Client logged in." 
    with
      | Not_authenticated ->
          (
            log#f 3 "Returned 401." ;
            write_answer c (http_error_page 401
           "Unauthorized\r\nWWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
                                              "No login / password supplied.") ;
            failwith "no auth given"
          )
      | Not_supported ->
          (
            log#f 3 "Returned 401." ;
            write_answer c (http_error_page 401
           "Unauthorized\r\nWWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
                                              "No login / password supplied.") ;
            failwith "bad auth scheme"
          )

let handle_http_source_request c uri headers =
  log#f 3 "SOURCE request on %s." uri ;
  auth_check c uri headers ;
  try
    let s = find_source uri in
    match s#is_taken with
      | true -> raise Mount_taken
      | _ ->   let stype = List.assoc "CONTENT-TYPE" headers in
	       s#register_decoder stype ;
               log#f 3 "Adding source on mountpoint '%s' with type '%s'." uri stype ;
               write_answer c "HTTP/1.0 200 OK\r\n\r\n" ;
               s#relay c
  with
    | Mount_taken ->
        (
          log#f 3 "Returned 401." ;
          write_answer c (http_error_page 401
         "Unauthorized\r\nWWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
                                            "Mountpoint in use") ;
          failwith "Mountpoint in use"
        )
    | Not_found ->
        (
          log#f 3 "Returned 404 for '%s'." uri ;
          write_answer c (http_error_page 404 "Not found"
                                         "This mountpoint isn't available.") ;
          failwith "no such mountpoint"
        )
    | Unknown_codec -> 
        (
          log#f 3 "Returned 401 for '%s'." uri ;
          write_answer c (http_error_page 401 "Not recognized"
                                         "This stream's format recognized.") ;
          failwith "bad codec"
        )
    | e -> 
        (
          log#f 3 "Returned 401 for '%s'." uri ;
          write_answer c (http_error_page 401 "Error"
                                         "The server could not handle your request.") ;
          failwith (Printexc.to_string e)
        )

let handle_http_get_request c uri headers =
  log#f 3 "GET request on %s." uri ;
  let default =
    ( "HTTP/1.0 200 OK\r\n" ^
      "Content-Type: text/html\r\n\r\n" ^
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ^
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" " ^
      "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n" ^
      "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">" ^
      "<head><title>Liquidsoap source harbor</title></head>" ^
      "<body><p>Liquidsoap's harbor main page</p></body></html>\n" )
  in
  let update_metadata uri = 
    let rex = Pcre.regexp "^/admin/metadata\\?(.+)$" in
    let sub =  Pcre.exec ~rex:rex uri in
    let args = Http.args_split (Http.url_decode (Pcre.get_substring sub 1)) in
    match Hashtbl.find args "mode" with
      | "updinfo" -> let mount = Hashtbl.find args "mount" in
                     log#f 3 "update metadatas request for mount %s" mount;
                     auth_check c uri headers ;
                     let s = find_source mount in
                     let ans = Printf.sprintf 
		                  "HTTP/1.0 200 OK\r\n\r\nUpdated metadatas for mount %s\n" 
				  mount 
                     in
                     Hashtbl.remove args "mount";
		     Hashtbl.remove args "mode";
                     s#insert_metadata args ;
                     write_answer c ans ; 
                     raise Success
     | _ -> raise Internal
  in
  try 
     match uri with 
       | "/" -> write_answer c default ; raise Success
       | _ -> update_metadata uri
  with
    | Success -> failwith "success!"
    | _ ->  log#f 3 "Returned 404 for '%s'." uri ;
            write_answer c (http_error_page 404 "Not found"
                                           "This page isn't available.") ;
            failwith "no such page"

let handle_client add c =
  (* Process the command [s] and move on to step [k] *)
  let process s =
    let lines = Str.split (Str.regexp "\n") s in
    let (hmethod, huri, hprotocol) = parse_http_request_line (List.nth lines 0)
    in
      (
        match hmethod with
          | Source -> handle_http_source_request c huri (parse_headers (List.tl lines))
          | Get -> handle_http_get_request c huri (parse_headers (List.tl lines))
          | Unhandled ->
            (
              log#f 3 "Returned 501." ;
              write_answer c (http_error_page 501 "Not Implemented"
                                "The server did not understand your request.") ;
              failwith "cannot handle this, exiting"
            )
      )
  in
  (* Read and process lines *)
  let rec f acc () =
    let buf = String.make 100 ' ' in
    let input = Unix.read c buf 0 100 in
    if input<=0 then failwith "eof" ;
    let acc = acc ^ (String.sub buf 0 input) in
      try
        let index = 
          try 
            Str.search_forward (Str.regexp "\r\n\r\n") acc 0
          with 
            (* Workaround : also search for non standard request, because
             * of some nasty (but famous) source clients... 
             * See: http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=441281 *)
            | Not_found -> Str.search_forward (Str.regexp "\n\n") acc 0
        in
        let req = String.sub acc 0 index in
          process req
      with
        | Not_found -> add (Read (c,f acc))
  in
    add (Read (c,f ""))

(* {1 The server} *)

let start_harbor add =
  let port = conf_harbor_port#get in
  let bind_addr_inet =
    inet_addr_of_string (conf_harbor_bind_addr#get)
  in
  let bind_addr = ADDR_INET(bind_addr_inet, port) in
  (* Max. one connection per source.. *)
  let max_conn = Hashtbl.length sources in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let rec incoming () =
    add (Read (sock,incoming)) ;
    try
      let (s,caller) = accept sock in
      let ip =
      let a = match caller with
          | ADDR_INET (a,_) -> a
          | _ -> assert false
      in
      try
        (gethostbyaddr a).h_name
      with
        | Not_found -> string_of_inet_addr a
      in
      handle_client add s ;
      log#f 3 "New client: %s" ip
    with e -> log#f 2 "Failed to accept new client: %S" (Printexc.to_string e)
    in
    setsockopt sock SO_REUSEADDR true ;
    begin try bind sock bind_addr with
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
          failwith (Printf.sprintf "port %d already taken" port)
    end ;
    listen sock max_conn ;
    add (Read (sock,incoming))

let start () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  if Hashtbl.length sources > 0 then
    ignore
      (Tutils.create scheduler
         (fun add ->
            start_harbor add)
         "source harbor")

let () = ignore (Dtools.Init.at_start start)
