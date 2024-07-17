(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

let conf =
  Conf.void ~p:(Configure.conf#plug "server")
    "Server configuration"
    ~comments:[
      "The server is an abstract text-command-based communication protocol, " ;
      "which can be used through several interfaces."
    ]

let conf_socket =
  Conf.bool ~p:(conf#plug "socket") ~d:false
    "Support for communication via a UNIX domain socket interface"
    ~comments:[
      "The main advantage of this method is that you can set very precisely" ;
      "the access permissions for the socket, just like for any other file." ;
      "A useful command to use this interface is: \"socat stdin unix:<path>\"."
    ]
let conf_socket_path =
  Conf.string ~p:(conf_socket#plug "path") ~d:"<sysrundir>/<script>.sock"
    "Path of the UNIX domain socket"
    ~comments:[
      "In this filename, <pid>, <script> and <sysrundir> are replaced by ";
      "their respective values: PID of the instance of liquidsoap,";
      "base name of the .liq script (if any), default runtime data directory."
    ]
let conf_socket_perms =
  Conf.int ~p:(conf_socket#plug "permissions") ~d:0o600
    "Socket permissions, up to umask"
    ~comments:[
      "This parameter is better written in octal notation. Although you can " ;
      "write octal numbers like 0o660, they are not displayed back in octal. ";
      "For example, the default value 384 is the decimal for 0o600."
    ]

let conf_telnet =
  Conf.bool ~p:(conf#plug "telnet") ~d:false
    "Support for communication via a telnet interface"
    ~comments:[
      "This allows you to communicate with the server via a telnet interface,";
      "i.e., a simple text-based communication over TCP." ;
      "The standard \"telnet\" command will allow you to communicate through" ;
      "that interface, as well as the telnet libraries available in most"     ;
      "script languages." ;
      "Since there is currently no authentification, you should be careful"   ;
      "about who can access this interface: either restrict it to connections";
      "from localhost (using the bind_addr param) or set up a firewall."
    ]
let conf_telnet_bind_addr =
  Conf.string ~p:(conf_telnet#plug "bind_addr") ~d:"127.0.0.1"
    "Network mask from which the telnet server should accept connections"
let conf_telnet_port =
  Conf.int ~p:(conf_telnet#plug "port") ~d:1234
    "Port on which the telnet server should listen"

let log = Log.make ["server"]

(* {1 Manage available commands and namespaces} *)

type namespace = string list

let namespaces = Hashtbl.create 10
let commands : (string,(string->string)*string*string) Hashtbl.t =
  Hashtbl.create 50

(* First, get a fresh namespace *)
let register ns kind =
  let c = ref 0 in
  let mkns () = if !c = 0 then ns else ns@[string_of_int !c] in
  let ns =
    while Hashtbl.mem namespaces (mkns ()) do
      incr c ;
    done ;
    mkns ()
  in
    Hashtbl.add namespaces ns kind ;
    ns

let to_string = String.concat "."

let rec prefix_ns cmd ns = to_string (ns@[cmd])

(* Then add your commands to that namespace *)
let add ~ns ?usage ~descr cmd handler =
  let usage = match usage with None -> cmd | Some u -> u in
  let usage = prefix_ns usage ns in
    Hashtbl.add commands (prefix_ns cmd ns) (handler,usage,descr)

(* ... maybe remove them *)
let remove ~ns cmd =
  Hashtbl.remove commands (prefix_ns cmd ns)

(* The usage string sums up all the commands... *)
let usage () =
  let l = Hashtbl.fold (fun k v l -> (k,v)::l) commands [] in
  let l = List.sort compare l in
    List.fold_left
      (fun s (k,(h,u,_)) -> s^(Printf.sprintf "\n| %s" u))
      "" l

(** {1 Handling of a client} *)

exception Exit

(** The very-builtin commands *)
let () =
  let add = add ~ns:[] in
    add "exit" ~descr:"Close current server session." (fun args -> raise Exit) ;
    add "quit" ~descr:"Close current server session." (fun args -> raise Exit) ;
    add "help" ~usage:"help [<command>]" 
               ~descr:"Get information on available commands." 
      (fun args -> 
         try
            let args = 
              Pcre.substitute ~pat:"\\s*" 
                              ~subst:(fun _ -> "") args
            in
            let (_,us,d) = Hashtbl.find commands args in
	    Printf.sprintf
            "\nHelp for command %s.\n\nUsage: %s\n  %s"
	    args us d
         with
          | Not_found ->
              (if args <> "" then "No such command: " ^ args ^ "\n" else "")
               ^ "Available commands:" ^ (usage ()) ^ "\n\n"
               ^ "Type \"help <command>\" for more information.") ;
    add "list" 
        ~descr:"Get the list of available operators with their interfaces." 
               (fun _ ->
                  String.concat "\n"
                    (Hashtbl.fold
                       (fun k v s ->
                          (Printf.sprintf "%s : %s" (to_string k) v)::s)
                       namespaces []))

let exec s =
  let s,args =
    try
      let c = String.index s ' ' in
        (String.sub s 0 c),
        (String.sub s (c+1) ((String.length s)-c-1))
    with
      | Not_found -> s,""
  in
    if Hashtbl.mem commands s then
      try
        let fst (x,_,_) = x in
        (fst (Hashtbl.find commands s)) args
      with
        | Exit -> raise Exit
        | e -> Printf.sprintf "ERROR: %s" (Printexc.to_string e)
    else
      raise Not_found

let priority = Tutils.Non_blocking

let handle_client socket =
  let on_error _ =
    log#f 3 "Client left." ;
    try
      Unix.shutdown socket Unix.SHUTDOWN_ALL ;
      Unix.close socket
    with
      | _ -> ()
  in
  (* Read and process lines *)
  let marker = Duppy.Io.Split "[\r\n]+" in
  let recursive = false in
  (* Process the command [s] and start a new reading poll *)
  let rec process l =
    let l,init =
      match List.rev l with
        | []
        | _ :: [] -> assert false (* Should not happen *)
        | e :: l -> List.rev l,e
    in
    let exit = ref false in
    let process_elem s =
      let answer =
        try exec s with
          | Not_found ->
              "ERROR: unknown command, type \"help\" to get a list of commands."
          | Exit -> exit := true ; "Bye!"
      in
      let len = String.length answer in
        if len > 0 && answer.[len - 1] = '\n' then
          answer^"END\n"
        else
          answer^"\nEND\n"
    in
    let l = List.map process_elem l in
    let answer = String.concat "\r\n" l in
    let exec () =
      if !exit then
        on_error ()
      else
        Duppy.Io.read ~priority ~recursive ~on_error ~init
          Tutils.scheduler socket marker process
    in
    Duppy.Io.write ~priority ~exec ~on_error
      Tutils.scheduler socket answer
  in
  Duppy.Io.read ~priority ~recursive ~on_error
    Tutils.scheduler socket marker process

(* {1 The server} *)

let start_socket () =
  let socket_path = Configure.subst_vars (conf_socket_path#get) in
  let socket_name = Filename.basename socket_path in
  let socket_dir = Filename.dirname socket_path in
  let bind_addr = ADDR_UNIX socket_path in
  let rights = conf_socket_perms#get in
  let max_conn = 10 in
  let sock = socket PF_UNIX SOCK_STREAM 0 in
  (* Set TCP_NODELAY on the socket *)
  Liq_sockets.set_tcp_nodelay sock true ;
  let rec incoming _ =
    begin
      try
        let (socket,caller) = accept sock in
          log#f 3 "New client on %s" socket_name ;
          handle_client socket
      with e ->
        log#f 2 "Failed to accept new client: %S" (Printexc.to_string e)
    end ;
    [{ Duppy.Task.
         priority = priority ;
         events = [`Read sock] ;
         handler = incoming }]
  in
    setsockopt sock SO_REUSEADDR true ;
    (* Try to close the socket if exists.. *)
    if not (Utils.dir_exists socket_dir) then
      failwith
        (Printf.sprintf "Unknown directory for the socket: %s" socket_dir) ;
    if Sys.file_exists socket_path then unlink socket_path ;
    begin try bind sock bind_addr with
      | Unix.Unix_error(Unix.EACCES, "bind", "") ->
          failwith (Printf.sprintf "access to socket %s denied" socket_path)
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
          failwith (Printf.sprintf "socket %s already taken" socket_path)
    end ;
    ignore (Dtools.Init.at_stop
              (fun () ->
                 log#f 3 "Closing %s" socket_name ;
                 Unix.shutdown sock SHUTDOWN_ALL ;
                 Unix.close sock ;
                 Unix.unlink socket_path)) ;
    chmod socket_path rights ;
    listen sock max_conn ;
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority ;
          events = [`Read sock] ;
          handler = incoming }

let start_telnet () =
  let port = conf_telnet_port#get in
  let bind_addr_inet =
    inet_addr_of_string (conf_telnet_bind_addr#get)
  in
  let bind_addr = ADDR_INET(bind_addr_inet, port) in
  let max_conn = 10 in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* Set TCP_NODELAY on the socket *)
  Liq_sockets.set_tcp_nodelay sock true ;
  let rec incoming _ =
    begin
      try
        let (socket,caller) = accept sock in
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
        log#f 3 "New client: %s" ip ;
        handle_client socket;
      with e ->
      log#f 2 "Failed to accept new client: %S" (Printexc.to_string e)
    end ;
    [{ Duppy.Task.
         priority = priority ;
         events   = [`Read sock] ;
         handler  = incoming }]
    in
    setsockopt sock SO_REUSEADDR true ;
    begin try bind sock bind_addr with
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
          failwith (Printf.sprintf "port %d already taken" port)
    end ;
    listen sock max_conn ;
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority ;
          events   = [`Read sock] ;
          handler  = incoming }

let start () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  let telnet = conf_telnet#get in
  let socket = conf_socket#get in
  if telnet || socket then begin
    Tutils.need_non_blocking_queue () ;
    if telnet then start_telnet () ;
    if socket then start_socket ()
  end

let () = ignore (Dtools.Init.at_start start)

(* Re-wrap exec for external use *)
let exec s =
  try exec s with Exit -> "ERROR: Attempt to exit!"
