(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

let log = Log.log ~label:"server"

(* {1 Light general-purpose multitask server} *)

type task =
  | Read    of (Unix.file_descr * (unit -> unit))
  | Write   of (Unix.file_descr * (unit -> unit))

let scheduler init =
  let read    = ref [] in
  let write   = ref [] in
  let special = ref [] in
  let add set a = assert (not (List.mem a !set)) ; set := a :: !set in
  let add = function
    | Read  (s,f) -> add read  (s,f) ; add special s
    | Write (s,f) -> add write (s,f) ; add special s
  in
  let filter  x = List.filter (fun s -> not (List.mem s x)) in
  let filter2 x = List.filter (fun (s,_) -> not (List.mem s x)) in
  let close s =
    log 3 "Client left." ;
    try
      Unix.shutdown s Unix.SHUTDOWN_ALL ;
      Unix.close s
    with
      | _ -> ()
  in
  let process set sockets =
    (* Events are triggered on a list of sockets.
     * Execute the corresponding handlers, and remove them from the set. *)
    let f = List.map (fun s -> s, List.assoc s !set) sockets in
      set := filter2 sockets !set ;
      special := filter sockets !special ;
      List.iter
        (fun (s,f) -> try f () with _ -> close s)
        f
  in
  let close_special r w x =
      List.iter close x ;
      read  := (filter2 x) !read ;
      write := (filter2 x) !write ;
      special := (filter x) !special ;
      (filter x) r, (filter x) w
  in
    init add ;
    while not !Root.shutdown do
      let r,w,x =
        Utils.select (List.map fst !read) (List.map fst !write) !special (-1.)
      in
      let r,w = close_special r w x in
        process read r ;
        process write w
    done

(* {1 Manage available commands and namespaces} *)

type namespace = string list

let namespaces = Hashtbl.create 10
let commands : (string,(string->string)*string) Hashtbl.t = Hashtbl.create 50

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
let add ~ns ?usage cmd handler =
  let usage = match usage with None -> cmd | Some u -> u in
  let usage = prefix_ns usage ns in
    Hashtbl.add commands (prefix_ns cmd ns) (handler,usage)

(* ... maybe remove them *)
let remove ~ns cmd =
  Hashtbl.remove commands (prefix_ns cmd ns)

(* The usage string sums up all the commands... *)
let usage () =
  let l = Hashtbl.fold (fun k v l -> (k,v)::l) commands [] in
  let l = List.sort compare l in
    List.fold_left
      (fun s (k,(h,u)) -> s^(Printf.sprintf "\n| %s" u))
      "" l

(** {1 Handling of a client} *)

exception Exit

(** The very-builtin commands *)
let () =
  let add = add ~ns:[] in
    add "exit" (fun args -> raise Exit) ;
    add "quit" (fun args -> raise Exit) ;
    add "help" (fun _ -> "Available commands:" ^ (usage ())) ;
    add "list" (fun _ ->
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
        (fst (Hashtbl.find commands s)) args
      with
        | Exit -> raise Exit
        | e -> Printf.sprintf "ERROR: %s" (Printexc.to_string e)
    else
      raise Not_found

let handle_client add c =
  (* Write answer [s] and move on to step [k] *)
  let write s k =
    ignore (Unix.write c s 0 (String.length s)) ;
    add k
  in
  (* Process the command [s] and move on to step [k] *)
  let process s k =
    let s =
      if s.[(String.length s) - 1] = '\r' then
        String.sub s 0 ((String.length s)-1)
      else
        s
    in
    let answer =
      try exec s with
        | Not_found ->
            "ERROR: unknown command, type \"help\" to get a list of commands."
    in
    let answer =
      let len = String.length answer in
        if len > 0 && answer.[len - 1] = '\n' then
          answer^"END\n"
        else
          answer^"\nEND\n"
    in
      add (Write (c,(fun () -> write answer k)))
  in
  (* Read and process lines *)
  let rec f acc () =
    let buf = String.make 100 ' ' in
    let input = Unix.read c buf 0 100 in
    if input<=0 then failwith "eof" ;
    let acc = acc ^ (String.sub buf 0 input) in
      try
        let index = String.index acc '\n' in
        let line = String.sub acc 0 index in
        let acc  =
          String.sub acc (index+1) (String.length acc - index - 1)
        in
          process line (Read (c,f acc))
      with
        | Not_found -> add (Read (c,f acc))
  in
    add (Read (c,f ""))

(* {1 The server} *)

let () =
  Dtools.Var.register "socket" Dtools.Var.Bool ;
  Dtools.Var.register "socket.dir" Dtools.Var.String ;
  Dtools.Var.register "socket.file" Dtools.Var.String ;
  Dtools.Var.register "socket.permissions" Dtools.Var.Int

let start_socket add =
  let socket_dir =
    Dtools.Conf.get_string ~default:Configure.rundir "socket.dir"
  in
  let socket_name =
    socket_dir ^ "/" ^
    (let default = Printf.sprintf "%d.sock" (Unix.getpid ()) in
       Dtools.Conf.get_string ~default "socket.file")
  in
  let bind_addr = ADDR_UNIX socket_name in
  let rights = Dtools.Conf.get_int ~default:0o600 "socket.permissions" in
  let max_conn = 10 in
  let sock = socket PF_UNIX SOCK_STREAM 0 in
  let rec incoming () =
    add (Read (sock,incoming)) ;
    try
      let (s,caller) = accept sock in
      handle_client add s ;
      log 3 (Log.f "New client on "^socket_name)
    with e ->
      log 2 (Log.f "Failed to accept new client: %S"
               (Printexc.to_string e))
  in
    setsockopt sock SO_REUSEADDR true ;
    (* Try to close the socket if exists.. *)
    if not (Utils.dir_exists socket_dir) then
      failwith
        (Printf.sprintf "Unknown directory 'socket.dir': %s" socket_dir) ;
    if Sys.file_exists socket_name then unlink socket_name ;
    begin try bind sock bind_addr with
      | Unix.Unix_error(Unix.EACCES, "bind", "") ->
          failwith (Printf.sprintf "access to socket %s denied" socket_name)
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
          failwith (Printf.sprintf "socket %s already taken" socket_name)
    end ;
    ignore (Dtools.Init.at_stop
              (fun () ->
                 Dtools.Log.log ~label:"server" 3
                   (Printf.sprintf "Closing %s" socket_name) ;
                 Unix.shutdown sock SHUTDOWN_ALL ;
                 Unix.close sock ;
                 Unix.unlink socket_name)) ;
    chmod socket_name rights ;
    listen sock max_conn ;
    add (Read (sock,incoming))

let () =
  Dtools.Var.register "telnet" Dtools.Var.Bool ;
  Dtools.Var.register "telnet.port" Dtools.Var.Int ;
  Dtools.Var.register "telnet.bind_addr" Dtools.Var.String

let start_telnet add =
  let port = Dtools.Conf.get_int ~default:1234 "telnet.port" in
  let bind_addr_inet =
    inet_addr_of_string
      (Dtools.Conf.get_string ~default:"127.0.0.1" "telnet.bind_addr")
  in
  let bind_addr = ADDR_INET(bind_addr_inet, port) in
  let max_conn = 10 in
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
      log 3 (Log.f "New client: %s" ip)
    with e ->
    log 2 (Log.f "Failed to accept new client: %S"
             (Printexc.to_string e))
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
  let telnet = Dtools.Conf.get_bool ~default:false "telnet" in
  let socket = Dtools.Conf.get_bool ~default:false "socket" in
  if telnet || socket then
    ignore
      (Tutils.create scheduler
         (fun add ->
            if telnet then start_telnet add;
            if socket then start_socket add)
         "command server")

let () = ignore (Dtools.Init.at_start start)

(* Re-wrap exec for external use *)
let exec s =
  try exec s with Exit -> "ERROR: Attempt to exit!"
