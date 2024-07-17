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
        Unix.select (List.map fst !read) (List.map fst !write) !special (-1.)
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

(* Register the first commands *)
exception Exit
let _ =
  let add = add ~ns:[] in
    add "exit" (fun args -> raise Exit) ;
    add "quit" (fun args -> raise Exit) ;
    add "help" (fun _ -> "Available commands:" ^ (usage ())) ;
    add "list" (fun _ ->
                  String.concat "\n"
                    (Hashtbl.fold
                       (fun k v s ->
                          (Printf.sprintf "%s : %s" (to_string k) v)::s)
                       namespaces [])) ;
    add "alive" (fun args ->
                   String.concat " "
                     (List.map
                        string_of_int
                        (Request.alive_requests ()))) ;
    add "on_air" (fun args ->
                    String.concat " "
                      (List.map
                         string_of_int
                         (Request.on_air_requests ()))) ;
    add "resolving" (fun args ->
                       String.concat " "
                         (List.map
                            string_of_int
                            (Request.resolving_requests ()))) ;
    add "trace" ~usage:"trace <rid>"
      (fun args ->
         let id = int_of_string args in
           begin
             match Request.from_id id with
             | Some r ->
                 let log = Request.get_log r in
                   Request.string_of_log log
             | None -> "No such request."
           end) ;
    add "metadata" ~usage:"metadata <rid>"
      (fun args ->
         let id = int_of_string args in
           begin
             match Request.from_id id with
             | Some r ->
                 let m = Request.get_all_metadata r in
                   Request.string_of_metadata m
             | None -> "No such request."
           end) ;
    add "uptime"
      (fun _ ->
         let date = int_of_float (Root.uptime ()) in
           Printf.sprintf "%dj %02dh %02dm %02ds"
             (date/(24*60*60))
             ((date mod (24*60*60)) / (60*60))
             ((date mod (60*60)) / 60)
             (date mod 60))

(** {1 Handling of a client} *)

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
    let s,args =
      try
        let c = String.index s ' ' in
          (String.sub s 0 c),
          (String.sub s (c+1) ((String.length s)-c-1))
      with
        | Not_found -> s,""
    in
    let answer =
      if Hashtbl.mem commands s then
        try
          (fst (Hashtbl.find commands s)) args
        with
          | Exit -> raise Exit
          | e -> Printf.sprintf "ERROR: %s" (Printexc.to_string e)
      else
        "ERROR: unknown command, type \"help\" to get a list of commands."
    in
      add (Write (c,(fun () -> write (answer^"\nEND\n") k)))
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

let start () =
  let port = Dtools.Conf.get_int ~default:1234 "server.port" in
  let bind_addr =
    if Dtools.Conf.get_bool ~default:false "server.public" then
      ADDR_INET(inet_addr_any, port)
    else
      ADDR_INET(inet_addr_of_string "127.0.0.1", port)
  in
  let max_conn = 10 in
    ignore
      (Tutils.create scheduler
         (fun add ->
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
              add (Read (sock,incoming)))
         "telnet server")

let _ =
  Dtools.Init.at_start
    (fun () ->
       Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
       start ())
