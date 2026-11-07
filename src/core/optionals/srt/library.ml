(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let log = Log.make ["srt"]
let conf = Dtools.Conf.void ~p:(Configure.conf#plug "srt") "SRT configuration"

let conf_prefer_address =
  Dtools.Conf.string
    ~p:(conf#plug "prefer_address")
    ~d:"system"
    "Preferred address type when resolving hostnames. One of: `\"system\"`, \
     `\"ipv4\"` or `\"ipv6\"`."

let conf_enforced_encryption =
  Dtools.Conf.bool
    ~p:(conf#plug "enforced_encryption")
    ~d:true "Enforce encryption on every SRT socket."

let conf_log =
  Dtools.Conf.bool ~p:(conf#plug "log") ~d:true
    "Route libsrt logs through liquidsoap's logs."

let conf_verbosity =
  Dtools.Conf.string
    ~p:(conf_log#plug "verbosity")
    ~d:"warning"
    "libsrt log verbosity. One of: `\"critical\"`, `\"error\"`, `\"warning\"`, \
     `\"notice\"` or `\"debug\"`."

let conf_level =
  Dtools.Conf.int ~p:(conf_log#plug "level") ~d:4
    "Level at which libsrt messages are logged."

let conf_poll = Dtools.Conf.void ~p:(conf#plug "poll") "Readiness loop"

let conf_timeout =
  Dtools.Conf.float ~p:(conf_poll#plug "timeout") ~d:0.1
    "Longest wait of the readiness loop, in seconds."

let initialized = Atomic.make false

(* Threads that may call libsrt: cleaning libsrt up under one of them corrupts
   the process's memory. *)
let in_flight = Atomic.make 0
let cleanup_wait = 5.

let cleanup () =
  let deadline = Unix.gettimeofday () +. cleanup_wait in
  (* ponytail: polls the count; runs once, at exit. *)
  while Atomic.get in_flight > 0 && Unix.gettimeofday () < deadline do
    Thread.delay 0.01
  done;
  if Atomic.get in_flight > 0 then
    log#severe "libsrt is still in use after %.0fs: skipping its cleanup."
      cleanup_wait
  else (
    Srt.cleanup ();
    log#info "libsrt cleaned up.")

let verbosity () =
  match conf_verbosity#get with
    | "critical" -> `Critical
    | "error" -> `Error
    | "warning" -> `Warning
    | "notice" -> `Notice
    | "debug" -> `Debug
    | _ ->
        log#severe "Invalid value for \"srt.log.verbosity\"!";
        `Error

let strip_trailing s =
  let rec last i =
    if i > 0 && List.mem s.[i - 1] [' '; '\r'; '\n'] then last (i - 1) else i
  in
  String.sub s 0 (last (String.length s))

let start_library () =
  Srt.startup ();
  if conf_log#get then (
    Srt.Log.setloglevel (verbosity ());
    Srt.Log.set_handler (fun { Srt.Log.message } ->
        log#f conf_level#get "%s" (strip_trailing message)))

let init () =
  if not (Atomic.exchange initialized true) then (
    Lifecycle.on_start ~name:"srt initialization" start_library;
    Lifecycle.on_final_cleanup ~name:"srt cleanup" cleanup)

type shutdown_hook = { key : int; run : unit -> unit }

module Hooks = Weak.Make (struct
  type t = shutdown_hook

  let equal a b = a.key = b.key
  let hash a = a.key
end)

let hooks = Hooks.create 16
let hooks_m = Mutex.create ()
let hook_keys = Atomic.make 0
let shutting_down = Atomic.make false

let register_shutdown run =
  let hook = { key = Atomic.fetch_and_add hook_keys 1; run } in
  Mutex.protect hooks_m (fun () -> Hooks.add hooks hook);
  hook

let () =
  Lifecycle.before_core_shutdown ~name:"srt shutdown" (fun () ->
      Atomic.set shutting_down true;
      let hooks =
        Mutex.protect hooks_m (fun () -> Hooks.fold List.cons hooks [])
      in
      List.iter
        (fun { run } ->
          try run ()
          with exn ->
            log#severe "Error while releasing SRT operator: %s"
              (Printexc.to_string exn))
        hooks)

let ms_of_seconds s = int_of_float (Float.ceil (s *. 1000.))

let is_ipv6 = function
  | Unix.ADDR_INET (ip, _) -> Unix.is_inet6_addr ip
  | Unix.ADDR_UNIX _ -> false

let resolve ~prefer_address host port =
  let family =
    match prefer_address with
      | `System -> []
      | `Ipv4 -> [Unix.AI_FAMILY Unix.PF_INET]
      | `Ipv6 -> [Unix.AI_FAMILY Unix.PF_INET6]
  in
  match
    Unix.getaddrinfo host (string_of_int port)
      (Unix.AI_SOCKTYPE Unix.SOCK_DGRAM :: family)
  with
    | [] ->
        failwith (Printf.sprintf "Could not resolve address: %s:%d" host port)
    | answers ->
        let addresses = List.map (fun { Unix.ai_addr } -> ai_addr) answers in
        log#debug "Address %s:%d resolved to: %s" host port
          (String.concat ", "
             (List.map (Utils.name_of_sockaddr ~show_port:true) addresses));
        addresses

let is_permanent = function
  | Srt.Error ((`Einvparam | `Einvop), _, _) -> true
  | Srt.Error (_, Some (Unix.EACCES | Unix.EPERM), _) -> true
  | _ -> false

let close_quietly socket = try Srt.close socket with _ -> ()

let create_socket ~payload_size ~messageapi () =
  let socket = Srt.create_socket () in
  try
    Srt.setsockflag socket Srt.transtype `Live;
    Srt.setsockflag socket Srt.payloadsize payload_size;
    Srt.setsockflag socket Srt.messageapi messageapi;
    Srt.setsockflag socket Srt.enforced_encryption conf_enforced_encryption#get;
    socket
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    close_quietly socket;
    Printexc.raise_with_backtrace exn bt

let sync_sources = Hashtbl.create 4
let sync_sources_m = Mutex.create ()

let sync_source name =
  Mutex.protect sync_sources_m (fun () ->
      match Hashtbl.find_opt sync_sources name with
        | Some sync_source -> sync_source
        | None ->
            let sync_source = Clock.Sync_source.make ~name `Self_paced in
            Hashtbl.replace sync_sources name sync_source;
            sync_source)

(* Counted from the call, as the scheduler may start the thread later. *)
let spawn fn =
  Atomic.incr in_flight;
  Duppy.thread ~priority:`Threaded Scheduler.raw (fun () ->
      Fun.protect ~finally:(fun () -> Atomic.decr in_flight) fn)

let spawn_joinable fn =
  let m = Mutex.create () in
  let finished_c = Condition.create () in
  let finished = ref false in
  let finish () =
    Mutex.protect m (fun () ->
        finished := true;
        Condition.broadcast finished_c)
  in
  spawn (fun () -> Fun.protect ~finally:finish fn);
  fun () ->
    Mutex.protect m (fun () ->
        while not !finished do
          Condition.wait finished_c m
        done)

let after delay fn =
  Scheduler.Task.add
    {
      Scheduler.Task.priority = `Non_blocking;
      events = [`Delay delay];
      handler =
        (fun _ ->
          fn ();
          []);
    }
