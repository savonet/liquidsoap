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

open Extralib
module S = Lo.Server

let osc = Modules.osc

let conf_osc =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "osc")
    "Interactions through the OSC protocol."

let conf_port =
  Dtools.Conf.int ~p:(conf_osc#plug "port") ~d:7777 "Port for OSC server."

(* (path,type),handler *)
let handlers = ref []
let add_handler path t f = handlers := ((path, t), f) :: !handlers

let handler path (data : Lo.Message.data array) =
  let typ = function
    | `Float _ | `Double _ -> `Float
    | `Int32 _ | `Int64 _ -> `Int
    | `True | `False -> `Bool
    | `String _ | `Symbol _ -> `String
    | _ -> failwith "Unhandled value."
  in
  let value = function
    | `Float x | `Double x -> Lang.float x
    | `Int32 x | `Int64 x -> Lang.int x
    | (`True | `False) as b -> Lang.bool (b = `True)
    | `String s | `Symbol s -> Lang.string s
    | _ -> failwith "Unhandled value."
  in
  try
    let t = Array.map typ data in
    let v = Array.map value data in
    let h = List.assoc_all (path, t) !handlers in
    List.iter (fun f -> f v) h
  with _ -> ()

(* We have to start the server _after_
   daemonizing. See: savonet/liquidsoap#1365.

   There are two cases:
   - Server is requested before we daemonize (i.e.
     in a top-level call), start it after daemonization.
   - Server is requested after we daemonize (i.e.
     in a callback), start it immediately. *)
let server = ref None
let should_start = ref false
let started = ref false
let started_m = Mutex.create ()
let log = Log.make ["lo"]

let start_server () =
  log#info "Starting OSC server";
  let port = conf_port#get in
  let s = S.create port handler in
  server := Some s;
  ignore
    (Thread.create
       (fun () ->
         try
           while true do
             S.recv s
           done
         with
           | Lo.Server.Stopped -> ()
           | exn ->
               let backtrace = Printexc.get_backtrace () in
               log#important "OSC server thread exited with exception: %s\n%s"
                 (Printexc.to_string exn) backtrace)
       ())

let () =
  Lifecycle.on_start ~name:"lo initialization"
    (Mutex_utils.mutexify started_m (fun () ->
         if !should_start && !server = None then start_server ()
         else started := true))

let () =
  Lifecycle.on_core_shutdown ~name:"lo shutdown"
    (Mutex_utils.mutexify started_m (fun () ->
         match !server with
           | Some s ->
               log#info "Stopping OSC server";
               S.stop s;
               server := None
           | None -> ()))

let start_server =
  Mutex_utils.mutexify started_m (fun () ->
      if !started && !server = None then start_server ()
      else should_start := true)

let register name osc_t liq_t =
  let val_array vv =
    match Array.length vv with
      | 1 -> vv.(0)
      | 2 -> Lang.product vv.(0) vv.(1)
      | _ -> assert false
  in
  ignore
    (Lang.add_builtin ~base:osc name ~category:`Interaction
       [
         ("", Lang.string_t, None, Some "OSC path.");
         ("", liq_t, None, Some "Initial value.");
       ]
       (Lang.fun_t [] liq_t) ~descr:"Read from an OSC path."
       (fun p ->
         let path = Lang.to_string (Lang.assoc "" 1 p) in
         let v = Lang.assoc "" 2 p in
         let v = ref v in
         let handle vv = v := val_array vv in
         add_handler path osc_t handle;
         start_server ();
         Lang.val_fun [] (fun _ -> !v)));
  ignore
    (Lang.add_builtin ~base:osc ("on_" ^ name) ~category:`Interaction
       [
         ("", Lang.string_t, None, Some "OSC path.");
         ( "",
           Lang.fun_t [(false, "", liq_t)] Lang.unit_t,
           None,
           Some "Callback function." );
       ]
       Lang.unit_t ~descr:"Register a callback on OSC messages."
       (fun p ->
         let path = Lang.to_string (Lang.assoc "" 1 p) in
         let f = Lang.assoc "" 2 p in
         let handle v =
           let v = val_array v in
           ignore (Lang.apply f [("", v)])
         in
         add_handler path osc_t handle;
         start_server ();
         Lang.unit));
  ignore
    (Lang.add_builtin ~base:osc ("send_" ^ name) ~category:`Interaction
       [
         ("host", Lang.string_t, None, Some "OSC client address.");
         ("port", Lang.int_t, None, Some "OSC client port.");
         ("", Lang.string_t, None, Some "OSC path.");
         ("", liq_t, None, Some "Value to send.");
       ]
       Lang.unit_t ~descr:"Send a value to an OSC client."
       (fun p ->
         let host = Lang.to_string (List.assoc "host" p) in
         let port = Lang.to_int (List.assoc "port" p) in
         let path = Lang.to_string (Lang.assoc "" 1 p) in
         let v = Lang.assoc "" 2 p in
         let address = Lo.Address.create host port in
         let osc_val v =
           match v with
             | Value.Bool { value = b } -> if b then [`True] else [`False]
             | Value.String { value = s } -> [`String s]
             | Value.Float { value = x } -> [`Float x]
             | _ -> failwith "Unhandled value."
         in
         (* There was a bug in early versions of lo bindings and anyway we don't
            really want errors to show up here... *)
         (try Lo.send address path (osc_val v) with _ -> ());
         Lang.unit))

let () =
  register "float" [| `Float |] Lang.float_t;
  register "float_pair" [| `Float; `Float |]
    (Lang.product_t Lang.float_t Lang.float_t);
  register "int" [| `Int |] Lang.int_t;
  register "int_pair" [| `Int; `Int |] (Lang.product_t Lang.int_t Lang.int_t);
  register "bool" [| `Bool |] Lang.bool_t;
  register "string" [| `String |] Lang.string_t;
  register "string_pair" [| `String; `String |]
    (Lang.product_t Lang.string_t Lang.string_t)
