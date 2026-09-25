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

let reload_on_arg =
  ( "reload_on",
    Lang.nullable_t (Lang.getter_t Lang.bool_t),
    Some Lang.null,
    Some
      "Checked on each new connection in server mode: when `true`, the \
       certificate and key are read again as with `reload()`, and the previous \
       ones are kept if that fails. Defaults to once a day." )

let current_day () =
  let time = Unix.localtime (Unix.time ()) in
  (time.Unix.tm_year, time.Unix.tm_yday)

let once_a_day () =
  let last_day = Atomic.make (current_day ()) in
  fun () ->
    let day = current_day () in
    day <> Atomic.exchange last_day day

let log = Log.make ["http"; "reload"]

(* The config is built for the first server and shared by every port opened
   with the transport, so a reload reaches all of them. *)
let server_config ~reload_on build =
  let reload_on =
    match Lang.to_option reload_on with
      | None -> once_a_day ()
      | Some reload_on -> fun () -> Lang.to_bool (Lang.to_getter reload_on ())
  in
  let config = Atomic.make None in
  let reload () =
    match Atomic.get config with
      | None -> ()
      | Some _ -> Atomic.set config (Some (build ()))
  in
  let current () =
    (match Atomic.get config with
      | None -> ignore (Atomic.compare_and_set config None (Some (build ())))
      | Some _ when reload_on () -> (
          try reload ()
          with exn ->
            log#severe
              "Could not reload certificate, keeping the previous one: %s"
              (Printexc.to_string exn))
      | Some _ -> ());
    Option.get (Atomic.get config)
  in
  (current, reload)

let add_builtin ~base ~descr ~transport_t name proto make =
  ignore
    (Lang.add_builtin ~base ~category:`Internet ~descr name
       (reload_on_arg :: proto)
       (Lang.method_t transport_t
          [
            ( "reload",
              ([], Lang.fun_t [] Lang.unit_t),
              "Read the certificate and key again and use them for new \
               connections. Open connections keep the previous ones. Raises \
               and keeps the previous ones if they cannot be loaded." );
          ])
       (fun p ->
         let build, transport = make p in
         let server_config, reload =
           server_config ~reload_on:(List.assoc "reload_on" p) build
         in
         Lang.meth (transport server_config)
           [
             ( "reload",
               Lang.val_fun [] (fun _ ->
                   reload ();
                   Lang.unit) );
           ]))
