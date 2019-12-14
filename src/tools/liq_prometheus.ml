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

open Cohttp_lwt_unix
module Prometheus_server = Prometheus_app.Cohttp (Server)
open Prometheus_server

let log = Log.make ["prometheus"; "server"]

let conf_prometheus =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "prometheus")
    "Metric reporting using prometheus."

let conf_server =
  Dtools.Conf.bool
    ~p:(conf_prometheus#plug "server")
    ~d:false "Enable the prometheus server."

let conf_port =
  Dtools.Conf.int ~p:(conf_server#plug "port") ~d:9090
    "Port to run the server on."

let server () =
  Server.create ~mode:(`TCP (`Port conf_port#get)) (Server.make ~callback ())

let _ =
  Dtools.Init.at_start (fun () ->
      if conf_server#get then
        ignore
          (Thread.create
             (fun () ->
               log#important "Starting prometheus server on port %d"
                 conf_port#get ;
               Lwt_main.run (server ()) ;
               log#important "Prometheus server shutdown!")
             ()))
