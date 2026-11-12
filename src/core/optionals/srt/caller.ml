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

open Connection

class ['reaction] connection ~log ~name ~params ~kind () =
  object (self)
    inherit ['reaction] Connection.connection ~log ~name ~params ~kind ()

    method private connect_role generation =
      let attempt = { attempt_socket = None } in
      Mutex.protect m (fun () -> state <- Attempting attempt);
      self#spawn_attempt generation attempt

    method private retry generation delay =
      let start () =
        let attempt = { attempt_socket = None } in
        let go =
          Mutex.protect m (fun () ->
              self#current generation && is_waiting state
              &&
              (state <- Attempting attempt;
               true))
        in
        if go then self#spawn_attempt generation attempt
      in
      if delay <= 0. then start ()
      else Library.after delay (fun () -> Events.post events start)

    method private spawn_attempt generation attempt =
      Library.spawn (fun () ->
          let result =
            try
              let addresses =
                Library.resolve ~prefer_address:params.prefer_address
                  params.host params.port
              in
              (log ())#info "Connecting to srt://%s:%d" params.host params.port;
              Ok (self#connect_addresses generation attempt addresses)
            with exn -> Error exn
          in
          Events.post events (fun () -> self#attempt_done generation result))

    method private set_caller_options socket =
      Option.iter (Srt.setsockflag socket Srt.streamid) params.streamid;
      self#set_encryption socket;
      Option.iter
        (fun timeout ->
          Srt.setsockflag socket Srt.conntimeo (Library.ms_of_seconds timeout))
        params.connection_timeout;
      kind.setup_socket socket

    method private connect_addresses generation attempt =
      function
      | [] -> assert false
      | address :: rest -> (
          let socket = self#new_socket in
          Mutex.protect m (fun () -> attempt.attempt_socket <- Some socket);
          let created = ref false in
          match
            self#set_caller_options socket;
            Events.run events (fun () ->
                created := true;
                self#socket_event "connect" socket);
            if not (Mutex.protect m (fun () -> self#current generation)) then
              failwith "connection abandoned";
            Srt.connect socket address
          with
            | () -> (socket, address, !created)
            | exception exn ->
                let exn =
                  match exn with
                    | Srt.Error (`Econnrej, system, message) ->
                        Srt.Error
                          ( `Econnrej,
                            system,
                            message ^ ": " ^ Srt.getrejectreason socket )
                    | exn -> exn
                in
                Events.run events (fun () ->
                    self#close_socket ~created:!created socket);
                if rest = [] then raise exn
                else self#connect_addresses generation attempt rest)

    method private attempt_done generation result =
      let current () = self#current generation && is_attempting state in
      match result with
        | Ok (socket, address, created) -> (
            let peer =
              Mutex.protect m (fun () ->
                  if current () then (
                    let peer = self#make_peer ~created socket address in
                    state <- Connected peer;
                    Some peer)
                  else None)
            in
            match peer with
              | Some peer ->
                  (log ())#important "Connected to srt://%s"
                    (Utils.name_of_sockaddr ~show_port:true address);
                  self#peer_connected peer
              | None -> self#close_socket ~created socket)
        | Error exn ->
            let permanent = Library.is_permanent exn in
            let still_current =
              Mutex.protect m (fun () ->
                  current ()
                  &&
                  (state <- (if permanent then Idle else Waiting);
                   true))
            in
            if still_current then (
              let message =
                Printf.sprintf "Connection to srt://%s:%d failed: %s"
                  params.host params.port (Printexc.to_string exn)
              in
              if permanent then (log ())#severe "%s" message
              else (
                (log ())#important "%s" message;
                self#retry generation params.polling_delay))
  end
