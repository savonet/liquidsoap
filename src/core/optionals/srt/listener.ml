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

let backlog = function Some n -> max n 1 | None -> 128

class ['reaction] connection ~log ~name ~params ~kind () =
  object (self)
    inherit ['reaction] Connection.connection ~log ~name ~params ~kind ()
    method private connect_role generation = self#listen ~first:true generation

    method private retry generation delay =
      Library.after delay (fun () ->
          if Mutex.protect m (fun () -> self#current generation) then
            self#listen ~first:false generation)

    method private listen ~first generation =
      let attempt () =
        try Ok (self#setup_listener generation) with exn -> Error exn
      in
      if first && not (Events.on_runner events) then
        self#listen_result ~raise_permanent:true generation
          (Events.run events attempt)
      else
        Events.post events (fun () ->
            self#listen_result ~raise_permanent:false generation (attempt ()))

    method private listen_result ~raise_permanent generation =
      function
      | Ok () -> ()
      | Error exn ->
          let message =
            Printf.sprintf "Listening on srt://%s:%d failed: %s"
              params.bind_address params.port (Printexc.to_string exn)
          in
          if Library.is_permanent exn then (
            if raise_permanent then
              failwith (Printf.sprintf "%s: %s" name message);
            (log ())#severe "%s" message)
          else (
            (log ())#important "%s" message;
            let waiting =
              Mutex.protect m (fun () ->
                  self#current generation
                  &&
                  (state <- Waiting;
                   true))
            in
            if waiting then self#retry generation params.polling_delay)

    method private setup_listener generation =
      let bind_address =
        match (params.bind_address, params.prefer_address) with
          | "0.0.0.0", `Ipv6 -> "::"
          | bind_address, _ -> bind_address
      in
      let address =
        List.hd
          (Library.resolve ~prefer_address:params.prefer_address bind_address
             params.port)
      in
      let socket = self#new_socket in
      let created = ref false in
      try
        (match params.ipv6only with
          | Some v -> Srt.setsockflag socket Srt.ipv6only v
          | None ->
              if Library.is_ipv6 address then
                Srt.setsockflag socket Srt.ipv6only true);
        self#set_encryption socket;
        created := true;
        self#socket_event "listen" socket;
        Srt.bind socket address;
        Srt.listen_callback socket (self#admit generation);
        Srt.listen socket (backlog kind.client_limit);
        (log ())#info "Listening on srt://%s"
          (Utils.name_of_sockaddr ~show_port:true address);
        let listening =
          Mutex.protect m (fun () ->
              self#current generation
              &&
              (state <- Listening { listener = socket; peers = [] };
               true))
        in
        if listening then (
          Poll.register socket (self#accept generation);
          self#accept generation socket)
        else self#close_socket ~created:true socket
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        self#close_socket ~created:!created socket;
        Printexc.raise_with_backtrace exn bt

    method private admitted_count generation =
      Mutex.protect m (fun () ->
          match state with
            | Listening { peers } when self#current generation ->
                Some (List.length peers)
            | _ -> None)

    method private admit generation socket hs_version peeraddr streamid =
      match (self#admitted_count generation, kind.client_limit) with
        | None, _ -> false
        | Some count, Some limit when count >= limit -> false
        | Some _, _ -> (
            match params.listen_callback with
              | None -> true
              | Some fn -> (
                  try
                    fn ~hs_version
                      ~peeraddr:(Utils.name_of_sockaddr peeraddr)
                      ~streamid socket
                  with exn ->
                    (log ())#severe "Error in listen_callback: %s"
                      (Printexc.to_string exn);
                    false))

    method private accept generation listener =
      match Srt.accept listener with
        | exception Srt.Error (`Easyncrcv, _, _) -> ()
        | exception exn ->
            (log ())#important "Failed to accept new connection: %s"
              (Printexc.to_string exn)
        | socket, origin ->
            self#admit_accepted generation socket origin;
            self#accept generation listener

    method private admit_accepted generation socket origin =
      Atomic.incr open_sockets;
      (log ())#info "New connection from %s"
        (Utils.name_of_sockaddr ~show_port:true origin);
      let peer =
        Mutex.protect m (fun () ->
            match state with
              | Listening l
                when self#current generation
                     && Option.fold ~none:true
                          ~some:(fun n -> List.length l.peers < n)
                          kind.client_limit ->
                  let peer = self#make_peer ~created:false socket origin in
                  l.peers <- peer :: l.peers;
                  Some peer
              | _ -> None)
      in
      match peer with
        | None -> self#close_socket ~created:false socket
        | Some peer -> Events.post events (fun () -> self#accepted peer)

    method private accepted peer =
      match
        Srt.setsockflag peer.handle.socket Srt.rcvsyn true;
        Srt.setsockflag peer.handle.socket Srt.sndsyn true;
        kind.setup_socket peer.handle.socket;
        peer.created <- true;
        self#socket_event "incoming" peer.handle.socket
      with
        | () -> self#peer_connected peer
        | exception exn ->
            (log ())#important "Setting up new connection failed: %s"
              (Printexc.to_string exn);
            self#end_peer peer ~clean:false
  end
