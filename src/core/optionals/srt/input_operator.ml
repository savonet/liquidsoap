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

type reader = {
  handle : Connection.peer_handle;
  dump : out_channel option;
  mutable finished : (unit -> unit) option;
  mutable paced : bool;
}

(** The decoder's read function: one message per receive, into a buffer of the
    largest live payload so that no sender's payload is truncated. *)
let receiver reader =
  let received = Bytes.create Srt.live_max_payload_size in
  let position = ref 0 in
  let length = ref 0 in
  let ended = ref false in
  let read buffer offset len =
    if !position >= !length && not !ended then (
      match
        Srt.recvmsg reader.handle.socket received Srt.live_max_payload_size
      with
        | n when n > 0 ->
            Option.iter (fun dump -> output dump received 0 n) reader.dump;
            position := 0;
            length := n
        | _ -> ended := true
        | exception Srt.Error _ -> ended := true);
    let n = min len (!length - !position) in
    Bytes.blit received !position buffer offset n;
    position := !position + n;
    n
  in
  (read, fun () -> !ended)

let decode ~(log : Log.t) ~ctype ~content_type ~buffer ~on_data reader =
  match Decoder.get_stream_decoder ~ctype content_type with
    | None ->
        log#important "Could not find a decoder for content type %s"
          content_type
    | Some create ->
        Generator.add_track_mark buffer;
        let read, ended = receiver reader in
        let decoder =
          create { Decoder.read; tell = None; length = None; lseek = None }
        in
        let decoder_buffer = Decoder.mk_buffer ~ctype buffer in
        Fun.protect ~finally:decoder.Decoder.close (fun () ->
            try
              while true do
                decoder.Decoder.decode decoder_buffer;
                on_data ()
              done
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              if ended () then (
                (try decoder.Decoder.eof decoder_buffer with _ -> ());
                on_data ());
              Printexc.raise_with_backtrace exn bt)

let read_loop ~(log : Log.t) ~ctype ~content_type ~buffer ~on_data reader =
  (try decode ~log ~ctype ~content_type ~buffer ~on_data reader
   with exn -> log#info "Feeding stopped: %s" (Printexc.to_string exn));
  Option.iter close_out_noerr reader.dump;
  reader.handle.fail ()

let stream_name ~params reader =
  let streamid =
    try Srt.getsockflag reader.handle.socket Srt.streamid with _ -> ""
  in
  let stream =
    match params.Connection.mode with
      | `Caller -> Utils.name_of_sockaddr ~show_port:true reader.handle.address
      | `Listener -> Printf.sprintf "%s:%d" params.bind_address params.port
  in
  Printf.sprintf "srt://%s%s" stream
    (if streamid = "" then "" else "?streamid=" ^ streamid)

let paces socket = try Srt.getsockflag socket Srt.tsbpdmode with _ -> false

class input ~params ~read_timeout ~max ~self_sync ~dump ~content_type ~autostart
  =
  object (self)
    inherit
      Start_stop.active_source ~name:"input.srt" ~fallible:true ~autostart ()

    val parker = Clock.Parker.create ()
    val pacing = Atomic.make false

    (** Set when a wait for data timed out: the input is ready again only once a
        full frame is buffered. *)
    val starved = Atomic.make true

    val mutable connection = None
    method connection : reader Connection.connection = Option.get connection

    initializer
      connection <-
        Some
          (Common.connection
             ~log:(fun () -> self#log)
             ~name:"input.srt" ~params ~kind:self#kind ());
      self#on_wake_up (fun () ->
          Generator.set_max_length self#buffer
            (Some (Frame.main_of_seconds max)))

    method private kind =
      {
        Connection.client_limit = Some 1;
        setup_socket = ignore;
        connected = self#peer_connected;
        live = self#peer_live;
        ending = (fun _ ~clean:_ k -> k ());
        ended = self#peer_ended;
        shutdown_wait = 0.;
      }

    method private peer_connected handle =
      let dump =
        if dump = "" then None
        else Some (open_out_bin (Lang_string.home_unrelate dump))
      in
      let reader = { handle; dump; finished = None; paced = false } in
      Atomic.set starved true;
      let on_data () = Clock.Parker.unpark parker in
      reader.finished <-
        Some
          (Library.spawn_joinable (fun () ->
               read_loop ~log:self#log ~ctype:self#content_type ~content_type
                 ~buffer:self#buffer ~on_data reader));
      reader

    method private peer_live reader =
      if self_sync && paces reader.handle.socket then (
        reader.paced <- true;
        (* The clock leaves the scheduler's pool for a thread on this sync
           source, before any tick waits for data. *)
        self#set_sync_source
          (Some (Library.sync_source (stream_name ~params reader)));
        Atomic.set pacing true)

    method private peer_ended reader =
      Atomic.set pacing false;
      Clock.Parker.unpark parker;
      Option.iter (fun finished -> finished ()) reader.finished;
      if reader.paced then self#set_sync_source None

    method private start = self#connection#start
    method private stop = self#connection#stop
    method remaining = -1
    method abort_track = Generator.add_track_mark self#buffer
    method effective_source = (self :> Source.source)

    method private self_sync =
      if self_sync then self#dynamic_self_sync else (`Static, None)

    method private frame_size = Lazy.Mutexed.force Frame.size

    method private can_generate_frame =
      self#connection#wanted
      &&
      if Generator.length self#buffer >= self#frame_size then (
        Atomic.set starved false;
        true)
      else Atomic.get pacing && not (Atomic.get starved)

    method private wait_for_data =
      let deadline =
        Option.map (fun t -> Unix.gettimeofday () +. t) read_timeout
      in
      let rec wait () =
        if Generator.length self#buffer < self#frame_size && Atomic.get pacing
        then (
          match deadline with
            | None ->
                Clock.Parker.park_thread parker;
                wait ()
            | Some deadline ->
                let left = deadline -. Unix.gettimeofday () in
                if left > 0. then (
                  Clock.Parker.park_thread ~delay:left parker;
                  wait ()))
      in
      wait ()

    method private generate_frame =
      if Atomic.get pacing then self#wait_for_data;
      let frame = Generator.slice self#buffer self#frame_size in
      if Frame.position frame < self#frame_size then (
        Atomic.set starved true;
        Frame.add_track_mark frame (Frame.position frame))
      else frame
  end

let _ =
  Lang.add_operator ~base:Modules.input "srt" ~return_t:(Lang.univ_t ())
    ~category:(`Input `Active) ~meth:(Common.meth ())
    ~callbacks:(Common.callbacks () @ Start_stop.callbacks ~label:"input")
    ~descr:"Receive a SRT stream, as a listener or as a caller."
    (Common.proto ~mode:`Listener
    @ Start_stop.active_source_proto ~fallible_opt:`Nope
    @ [
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum duration of the buffered data, in seconds." );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some
            "Pace the clock while a connected stream is delivered by sender \
             timestamps." );
        ( "dump",
          Lang.string_t,
          Some (Lang.string ""),
          Some "File receiving a copy of the received bytes; `\"\"` for none."
        );
        ( "content_type",
          Lang.string_t,
          Some (Lang.string "application/ffmpeg"),
          Some "MIME type used to find a stream decoder." );
      ])
    (fun p ->
      Library.init ();
      let params, read_timeout, _ = Common.parse p in
      let max = Common.check_positive p "max" in
      new input
        ~params ~read_timeout ~max:(Option.get max)
        ~self_sync:(Lang.to_bool (List.assoc "self_sync" p))
        ~dump:(Lang.to_string (List.assoc "dump" p))
        ~content_type:(Lang.to_string (List.assoc "content_type" p))
        ~autostart:(Lang.to_bool (List.assoc "start" p)))
