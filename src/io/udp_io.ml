(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let max_packet_size = if Configure.host = "osx" then 9216 else 65535

module Tutils = struct
  include Tutils

  (* Thread with preemptive kill/wait mechanism, see mli for details. *)

  (** Preemptive stoppable thread.
   *
   * The thread function receives a [should_stop,has_stop] pair on startup.
   * It should regularly poll the [should_stop] and stop when asked to.
   * Before stopping it should call [has_stopped].
   *
   * The function returns a [kill,wait] pair. The first function should be
   * called to request that the thread stops, and the second to wait
   * that it has effectively stopped. *)
  let stoppable_thread f name =
    let cond = Condition.create () in
    let lock = Mutex.create () in
    let should_stop = ref false in
    let has_stopped = ref false in
    let kill = mutexify lock (fun () -> should_stop := true) in
    let wait () = wait cond lock (fun () -> !has_stopped) in
    let should_stop = mutexify lock (fun () -> !should_stop) in
    let has_stopped =
      mutexify lock (fun () ->
          has_stopped := true;
          Condition.signal cond)
    in
    let _ = create f (should_stop, has_stopped) name in
    (kill, wait)
end

class output ~kind ~on_start ~on_stop ~infallible ~autostart ~hostname ~port
  ~encoder_factory source =
  object (self)
    inherit
      Output.encoded
        ~output_kind:"udp" ~content_kind:kind ~on_start ~on_stop ~infallible
          ~autostart
        ~name:(Printf.sprintf "udp://%s:%d" hostname port)
        source

    val mutable socket_send = None
    val mutable encoder = None

    method private start =
      let socket =
        Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_DGRAM
          (Unix.getprotobyname "udp").Unix.p_proto
      in
      let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      socket_send <-
        Some
          (fun msg off len ->
            let msg = Bytes.of_string msg in
            let rec f pos =
              if off + pos < len then (
                let len = min (len - pos) max_packet_size in
                let len = Unix.sendto socket msg (off + pos) len [] portaddr in
                f (pos + len))
            in
            f 0);
      encoder <- Some (encoder_factory self#id Meta_format.empty_metadata)

    method private reset =
      self#start;
      self#stop

    method private stop =
      socket_send <- None;
      encoder <- None

    method private encode frame ofs len =
      (Option.get encoder).Encoder.encode frame ofs len

    method private insert_metadata m =
      (Option.get encoder).Encoder.insert_metadata m

    method private send data =
      let socket_send = Option.get socket_send in
      Strings.iter (fun s o l -> ignore (socket_send s o l)) data
  end

class input ~kind ~hostname ~port ~get_stream_decoder ~bufferize ~log_overfull =
  let max_ticks = 2 * Frame.main_of_seconds bufferize in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  object (self)
    inherit
      Generated.source
        (Generator.create ~log ~log_overfull ~overfull:(`Drop_old max_ticks)
           `Undefined)
        ~empty_on_abort:false ~bufferize

    inherit
      Start_stop.active_source
        ~name:"input.udp" ~content_kind:kind ~clock_safe:false ~fallible:true
          ~on_start:ignore ~on_stop:ignore ~autostart:true ()

    initializer log_ref := fun s -> self#log#important "%s" s
    val mutable kill_feeding = None
    val mutable wait_feeding = None
    val mutable decoder_factory = None
    method private decoder_factory = Option.get decoder_factory

    method private start =
      begin
        decoder_factory <- Some (get_stream_decoder self#ctype);
        match wait_feeding with
          | None -> ()
          | Some f ->
              f ();
              wait_feeding <- None
      end;
      let kill, wait = Tutils.stoppable_thread self#feed "UDP input" in
      kill_feeding <- Some kill;
      wait_feeding <- Some wait

    method private stop =
      (Option.get kill_feeding) ();
      kill_feeding <- None;
      decoder_factory <- None

    method private feed (should_stop, has_stopped) =
      let socket =
        Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_DGRAM
          (Unix.getprotobyname "udp").Unix.p_proto
      in
      let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
      let addr = Unix.ADDR_INET (ipaddr, port) in
      Unix.bind socket addr;

      (* Wait until there's something to read or we must stop. *)
      let rec wait () =
        if should_stop () then failwith "stop";
        let l, _, _ = Unix.select [socket] [] [] 1. in
        if l = [] then wait ()
      in
      (* Read data from the network. *)
      let read buf ofs len =
        wait ();
        let n, _ = Unix.recvfrom socket buf ofs len [] in
        n
      in
      let input = { Decoder.read; tell = None; length = None; lseek = None } in
      try
        (* Feeding loop. *)
        let decoder = self#decoder_factory input in
        let buffer = Decoder.mk_buffer ~ctype:self#ctype generator in
        while true do
          if should_stop () then failwith "stop";
          decoder.Decoder.decode buffer
        done
      with e ->
        Generator.add_break ~sync:true generator;

        (* Closing the socket is slightly overkill but
         * we need to recreate the decoder anyway, which
         * might loose some data too. *)
        Unix.close socket;
        begin
          match e with
          | Failure s -> self#log#severe "Feeding stopped: %s." s
          | e -> self#log#severe "Feeding stopped: %s." (Printexc.to_string e)
        end;
        if should_stop () then has_stopped ()
        else self#feed (should_stop, has_stopped)
  end

let () =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "output.udp"
    ~descr:"Output encoded data to UDP, without any control whatsoever."
    ~category:`Output
    ~flags:[`Hidden; `Deprecated; `Experimental]
    (Output.proto
    @ [
        ("port", Lang.int_t, None, None);
        ("host", Lang.string_t, None, None);
        ("", Lang.format_t k, None, Some "Encoding format.");
        ("", Lang.source_t k, None, None);
      ])
    ~return_t:k
    (fun p ->
      (* Generic output parameters *)
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      (* Specific UDP parameters *)
      let port = Lang.to_int (List.assoc "port" p) in
      let hostname = Lang.to_string (List.assoc "host" p) in
      let fmt =
        let fmt = Lang.assoc "" 1 p in
        try Encoder.get_factory (Lang.to_format fmt)
        with Not_found ->
          raise
            (Error.Invalid_value
               (fmt, "Cannot get a stream encoder for that format"))
      in
      let source = Lang.assoc "" 2 p in
      let kind = Kind.of_kind kind in
      (new output
         ~kind ~on_start ~on_stop ~infallible ~autostart ~hostname ~port
         ~encoder_factory:fmt source
        :> Source.source))

let () =
  let kind = Lang.any in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "input.udp"
    ~descr:"Input encoded data from UDP, without any control whatsoever."
    ~category:`Input
    ~flags:[`Hidden; `Deprecated; `Experimental]
    [
      ("port", Lang.int_t, None, None);
      ("host", Lang.string_t, None, None);
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Duration of buffered data before starting playout." );
      ( "log_overfull",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Log when the source's buffer is overfull." );
      ("", Lang.string_t, None, Some "Mime type.");
    ]
    ~return_t:k
    (fun p ->
      (* Specific UDP parameters *)
      let port = Lang.to_int (List.assoc "port" p) in
      let hostname = Lang.to_string (List.assoc "host" p) in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let mime = Lang.to_string (Lang.assoc "" 1 p) in
      let get_stream_decoder ctype =
        match Decoder.get_stream_decoder ~ctype mime with
          | None ->
              raise
                (Error.Invalid_value
                   ( Lang.assoc "" 1 p,
                     "Cannot get a stream decoder for this MIME" ))
          | Some decoder_factory -> decoder_factory
      in
      let kind = Kind.of_kind kind in
      (new input
         ~kind ~hostname ~port ~bufferize ~log_overfull ~get_stream_decoder
        :> Source.source))
