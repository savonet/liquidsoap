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

    method private output_start =
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
          (Unix.getprotobyname "udp").Unix.p_proto
      in
      let ipaddr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      socket_send <-
        Some
          (fun msg off len ->
            Unix.sendto socket (Bytes.of_string msg) off len [] portaddr);
      encoder <- Some (encoder_factory self#id Meta_format.empty_metadata)

    method private output_reset =
      self#output_start;
      self#output_stop

    method private output_stop =
      socket_send <- None;
      encoder <- None

    method private encode frame ofs len =
      (Utils.get_some encoder).Encoder.encode frame ofs len

    method private insert_metadata m =
      (Utils.get_some encoder).Encoder.insert_metadata m

    method private send data =
      let socket_send = Utils.get_some socket_send in
      Strings.iter (fun s o l -> ignore (socket_send s o l)) data
  end

module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

class input ~kind ~hostname ~port ~decoder_factory ~bufferize =
  let max_ticks = 2 * Frame.master_of_seconds bufferize in
  (* A log function for our generator: start with a stub, and replace it
   * when we have a proper logger with our ID on it. *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  object (self)
    inherit Source.source ~name:"input.udp" kind

    inherit
      Generated.source
        (Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined)
        ~empty_on_abort:false ~bufferize

    inherit
      Start_stop.async
        ~source_kind:"udp"
        ~name:(Printf.sprintf "udp://%s:%d" hostname port)
        ~on_start:ignore ~on_stop:ignore ~autostart:true

    initializer log_ref := fun s -> self#log#important "%s" s

    val mutable kill_feeding = None

    val mutable wait_feeding = None

    method private start =
      begin
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
      (Utils.get_some kill_feeding) ();
      kill_feeding <- None

    method private output_reset =
      request_stop <- true;
      request_start <- true

    method private is_active = true

    method private stype = Source.Fallible

    method private feed (should_stop, has_stopped) =
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
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
        let decoder = decoder_factory input in
        while true do
          if should_stop () then failwith "stop";
          decoder.Decoder.decode generator
        done
      with e ->
        Generator.add_break ~sync:`Drop generator;

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
  let k = Lang.univ_t () in
  Lang.add_operator "output.udp" ~active:true
    ~descr:"Output encoded data to UDP, without any control whatsoever."
    ~category:Lang.Output ~flags:[Lang.Experimental]
    ( Output.proto
    @ [
        ("port", Lang.int_t, None, None);
        ("host", Lang.string_t, None, None);
        ("", Lang.format_t k, None, Some "Encoding format.");
        ("", Lang.source_t k, None, None);
      ] )
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
      (* Generic output parameters *)
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      (* Specific UDP parameters *)
      let port = Lang.to_int (List.assoc "port" p) in
      let hostname = Lang.to_string (List.assoc "host" p) in
      let fmt =
        let fmt = Lang.assoc "" 1 p in
        try Encoder.get_factory (Lang.to_format fmt)
        with Not_found ->
          raise
            (Lang_errors.Invalid_value
               (fmt, "Cannot get a stream encoder for that format"))
      in
      let source = Lang.assoc "" 2 p in
      ( new output
          ~kind ~on_start ~on_stop ~infallible ~autostart ~hostname ~port
          ~encoder_factory:fmt source
        :> Source.source ))

let () =
  let k = Lang.univ_t () in
  Lang.add_operator "input.udp" ~active:true
    ~descr:"Input encoded data from UDP, without any control whatsoever."
    ~category:Lang.Input ~flags:[Lang.Experimental]
    [
      ("port", Lang.int_t, None, None);
      ("host", Lang.string_t, None, None);
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Duration of buffered data before starting playout." );
      ("", Lang.string_t, None, Some "Mime type.");
    ]
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
      (* Specific UDP parameters *)
      let port = Lang.to_int (List.assoc "port" p) in
      let hostname = Lang.to_string (List.assoc "host" p) in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let mime = Lang.to_string (Lang.assoc "" 1 p) in
      match Decoder.get_stream_decoder mime kind with
        | None ->
            raise
              (Lang_errors.Invalid_value
                 (Lang.assoc "" 1 p, "Cannot get a stream decoder for this MIME"))
        | Some decoder_factory ->
            ( new input ~kind ~hostname ~port ~bufferize ~decoder_factory
              :> Source.source ))
