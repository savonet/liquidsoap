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

(** SRT input *)

exception Done
  
module G = Generator
module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make(Generator)

let () =
  Srt.startup ();
  ignore (Dtools.Init.at_stop Srt.cleanup)

type handler = {
  socket: Srt.socket;
  poll: Srt.poll
}

class virtual base ~payload_size ~mode ~poll_delay ~messageapi =
  (* In ms *)
  let timeout = int_of_float (poll_delay *. 1000.) in
object(self)
  val mutex = Mutex.create ()
  val mutable handler = None

  method private string_of_address = function
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (addr,port) ->
        Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

  (* No blocking operation in prepare_socket, plz! *)
  method virtual private prepare_socket : Srt.socket -> unit

  method private get_socket =
    Tutils.mutexify mutex (fun () ->
      match handler with
        | Some {socket} -> socket
        | None ->
            let socket =
               Srt.socket Unix.PF_INET Unix.SOCK_DGRAM 0
            in
            Srt.setsockflag socket Srt.payloadsize payload_size;
            Srt.setsockflag socket Srt.transtype `Live;
            Srt.setsockflag socket Srt.messageapi messageapi;
            self#prepare_socket socket;
            let poll = Srt.epoll_create () in
            begin
             match mode with
               | `Read  ->
                   Srt.setsockflag socket Srt.rcvsyn false
               | `Write ->
                   Srt.setsockflag socket Srt.sndsyn false
           end;
           let f () = Srt.epoll_release poll in 
           let h = {socket;poll} in
           Gc.finalise_last f h;
           handler <- Some h;
           socket) ()

  method private close_socket =
    Tutils.mutexify mutex (fun () ->
      match handler with
        | None -> ()
        | Some {socket} ->
            Srt.close socket;
            handler <- None) ()

  method private get_poll =
    Tutils.mutexify mutex (fun () ->
      match handler with
        | None -> raise Done
        | Some {poll} -> poll) ()

  method private poll ~should_stop socket =
    let poll = self#get_poll in
    Srt.setsockflag socket Srt.sndsyn false;
    Srt.setsockflag socket Srt.rcvsyn false;
    Srt.epoll_add_usock poll socket (mode:>Srt.poll_flag);
    let max_read, max_write =
      match mode with
        | `Read ->  1, 0
        | `Write -> 0, 1
    in
    let rec f () =
      try
        if should_stop () then raise Done;
        ignore(Srt.epoll_wait poll ~max_read ~max_write ~timeout);
      with
        | Srt.Error(`Etimeout,_) when not (should_stop ()) -> f ()
        | exn ->
           Srt.epoll_remove_usock poll socket;
           raise exn
    in
    f ();
    Srt.epoll_remove_usock poll socket
end
  
class input ~kind ~bind_address ~bufferize ~max ~payload_size
            ~poll_delay ~on_connect ~on_disconnect ~messageapi format =
  let max_ticks = Frame.master_of_seconds (Stdlib.max max bufferize) in
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
  let abg =
    Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined
  in
object (self)
  
  inherit base ~mode:`Read ~payload_size ~messageapi ~poll_delay
  inherit Source.source ~name:"input.srt" kind as super
  inherit
    Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable feeding_thread = None

  method stype = Source.Fallible

  method private log_origin s =
    try
      self#log#info "New connection from %s"
        (self#string_of_address s)
    with exn ->
      self#log#important "Error while fetching connection source: %s"
        (Printexc.to_string exn)

  method private prepare_socket s =
    Srt.bind s bind_address;
    Srt.listen s 1;
    self#log#info "Setting up socket to listen at %s"
      (self#string_of_address bind_address);

  method private create_decoder ~should_stop socket =
    let create_decoder =
      match
        Decoder.get_stream_decoder format kind
      with
        | Some d -> d
        | None -> raise Harbor.Unknown_codec
    in
    let buf = Buffer.create payload_size in
    let tmp = Bytes.create payload_size in
    let read len =
      if Buffer.length buf < len then
       begin
        if should_stop () then raise Done;
        self#poll ~should_stop socket;
        if should_stop () then raise Done;
        let input = Srt.recvmsg socket tmp payload_size in
        if input = 0 then raise End_of_file;
        Buffer.add_subbytes buf tmp 0 input
       end;
      let len = min len (Buffer.length buf) in
      let ret =
        Buffer.sub buf 0 len
      in
      Utils.buffer_drop buf len;
      ret,len
    in
    create_decoder { Decoder.
      read = read ;
      tell = None;
      length = None;
      lseek = None }

  method private feed (should_stop,has_stopped) =
    try
      let s = self#get_socket in
      if should_stop () then raise Done;
      self#poll ~should_stop s;
      if should_stop () then raise Done;
      let client, origin =
        Srt.accept s
      in 
      if should_stop () then raise Done;
      self#log_origin origin;
      on_connect ();
      Generator.set_mode generator `Undefined ;
      let decoder =
        self#create_decoder ~should_stop client
      in
      while true do
        if should_stop () then raise Done;
        let buffered = Generator.length abg in
        if max_ticks <= buffered then
          Thread.delay (Frame.seconds_of_audio (buffered-3*max_ticks/4));
        if should_stop () then raise Done;
        decoder.Decoder.decode generator
      done
    with
      | e ->
          self#log#severe "Feeding stopped: %s." (Printexc.to_string e);
          (* Feeding has stopped: adding a break here. *)
          Generator.add_break ~sync:`Drop generator ;
          on_disconnect ();
          self#close_socket;
          if not (should_stop ()) then
            self#feed (should_stop,has_stopped)
          else
            has_stopped ()
    
  method wake_up act =
    super#wake_up act ;
    begin match feeding_thread with
      | None -> ()
      | Some (_,wait) -> wait ()
    end ;
    let (kill, wait) =
      Tutils.stoppable_thread self#feed self#id
    in
    let kill () =
       kill ();
       self#close_socket
    in
    feeding_thread <- Some (kill,wait)
  
  method sleep =
    begin match feeding_thread with
      | None -> ()
      | Some (kill,_) -> kill ()
    end ;
    feeding_thread <- None ;
end
  
let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "input.srt"
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Input
    ~descr:"Start a SRT agent in listener mode to receive and decode a stream."
    [ "bind_address", Lang.string_t, Some (Lang.string "0.0.0.0"),
      Some "Address to bind on the local machine.";

      "port", Lang.int_t, Some (Lang.int 8000),
      Some "Port to bind on the local machine. The term `port` as used in SRT \
            is occasionally identical to the term `UDP port`. However SRT \
            offers more flexibility than UDP because it manages ports as its \
            own resources. For example, one port may be shared between various \
            services.";

      "poll_delay", Lang.float_t, Some (Lang.float 0.1),
      Some "Timeout for the socket accept polling loop.";
  
      "buffer", Lang.float_t, Some (Lang.float 2.),
      Some "Duration of the pre-buffered data.";
  
      "on_connect",
      Lang.fun_t [false,"",Lang.unit_t] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to execute when a source is connected.";
  
      "on_disconnect",Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to excecute when a stream is disconnected";
  
      "max", Lang.float_t, Some (Lang.float 10.),
      Some "Maximum duration of the buffered data." ;

      "payload_size", Lang.int_t, Some (Lang.int 1316),
      Some "Payload size." ;

      "messageapi", Lang.bool_t, Some (Lang.bool true),
      Some "Use message api" ;

      "", Lang.string_t, None,
        Some "Mime (Content-Type) used to find a decoder for the input stream." ]
      (fun p kind ->
         let bind_address = Lang.to_string (List.assoc "bind_address" p) in
         let bind_address =
           try
             Unix.inet_addr_of_string bind_address
           with exn ->
             raise (Lang_errors.Invalid_value
                (List.assoc "bind_address" p,
                 Printf.sprintf "Invalid address: %s" (Printexc.to_string exn)))
         in
         let port = Lang.to_int (List.assoc "port" p) in
         let bind_address =
           Unix.ADDR_INET (bind_address,port)
         in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let max = Lang.to_float (List.assoc "max" p) in
         if bufferize >= max then
           raise (Lang_errors.Invalid_value
                    (List.assoc "max" p,
                     "Maximum buffering inferior to pre-buffered data"));
         let messageapi = Lang.to_bool (List.assoc "messageapi" p) in
         let payload_size = Lang.to_int (List.assoc "payload_size" p) in
         let poll_delay =
           Lang.to_float (List.assoc "poll_delay" p)
         in
         let on_connect () =
           ignore
             (Lang.apply ~t:Lang.unit_t (List.assoc "on_connect" p) [])
         in
         let on_disconnect () =
           ignore
             (Lang.apply ~t:Lang.unit_t (List.assoc "on_disconnect" p) [])
         in
         let format = Lang.to_string (List.assoc "" p) in
         ((new input ~kind ~bind_address ~payload_size
                    ~on_connect ~on_disconnect ~messageapi
                    ~poll_delay ~bufferize ~max format):>Source.source))

class output ~kind ~payload_size ~messageapi
  ~on_start ~on_stop ~infallible ~autostart
  ~poll_delay ~port ~hostname ~encoder_factory source =
object (self)

  inherit base ~mode:`Write ~payload_size ~messageapi ~poll_delay
  inherit
    Output.encoded ~output_kind:"srt" ~content_kind:kind
      ~on_start ~on_stop ~infallible ~autostart
      ~name:"output.srt" source

  val data_condition = Condition.create ()
  val data_mutex = Mutex.create ()
  val buffer = Buffer.create payload_size
  val tmp = Bytes.create payload_size
  val mutable encoder = None
  val mutable seeding_thread = None 

  method private prepare_socket _ = ()

  method private send_chunk should_stop =
    let socket = self#get_socket in
    let send data =
      if messageapi then
        Srt.sendmsg socket data (-1) false
      else
        Srt.send socket data
    in
    Tutils.mutexify data_mutex (fun () ->
      Buffer.blit buffer 0 tmp 0 payload_size;
      Utils.buffer_drop buffer payload_size) ();
    let rec f = function
      | pos when pos < payload_size ->
        if should_stop () then raise Done;
        self#poll ~should_stop socket;
        if should_stop () then raise Done;
        let ret =
          send (Bytes.sub tmp pos (payload_size-pos))
        in
        f (pos+ret)
      | _ -> ()
    in
    f 0

  method private wait_for_data =
    Tutils.mutexify data_mutex (fun should_stop ->
      if should_stop () then raise Done;
      while Buffer.length buffer < payload_size do
        if should_stop () then raise Done;
        Condition.wait data_condition data_mutex
      done)

  method private get_encoder =
    Tutils.mutexify data_mutex (fun () ->
      match encoder with
        | Some enc -> enc
        | None ->
            let enc = 
              encoder_factory self#id Meta_format.empty_metadata
            in
            encoder <- Some enc;
            enc) ()

  method private clear_encoder =
    Tutils.mutexify data_mutex (fun () ->
      Buffer.reset buffer;
      encoder <- None) ()

  method private connect_socket should_stop =
    let ipaddr =
      (Unix.gethostbyname hostname).Unix.h_addr_list.(0)
    in
    let sockaddr = Unix.ADDR_INET (ipaddr, port) in
    let socket = self#get_socket in
    try
      self#log#important "Connecting to srt://%s:%d.." hostname port;
      let socket = self#get_socket in
      Srt.setsockflag socket Srt.sndsyn true;
      Srt.setsockflag socket Srt.rcvsyn true;
      Srt.connect self#get_socket sockaddr;
      self#log#important "Output connected!"
   with
     | Srt.Error(`Etimeout,_) when not (should_stop ()) ->
        self#log#important "Timeout while trying to connect to srt://%s:%d.." hostname port;
        self#poll ~should_stop socket ;
        self#connect_socket should_stop

  method private seed (should_stop,has_stopped) =
    try
      self#clear_encoder;
      if should_stop () then raise Done; 
      self#connect_socket should_stop;
      let rec f () =
        self#wait_for_data should_stop;
        self#send_chunk should_stop;
        f ()
      in
      f ()
    with exn ->
      self#log#important "Error while sending data: %s" (Printexc.to_string exn);
      self#close_socket;
      if should_stop () then
        has_stopped ()
      else
        self#seed (should_stop,has_stopped)

  method private output_start =
    begin match seeding_thread with
      | None -> ()
      | Some (_,wait) -> wait ()
    end;
    let (kill, wait) =
      Tutils.stoppable_thread self#seed self#id
    in
    let kill () =
      kill ();
      Condition.signal data_condition
    in
    seeding_thread <-
      Some (kill, wait)

  method private output_reset = self#output_start ; self#output_stop

  method private output_stop =
    match seeding_thread with
      | None -> ()
      | Some (kill,_) -> kill ()

  method private encode frame ofs len =
    self#get_encoder.Encoder.encode frame ofs len

  method private insert_metadata m =
    self#get_encoder.Encoder.insert_metadata m

  method private send =
    Tutils.mutexify data_mutex (fun data ->
      Buffer.add_string buffer data;
      Condition.signal data_condition)
end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "output.srt" ~active:true
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Output
    ~descr:"Send a SRT stream to a distant host."
    (Output.proto @ [ 
      "host", Lang.string_t, Some (Lang.string "localhost"),
      Some "Address to connect to.";

      "port", Lang.int_t, Some (Lang.int 8000),
      Some "Port to bind on the local machine. The term `port` as used in SRT \
            is occasionally identical to the term `UDP port`. However SRT \
            offers more flexibility than UDP because it manages ports as its \
            own resources. For example, one port may be shared between various \
            services.";

      "poll_delay", Lang.float_t, Some (Lang.float 2.),
      Some "Timeout for socket connection. In some cases, liquidsoap may \
            have to wait for that amount of time during a shutdown.";

      "payload_size", Lang.int_t, Some (Lang.int 1316),
      Some "Payload size." ;

      "messageapi", Lang.bool_t, Some (Lang.bool true),
      Some "Use message api" ;

      "", Lang.format_t kind, None, Some "Encoding format.";

      "", Lang.source_t kind, None, None ])
      (fun p kind ->
         let hostname =
           Lang.to_string (List.assoc "host" p)
         in
         let port = Lang.to_int (List.assoc "port" p) in
         let messageapi = Lang.to_bool (List.assoc "messageapi" p) in
         let payload_size = Lang.to_int (List.assoc "payload_size" p) in
         let source = Lang.assoc "" 2 p in
         let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
         let autostart = Lang.to_bool (List.assoc "start" p) in
         let poll_delay =
           Lang.to_float (List.assoc "poll_delay" p)
         in
         let on_start =
           let f = List.assoc "on_start" p in
             fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
         in
         let on_stop =
           let f = List.assoc "on_stop" p in
             fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
         in
         let encoder_factory =
           let fmt = Lang.assoc "" 1 p in
           try Encoder.get_factory (Lang.to_format fmt) with
             | Not_found ->
                 raise (Lang_errors.Invalid_value
                          (fmt,
                           "Cannot get a stream encoder for that format"))
         in
         ((new output ~kind ~hostname ~port ~payload_size ~autostart
                      ~on_start ~on_stop ~infallible ~messageapi
                      ~poll_delay ~encoder_factory source):>Source.source))
