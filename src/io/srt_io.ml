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
  
module G = Generator
module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make(Generator)
  
class input ~kind ~bind_address ~bufferize ~max ~payload_size
            ~on_connect ~on_disconnect format =
  let max_ticks = Frame.master_of_seconds (Stdlib.max max bufferize) in
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
  let abg =
    Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined
  in
object (self)
  
  inherit  Source.source ~name:"input.srt" kind as super
  inherit
    Generated.source abg ~empty_on_abort:false ~bufferize

  val mutable socket = None
  val mutable kill_feeding = None
  val mutable wait_feeding = None

  method private string_of_address = function
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (addr,port) ->
        Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

  method private get_socket =
    match socket with
      | Some s -> s
      | None ->
          let s =
             Srt.socket Unix.PF_INET Unix.SOCK_DGRAM 0
          in
          Srt.setsockflag s Srt.payloadsize payload_size;
          Srt.setsockflag s Srt.transtype `Live;
          Srt.setsockflag s Srt.messageapi true;
          Srt.bind s bind_address;
          Srt.listen s 1;
          self#log#info "Setting up socket to listen at %s"
            (self#string_of_address bind_address);
          socket <- Some s;
          s

  method private close_socket =
    match socket with
      | None -> ()
      | Some s ->
          Srt.close s;
          socket <- None

  method stype = Source.Fallible

  method private log_origin s =
    try
      self#log#info "New connection from %s"
        (self#string_of_address s)
    with exn ->
      self#log#important "Error while fetching connection source: %s"
        (Printexc.to_string exn)

  method private feed (should_stop,has_stopped) =
    let client, origin =
      Srt.accept self#get_socket
    in 
    self#log_origin origin;
    on_connect ();
    Generator.set_mode generator `Undefined ;
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
        let input = Srt.recvmsg client tmp payload_size in
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
    let input =
      { Decoder.
         read = read ;
         tell = None;
         length = None;
         lseek = None }
    in
    let decoder = create_decoder input in
    try
      while true do
        if should_stop () then failwith "stop" ;
        let buffered = Generator.length abg in
        if max_ticks <= buffered then
          Thread.delay (Frame.seconds_of_audio (buffered-3*max_ticks/4));
        decoder.Decoder.decode generator
      done
    with
      | e ->
          (* Feeding has stopped: adding a break here. *)
          Generator.add_break ~sync:`Drop generator ;
          on_disconnect ();
          has_stopped ();
          self#log#severe "Feeding stopped: %s." (Printexc.to_string e);
          if not (should_stop ()) then
            self#feed  (should_stop,has_stopped)
    
  method wake_up act =
    super#wake_up act ;
    begin match wait_feeding with
      | None -> ()
      | Some f -> f (); wait_feeding <- None
    end ;
    let kill,wait = Tutils.stoppable_thread self#feed "SRT input" in
    kill_feeding <- Some kill;
    wait_feeding <- Some wait
  
  method sleep =
    (Utils.get_some kill_feeding) ();
    kill_feeding <- None;
    self#close_socket
end
  
let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "input.srt"
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Input
    ~descr:"Receive a SRT stream."
    [ "bind_address", Lang.string_t, Some (Lang.string "0.0.0.0"),
      Some "Address to bind on the local machine.";

      "port", Lang.int_t, Some (Lang.int 0),
      Some "Port to bind on the local machine (note: ports in SRT are \
            different and not related to TCP ports.";
  
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

      "", Lang.string_t, None,
        Some "Mime (Content-Type) used to find a decoder for the iput stream." ]
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
         let payload_size = Lang.to_int (List.assoc "payload_size" p) in
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
                    ~on_connect ~on_disconnect
                    ~bufferize ~max format):>Source.source))
