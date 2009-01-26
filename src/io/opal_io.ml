module Opal =
struct
  type t

  external init : unit -> t = "caml_opal_init"

  external shutdown : t -> unit = "caml_opal_shutdown"

  external set_general_parameters : t -> string -> string -> bool -> unit = "caml_opal_set_general_parameters"

  let set_general_parameters ?(auto_rx_media="audio") ?(auto_tx_media="audio") ?(write=true) h =
    set_general_parameters
      h
      auto_rx_media
      auto_tx_media
      write

  external set_protocol_parameters : t -> string -> string -> string -> unit = "caml_opal_set_protocol_parameters"

  let set_protocol_parameters ~username ~displayname ?(interface="*") h =
    set_protocol_parameters h username displayname interface

  external read_data : t -> string * string * string * string = "caml_opal_read_data" (* token, id, format, data *)


  (** {2 Messages} *)
  type message

  external get_message : t -> int option -> message = "caml_opal_get_message"

  let get_message ?timeout h = get_message h timeout

  external free_message : message -> unit = "caml_opal_free_message"

  type message_type =
    | Type_unknown
    | Type_ind_incoming_call of string * string * string * string * string * string * string (** call token, local address, remote address, remote party number, remote display name, called address, called party number *)

  external get_message_type : message -> message_type = "caml_opal_get_message_type"

  external answer_call : t -> string -> unit = "caml_opal_answer_call"
end

let ringbuffer_frames = 10

class input =
object (self)
  inherit Source.active_source

  val mutable handle = None

  method stype = Source.Fallible
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  val write_rb = Ringbuffer.TS.create (Fmt.channels ()) (Fmt.samples_per_frame () * ringbuffer_frames)

  method output_get_ready =
    (* TODO: init only once *)
    let h = Opal.init () in
    let message_handler () =
      while true do
        let m = Opal.get_message h in
          (
            Printf.printf "Got message!\n%!";
            match Opal.get_message_type m with
              | Opal.Type_ind_incoming_call (t, la, ra, rpn, rdn, ca, cpn) ->
                  Printf.printf "Call from %s (%s to %s).\n%!" rdn ra la;
                  Opal.answer_call h t
              | _ -> ()
          );
          Opal.free_message m
      done
    in
    let rec reader () =
      let buflen = 1024 in
      let fbuf = Array.init (Fmt.channels ()) (fun _ -> Array.make buflen 0.) in
        while true do
          let token, id, fmt, data = Opal.read_data h in
          let len = String.length data in
            (* Printf.printf "Received %d bytes of %s for stream %s on call %s.\n%!" len fmt id token; *)
            (* let len = Unix.read fd buf 0 (buflen*4) in *)
            assert (len/4 <= buflen);
            Float_pcm.from_s16le fbuf 0 data 0 (len/4);
            if Ringbuffer.TS.write_space write_rb >= len/4 then
              Ringbuffer.TS.write write_rb fbuf 0 (len/4)
            else
              () (* Printf.printf "Not enough space in ringbuffer. Dropping.\n%!" *)
        done
    in
      handle <- Some h;
      ignore (Dtools.Init.at_stop (fun () -> Opal.shutdown h));
      Opal.set_general_parameters h ~auto_rx_media:"audio" ~auto_tx_media:"audio";
      Opal.set_protocol_parameters h ~username:"liq" ~displayname:"Liquidsoap live";
      ignore (Thread.create reader ());
      ignore (Thread.create message_handler ())

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let buf = AFrame.get_float_pcm frame in
    let samples = AFrame.size frame in
      (*
        let available = Ringbuffer.TS.read_space write_rb in
          if available <> 0 then
            Printf.printf "Available: %d.\n%!" available;
       *)
      if Ringbuffer.TS.read_space write_rb >= samples then
          Ringbuffer.TS.read write_rb buf 0 samples
      else
        (); (* Printf.printf "Not enough samples in ringbuffer.\n%!"; *)
      AFrame.add_break frame samples
end

let () =
  Lang.add_operator "input.voip"
    [
    ]
    ~category:Lang.Input
    ~descr:"Stream from voip calls using opal library."
    (fun p ->
       (* let e f v = f (List.assoc v p) in *)
         ((new input):>Source.source))
