module Opal =
struct
  exception Error of string

  let () =
    Callback.register_exception "opal_exn_error" (Error "")

  type t

  external init : unit -> t = "caml_opal_init"

  external shutdown : t -> unit = "caml_opal_shutdown"

  external set_general_parameters :
             t -> string -> string -> bool -> unit
             = "caml_opal_set_general_parameters"

  let set_general_parameters
        ?(auto_rx_media="audio")
        ?(auto_tx_media="audio")
        ?(write=true) h =
    set_general_parameters
      h
      auto_rx_media
      auto_tx_media
      write

  external set_protocol_parameters :
      t -> string -> string -> string -> unit
      = "caml_opal_set_protocol_parameters"

  let set_protocol_parameters ~username ~displayname ?(interface="*") h =
    set_protocol_parameters h username displayname interface

  external read_data :
      t -> string * string * string * string (** token, id, format, data *)
      = "caml_opal_read_data"


  (** {2 Messages} *)
  type message

  external get_message : t -> int option -> message = "caml_opal_get_message"

  let get_message ?timeout h = get_message h timeout

  external free_message : message -> unit = "caml_opal_free_message"

  type message_type =
    | Type_unknown
    | Type_ind_alerting
    | Type_ind_established
    | Type_ind_incoming_call of
        string * string * string * string * string * string * string
        (** call token, local address, remote address, remote party number,
          * remote display name, called address, called party number *)
    | Type_ind_call_cleared of string
    | Type_ind_media_stream of
        string * bool * string (** type, state is opened, format *)

  external get_message_type : message -> message_type
    = "caml_opal_get_message_type"

  external answer_call : t -> string -> unit = "caml_opal_answer_call"
end

class input =
  let ringbuffer_length = 10 * Fmt.samples_per_frame () in
object (self)
  inherit Source.active_source

  val mutable handle = None

  method stype = Source.Fallible
  method remaining = -1
  method is_ready = true

  method abort_track = ()

  method output = if AFrame.is_partial memo then self#get_frame memo

  val write_rb = Ringbuffer.TS.create (Fmt.channels ()) ringbuffer_length

  method output_get_ready =
    (* TODO: init only once *)
    let h = Opal.init () in
    let message_handler () =
      while true do
        let m = Opal.get_message h in
          (
            match Opal.get_message_type m with
              | Opal.Type_ind_incoming_call (t, la, ra, rpn, rdn, ca, cpn) ->
                  self#log#f 3 "Call from %s (%s to %s)." rdn ra la;
                  Opal.answer_call h t
              | Opal.Type_ind_alerting ->
                  self#log#f 3 "Ringing."
              | Opal.Type_ind_established ->
                  self#log#f 3 "Connection established."
              | Opal.Type_ind_call_cleared s ->
                  self#log#f 3 "Call cleared: %s." s
              | Opal.Type_ind_media_stream (t, o, f) ->
                  self#log#f 3 "Media stream %s %s using %s."
                    t (if o then "opened" else "closed") f
              | _ ->
                  self#log#f 3 "Unknown message."
          );
          Opal.free_message m
      done
    in
    let rec reader () =
      let buflen = 1024 in
      let fbuf = [|Array.make buflen 0.|] in
      let conv = Audio_converter.Samplerate.create 1 in
      let outfreq = float_of_int (Fmt.samples_per_second ()) in
        while true do
          let token, id, fmt, data = Opal.read_data h in
          let len = String.length data / 2 in
          let infreq =
            match fmt with
              | "PCM-16" -> 8000.
              | "PCM-16-16kHz" -> 16000.
              | _ ->
                  self#log#f 2 "Cannot handle %s format." fmt;
                  assert false
          in
            (* self#log#f 5
                 "Received %d bytes of %s for stream %s on call %s."
                 len fmt id token; *)
            assert (len <= buflen);
            Float_pcm.from_s16le fbuf 0 data 0 len;
            let fbuf =
              Audio_converter.Samplerate.resample
                conv (outfreq /. infreq) fbuf 0 len
            in
            let fbuf =
              let fbuf = fbuf.(0) in Array.create (Fmt.channels ()) fbuf
            in
              if Ringbuffer.TS.write_space write_rb >= len then
                Ringbuffer.TS.write write_rb fbuf 0 len
              else
                (* self#log#f 4 "Not enough space in ringbuffer. Dropping." *)
                ()
        done
    in
      handle <- Some h;
      ignore (Dtools.Init.at_stop (fun () -> Opal.shutdown h));
      Opal.set_general_parameters h
        ~auto_rx_media:"audio" ~auto_tx_media:"audio";
      Opal.set_protocol_parameters h
        ~username:"liq" ~displayname:"Liquidsoap live";
      ignore (Tutils.create reader () "Opal reader");
      ignore (Tutils.create message_handler () "Opal message handler")

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let buf = AFrame.get_float_pcm frame in
    let samples = AFrame.size frame in
      (*
        let available = Ringbuffer.TS.read_space write_rb in
          if available <> 0 then
            self#log#f 6 "Available: %d." available;
       *)
      if Ringbuffer.TS.read_space write_rb >= samples then
          Ringbuffer.TS.read write_rb buf 0 samples
      else
        (); (* self#log#f 4 "Not enough samples in ringbuffer."; *)
      AFrame.add_break frame samples
end

let () =
  Lang.add_operator "input.voip"
    [
    ]
    ~category:Lang.Input
    ~descr:"Stream from voip calls using opal library."
    (fun p _ -> (new input :> Source.source))
