type t
external init : unit -> t = "caml_opal_init"
external shutdown : t -> unit = "caml_opal_shutdown"
external set_general_parameters : t -> string -> string -> (string -> string -> unit) -> unit = "caml_opal_set_general_parameters"
external set_protocol_parameters : t -> string -> string -> string -> unit = "caml_opal_set_protocol_parameters"

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

class input =
object (self)
  inherit Source.active_source

  val mutable handle = None

  method stype = Source.Fallible
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_get_ready =
    (* TODO: init only once *)
    let h = init () in
    let reader () =
      while true do
        let m = get_message h in
          (
            Printf.printf "Got message!\n%!";
            match get_message_type m with
              | Type_ind_incoming_call (t, la, ra, rpn, rdn, ca, cpn) ->
                  Printf.printf "Call from %s (%s to %s).\n%!" rdn ra la;
                  answer_call h t
              | _ -> ()
          );
          free_message m
      done
    in
      handle <- Some h;
      ignore (Dtools.Init.at_stop (fun () -> shutdown h));
      set_general_parameters h "audio" "audio" (fun f d -> Printf.printf "Read %s.\n%!" f);
      set_protocol_parameters h "liq" "Liquidsoap live" "*";
      ignore (Thread.create reader ())

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let buf = AFrame.get_float_pcm frame in
    let samples = Array.length buf.(0) in
    let len = 2 * (Array.length buf) * samples in
    let s = String.create len in
      (* TODO: fill s... *)
      Float_pcm.from_s16le buf 0 s 0 samples;
      AFrame.add_break frame (AFrame.size frame)
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
