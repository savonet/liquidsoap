type t
external init : unit -> t = "caml_opal_init"
external shutdown : t -> unit = "caml_opal_shutdown"
external set_protocol_parameters : t -> string -> string -> string -> unit = "caml_opal_set_protocol_parameters"

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
      handle <- Some h;
      ignore (Dtools.Init.at_stop (fun () -> shutdown h));
      set_protocol_parameters h "liq" "Liquidsoap live" "*"

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

(*
let () =
  Lang.add_operator "input.voip"
    [
    ]
    ~category:Lang.Input
    ~descr:"Stream from voip calls using opal library."
    (fun p ->
       (* let e f v = f (List.assoc v p) in *)
         ((new input):>Source.source))
*)
