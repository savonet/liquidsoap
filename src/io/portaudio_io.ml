(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.self_sync "pa")

let initialized = ref false

class virtual base =
object (self)
  initializer
    if not !initialized then begin
      Portaudio.init ();
      initialized := true
    end

  method virtual log : Dtools.Log.t

  (* TODO: inline this to be more efficient? *)
  method handle lbl f =
    try f () with
      | Portaudio.Error n ->
          failwith
            (Printf.sprintf
               "Portaudio error in %s: %s" lbl (Portaudio.string_of_error n))
      | Portaudio.Unanticipated_host_error ->
          let n, s = Portaudio.get_last_host_error () in
            if n = 0 then
              self#log#f 3
                "Unanticipated host error in %s. (ignoring)" lbl
            else
              self#log#f 3
                "Unanticipated host error %d in %s: %s. (ignoring)" n lbl s
end

class output ~kind ~clock_safe ~start ~on_start 
             ~on_stop ~infallible buflen val_source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)

  inherit base
  inherit
    Output.output
      ~infallible ~on_stop ~on_start ~content_kind:kind
      ~name:"output.portaudio" ~output_kind:"output.portaudio" val_source start
    as super

  method private set_clock =
    super#set_clock ;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((get_clock ()):>Clock.clock))

  val mutable stream = None

  method private open_device =
    self#handle
      "open_default_stream"
      (fun () ->
         stream <- Some (Portaudio.open_default_stream
                           0 channels samples_per_second buflen));
    self#handle "start_stream"
      (fun () -> Portaudio.start_stream (Utils.get_some stream))

  method private close_device = 
    match stream with
      | None -> ()
      | Some s -> 
          Portaudio.close_stream s ;
          stream <- None

  method output_start =
    if clock_safe then
      (get_clock ())#register_blocking_source ;
    self#open_device

  method output_stop =
    if clock_safe then
      (get_clock ())#unregister_blocking_source ;
    self#close_device

  method output_reset = 
    self#close_device ;
    self#open_device

  method output_send memo =
    let stream = Utils.get_some stream in
    let buf = AFrame.content memo 0 in
      self#handle "write_stream"
        (fun () -> Portaudio.write_stream stream buf 0 (Array.length buf.(0)))

end

class input ~kind ~clock_safe ~start ~on_start ~on_stop ~fallible buflen =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)

  inherit base
  inherit
    Start_stop.input
      ~content_kind:kind
      ~source_kind:"portaudio"
      ~name:"portaudio_in"
      ~on_start ~on_stop
      ~fallible ~autostart:start
    as super

  method private set_clock =
    super#set_clock ;
    if clock_safe then
      Clock.unify self#clock (Clock.create_known ((get_clock ()):>Clock.clock))

  method private start =
    if clock_safe then
      (get_clock ())#register_blocking_source ;
    self#open_device

  method private stop =
    if clock_safe then
      (get_clock ())#unregister_blocking_source ;
    self#close_device

  val mutable stream = None

  method private open_device =
    self#handle
      "open_default_stream"
      (fun () ->
         stream <-
           Some (Portaudio.open_default_stream
                   channels 0 samples_per_second buflen));
    self#handle
      "start_stream"
      (fun () -> Portaudio.start_stream (Utils.get_some stream))

  method private close_device = 
    Portaudio.close_stream (Utils.get_some stream) ;
    stream <- None

  method output_reset =
    self#close_device ;
    self#open_device

  method input frame =
    assert (0 = AFrame.position frame) ;
    let stream = Utils.get_some stream in
    let buf = AFrame.content_of_type ~channels frame 0 in
      self#handle
        "read_stream"
        (fun () -> Portaudio.read_stream stream buf 0 (Array.length buf.(0)));
      AFrame.add_break frame (AFrame.size ())

end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ())
  in
  Lang.add_operator "output.portaudio"
    (Output.proto @ [
      "clock_safe", Lang.bool_t, Some (Lang.bool true),
        Some "Force the use of the dedicated Portaudio clock." ;
      "buflen", Lang.int_t, Some (Lang.int 256),
        Some "Length of a buffer in samples.";
      "", Lang.source_t k, None, None
    ])
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Output
    ~descr:"Output the source's stream to a portaudio output device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let buflen = e Lang.to_int "buflen" in
       let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
       let start = Lang.to_bool (List.assoc "start" p) in
       let on_start =
         let f = List.assoc "on_start" p in
           fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
       let on_stop =
         let f = List.assoc "on_stop" p in
           fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
       let source = List.assoc "" p in
       let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
         ((new output ~kind ~start ~on_start ~on_stop ~infallible 
                      ~clock_safe buflen source):>Source.source)) ;

  Lang.add_operator "input.portaudio"
    (Start_stop.input_proto @ [
      "clock_safe", Lang.bool_t, Some (Lang.bool true),
        Some "Force the use of the dedicated Portaudio clock." ;
      "buflen", Lang.int_t, Some (Lang.int 256),
        Some "Length of a buffer in samples.";
    ])
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Stream from a portaudio input device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let buflen = e Lang.to_int "buflen" in
       let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
       let start = Lang.to_bool (List.assoc "start" p) in
       let fallible = Lang.to_bool (List.assoc "fallible" p) in
       let on_start =
         let f = List.assoc "on_start" p in
           fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
       let on_stop =
         let f = List.assoc "on_stop" p in
           fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
         ((new input ~kind ~clock_safe
             ~start ~on_start ~on_stop ~fallible buflen):>Source.source))
