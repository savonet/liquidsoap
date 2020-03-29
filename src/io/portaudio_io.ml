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

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.clock "portaudio")

let initialized = ref false

class virtual base =
  object (self)
    initializer
    if not !initialized then (
      Portaudio.init ();
      initialized := true )

    method virtual log : Log.t

    (* TODO: inline this to be more efficient? *)
    method handle lbl f =
      try f () with
        | Portaudio.Error n ->
            failwith
              (Printf.sprintf "Portaudio error in %s: %s" lbl
                 (Portaudio.string_of_error n))
        | Portaudio.Unanticipated_host_error ->
            let n, s = Portaudio.get_last_host_error () in
            if n = 0 then
              self#log#important "Unanticipated host error in %s. (ignoring)"
                lbl
            else
              self#log#important
                "Unanticipated host error %d in %s: %s. (ignoring)" n lbl s
  end

class output ~kind ~clock_safe ~start ~on_start ~on_stop ~infallible buflen
  val_source =
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit base

    inherit
      Output.output
        ~infallible ~on_stop ~on_start ~content_kind:kind
          ~name:"output.portaudio" ~output_kind:"output.portaudio" val_source
          start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    val mutable stream = None

    method self_sync = stream <> None

    method private open_device =
      self#handle "open_default_stream" (fun () ->
          stream <-
            Some
              (Portaudio.open_default_stream 0 channels samples_per_second
                 buflen));
      self#handle "start_stream" (fun () ->
          Portaudio.start_stream (Utils.get_some stream))

    method private close_device =
      match stream with
        | None -> ()
        | Some s ->
            Portaudio.close_stream s;
            stream <- None

    method output_start = self#open_device

    method output_stop = self#close_device

    method output_reset =
      self#close_device;
      self#open_device

    method output_send memo =
      let stream = Utils.get_some stream in
      let buf = AFrame.content memo in
      self#handle "write_stream" (fun () ->
          let len = Audio.length buf in
          (* TODO: non-interleaved format does not seem to be supported here *)
          (*
           let ba = Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout channels len in
           for c = 0 to channels - 1 do
             Bigarray.Array1.blit buf.(c) (Bigarray.Array2.slice_left ba c)
           done;
           let ba = Bigarray.genarray_of_array2 ba in
           *)
          let ba = Bigarray.genarray_of_array1 (Audio.interleave buf) in
          Portaudio.write_stream_ba stream ba 0 len)
  end

class input ~kind ~clock_safe ~start ~on_start ~on_stop ~fallible buflen =
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit base

    inherit
      Start_stop.input
        ~content_kind:kind ~source_kind:"portaudio" ~name:"portaudio_in"
          ~on_start ~on_stop ~fallible ~autostart:start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    method private start = self#open_device

    method private stop = self#close_device

    val mutable stream = None

    method self_sync = stream <> None

    method private open_device =
      self#handle "open_default_stream" (fun () ->
          stream <-
            Some
              (Portaudio.open_default_stream channels 0 samples_per_second
                 buflen));
      self#handle "start_stream" (fun () ->
          Portaudio.start_stream (Utils.get_some stream))

    method private close_device =
      Portaudio.close_stream (Utils.get_some stream);
      stream <- None

    method output_reset =
      self#close_device;
      self#open_device

    method input frame =
      assert (0 = AFrame.position frame);
      let stream = Utils.get_some stream in
      let buf = AFrame.content frame in
      self#handle "read_stream" (fun () ->
          let len = Audio.length buf in
          let ibuf =
            Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
              (channels * len)
          in
          Portaudio.read_stream_ba stream
            (Bigarray.genarray_of_array1 ibuf)
            0 len;
          for c = 0 to channels - 1 do
            let bufc = buf.(c) in
            for i = 0 to len - 1 do
              bufc.{i} <- Bigarray.Array1.unsafe_get ibuf ((i * channels) + c)
            done
          done);
      AFrame.add_break frame (AFrame.size ())
  end

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_with ~audio:1 ()) in
  Lang.add_operator "output.portaudio" ~active:true
    ( Output.proto
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated Portaudio clock." );
        ( "buflen",
          Lang.int_t,
          Some (Lang.int 256),
          Some "Length of a buffer in samples." );
        ("", Lang.source_t k, None, None);
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Output
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
      ( new output
          ~kind ~start ~on_start ~on_stop ~infallible ~clock_safe buflen source
        :> Source.source ));
  Lang.add_operator "input.portaudio"
    ( Start_stop.input_proto
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated Portaudio clock." );
        ( "buflen",
          Lang.int_t,
          Some (Lang.int 256),
          Some "Length of a buffer in samples." );
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Input
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
      ( new input ~kind ~clock_safe ~start ~on_start ~on_stop ~fallible buflen
        :> Source.source ))
