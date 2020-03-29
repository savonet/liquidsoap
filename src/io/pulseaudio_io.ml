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

open Pulseaudio

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.clock "pulseaudio")

(** Error translator *)
let error_translator e =
  match e with
    | Pulseaudio.Error n ->
        Some
          (Printf.sprintf "Pulseaudio error: %s" (Pulseaudio.string_of_error n))
    | _ -> None

let () = Printexc.register_printer error_translator

class virtual base ~client ~device =
  let device = if device = "" then None else Some device in
  object
    val client_name = client

    val dev = device

    method virtual log : Log.t

    method self_sync = dev <> None
  end

class output ~infallible ~start ~on_start ~on_stop ~kind p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_string (List.assoc "device" p) in
  let name = Printf.sprintf "pulse_out(%s:%s)" client device in
  let val_source = List.assoc "" p in
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  object (self)
    inherit base ~client ~device

    inherit
      Output.output
        ~infallible ~on_stop ~on_start ~content_kind:kind ~name
          ~output_kind:"output.pulseaudio" val_source start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    val mutable stream = None

    method open_device =
      let ss =
        {
          sample_format = Sample_format_float32le;
          sample_rate = samples_per_second;
          sample_chans = channels;
        }
      in
      stream <-
        Some
          (Pulseaudio.Simple.create ~client_name ~stream_name:self#id ?dev
             ~dir:Dir_playback ~sample:ss ())

    method close_device =
      match stream with
        | None -> ()
        | Some s ->
            Pulseaudio.Simple.free s;
            stream <- None

    method output_start = self#open_device

    method output_stop = self#close_device

    method output_reset =
      self#close_device;
      self#open_device

    method output_send memo =
      let stream = Utils.get_some stream in
      let buf = AFrame.content memo in
      let chans = Array.length buf in
      let len = Audio.length buf in
      let buf' =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (chans * len)
      in
      for c = 0 to chans - 1 do
        let bufc = buf.(c) in
        for i = 0 to len - 1 do
          Bigarray.Array1.unsafe_set buf'
            ((i * chans) + c)
            (Bigarray.Array1.unsafe_get bufc i)
        done
      done;
      Simple.write_ba stream buf'
  end

class input ~kind p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_string (List.assoc "device" p) in
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
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.input
        ~content_kind:kind ~source_kind:"pulse"
        ~name:(Printf.sprintf "pulse_in(%s)" device)
        ~on_start ~on_stop ~autostart:start ~fallible as super

    inherit base ~client ~device

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    method private start = self#open_device

    method private stop = self#close_device

    method output_reset =
      self#close_device;
      self#open_device

    val mutable stream = None

    method private open_device =
      let ss =
        {
          sample_format = Sample_format_float32le;
          sample_rate = samples_per_second;
          sample_chans = channels;
        }
      in
      stream <-
        Some
          (Pulseaudio.Simple.create ~client_name ~stream_name:self#id
             ~dir:Dir_record ?dev ~sample:ss ())

    method private close_device =
      Pulseaudio.Simple.free (Utils.get_some stream);
      stream <- None

    method input frame =
      assert (0 = AFrame.position frame);
      let stream = Utils.get_some stream in
      let len = AFrame.size () in
      let ibuf =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
          (channels * len)
      in
      let buf = AFrame.content frame in
      Simple.read_ba stream ibuf;
      for c = 0 to channels - 1 do
        let bufc = buf.(c) in
        for i = 0 to len - 1 do
          bufc.{i} <- Bigarray.Array1.unsafe_get ibuf ((i * channels) + c)
        done
      done;
      AFrame.add_break frame (AFrame.size ())
  end

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_with ~audio:1 ()) in
  let proto =
    [
      ("client", Lang.string_t, Some (Lang.string "liquidsoap"), None);
      ( "device",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Device to use. Uses default if set to \"\"." );
      ( "clock_safe",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Force the use of the dedicated Pulseaudio clock." );
    ]
  in
  Lang.add_operator "output.pulseaudio" ~active:true
    (Output.proto @ proto @ [("", Lang.source_t k, None, None)])
    ~kind:(Lang.Unconstrained k) ~category:Lang.Output
    ~descr:"Output the source's stream to a portaudio output device."
    (fun p kind ->
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
      (new output ~infallible ~on_start ~on_stop ~start ~kind p :> Source.source));
  Lang.add_operator "input.pulseaudio" ~active:true
    (Start_stop.input_proto @ proto)
    ~kind:(Lang.Unconstrained k) ~category:Lang.Input
    ~descr:"Stream from a portaudio input device." (fun p kind ->
      (new input ~kind p :> Source.source))
