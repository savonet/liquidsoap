(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

open Mm
open Pulseaudio

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> Clock.clock "pulseaudio")

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
    method self_sync : Source.self_sync = (`Dynamic, dev <> None)
  end

class output ~infallible ~register_telnet ~start ~on_start ~on_stop p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_string (List.assoc "device" p) in
  let name = Printf.sprintf "pulse_out(%s:%s)" client device in
  let val_source = List.assoc "" p in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  object (self)
    inherit base ~client ~device

    inherit!
      Output.output
        ~infallible ~register_telnet ~on_stop ~on_start ~name
          ~output_kind:"output.pulseaudio" val_source start as super

    method! private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify ~pos:self#pos self#clock
          (Clock.create_known (get_clock () :> Source.clock))

    val mutable stream = None

    method open_device =
      let ss =
        {
          sample_format = Sample_format_float32le;
          sample_rate = samples_per_second;
          sample_chans = self#audio_channels;
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

    method start = self#open_device
    method stop = self#close_device

    method! reset =
      self#close_device;
      self#open_device

    method send_frame memo =
      let stream = Option.get stream in
      let buf = AFrame.pcm memo in
      let len = Audio.length buf in
      Simple.write stream buf 0 len
  end

class input p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_string (List.assoc "device" p) in
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  let start = Lang.to_bool (List.assoc "start" p) in
  let fallible = Lang.to_bool (List.assoc "fallible" p) in
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply f [])
  in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.active_source
        ~get_clock ~name:"input.pulseaudio" ~clock_safe ~on_start ~on_stop
          ~autostart:start ~fallible ()

    inherit base ~client ~device
    method private start = self#open_device
    method private stop = self#close_device
    val mutable stream = None
    method remaining = -1
    method abort_track = ()
    method seek_source = (self :> Source.source)

    method private open_device =
      let ss =
        {
          sample_format = Sample_format_float32le;
          sample_rate = samples_per_second;
          sample_chans = self#audio_channels;
        }
      in
      stream <-
        Some
          (Pulseaudio.Simple.create ~client_name ~stream_name:self#id
             ~dir:Dir_record ?dev ~sample:ss ())

    method private close_device =
      Pulseaudio.Simple.free (Option.get stream);
      stream <- None

    method generate_frame =
      let size = Lazy.force Frame.size in
      let frame = Frame.create ~length:size self#content_type in
      let buf = Content.Audio.get_data (Frame.get frame Frame.Fields.audio) in
      let stream = Option.get stream in
      Simple.read stream buf 0 (Frame.audio_of_main size);
      Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data buf
  end

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

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "pulseaudio"
    (Output.proto @ proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:Output.meth
    ~descr:"Output the source's stream to a pulseaudio output device."
    (fun p ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      (new output ~infallible ~register_telnet ~on_start ~on_stop ~start p
        :> Output.output))

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "pulseaudio"
    (Start_stop.active_source_proto ~clock_safe:true ~fallible_opt:(`Yep false)
    @ proto)
    ~return_t ~category:`Input ~meth:(Start_stop.meth ())
    ~descr:"Stream from a pulseaudio input device."
    (fun p -> new input p)
