(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "pulseaudio"
end)

let sync_source = SyncSource.make ()

(** Error translator *)
let error_translator e =
  match e with
    | Pulseaudio.Error n ->
        Some
          (Printf.sprintf "Pulseaudio error: %s" (Pulseaudio.string_of_error n))
    | _ -> None

let () = Printexc.register_printer error_translator

class virtual base ~self_sync ~client ~device =
  object
    val client_name = client
    val dev = device
    val mutable stream = None

    method self_sync : Clock.self_sync =
      if self_sync then
        (`Dynamic, if stream <> None then Some sync_source else None)
      else (`Static, None)
  end

let log = Log.make ["pulseaudio"]

class output ~infallible ~register_telnet ~start p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_valued_option Lang.to_string (List.assoc "device" p) in
  let device =
    if device = Some "" then (
      log#important
        "Empty device name \"\" is deprecated! Please use `null` instead..";
      None)
    else device
  in
  let retry_delay = Lang.to_float (List.assoc "retry_delay" p) in
  let on_error = List.assoc "on_error" p in
  let on_error s = ignore (Lang.apply on_error [("", Lang.string s)]) in
  let name =
    Printf.sprintf "pulse_out(%s:%s)" client
      (match device with None -> "(default)" | Some s -> s)
  in
  let val_source = List.assoc "" p in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
  object (self)
    inherit base ~self_sync ~client ~device

    inherit
      Output.output
        ~infallible ~register_telnet ~name ~output_kind:"output.pulseaudio"
          val_source start

    val mutable last_try = 0.

    method private open_device =
      let now = Unix.gettimeofday () in
      try
        if last_try +. retry_delay < now then (
          last_try <- now;
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
                 ~dir:Dir_playback ~sample:ss ()))
      with exn ->
        let bt = Printexc.get_backtrace () in
        let error =
          Printf.sprintf "Failed to open pulse audio device: %s"
            (Printexc.to_string exn)
        in
        on_error error;
        Utils.log_exception ~log:self#log ~bt error

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
      if stream = None then self#open_device;
      match stream with
        | Some stream -> (
            let buf = AFrame.pcm memo in
            let len = Audio.length buf in
            try Simple.write stream buf 0 len
            with exn ->
              let bt = Printexc.get_backtrace () in
              last_try <- Unix.gettimeofday ();
              self#close_device;
              let error =
                Printf.sprintf "Failed to send pulse audio data: %s"
                  (Printexc.to_string exn)
              in
              on_error error;
              Utils.log_exception ~log:self#log ~bt error)
        | None -> ()
  end

class input p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_valued_option Lang.to_string (List.assoc "device" p) in
  let device =
    if device = Some "" then (
      log#important
        "Empty device name \"\" is deprecated! Please use `null` instead..";
      None)
    else device
  in
  let retry_delay = Lang.to_float (List.assoc "retry_delay" p) in
  let on_error = List.assoc "on_error" p in
  let on_error s = ignore (Lang.apply on_error [("", Lang.string s)]) in
  let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
  let start = Lang.to_bool (List.assoc "start" p) in
  let fallible = Lang.to_bool (List.assoc "fallible" p) in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.active_source
        ~name:"input.pulseaudio" ~autostart:start ~fallible () as active_source

    inherit base ~self_sync ~client ~device
    method private start = self#open_device
    method private stop = self#close_device
    method remaining = -1
    method abort_track = ()
    method seek_source = (self :> Source.source)

    method private can_generate_frame =
      match (active_source#started, stream) with
        | true, None ->
            self#open_device;
            stream <> None
        | v, _ -> v

    val mutable last_try = 0.

    method private open_device =
      let now = Unix.gettimeofday () in
      if last_try +. retry_delay < now then (
        last_try <- now;
        let ss =
          {
            sample_format = Sample_format_float32le;
            sample_rate = samples_per_second;
            sample_chans = self#audio_channels;
          }
        in
        try
          stream <-
            Some
              (Pulseaudio.Simple.create ~client_name ~stream_name:self#id
                 ~dir:Dir_record ?dev ~sample:ss ())
        with exn when fallible ->
          let bt = Printexc.get_backtrace () in
          let error =
            Printf.sprintf "Error while connecting to pulseaudio: %s"
              (Printexc.to_string exn)
          in
          on_error error;
          Utils.log_exception ~log:self#log ~bt error)

    method private close_device =
      match stream with
        | Some device ->
            Pulseaudio.Simple.free device;
            stream <- None
        | None -> ()

    method generate_frame =
      try
        let size = Lazy.force Frame.size in
        let frame = Frame.create ~length:size self#content_type in
        let buf = Content.Audio.get_data (Frame.get frame Frame.Fields.audio) in
        let stream = Option.get stream in
        Simple.read stream buf 0 (Frame.audio_of_main size);
        Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data buf
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        last_try <- Unix.gettimeofday ();
        self#close_device;
        if fallible then (
          let error =
            Printf.sprintf "Error while reading from pulseaudio: %s"
              (Printexc.to_string exn)
          in
          on_error error;
          Utils.log_exception ~log:self#log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            error;
          self#empty_frame)
        else Printexc.raise_with_backtrace exn bt
  end

let on_error =
  Lang.eval ~cache:false ~typecheck:false ~stdlib:`Disabled "fun (_) -> ()"

let proto =
  [
    ("client", Lang.string_t, Some (Lang.string "liquidsoap"), None);
    ( "device",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "Device to use. Uses default if set to `null`." );
    ( "retry_delay",
      Lang.float_t,
      Some (Lang.float 1.),
      Some "When fallible, time to wait before trying to connect again." );
    ( "on_error",
      Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
      Some on_error,
      Some
        "Function executed when an operation with the pulseaudio server \
         returns an error." );
    ( "self_sync",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "Mark the source as being synchronized by the pulseaudio driver." );
  ]

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "pulseaudio"
    (Output.proto @ proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:Output.meth
    ~callbacks:Output.callbacks
    ~descr:"Output the source's stream to a pulseaudio output device."
    (fun p ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      (new output ~infallible ~register_telnet ~start p :> Output.output))

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "pulseaudio"
    (Start_stop.active_source_proto ~fallible_opt:(`Yep true) @ proto)
    ~return_t ~category:`Input ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~descr:"Stream from a pulseaudio input device."
    (fun p -> new input p)
