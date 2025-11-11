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

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "portaudio"
end)

let sync_source = SyncSource.make ()
let initialized = ref false

let () =
  Extra_args.add
    ( ["--list-portaudio-devices"],
      Arg.Unit
        (fun () ->
          Portaudio.init ();
          let c = Portaudio.get_device_count () in
          Printf.printf "Portaudio has %d devices:\n%!" c;
          let rec f p =
            if p < c then (
              let {
                Portaudio.d_name;
                d_host_api;
                d_max_input_channels;
                d_max_output_channels;
                d_default_low_input_latency;
                d_default_low_output_latency;
                d_default_high_input_latency;
                d_default_high_output_latency;
                d_default_sample_rate;
              } =
                Portaudio.get_device_info p
              in
              Printf.printf
                {|
Device ID %d:
- name: %s
- host API: %d
- max input channels: %d
- max output channels: %d
- default low input latency: %.02f
- default low output latency: %.02f
- default high input latency: %.02f
- default high output latency: %.02f
- default sample rate: %.02f
|}
                p d_name d_host_api d_max_input_channels d_max_output_channels
                d_default_low_input_latency d_default_low_output_latency
                d_default_high_input_latency d_default_high_output_latency
                d_default_sample_rate;
              f (p + 1))
          in
          f 0;
          Portaudio.terminate ();
          exit 0),
      "List all available portaudio devices" )

class virtual base =
  object (self)
    initializer
      if not !initialized then (
        Portaudio.init ();
        initialized := true)

    method virtual log : Log.t

    method get_device ~(mode : [ `Input | `Output ]) ~latency ~channels ~buflen
        ~device_name ~device_id =
      let samples_per_second = Lazy.force Frame.audio_rate in
      let device_id =
        match (device_name, device_id) with
          | Some _, Some id ->
              self#log#important
                "Both name and id were given for device, ignoring name.";
              Some id
          | None, Some id -> Some id
          | None, None -> None
          | Some name, None -> (
              let names =
                List.init (Portaudio.get_device_count ()) (fun i ->
                    ((Portaudio.get_device_info i).Portaudio.d_name, i))
              in
              match List.assoc_opt name names with
                | Some id -> Some id
                | None ->
                    let names =
                      String.concat ", "
                      @@ List.map Lang_string.quote_string
                      @@ List.map fst names
                    in
                    failwith
                      (Printf.sprintf
                         "Could not find portaudio device named %s, available \
                          devices are %s."
                         name names))
      in
      match device_id with
        | None ->
            Portaudio.open_default_stream ~format:Portaudio.format_float32 0
              channels samples_per_second buflen
        | Some device ->
            let device_info = Portaudio.get_device_info device in
            let inparams, outparams =
              match mode with
                | `Input ->
                    let latency =
                      Option.value
                        ~default:
                          device_info.Portaudio.d_default_high_output_latency
                        latency
                    in
                    ( Some
                        {
                          Portaudio.channels;
                          device;
                          sample_format = Portaudio.format_float32;
                          latency;
                        },
                      None )
                | `Output ->
                    let latency =
                      Option.value
                        ~default:
                          device_info.Portaudio.d_default_high_input_latency
                        latency
                    in
                    ( None,
                      Some
                        {
                          Portaudio.channels;
                          device;
                          sample_format = Portaudio.format_float32;
                          latency;
                        } )
            in
            Portaudio.open_stream inparams outparams (float samples_per_second)
              buflen []

    (* TODO: inline this to be more efficient? *)
    method handle lbl f =
      try f () with
        | Portaudio.Error n ->
            let bt = Printexc.get_raw_backtrace () in
            let exn =
              Failure
                (Printf.sprintf "Portaudio error in %s: %s" lbl
                   (Portaudio.string_of_error n))
            in
            Printexc.raise_with_backtrace exn bt
        | Portaudio.Unanticipated_host_error ->
            let n, s = Portaudio.get_last_host_error () in
            if n = 0 then
              self#log#important "Unanticipated host error in %s. (ignoring)"
                lbl
            else
              self#log#important
                "Unanticipated host error %d in %s: %s. (ignoring)" n lbl s
  end

class output ~self_sync ~start ~infallible ~register_telnet ~device_name
  ~device_id ~latency buflen val_source =
  object (self)
    inherit base

    inherit
      Output.output
        ~infallible ~register_telnet ~name:"output.portaudio"
          ~output_kind:"output.portaudio" val_source start

    val mutable stream = None

    method self_sync =
      if self_sync then
        (`Dynamic, if stream <> None then Some sync_source else None)
      else (`Static, None)

    method private open_device =
      self#handle "open_device" (fun () ->
          stream <-
            Some
              (self#get_device ~mode:`Output ~latency
                 ~channels:self#audio_channels ~buflen ~device_name ~device_id));
      self#handle "start_stream" (fun () ->
          Portaudio.start_stream (Option.get stream))

    method private close_device =
      match stream with
        | None -> ()
        | Some s ->
            Portaudio.close_stream s;
            stream <- None

    method start = self#open_device
    method stop = self#close_device

    method! reset =
      self#close_device;
      self#open_device

    method send_frame memo =
      let stream = Option.get stream in
      let buf = AFrame.pcm memo in
      self#handle "write_stream" (fun () ->
          let len = Audio.length buf in
          Portaudio.write_stream stream buf 0 len)
  end

class input ~self_sync ~start ~fallible ~device_name ~device_id ~latency buflen
  =
  object (self)
    inherit base

    inherit
      Start_stop.active_source
        ~name:"input.portaudio" ~fallible ~autostart:start () as active_source

    method private start = self#open_device
    method private stop = self#close_device
    val mutable stream = None

    method self_sync =
      if self_sync then
        (`Dynamic, if stream <> None then Some sync_source else None)
      else (`Static, None)

    method abort_track = ()
    method remaining = -1
    method seek_source = (self :> Source.source)
    method private can_generate_frame = active_source#started

    method private open_device =
      self#handle "open_device" (fun () ->
          stream <-
            Some
              (self#get_device ~mode:`Input ~latency
                 ~channels:self#audio_channels ~buflen ~device_name ~device_id));
      self#handle "start_stream" (fun () ->
          Portaudio.start_stream (Option.get stream))

    method private close_device =
      Portaudio.close_stream (Option.get stream);
      stream <- None

    method generate_frame =
      let size = Lazy.force Frame.size in
      let frame = Frame.create ~length:size self#content_type in
      let buf = Content.Audio.get_data (Frame.get frame Frame.Fields.audio) in
      let stream = Option.get stream in
      self#handle "read_stream" (fun () ->
          Portaudio.read_stream stream buf 0 (Array.length buf.(0)));
      Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data buf
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "portaudio"
    (Output.proto
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the portaudio driver."
        );
        ( "buflen",
          Lang.int_t,
          Some (Lang.int 256),
          Some "Length of a buffer in samples." );
        ( "device_id",
          Lang.nullable_t Lang.int_t,
          Some Lang.null,
          Some "Device ID. Uses default device if `null`." );
        ( "device_name",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "Device name." );
        ( "latency",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some "Device latency. Only used when specifying device ID." );
        ("", Lang.source_t frame_t, None, None);
      ])
    ~return_t:frame_t ~category:`Output ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
    ~descr:"Output the source's stream to a portaudio output device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let buflen = e Lang.to_int "buflen" in
      let device_name =
        Lang.to_valued_option Lang.to_string @@ List.assoc "device_name" p
      in
      let device_id =
        Lang.to_valued_option Lang.to_int (List.assoc "device_id" p)
      in
      let latency =
        Lang.to_valued_option Lang.to_float (List.assoc "latency" p)
      in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let source = List.assoc "" p in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      (new output
         ~start ~infallible ~register_telnet ~self_sync ~device_name ~device_id
         ~latency buflen source
        :> Output.output))

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "portaudio"
    (Start_stop.active_source_proto ~fallible_opt:(`Yep false)
    @ [
        ( "buflen",
          Lang.int_t,
          Some (Lang.int 256),
          Some "Length of a buffer in samples." );
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the portaudio driver."
        );
        ( "device_id",
          Lang.nullable_t Lang.int_t,
          Some Lang.null,
          Some "Device ID. Uses default device if `null`." );
        ( "device_name",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "Device name." );
        ( "latency",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some "Device latency. Only used when specifying device ID." );
      ])
    ~return_t ~category:`Input ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~descr:"Stream from a portaudio input device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let buflen = e Lang.to_int "buflen" in
      let device_name =
        Lang.to_valued_option Lang.to_string @@ List.assoc "device_name" p
      in
      let device_id =
        Lang.to_valued_option Lang.to_int (List.assoc "device_id" p)
      in
      let latency =
        Lang.to_valued_option Lang.to_float (List.assoc "latency" p)
      in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      new input
        ~self_sync ~start ~fallible ~device_name ~device_id ~latency buflen)
