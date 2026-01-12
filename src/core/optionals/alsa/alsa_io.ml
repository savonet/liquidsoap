(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
open Alsa

exception Error of string

let handle lbl f x =
  try f x
  with e ->
    failwith
      (Printf.sprintf "Error while setting %s: %s" lbl (string_of_error e))

class virtual base ~buffer_size:buffer_size_seconds ~self_sync dev mode =
  let samples_per_second = Lazy.force Frame.audio_rate in
  let periods = Alsa_settings.periods#get in
  let buffer_size = Frame.audio_of_seconds buffer_size_seconds in
  object (self)
    method virtual log : Log.t
    method virtual audio_channels : int
    val mutable alsa_rate = -1
    val mutable pcm = None
    val mutable write = Pcm.writen_float
    val mutable read = Pcm.readn_float
    val mutable alsa_buffer_size = buffer_size
    method private alsa_buffer_size = alsa_buffer_size
    method virtual content_type : Frame.content_type
    val mutable gen = None

    method generator =
      match gen with
        | Some g -> g
        | None ->
            let g = Generator.create self#content_type in
            gen <- Some g;
            g

    method self_sync : Clock.self_sync =
      if self_sync then
        (`Dynamic, if pcm <> None then Some Alsa_settings.sync_source else None)
      else (`Static, None)

    method open_device =
      self#log#important "Using ALSA %s." (Alsa.get_version ());
      try
        let dev =
          match pcm with
            | None -> handle "open_pcm" (Pcm.open_pcm dev mode) []
            | Some d -> d
        in
        let params = Pcm.get_params dev in
        (try
           handle "access"
             (Pcm.set_access dev params)
             Pcm.Access_rw_noninterleaved;
           handle "format" (Pcm.set_format dev params) Pcm.Format_float
         with _ -> (
           (* If we can't get floats we fallback on interleaved s16le *)
           self#log#important "Falling back on interleaved S16LE";
           handle "format" (Pcm.set_format dev params) Pcm.Format_s16_le;
           try
             Pcm.set_access dev params Pcm.Access_rw_interleaved;
             write <-
               (fun pcm buf ofs len ->
                 let sbuf = Bytes.create (2 * len * Array.length buf) in
                 Audio.S16LE.of_audio buf ofs sbuf 0 len;
                 Pcm.writei pcm sbuf 0 len);
             read <-
               (fun pcm buf ofs len ->
                 let sbuf = Bytes.make (2 * 2 * len) (Char.chr 0) in
                 let r = Pcm.readi pcm sbuf 0 len in
                 Audio.S16LE.to_audio (Bytes.unsafe_to_string sbuf) 0 buf ofs r;
                 r)
           with Alsa.Invalid_argument ->
             self#log#important "Falling back on non-interleaved S16LE";
             handle "access"
               (Pcm.set_access dev params)
               Pcm.Access_rw_noninterleaved;
             write <-
               (fun pcm buf ofs len ->
                 let sbuf =
                   Array.init self#audio_channels (fun _ ->
                       Bytes.make (2 * len) (Char.chr 0))
                 in
                 for c = 0 to Audio.channels buf - 1 do
                   Audio.S16LE.of_audio [| buf.(c) |] ofs sbuf.(c) 0 len
                 done;
                 Pcm.writen pcm sbuf 0 len);
             read <-
               (fun pcm buf ofs len ->
                 let sbuf =
                   Array.init self#audio_channels (fun _ ->
                       Bytes.make (2 * len) (Char.chr 0))
                 in
                 let r = Pcm.readn pcm sbuf 0 len in
                 for c = 0 to Audio.channels buf - 1 do
                   Audio.S16LE.to_audio
                     (Bytes.unsafe_to_string sbuf.(c))
                     0
                     [| buf.(c) |]
                     ofs len
                 done;
                 r)));
        handle "channels" (Pcm.set_channels dev params) self#audio_channels;
        let rate =
          handle "rate" (Pcm.set_rate_near dev params samples_per_second) Dir_eq
        in
        alsa_buffer_size <-
          handle "buffer size"
            (Pcm.set_buffer_size_near dev params)
            (Frame.audio_of_main buffer_size);
        let periods =
          if periods > 0 then (
            handle "periods" (Pcm.set_periods dev params periods) Dir_eq;
            periods)
          else fst (Pcm.get_periods_max params)
        in
        alsa_rate <- rate;
        if rate <> samples_per_second then
          self#log#important
            "Could not set sample rate to 'frequency' (%d Hz), got %d."
            samples_per_second rate;
        if buffer_size <> alsa_buffer_size then
          self#log#important
            "Could not set buffer size to: %.02fs (%d samples), got: %.02f (%d \
             samples)."
            (Frame.seconds_of_audio buffer_size)
            buffer_size
            (Frame.seconds_of_audio alsa_buffer_size)
            alsa_buffer_size;
        self#log#important "Samplefreq=%dHz, Bufsize=%dB, Frame=%dB, Periods=%d"
          alsa_rate alsa_buffer_size
          (Pcm.get_frame_size params)
          periods;
        (try Pcm.set_params dev params
         with Alsa.Invalid_argument as e ->
           self#log#critical
             "Setting alsa parameters failed (invalid argument)!";
           raise e);
        handle "non-blocking" (Pcm.set_nonblock dev) false;
        pcm <- Some dev
      with Unknown_error _ as e -> raise (Error (string_of_error e))

    method close_device =
      match pcm with
        | Some d ->
            Pcm.close d;
            pcm <- None
        | None -> ()

    method reset =
      self#close_device;
      self#open_device
  end

class output ~buffer_size ~self_sync ~start ~infallible ~register_telnet dev
  val_source =
  let samples_per_second = Lazy.force Frame.audio_rate in
  let name = Printf.sprintf "alsa_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~register_telnet ~name ~output_kind:"output.alsa" val_source
          start

    inherit! base ~buffer_size ~self_sync dev [Pcm.Playback]
    val mutable samplerate_converter = None

    method samplerate_converter =
      match samplerate_converter with
        | Some samplerate_converter -> samplerate_converter
        | None ->
            let sc = Audio_converter.Samplerate.create self#audio_channels in
            samplerate_converter <- Some sc;
            sc

    method start = self#open_device

    method stop =
      (match (pcm, 0 < Generator.length self#generator) with
        | Some _, true ->
            self#write_frame
              (Generator.slice self#generator (Generator.length self#generator))
        | _ -> ());
      self#close_device

    method send_frame memo =
      let gen = self#generator in
      Generator.append gen memo;
      let buffer_size = Frame.main_of_audio self#alsa_buffer_size in
      if buffer_size <= Generator.length gen then
        self#write_frame (Generator.slice gen buffer_size)

    method private write_frame frame =
      let pcm = Option.get pcm in
      let buf = AFrame.pcm frame in
      let len = Audio.length buf in
      let buf, ofs, len =
        if alsa_rate = samples_per_second then (buf, 0, len)
        else
          Audio_converter.Samplerate.resample self#samplerate_converter
            (float alsa_rate /. float samples_per_second)
            buf 0 len
      in
      let rec pcm_write ofs len =
        if 0 < len then (
          let written = write pcm buf ofs len in
          pcm_write (ofs + written) (len - written))
      in
      try pcm_write ofs len
      with e ->
        begin match e with
          | Buffer_xrun ->
              self#log#severe
                "Underrun! You may minimize them by increasing the buffer size."
          | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover..";
          Pcm.recover pcm e;
          self#output)
        else raise e
  end

class input ~buffer_size ~self_sync ~start ~fallible dev =
  object (self)
    inherit base ~buffer_size ~self_sync dev [Pcm.Capture]

    inherit!
      Start_stop.active_source
        ~name:(Printf.sprintf "alsa_in(%s)" dev)
        ~fallible ~autostart:start () as active_source

    method private start = self#open_device
    method private stop = self#close_device
    method remaining = -1
    method abort_track = ()
    method effective_source = (self :> Source.source)
    method private can_generate_frame = active_source#started

    (* TODO: convert samplerate *)
    method private generate_frame =
      let pcm = Option.get pcm in
      let length = Lazy.force Frame.size in
      let alsa_buffer_size = self#alsa_buffer_size in
      let gen = self#generator in
      let format = Frame.Fields.find Frame.Fields.audio self#content_type in
      try
        while Generator.length gen < length do
          let c =
            Content.make ~length:(Frame.main_of_audio alsa_buffer_size) format
          in
          let read = read pcm (Content.Audio.get_data c) 0 alsa_buffer_size in
          Generator.put gen Frame.Fields.audio
            (Content.sub c 0 (Frame.main_of_audio read))
        done;
        Generator.slice gen length
      with e ->
        begin match e with
          | Buffer_xrun ->
              self#log#severe
                "Overrun! You may minimize them by increasing the buffer size."
          | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover..";
          Pcm.recover pcm e;
          self#generate_frame)
        else raise e
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "alsa"
    (Output.proto
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the ALSA driver." );
        ( "buffer_size",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some
            "ALSA buffer size in seconds. Defaults to frame duration when \
             `null`." );
        ( "device",
          Lang.string_t,
          Some (Lang.string "default"),
          Some "Alsa device to use" );
        ("", Lang.source_t frame_t, None, None);
      ])
    ~return_t:frame_t ~category:`Output ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
    ~descr:"Output the source's stream to an ALSA output device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let self_sync = e Lang.to_bool "self_sync" in
      let device = e Lang.to_string "device" in
      let source = List.assoc "" p in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let buffer_size =
        match
          Lang.to_valued_option Lang.to_float (List.assoc "buffer_size" p)
        with
          | None -> Lazy.force Frame.duration
          | Some v -> v
      in
      let start = Lang.to_bool (List.assoc "start" p) in
      (new output
         ~buffer_size ~self_sync ~infallible ~register_telnet ~start device
         source
        :> Output.output))

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "alsa"
    (Start_stop.active_source_proto ~fallible_opt:(`Yep false)
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the ALSA driver." );
        ( "buffer_size",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some
            "ALSA buffer size in seconds. Defaults to frame duration when \
             `null`." );
        ( "device",
          Lang.string_t,
          Some (Lang.string "default"),
          Some "Alsa device to use" );
      ])
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~return_t ~category:`Input ~descr:"Stream from an ALSA input device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let self_sync = e Lang.to_bool "self_sync" in
      let device = e Lang.to_string "device" in
      let buffer_size =
        match
          Lang.to_valued_option Lang.to_float (List.assoc "buffer_size" p)
        with
          | None -> Lazy.force Frame.duration
          | Some v -> v
      in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      (new input ~buffer_size ~self_sync ~fallible ~start device
        :> Start_stop.active_source))
